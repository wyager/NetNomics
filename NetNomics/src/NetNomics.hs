{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module NetNomics (
    encodePreferences, decodePreferences, mkPreferences
    encodeConnectionDetails, decodeConnectionDetails, mkConnectionDetails
    utility
) where

-- Can use standard Prelude functions like "P.length"
import qualified Prelude as P

import Data.Word (Word32)

import Numeric.Units.Dimensional.Prelude
import qualified Numeric.Units.Dimensional.UnitNames as Names
import Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))

import Data.Serialize (Serialize, put, get, putWord32be, getWord32be, encode, decode)
import Data.ByteString (ByteString)

data Bounds unit num = Bounds {
        min :: Quantity unit num,
        max :: Quantity unit num
    }

-- If this stuff tickles your fancy, read about Lie Groups and their Algebras
newtype Logarithmic (unit :: Dimension) num = Logarithmic (Dimensionless num) deriving (Show, Functor)

exp' :: Floating num => Bounds unit num -> Logarithmic unit num -> Quantity unit num
exp' (Bounds min max) (Logarithmic x) = min * ((max / min) ** x)

log' :: Floating num => Bounds unit num -> Quantity unit num -> Logarithmic unit num
log' (Bounds min max) q = Logarithmic (log (q / min) / log (max / min))

type DBandwidth = DAmountOfSubstance / DTime
-- Dimensional doesn't have "bit" as a primitive unit so we're using moles as bits.
bit = mole

bandwidthBounds :: Floating num => Bounds DBandwidth num
bandwidthBounds = Bounds (1 *~ (milli bit / second)) (1 *~ (yotta bit / second))

latencyBounds :: Floating num => Bounds DTime num
latencyBounds = Bounds (1 *~ nano second) (10 *~ giga second)

deviationBounds :: Floating num => Bounds DTime num
deviationBounds = Bounds (0.1 *~ nano second) (1 *~ giga second)

packetLossBounds :: Floating num => Bounds DOne num
packetLossBounds = Bounds (1e-30 *~ one) (1 *~ one)

selectivityBounds :: Floating num => Bounds DOne num
selectivityBounds = Bounds (0.1 *~ one) (1000 *~ one)

data Preference (unit :: Dimension) num = Preference {
        center :: Logarithmic unit num,
        selectivity :: Logarithmic DOne num
    } deriving (Functor, Foldable, Traversable)

data Direction = PreferBigger | PreferSmaller

signOf :: Num num => Direction -> Dimensionless num
signOf PreferBigger = negate _1
signOf PreferSmaller = _1

singleUtility :: Floating num => Bounds unit num -> Direction -> Preference unit num -> Logarithmic unit num -> Dimensionless num
singleUtility bounds direction (Preference center selectivity) value = _1 / (_1 + exponent)
    where 
    exponent = (exp' bounds value / exp' bounds center) ** (signOf direction * exp' selectivityBounds selectivity)


data Preferences num = Preferences {
        bandwidthPreference :: Preference DBandwidth num,
        latencyPreference :: Preference DTime num,
        deviationPreference :: Preference DTime num,
        packetLossPreference :: Preference DOne num
    } deriving (Functor, Foldable, Traversable)

data ConnectionDetails num = ConnectionDetails {
        bandwidth :: Logarithmic DBandwidth num,
        latency :: Logarithmic DTime num,
        deviation :: Logarithmic DTime num,
        packetLoss :: Logarithmic DOne num
    } deriving (Functor, Foldable, Traversable)

utility :: Floating num => Preferences num -> ConnectionDetails num -> Dimensionless num
utility (Preferences pb pl pd pp) (ConnectionDetails cb cl cd cp) = ub * ul * ud * up
    where
    ub = singleUtility bandwidthBounds  PreferBigger  pb cb
    ul = singleUtility latencyBounds    PreferSmaller pl cl
    ud = singleUtility deviationBounds  PreferSmaller pd cd
    up = singleUtility packetLossBounds PreferSmaller pp cp

instance Serialize a => Serialize (Logarithmic unit a) where
    put (Logarithmic (Quantity x)) = put x
    get = (Logarithmic . Quantity) <$> get

instance Foldable (Logarithmic unit) where
    foldr f b (Logarithmic (Quantity x)) = f x b

instance Traversable (Logarithmic unit) where
    traverse f (Logarithmic (Quantity x)) = (Logarithmic . Quantity) <$> f x

instance Serialize a => Serialize (Preference unit a) where
    put (Preference c s) = put c >> put s
    get = Preference <$> get <*> get

instance Serialize a => Serialize (Preferences a) where
    put (Preferences b l d p) = put b >> put l >> put d >> put p
    get = Preferences <$> get <*> get <*> get <*> get

instance Serialize a => Serialize (ConnectionDetails a) where
    put (ConnectionDetails b l d p) = put b >> put l >> put d >> put p
    get = ConnectionDetails <$> get <*> get <*> get <*> get

data Q0_32 = Q0_32 Word32 deriving Show

instance Serialize Q0_32 where
    put (Q0_32 x) = putWord32be x
    get = Q0_32 <$> getWord32be

w32_max_as_double :: Double
w32_max_as_double = fromIntegral (maxBound :: Word32)

-- From 0 to 1 - 2^-32
q0_32_to_double :: Q0_32 -> Double
q0_32_to_double (Q0_32 x) = (fromIntegral x) P./ (1 P.+ w32_max_as_double)

q0_32_from_double :: Double -> Either String Q0_32
q0_32_from_double d
    | d < 0 = Left "Cannot encode negative value"
    | d > q0_32_to_double (Q0_32 maxBound) = Left "Cannot encode value greater than Q0.32 maxBound"
    | otherwise = Right $ Q0_32 $ round $ d P.* w32_max_as_double

encodePreferences :: Preferences Double -> Either String ByteString
encodePreferences prefs = encode <$> traverse q0_32_from_double prefs

decodePreferences :: ByteString -> Either String (Preferences Double)
decodePreferences bytes = fmap q0_32_to_double <$> decode bytes

encodeConnectionDetails :: ConnectionDetails Double -> Either String ByteString
encodeConnectionDetails conn = encode <$> traverse q0_32_from_double conn

decodeConnectionDetails :: ByteString -> Either String (ConnectionDetails Double)
decodeConnectionDetails bytes = fmap q0_32_to_double <$> decode bytes


mkConnectionDetails :: Double -> Double -> Double -> Double -> ConnectionDetails Double
mkConnectionDetails b l d p = ConnectionDetails {
        bandwidth = log' bandwidthBounds (b *~ (bit / second)),
        latency = log' latencyBounds (b *~ second),
        deviation = log' deviationBounds (b *~ second),
        packetLoss = log' packetLossBounds (b *~ one)
    }

mkPreferences :: Double -> Double 
              -> Double -> Double 
              -> Double -> Double 
              -> Double -> Double 
              -> Preferences Double
mkPreferences cb sb cl sl cd sd cp sp = Preferences {
        bandwidthPreference = Preference {
            center = log' bandwidthBounds (cb *~ (bit / second)),
            selectivity = log' selectivityBounds (sb *~ one)
        },
        latencyPreference = Preference {
            center = log' latencyBounds (cl *~ second),
            selectivity = log' selectivityBounds (sl *~ one )
        },
        deviationPreference = Preference {
            center = log' deviationBounds (cd *~ second),
            selectivity = log' selectivityBounds (sd *~ one)
        },
        packetLossPreference = Preference {
            center = log' packetLossBounds (cp *~ one),
            selectivity = log' selectivityBounds (sp *~ one)
        }
    }

examplePreferences :: Preferences Double
examplePreferences = Preferences {
        bandwidthPreference = Preference {
            center = log' bandwidthBounds (100 *~ (mega bit / second)),
            selectivity = log' selectivityBounds (0.5 *~ one)
        },
        latencyPreference = Preference {
            center = log' latencyBounds (10 *~ milli second),
            selectivity = log' selectivityBounds _1
        },
        deviationPreference = Preference {
            center = log' deviationBounds (3 *~ milli second),
            selectivity = log' selectivityBounds (0.8 *~ one)
        },
        packetLossPreference = Preference {
            center = log' packetLossBounds (0.0005 *~ one),
            selectivity = log' selectivityBounds (10 *~ one)
        }
    }



