{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
module NetNomics.Serialize (
    serializePreferences, serializeConnectionDetails,
    deserializePreferences, deserializeConnectionDetails
) where

-- Can use standard Prelude functions like "P.length"
import qualified Prelude as P

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))

import Data.Word (Word32)
import Data.Serialize (Serialize, put, get, putWord32be, getWord32be, encode, decode)
import Data.ByteString (ByteString)

import NetNomics (ConnectionDetails(..), Preferences(..), Preference(..), DBandwidth, bit)

-- Tools to compress all the values in the protocol to [0,1)

data Bounds unit num = Bounds {
        min :: Quantity unit num,
        max :: Quantity unit num
    }

-- If this stuff tickles your fancy, read about Lie Groups and their Algebras
newtype Logarithmic (unit :: Dimension) num = Logarithmic (Dimensionless num) 

exp' :: Floating num => Bounds unit num -> Logarithmic unit num -> Quantity unit num
exp' (Bounds min max) (Logarithmic x) = min * ((max / min) ** x)

log' :: Floating num => Bounds unit num -> Quantity unit num -> Logarithmic unit num
log' (Bounds min max) q = Logarithmic (log (q / min) / log (max / min))


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


-- Tools to (de)serialize the compressed values

data Fixed = Fixed Word32 deriving Show

instance Serialize Fixed where
    put (Fixed x) = putWord32be x
    get = Fixed <$> getWord32be

w32MaxAsDouble :: Double
w32MaxAsDouble = fromIntegral (maxBound :: Word32)

-- From 0 to 1 - 2^-32
fixedToDouble :: Fixed -> Double
fixedToDouble (Fixed x) = (fromIntegral x) P./ (1 P.+ w32MaxAsDouble)

data FixedError = FixedUnderflow | FixedOverflow
fixedFromDouble :: Double -> Either FixedError Fixed
fixedFromDouble d
    | d < 0 = Left FixedUnderflow
    | d > fixedToDouble (Fixed maxBound) = Left FixedOverflow
    | otherwise = Right $ Fixed $ round $ d P.* w32MaxAsDouble

data QuantityError = QuantityNonPositive | QuantityUnderflow | QuantityOverflow
encodeQuantity :: Bounds unit Double -> Quantity unit Double -> Either QuantityError Fixed
encodeQuantity bounds quantity
    | quantity <= _0 = Left QuantityNonPositive
    | otherwise = 
        let Logarithmic value = log' bounds quantity in
        case fixedFromDouble (value /~ one) of
            Left FixedOverflow -> Left QuantityOverflow
            Left FixedUnderflow -> Left QuantityUnderflow
            Right fixed -> Right fixed

encodePreference :: Bounds unit Double -> Preference unit Double -> Either QuantityError (Fixed, Fixed)
encodePreference bounds (Preference center selectivity) = do
    center <- encodeQuantity bounds center
    selectivity <- encodeQuantity selectivityBounds selectivity
    return (center, selectivity)

encodePreferences :: Preferences Double -> Either QuantityError (Fixed, Fixed, Fixed, Fixed, Fixed, Fixed, Fixed, Fixed)
encodePreferences (Preferences b l d p) = do
    (cb, sb) <- encodePreference bandwidthBounds   b
    (cl, sl) <- encodePreference latencyBounds     l
    (cd, sd) <- encodePreference deviationBounds   d
    (cp, sp) <- encodePreference selectivityBounds p
    return (cb, sb, cl, sl, cd, sd, cp, sp)

decodePreferences :: (Fixed, Fixed, Fixed, Fixed, Fixed, Fixed, Fixed, Fixed) -> Preferences Double
decodePreferences (cb, sb, cl, sl, cd, sd, cp, sp) 
    = Preferences 
        (decodePreference bandwidthBounds   cb sb)
        (decodePreference latencyBounds     cl sl)
        (decodePreference deviationBounds   cd sd)
        (decodePreference selectivityBounds cp sp)

decodePreference :: Bounds unit Double -> Fixed -> Fixed -> Preference unit Double
decodePreference bounds c s = Preference (decodeQuantity bounds c) (decodeQuantity selectivityBounds s)

decodeQuantity :: Bounds unit Double -> Fixed ->  Quantity unit Double
decodeQuantity bounds q = exp' bounds (Logarithmic (fixedToDouble q *~ one))


serializePreferences :: Preferences Double -> Either QuantityError ByteString
serializePreferences prefs = encode <$> encodePreferences prefs

deserializePreferences :: ByteString -> Either String (Preferences Double)
deserializePreferences bytes = decodePreferences <$> decode bytes

encodeConnectionDetails :: ConnectionDetails Double -> Either QuantityError (Fixed, Fixed, Fixed, Fixed)
encodeConnectionDetails (ConnectionDetails b l d p) = do
    b <- encodeQuantity bandwidthBounds   b
    l <- encodeQuantity latencyBounds     l
    d <- encodeQuantity deviationBounds   d
    p <- encodeQuantity selectivityBounds p
    return (b,l,d,p)

decodeConnectionDetails :: (Fixed, Fixed, Fixed, Fixed) -> ConnectionDetails Double
decodeConnectionDetails (b,l,d,p) = ConnectionDetails
    (decodeQuantity bandwidthBounds   b)
    (decodeQuantity latencyBounds     l)
    (decodeQuantity deviationBounds   d)
    (decodeQuantity selectivityBounds p)

serializeConnectionDetails :: ConnectionDetails Double -> Either QuantityError ByteString
serializeConnectionDetails details = encode <$> encodeConnectionDetails details

deserializeConnectionDetails :: ByteString -> Either String (ConnectionDetails Double)
deserializeConnectionDetails bytes = decodeConnectionDetails <$> decode bytes


