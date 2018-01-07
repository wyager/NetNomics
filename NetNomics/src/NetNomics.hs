{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
module NetNomics (
    mkConnectionDetails, mkPreferences, utility,
    ConnectionDetails(..), Preferences(..), Preference(..), DBandwidth, bit
) where

import Prelude () -- Hide all standard definitions
import Numeric.Units.Dimensional.Prelude

type DBandwidth = DAmountOfSubstance / DTime
-- Dimensional doesn't have "bit" as a primitive unit so we're using moles as bits.
bit = mole

data Preference (unit :: Dimension) num = Preference {
        center :: Quantity unit num,
        selectivity :: Quantity DOne num
    } 

singleUtility :: Floating num => Direction -> Preference unit num -> Quantity unit num -> Dimensionless num
singleUtility direction (Preference center selectivity) value = _1 / (_1 + exponent)
    where 
    exponent = (value / center) ** (signOf direction * selectivity)

data Direction = PreferBigger | PreferSmaller

signOf :: Num num => Direction -> Dimensionless num
signOf PreferBigger = negate _1
signOf PreferSmaller = _1

data Preferences num = Preferences {
        bandwidthPreference :: Preference DBandwidth num,
        latencyPreference :: Preference DTime num,
        deviationPreference :: Preference DTime num,
        packetLossPreference :: Preference DOne num
    }

data ConnectionDetails num = ConnectionDetails {
        bandwidth :: Quantity DBandwidth num,
        latency :: Quantity DTime num,
        deviation :: Quantity DTime num,
        packetLoss :: Quantity DOne num
    }

utility :: Floating num => Preferences num -> ConnectionDetails num -> num
utility (Preferences pb pl pd pp) (ConnectionDetails cb cl cd cp) = (ub * ul * ud * up) /~ one
    where
    ub = singleUtility PreferBigger  pb cb
    ul = singleUtility PreferSmaller pl cl
    ud = singleUtility PreferSmaller pd cd
    up = singleUtility PreferSmaller pp cp


mkConnectionDetails :: Double -> Double -> Double -> Double -> ConnectionDetails Double
mkConnectionDetails b l d p = ConnectionDetails {
        bandwidth = (b *~ (bit / second)),
        latency = (l *~ second),
        deviation = (b *~ second),
        packetLoss =  (b *~ one)
    }

mkPreferences :: Double -> Double 
              -> Double -> Double 
              -> Double -> Double 
              -> Double -> Double 
              -> Preferences Double
mkPreferences cb sb cl sl cd sd cp sp = Preferences {
        bandwidthPreference = Preference {
            center      = (cb *~ (bit / second)),
            selectivity = (sb *~ one)
        },
        latencyPreference = Preference {
            center      = (cl *~ second),
            selectivity = (sl *~ one )
        },
        deviationPreference = Preference {
            center      = (cd *~ second),
            selectivity = (sd *~ one)
        },
        packetLossPreference = Preference {
            center      = (cp *~ one),
            selectivity = (sp *~ one)
        }
    }