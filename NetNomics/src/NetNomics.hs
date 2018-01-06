{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
module NetNomics where

-- Can use standard Prelude functions like "P.length"
import qualified Prelude as P

import Data.Word (Word16)

import Numeric.Units.Dimensional.Prelude
import qualified Numeric.Units.Dimensional.UnitNames as Names

bit :: Fractional a => Unit Metric DAmountOfSubstance a
bit = mole

type Data = Quantity DAmountOfSubstance

type Bandwidth = Quantity (DAmountOfSubstance / DTime)

data CompressedUtility a = CompressedUtility {
        bandwidth_center                :: Bandwidth a,
        bandwidth_selectivity           :: Dimensionless a,
        latency_center                  :: Time a,
        latency_selectivity             :: Dimensionless a,
        latency_deviation_center        :: Time a,
        latency_deviation_selectivity   :: Dimensionless a,
        packet_loss_center              :: Dimensionless a,
        packet_loss_selectivity         :: Dimensionless a
    }

data Description a = Description {
        bandwidth                :: Bandwidth a,
        latency                  :: Time a,
        latency_deviation        :: Time a,
        packet_loss              :: Dimensionless a
    }

bandwidth_min, bandwidth_max :: Fractional a => Bandwidth a
bandwidth_min = 1 *~ (milli bit / second)
bandwidth_max = 1 *~ (yotta bit / second)

latency_min, latency_max :: Fractional a => Time a
latency_min = 1 *~ nano second
latency_max = 10 *~ giga second

latency_deviation_min, latency_deviation_max :: Fractional a => Time a
latency_deviation_min = 0.1 *~ nano second
latency_deviation_max = 1 *~ giga second

packet_loss_min, packet_loss_max :: Fractional a => Dimensionless a
packet_loss_min = 1e-30 *~ one
packet_loss_max = 1 *~ one

selectivity_min, selectivity_max :: Fractional a => Dimensionless a
selectivity_min = 0.1 *~ one
selectivity_max = 1000 *~ one

newtype Utility a = Utility (Dimensionless a) deriving Show

newtype Compressed (unit :: Dimension) a = Compressed (Dimensionless a)

-- divide :: Compressed 

data Sign = Positive | Negative

-- valueOf :: Sign -> Dimensionless
-- valueOf Positive = _1
-- valueOf Negative = negate _1


-- Takes a value 0 <= x <= 1 and spits out min <= y <= max.
-- The value grows smoothly.
decompress :: Floating a => Quantity unit a -> Quantity unit a -> Compressed unit a -> Quantity unit a
decompress min max (Compressed value) = min * ((max / min) ** value)

singleUtility :: Floating a 
              => Quantity unit a 
              -> Quantity unit a 
              -> Sign 
              -> Dimensionless a 
              -> Compressed unit a
              -> (Quantity unit a -> )
singleUtility center_min center_max sign selectivity_compressed center_compressed 
    = \value -> _1 / (_1 + exponent value)
    where
    exponent value = (value / center) ** (sign * selectivity)
    center = decompress center_min center_max center_compressed

-- utility :: Floating a => CompressedUtility a -> Description a -> Utility a
-- utility (CompressedUtility c_b p_b c_l p_l c_d p_d c_p p_p)
--         (Description b l d p) 
--         = Utility (u_b c_b p_b b)
--         where
--         u_b c p b = (_1 /) $ (_1 +) $ exp $ negate $ (sel *) $ base - center
--             where
--             sel = selectivity_min * (selectivity_max / selectivity_min) ** p
--             base = log (b / bandwidth_min)
--             center = log (c / )