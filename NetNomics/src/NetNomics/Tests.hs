module Main (main) where

import NetNomics (
    ConnectionDetails, Preferences,
    mkConnectionDetails, mkPreferences, utility)
import NetNomics.Serialize (
    serializePreferences, serializeConnectionDetails,
    deserializePreferences, deserializeConnectionDetails)

import Test.QuickCheck (forAll, Property, Gen, (==>))

prop :: Preferences Double -> ConnectionDetails Double -> Bool
prop prefs conn = utility prefs conn `like` utility (idP prefs) (idC conn)
    where
    idP = either (error "Bad preferences deserialize") id . deserializePreferences . 
          either (error "bad preferences") id .  serializePreferences
    idC = either (error "Bad connection deserialize") id . deserializeConnectionDetails . 
          either (error "bad preferences") id .  serializeConnectionDetails

like :: Double -> Double -> Bool
a `like` b = a + epsilon > b && a - epsilon < b
    where epsilon = 0.000001



main = print 5