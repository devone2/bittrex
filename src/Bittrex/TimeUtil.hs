{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.TimeUtil
(zeroAfterHours, addHours, mkZeroTime)
where
import           Data.Time.Clock
import           Data.Time.Calendar

hours :: DiffTime -> Int
hours t = floor $ t / (60*60)

mkHours :: Int -> DiffTime
mkHours h = secondsToDiffTime $ (fromIntegral h * 60 * 60)

toNominalDiffTime :: DiffTime -> NominalDiffTime
toNominalDiffTime = fromRational . toRational 

zeroAfterHours :: UTCTime -> UTCTime
zeroAfterHours u = UTCTime (utctDay u) (mkHours (hours (utctDayTime u)))

addHours :: Int -> UTCTime -> UTCTime
addHours h = addUTCTime (toNominalDiffTime $ mkHours h) 

mkZeroTime :: UTCTime
mkZeroTime = UTCTime  (fromGregorian 1970 1 1) (secondsToDiffTime 0)

