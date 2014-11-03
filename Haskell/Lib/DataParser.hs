module Lib.DataParser
  (
    readFrom,
    slice
  ) where

import System.Locale
import Data.Time
import Data.Time.Format
import Data.List.Split
import Control.Monad.IO.Class
import Data.List
import QSTKUtil.Date
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Maybe

slice :: (Eq a) => Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

--params :: ticker, start date, end date
readFrom :: String -> String -> Int -> Int -> String -> IO (String, [Double])
readFrom sd ed lb lf t =  do
                    all <- readFile ("Lib/Data/" ++ t ++ ".csv")
                    let prices =  zip dates (tail $ map last $ map (splitOn ",") $ lines all)
                                where dates = (tail $ map head $ map (splitOn ",") $ lines all)
                        dates = map fst prices
                        start = fromMaybe 0 (elemIndex ed dates) - lb
                        end = fromMaybe 0 (elemIndex sd dates) + lf
                        prices_from = slice start end (map snd prices)
                    return (t, map read (reverse $ prices_from) :: [Double])
