import qualified QSTKUtil.QSTsUtil as TSU
import qualified QSTKUtil.QSDateUtil as DU
import qualified Lib.DataParser as DP
import qualified QSTKUtil.Math.Statistics as Stats
import QSTKUtil.Date
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Maybe

main =  do
        print "Reading data..."
        let sd = "2008-01-02"
            ed = "2008-01-31"
            lb = 20
            syms = ["GOOG"]
        p <- mapM (DP.readFrom sd ed 0 lb) syms
        let p_data = Map.fromList p
            sma_data = Map.map (DP.slice lb end) $ Map.map (moving_average lb) p_data
                where end = (length $ head $ Map.elems p_data) - 1
            mstd_data = Map.map (moving_std lb) p_data
            bol_data = Map.mapWithKey (bol_band p_data mstd_data) sma_data
        print bol_data

bol_band :: Map.Map String [Double] -> Map.Map String [Double] -> String -> [Double] -> [Double]
bol_band _ _ _ [] = []
bol_band prices rstd sym sma = [(price - sma_val) / (rstd_val)] ++ bol_band (Map.map tail prices) (Map.map tail rstd) sym (tail sma)
                               where    price = head $ fromMaybe [0.0] $ Map.lookup sym prices
                                        sma_val = head sma
                                        rstd_val = head $ fromMaybe [0.0] $ Map.lookup sym rstd

moving_std :: Int -> [Double] -> [Double]
moving_std n prices
    | length (take n prices) < n = []
    | otherwise  = [Stats.stddev $ take n prices] ++ moving_std n (tail prices)

--moving_std :: Int -> Map.Map String [Double] -> String -> [Double] -> [Double]
--moving_std n ma sym prices
--    | (length prices) < 20 = [] ++ moving_std n (Map.map tail ma) sym (tail prices)
--    | otherwise            = [(difference prices n $ head ma_prices)] ++ moving_std n (Map.map tail ma) sym (tail prices)
--        where ma_prices = fromMaybe [0.0] $ Map.lookup sym ma
--
--difference :: [Double] -> Int -> Double -> Double
--difference prices n ma = sqrt $ (sum $ map (\x -> x*x) differences) / (read (show n) :: Double)
--                         where differences = map (+ (-ma)) (take n prices)

-- Code by Michael Orlitzky
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

moving_average :: (Fractional a) => Int -> [a] -> [a]
moving_average _ [] = []
moving_average n samples
    | n <= 0 = []
    | otherwise = map fst3 $ scanl1 average sample_triples
                  where divisors = map fromIntegral $ [1..n] ++ (repeat n)
                        n_agos = (map fromIntegral (replicate (n-1) 0)) ++ samples
                        sample_triples = zip3 samples divisors n_agos
                        average :: (Fractional b) => (b,b,b) -> (b,b,b) -> (b,b,b)
                        average (prev_avg, prev_div, dropme) (sample, divisor, n_ago) = (new_avg, divisor, n_ago)
                                where prev_sum = prev_avg * prev_div
                                      new_avg = (prev_sum + sample - dropme) / divisor
