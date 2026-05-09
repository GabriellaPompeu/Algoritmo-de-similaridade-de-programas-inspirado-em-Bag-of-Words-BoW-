module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map 
import qualified Data.Set as Set 
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)

normalize :: String -> String
normalize = map toLower

tokenize :: String -> String -> [String]
tokenize separators text = words[if c `elem` separators then ' ' else toLower c | c <- text]

buildFrequency :: Set.Set String -> String -> String -> Map.Map String Int
buildFrequency reserved separators text =
    foldl insertWord Map.empty tokens
    where
        tokens = tokenize separators text
        insertWord acc word = let weight = if Set.member word reserved then 2 else 1
            in Map.insertWith (+) word weight acc

calculateSimilarity :: Map.Map String Int -> Map.Map String Int -> Double
calculateSimilarity freq1 freq2 = if totalF1 == 0 then 0 else fromIntegral m / fromIntegral totalF1
    where
        entries = Map.toList freq1
        totalF1 = sum(Map.elems freq1)
        m = sum[f1 | (word, f1) <- entries, let f2 = Map.findWithDefault 0 word freq2, abs (f1 - f2) <= round(0.1 * fromIntegral f1)]


sortFrequency :: (Ord a1, Ord a2) => Map.Map a2 a1 -> [(a2, a1)]
sortFrequency freq = sortBy compareFreq(Map.toList freq)
    where
        compareFreq (w1, f1) (w2, f2) | f1 /= f2 = compare f2 f1 | otherwise = compare w1 w2

printEntry :: (String, Int) -> IO ()
printEntry (word, freq) = putStrLn (word ++ " " ++show freq)
