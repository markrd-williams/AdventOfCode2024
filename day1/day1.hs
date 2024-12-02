{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Day1 where
import System.IO
import Data.List (transpose)
import Data.Function

dist :: Int -> Int -> Int
dist x y = max (x-y) (y-x)

sort :: [Int] -> [Int]
sort []     = []
sort (x:xs) = sort l ++ [x] ++ sort r
  where
    l = filter (<= x) xs
    r = filter (x <) xs

solve1 :: [Int] -> [Int] -> Int
solve1 xs ys =  sum $ zipWith dist  (sort xs) (sort ys)

pair :: [a] -> (a, a)
pair (x : y : xs) = (x, y)

solve2 :: [Int] -> [Int] -> Int
solve2 xs ys = sum $ map (\x -> x * length (filter (==x) ys)) xs

main :: IO ()
main = do
  file <- readFile "input"
  let l = lines file & map (map read . words)
                     & transpose
                     & pair
  let ans1 = uncurry solve1 l
  print $ "Part 1: " ++ show ans1
  let ans2 = uncurry solve2 l
  print $ "Part 2: " ++ show ans2
  return ()
