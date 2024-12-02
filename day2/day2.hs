{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Day2 where


ordered :: (a -> a -> Bool) -> [a] -> Bool
ordered p [] = True
ordered p [x] = True
ordered p (x:y:ys) = p x y && ordered p (y : ys)

ordered_count :: (a -> a -> Bool) -> [a] -> Int
ordered_count p [] = 0
ordered_count p [x] = 0
ordered_count p (x:y:ys) = if p x y then ordered_count p (y : ys)
                                    else 1 + ordered_count p (y : ys)

p1 :: Int -> Int -> Bool
p1 x y = x <= y-1 && y-3 <= x

p2 :: Int -> Int -> Bool
p2 x y = x-1 >= y && y >= x-3

test :: [Int] -> Bool
test xs = ordered p1 xs || ordered p2 xs

solvea :: [[Int]] -> Int
solvea = length . filter test

remove :: Int -> [a] -> [a]
remove n [] = [] 
remove 0 (x:xs) = xs 
remove n (x:xs) = x : remove (n-1) xs 

test_drop :: [Int] -> Bool 
test_drop xs = test xs || any (\i -> test (remove i xs)) [0..length xs - 1]

solveb :: [[Int]] -> Int 
solveb = length . filter test_drop

main :: IO ()
main = do
  file <- readFile "input"
  let input = map (map read . words) $ lines file :: [[Int]]
  print $ "Part 1: " ++ show (solvea input)
  print $ "Part 2: " ++ show (solveb input)
  return ()