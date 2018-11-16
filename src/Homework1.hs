module Homework1 where

toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number <= 0 = []
  | otherwise = number `mod` 10 : toDigitsRev (number `div` 10)

toDigits :: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | otherwise = toDigits (number `div` 10) ++ [number `mod` 10]

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (first:(second:rest)) = first : second * 2 : doubleEveryOtherRev rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (number:rest)
  | number > 9 = sumDigits (toDigits number ++ rest)
  | otherwise = number + sumDigits rest

validate :: Integer -> Bool
validate number = (sumDigits . doubleEveryOther . toDigits) number `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
