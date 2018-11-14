module Homework1 where

toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number <= 0 = []
  | otherwise = number - number `div` 10 * 10 : toDigitsRev (number `div` 10)

toDigits :: Integer -> [Integer]
toDigits number = reverse (toDigitsRev number)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (first:(second:rest)) = first : second * 2 : doubleEveryOtherRev rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (first:second:rest)
  | first > 9 || second > 9 = sumDigits ( sumDigits (toDigits first) : sumDigits (toDigits second) : rest)
  | otherwise = first + second + sumDigits rest

validate :: Integer -> Bool
validate number =
  (result `div` 10) * 10 == result
  where result = sumDigits ( doubleEveryOther ( toDigits number ) )

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

main :: IO ()
main = do
  print (toDigitsRev 1234)
  print (toDigits 1234)
  print (toDigits 0)
  print (toDigits (-1))
  print (doubleEveryOther [1,2,3,4])
  print (doubleEveryOther [1,2,3,4,5])
  print (sumDigits [16, 1, 22, 3])
  print (validate 4012888888881881)
  print (validate 4012888888881882)
  print (hanoi 3 "a" "b" "c")
