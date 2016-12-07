hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0  = n `div` 2
  | otherwise       = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2   = -43
  | otherwise         = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq(hailstone n)

initListLength :: [Integer] -> Integer
initListLength [] = 0
initListLength (_:xs) = 1 + initListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []     = []
sumEveryTwo [x]  = [x]
sumEveryTwo (x:y:zx) = (x + y) : sumEveryTwo zx

hailstoneLen n = initListLength (hailstoneSeq n)
