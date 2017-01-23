-- Anonymous functions

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter gt100

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 = filter (\x -> x > 100)

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 = filter (>100)

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

-- Function composition

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

-- Currying and partial application

f :: Int -> Int -> Int
f x y = 2 * x + y

f' :: Int -> (Int -> Int)
f' x y = 2 * x + y

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

f'' :: (Int, Int) -> Int
f'' (x, y) = 2 * x + y

schonfinkel :: ((a, b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y)

unschonfinkel :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (x, y) = f x y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

-- Folds

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f [] = z
fold z f (x:xs) = f x (fold z f xs)

sum'' :: [Integer] -> Integer
sum'' = fold 0 (+)

product'' :: [Integer] -> Integer
product'' = fold 1 (*)

length'' :: [a] -> Int
length'' = fold 0 (\_ s -> 1 + s) -- \_ -> 1+, const (1+)