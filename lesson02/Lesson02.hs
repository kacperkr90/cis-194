{-# OPTIONS_GHC -Wall #-}

data Thing = Shoe
          | Ship
          | SealingWax
          | Cabbage
          | King
  deriving Show

shoe :: Thing
shoe = shoe

listO'Things :: [Thing]
listO'Things = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

data FailableDouble = Failure
                    | Ok Double
  deriving Show

ex01, ex02 :: FailableDouble
ex01 = Failure
ex02 = Ok 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (Ok d)  = d

type Name = String
type Age = Int
data Person = Person Name Age Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Age
getAge (Person _ a _) = a

baz :: Person -> Name
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favourite thing is lame."

ex03 :: Int
ex03 = case "Hello" of
  []       -> 3
  ('H':xs) -> length xs
  _        -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  Ok d    -> d

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
