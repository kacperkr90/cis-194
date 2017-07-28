{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.Monoid
import Data.Maybe
import Debug.Trace
import Employee
import Text.Read
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL newList updatedFun
  where
    fun = empFun e
    newList = e : l
    updatedFun = f + fun

instance Monoid GuestList where
  mempty = GL [] 0
  GL es1 f1 `mappend` GL es2 f2 = GL (es1 ++ es2) (f1 + f2)

listFun :: GuestList -> Fun
listFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2
  | l1 >= l2 = l1
  | otherwise = l2

treeFold
  :: (Show a, Show b)
  => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

sumFun :: Tree Employee -> Fun
sumFun = treeFold (\e es -> sum (empFun e : es))

funniestEmployee :: Tree Employee -> Employee
funniestEmployee = treeFold (\e rs -> foldl funnierEmployee e (e : rs))

saddestEmployee :: Tree Employee -> Employee
saddestEmployee = treeFold (\e rs -> foldl sadderEmployee e (e : rs))

oneOfEmployees :: (Employee -> Employee -> Bool) -> Employee -> Employee -> Employee
oneOfEmployees f e1 e2
  | f e1 e2 = e1
  | otherwise = e2

funnierEmployee :: Employee -> Employee -> Employee
funnierEmployee = oneOfEmployees (\x1 x2 -> empFun x1 >= empFun x2)

sadderEmployee :: Employee -> Employee -> Employee
sadderEmployee = oneOfEmployees (\x1 x2 -> empFun x1 <= empFun x2)

joinGLs :: [GuestList] -> GuestList
joinGLs = foldl (<>) mempty

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss gls = glCons boss $ joinGLs gls

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss pairs = (funniestWoBossGL, funniestWtBossGL)
  where
    wtBossGLs = map fst pairs
    woBossGLs = map snd pairs
    funniestWtBossGL = joinGLs wtBossGLs 
    funniestWoBossGL = combineGLs boss woBossGLs

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun (fst gls) (snd gls)
  where
    gls = treeFold nextLevel tree

testCompanyAsString :: String
testCompanyAsString = show testCompany 

parseEmployeesTree :: String -> Maybe (Tree Employee)
parseEmployeesTree = readMaybe

instance Ord Employee where
  compare (Emp n1 _) (Emp n2 _) = compare n1 n2

prettyShow :: GuestList -> String
prettyShow (GL emps fun) = title ++ employeesNamesSorted 
  where
    title = "Total fun: " ++ (show $ fun)
    employeesNamesSorted = unwords $ sort $ map ((++) "\n" . empName) emps

showAsPrettifiedGL :: String -> String
showAsPrettifiedGL input
  | isJust employeesTree = prettyShow $ maxFun $ fromJust employeesTree 
  | otherwise = "There was an error parsing the Tree Employee object."
  where
    employeesTree = parseEmployeesTree input
