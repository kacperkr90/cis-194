{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Debug.Trace
import Employee

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
  | listFun l1 >= listFun l2 = l1
  | otherwise = l2

treeFold
  :: (Show a, Show b)
  => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x []) = f x []
treeFold f (Node x ts) = f x (map (treeFold f) ts)

sumFun :: Tree Employee -> Fun
sumFun = treeFold (\e rs -> sum (empFun e : rs))

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

-- combineGLs :: Employee -> [GuestList] -> GuestList
-- combineGLs boss 

employeeAsGuestList :: Employee -> GuestList
employeeAsGuestList e = GL [e] (empFun e) 

buildOptimalGuestList :: Tree Employee -> GuestList 
buildOptimalGuestList (Node boss []) = employeeAsGuestList boss 
buildOptimalGuestList (Node boss subdivisions)  
  | bossFun > subdivisionsBossesFun = employeeAsGuestList boss `mappend` subdivisionsEmployeesGL 
  | otherwise = subdivisionsBossesGL 
  where
    bossFun = empFun boss
    subdivisionsBosses = map rootLabel subdivisions
    subdivisionsBossesGL = foldl mappend mempty $ map buildOptimalGuestList subdivisions 
    subdivisionsBossesFun = sum $ map empFun subdivisionsBosses
    subdivisionsEmployees = concat $ map subForest subdivisions
    subdivisionsEmployeesGL = foldl mappend mempty $ map buildOptimalGuestList subdivisionsEmployees
    
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (employeeAsGuestList boss, mempty)
nextLevel boss pairs = (glWithBoss, sndCombinedGL)
  where
    fstCombinedGL = foldl mappend mempty $ map fst pairs
    sndCombinedGL = foldl mappend mempty $ map snd pairs
    glWithBoss = employeeAsGuestList boss `mappend` fstCombinedGL

maxFun :: Tree Employee -> GuestList
maxFun (Node boss []) = employeeAsGuestList boss
maxFun _ = mempty
