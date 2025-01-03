module Int where

import Types
import Examples
import Utilities
import Data.Maybe

addP :: Polynomial -> Polynomial -> Polynomial
addP [(a, b)] [(x, y)]
  | y < b  = [(a, b), (x, y)]
  | y == b = [(a + x, b)]
  | y > b  = [(x, y), (a, b)]
addP ((a, b) : c) [(x, y)] 
  | y > b  = (x, y) : c
  | y == b = (a + x, b) : c
  | y < b  = (a, b) : addP c [(x, y)]
addP [(a, b)] ((x, y) : z) 
  | y > b  = (x, y) : addP [(a, b)] z
  | y == b = (a + x, b) : z
  | y < b  = (a, b) : (x, y) : z
addP ((a, b) : c) ((x, y) : z)
  | y > b  = (x, y) : addP ((a, b) : c) z
  | y == b = (a + x, b) : addP c z
  | y < b  = (a, b) : addP c ((x, y) : z)

--   = map (\(a, b) -> if b == y then (a + x, b) else (a, b)) poly1
-- addP poly1 ((x, y) : z) = addP (map (\(a, b) -> if b == y then (a + x, b) else (a, b)) poly1) z


mulP :: Polynomial -> Polynomial -> Polynomial
mulP poly1 [(x, y)] = map (\(a, b) -> (a * x, b + y)) poly1
mulP poly1 ((x, y) : z) = addP (map (\(a, b) -> (a * x, b + y)) poly1) (mulP poly1 z)


sumP :: [Polynomial] -> Polynomial 
sumP = foldr addP [(0, 0)]

prodP :: [Polynomial] -> Polynomial
prodP = foldr mulP [(1, 0)]

diffT :: Term -> Term
diffT (x, 0) = (0, 0)
diffT (x, y) = (x * fromInteger y, y - 1)

intT :: Term -> Term
intT (x, y) = (x / fromInteger (y + 1), y + 1)

diffP :: Polynomial -> Polynomial
diffP = map diffT

intP :: Polynomial -> Polynomial
intP = map intT

toExpr :: Rational -> Expr
toExpr n = P [(n, 0)]

isConstant :: Expr -> Bool
isConstant (P [(_, 0)]) = True
isConstant _ = False

diffE :: Expr -> Expr
diffE (P plist) = P (diffP plist)
diffE (Add expr1 expr2) = Add (diffE expr1) (diffE expr2)
diffE (Mul expr1 expr2) = Add (Mul (diffE expr2) expr1) (Mul (diffE expr1) expr2)
diffE (Pow expr1 power) = Pow (Mul (toExpr power) expr1) (power - 1)
diffE (Log expr1) = Pow expr1 (-1)

intE :: Expr -> Maybe Expr
intE (P plist) = Just (P (intP plist))
intE (Add expr1 expr2) = Just Add <*> intE expr1 <*> intE expr2
intE (Pow expr1 pwr) = Just Pow <*> intE expr1 <*> Just pwr
intE (Mul expr1 expr2) 
  | isConstant expr1 = Just Mul <*> Just expr1 <*> intE expr2
  | isConstant expr2 = Just Mul <*> Just expr2 <*> intE expr1
intE (Mul expr1 (Pow expr2 n))
  | diffE expr2 == expr1 = Just (Mul (toExpr (1 / (n + 1))) (Pow expr2 (n + 1)))
intE (Mul (Pow expr1 n) expr2)
  | diffE expr1 == expr2 = Just (Mul (toExpr (1 / (n + 1))) (Pow expr1 (n + 1)))
