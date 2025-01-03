module Examples where

import Types

p1, p2, p3, p4, p5 :: Polynomial
p1 = [(5,0)]
p2 = [(1,1)]
p3 = [(2,2),(-3,1),(1,0)]
p4 = [(4,1),(-3,0)]
p5 = [(2,3),(-2,1),(2,0)]

x :: Expr
x = P [(1,1)]

-- Basic polynomials
e1, e2, e3, e4, e5 :: Expr
e1 = P p1
e2 = P p2
e3 = P p3
e4 = P p4
e5 = P p5

-- Addition of polynomials
e6 :: Expr
e6 = Add e3 e5

-- Multiplication by constant
e7 :: Expr
e7 = Mul e1 e6

-- Simple functions of x
e8, e9 :: Expr
e8  = Log e2
e9  = Pow e2 (-1)

-- Inverse chain rule, id
e10, e11 :: Expr
e10 = Mul e4 e3
e11 = Mul (Pow x (-1)) (Log x)

-- Inverse chain rule, others
e12, e13 :: Expr
e12 = Mul e4 (Log e3)
e13 = Mul (Pow e3 (3/2)) e4

-- Now with a constant factor
e14, e15 :: Expr
e14 = Mul e4 (Pow (Mul e1 e3) (3/2))
e15 = Mul (P [(1,2), (-1,0)]) (Log (P [(1,3), (-3,1)]))

-- No integral to be found
e16 :: Expr
e16 = Mul (Log e3) (Pow e4 (1/2))
