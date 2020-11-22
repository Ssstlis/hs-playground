module Expr where

data Expr = Var String -- переменная
          | Const Integer -- целочисленная константа
          | Add Expr Expr -- сложение
          | Mul Expr Expr -- умножение

simplify :: Expr -> Expr
simplify = f
  where
    f e = case e of
      Add x y -> onAdd x y
      Mul x y -> onMul x y
      expr -> expr
    onAdd a b = case (simplify a, simplify b) of
     (Const x, Const y) -> Const $ x + y
     (Const 0, y) -> y
     (x, Const 0) -> x
     (x, y) -> Add x y
    onMul a b = case (simplify a, simplify b) of
      (Const x, Const y) -> Const $ x * y
      (Const 0, _) -> Const 0
      (_, Const 0) -> Const 0
      (Const 1, y) -> y
      (x, Const 1) -> x
      (x, y) -> Mul x y



-- for ghci
instance Show Expr where
  show exp = case exp of
    Var str -> "Var(" ++ str ++ ")"
    Const x -> "Const(" ++ show x ++ ")"
    Add expx expy -> "Add(" ++ show expx ++ ", " ++ show expy ++ ")"
    Mul expx expy -> "Mul(" ++ show expx ++ ", " ++ show expy ++ ")"

tests = 
  map simplify [
    Var "x"
    , Const 5
    , Add (Const 5) (Const 5)
    , Add (Var "x") (Const 5)
    , Add (Var "x") (Var "y")
    , Mul (Const 5) (Const 5)
    , Mul (Var "x") (Const 5)
    , Mul (Var "x") (Var "y")
    , Add (Add (Add (Add (Const 1) (Const 2)) (Const 1)) (Const 1)) (Const 1)
    , Mul (Mul (Mul (Mul (Const 1) (Const 2)) (Const 1)) (Const 1)) (Const 1)
    , Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12)
    , Add (Add (Add (Add (Const 0) (Const 2)) (Const 1)) (Const 1)) (Const 1)
    , Mul (Add (Var "a") (Const 0)) (Add (Var "b") (Const 0))
    , Mul (Mul (Mul (Mul (Const 1) (Const 2)) (Const 1)) (Const 1)) (Const 1)
    , Add (Var "x") (Add (Const 1) (Const (-1)))
    , Mul (Var "x") (Add (Const 1) (Const (-1)))
    , Mul (Var "x") (Add (Const 1) (Const (0)))
  ]