module Expr where


data Expr = Var String -- переменная
          | Const Integer -- целочисленная константа
          | Add Expr Expr -- сложение
          | Mul Expr Expr -- умножение

simplify :: Expr -> Expr
simplify ex = case ex of
  Add (Const x) (Const y) -> Const $ x + y
  Mul (Const x) (Const y) -> Const $ x * y
  Add (Const 0) y -> simplify y
  Add x (Const 0) -> simplify x
  Mul (Const 1) y -> simplify y
  Mul x (Const 1) -> simplify x
  Mul (Const 0) _ -> Const 0
  Mul _ (Const 0) -> Const 0
  Add expx expy -> simplify (Add (simplify expx) (simplify expy))
  Mul expx expy -> simplify (Mul (simplify expx) (simplify expy))
  expr -> expr

instance Show Expr where
  show exp = case exp of
    Var str -> "Var(" ++ str ++ ")"
    Const x -> "Const(" ++ show x ++ ")"
    Add expx expy -> "Add(" ++ show expx ++ ", " ++ show expy ++ ")"
    Mul expx expy -> "Mul(" ++ show expx ++ ", " ++ show expy ++ ")"
