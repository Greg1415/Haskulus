{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expr where
import           Control.Monad
import           Data.Bool
import           Debug.Trace
import           GHC.Float.RealFracMethods
e = 2.718281828459045235360287471352662497757247

data Expr = Const Double
          | X
          | Sum Expr Expr
          | Product Expr Expr
          | Quotient Expr Expr
          | Pow Expr Expr
          | Ln Expr

c0 = Const 0
c1 = Const 1
cn1 = Const (-1)
c2 = Const 2

instance Eq Expr where
  (==) a b = case (a, b) of
    (Const a, Const b)           -> a == b
    (X,X)                        -> True
    (Sum a b, Sum c d)           -> a==c&&b==d
    (Product a b, Product c d)   -> a==c&&b==d
    (Quotient a b, Quotient c d) -> a==c&&b==d
    (Pow a b, Pow c d)           -> a==c&&b==d
    (Ln a, Ln b)                 -> a == b
    _                            -> False

eqv :: Expr -> Expr -> Bool
eqv a b = case (a,b) of
  (Const a, Const b)                                      -> a == b
  (X,X)                                                   -> True
  (Sum a b, Sum c d)           | a==c&&b==d || a==d&&b==c -> True
  (Product a b, Product c d)   | a==c&&b==d || a==d&&b==c -> True
  (Quotient a b, Quotient c d) | a==c&&b==d               -> True
  (Pow a b, Pow c d)           | a==c&&b==d               -> True
  (Pow a (Const 1), b) | a==b                             -> True
  (a, Pow b (Const 1)) | a == b                           -> True
  (Ln a, Ln b)                                            -> a == b
  (Sum (Const 0) a, b) | a == b                           -> True
  (Sum a (Const 0),  b) | a ==b                           -> True
  _                                                       -> False

instance Show Expr where
  show expr = case expr of
    Const a      -> liftM3 bool shownum (const "e") (==e) a
    X            -> "x"
    Sum a b      -> showi 3 "+" a b
    Product a b  -> showi 2 "*" a b
    Quotient a b -> showi 2 "/" a b
    Pow a b      -> showi' 1 "^" a b
    Ln a         -> showf "ln" a
    where
      showp p a = "(" ++ p ++ show a ++ ")"
      showf f a = f ++ "(" ++ show a ++ ")"
      showi n i a b = show' n a ++ " " ++ i ++ " " ++ show' (n-1) b
      showi' n i a b = show' (n-1) a ++ " " ++ i ++ " " ++ show' n b
      show' x e | rank e > x = "(" ++ show e ++ ")"
                | otherwise = show e
      shownum f | (n, 0.0) <- properFractionDoubleInteger f = show n
                | otherwise = show f

recursively :: (Expr -> Expr) -> Expr -> Expr
recursively f = go
  where go expr = f $ case expr of
          Const _      -> expr
          X            -> expr
          Sum a b      -> Sum (go a) (go b)
          Product a b  -> Product (go a) (go b)
          Quotient a b -> Quotient (go a) (go b)
          Pow a b      -> Pow (go a) (go b)
          Ln a         -> Ln (go a)


rank x = case x of
  Sum _ _      -> 3
  Product _ _  -> 2
  Quotient _ _ -> 2
  Pow _ _      -> 1
  _            -> 0

isConst = \case
  Const _ -> True
  _ -> False
