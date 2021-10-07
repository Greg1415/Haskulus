module Derivate where

import           Expr

derivate :: Expr -> Expr
derivate = \case
  Const n      -> c0
  X            -> c1
  Sum a b      -> Sum (derivate a) (derivate b)
  Product a b  -> Sum (Product a (derivate b)) (Product b (derivate a))
  Quotient a b -> dquotient a b
  Pow a b      -> dpower a b
  Ln a         -> Quotient (derivate a) a

dquotient a b = Quotient
                (Sum (Product (derivate a) b) (Product cn1 (Product (derivate b) a)))
                (Pow b c2)

dpower a b = (Product
             (Pow a (Sum b cn1))
             (Sum
               (Product b (derivate a))
               (Product a
                (Product (Ln a) (derivate b)))))
