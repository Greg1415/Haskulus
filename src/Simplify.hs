{-# LANGUAGE TypeOperators, GADTs #-}
{-# LANGUAGE TupleSections #-}

module Simplify where
import           Data.Bool
import           Data.Either
import           Data.Maybe
import           Expr

simplify :: Expr -> Expr

simplify a = bool (simplify b) a (a==b)
  where b = recursively simplify' a

{-
A suite of transformations that can be applied successively to
simplify an expression tree.
-}
simplify' = foldConsts
  . removeIdentity
  . simplifyQuotients
  . condenseSums
  . condenseQuotients
  . condenseProducts
  . powerRules
  . simplifyDuplicate
  . orderConsts

foldConsts = \case
  Sum (Const a) (Const b)                  -> Const $ a + b
  Product (Const a) (Const b)              -> Const $ a * b
  Quotient (Const a) (Const b)             -> Const $ a / b
  Pow (Const a) (Const b)                  -> Const $ a ** b
  Ln (Const a)                             -> Const $ log a
  Product (Const a) (Product (Const b) c)  -> Product (Const $ a * b) c
  Quotient (Const a) (Product (Const b) c) -> Quotient (Const $ a / b) c
  other                                    -> other

removeIdentity = \case
  Product (Const 0) a  -> c0
  Product a (Const 0)  -> c0
  Product (Const 1) a  -> a
  Product a (Const 1)  -> a
  Quotient a (Const 1) -> a
  Sum (Const 0) a      -> a
  Sum a (Const 0)      -> a
  Pow a (Const 1)      -> a
  other                -> other

orderConsts = \case
  Sum a b@(Const _)                   -> Sum b a
  Sum (Const a) (Sum (Const b) c)     -> Sum (Const $ a + b) c
  Product a b@(Const _)               -> Product b a
  Product (Product a@(Const _) b) c   -> Product a (Product b c)
  Product a (Product b c@(Const _))   -> Product c (Product a b)
  Quotient (Product a@(Const  _) b) c -> Product a (Quotient b c)
  Product a (Product c@(Const _) b)   -> Product c (Product a b)
  other                               -> other

simplifyDuplicate = \case
  Sum a b      | eqv a b -> Product a c2
  Product a b  | eqv a b -> Pow a c2
  Quotient a b | eqv a b -> c1
  other                  -> other

simplifyQuotients = \case
  Quotient a (Quotient b c)           -> Quotient (Product a c) b
  Quotient (Quotient a b) c           -> Quotient a (Product b c)
  Quotient a (Const b)                -> Product a (Const (1/b))
  Product (Quotient a b) c            -> Quotient (Product a c) b
  Product a (Pow b (Const c)) | c < 0 -> Quotient a (Pow b (Const (-c)))
  Product a (Quotient b c) | not (isConst a) -> Quotient (Product a b) c
  Quotient (Product a b) c | eqv a c  -> b
  Quotient (Product a b) c | eqv a b  -> c
  Quotient (Product a@(Const _) b) c  -> Product a (Quotient b c)
  other                               -> other

condenseSums = \case
  Sum (Product a b) (Product c d) | eqv a c -> Product a (Sum b d)
                                  | eqv a d -> Product a (Sum b c) 
                                  | eqv b c -> Product b (Sum a d)
                                  | eqv b d -> Product b (Sum a c)

  Sum a (Product b c) | eqv a b             -> Product b (Sum c c1)
                      | eqv a c             -> Product c (Sum b c1)
                      | eqv a b             -> Product b (Sum c c1)
                      | eqv a c             -> Product c (Sum b c1)
  other                                     -> other

condenseProducts = \case
  Product (Pow a b) (Pow c d) | eqv a c -> Pow a (Sum b d)
  Product a (Pow b c)         | eqv a b -> Pow a (Sum c c1)
  Product (Pow b c) a         | eqv a b -> Pow a (Sum c c1)
  other                                 -> other

condenseQuotients = \case
  Quotient (Pow a b) (Pow c d) | eqv a c -> Pow a (Sum b (Product cn1 d))
  Quotient (Pow b c) a | eqv a b         -> Pow a (Sum c cn1)
  Quotient a (Pow b c) | eqv a b         -> Pow a (Sum c1 (Product cn1 c))
  other                                  -> other

powerRules = \case
  Pow (Pow a b) c -> Pow a (Product b c)
  other           -> other

