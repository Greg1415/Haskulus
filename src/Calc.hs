module Calc where
import           Derivate
import           Expr
import           Simplify

calc :: Expr -> Expr
calc = simplify . derivate . simplify
