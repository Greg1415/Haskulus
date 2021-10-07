module Main where
import           Calc
import           Control.Monad
import           Data.Either
import           Data.Functor
import           Parser
import           Simplify

main :: IO ()
main = do
  mapM_ putStrLn
    ["Haskulus calculates derivatives of equations."
    , "Operator precedence from highest to lowest is:"
    , "unary -, functions"
    , "/, *"
    , "-, +"
    , "%"
    , "Supported functions are: "
    , "ln"
    , "% is / but with lower precedence."
    , "Enter \"quit\" to exit."
    ]
  untilM_ (putStrLn "Enter an equation:" >> getLine) (=="quit") $ do
    runParseExpr <&> either (print) (\x -> do
                       putStrLn $ "You entered: " ++ show x
                       putStrLn $ "Derivative is: " ++ show (calc x))

untilM_ i p a = do
  r <- i
  if p r then pure () else a r >> untilM_ i p a
