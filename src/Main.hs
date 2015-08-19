-- | Main entry point to the application.
module Main where

import Term
import Parser
import Printer
import EncodeDecode

main :: IO ()
main = do
          putStrLn ""
          let e1 = parseExpr "let v = 0 in f xs v p where f Nil v p = 0 | f Cons(x,xs) v p = p x (f xs v p)"
              e2 = encode e1
          prettyPrint e1
          putStrLn ""
          prettyPrint e2
          --print e2
