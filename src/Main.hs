-- | Main entry point to the application.
module Main where

import Term
import Parser
import Printer
import EncodeDecode

main :: IO ()
main = do
          putStrLn ""
          let e1 = parseExpr "let v = 0 in f xt yt v p where f E E v p = v | f E B(y,yt1,yt2) v p = v | f B(x,xt1,xt2) E v p = v | f B(x,xt1,xt2) B(y,yt1,yt2) v p = (let v' = 1 in p x y (f xt1 yt1 v' p) (f xt2 yt2 v' p))"
              e2 = encode e1
          prettyPrint e1
          putStrLn ""
          prettyPrint e2
          --print e2
