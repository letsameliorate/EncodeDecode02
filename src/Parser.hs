module Parser where

import Term
import Aux
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language

potDef = emptyDef
         { commentStart     = "{-|",
           commentEnd       = "|-}",
           commentLine      = "--",
           nestedComments   = True,
           identStart       = do lower <|> digit,
           identLetter      = do alphaNum <|> oneOf "_'",
           reservedNames    = ["let", "in", "where"],
           caseSensitive    = True
         }

{-|
    Parse Pot to Term
|-}

lexer = T.makeTokenParser potDef

symbol      = T.symbol lexer
bracks      = T.parens lexer
semic       = T.semi lexer
comm        = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
natural     = T.natural lexer
integer     = T.integer lexer


list2ConsList [] = ConApp "Nil" []
list2ConsList (t:ts) = ConApp "Cons" [t, (list2ConsList ts)]

conName = do
             c <- upper
             cs <- many alphaNum
             return (c:cs)

makeWhere t@(FVarApp x as) [] = t
makeWhere t@(FVarApp x as) fds = makeFuns (getFNames t) (Where (x,as) fds)

makeFuns fnames (FVarApp x ts) = if x `elem` fnames
                                 then FunCall (x, (map (makeFuns fnames) ts))
                                 else FVarApp x (map (makeFuns fnames) ts)
makeFuns fnames (BVarApp i ts) = BVarApp i (map (makeFuns fnames) ts)
makeFuns fnames (ConApp c ts) = ConApp c (map (makeFuns fnames) ts)
makeFuns fnames (Lambda x t) = Lambda x (makeFuns fnames t)
makeFuns fnames (Let x t1 t2) = Let x (makeFuns fnames t1) (makeFuns fnames t2)
makeFuns fnames (FunCall (f, ts)) = FunCall (f, (map (makeFuns fnames) ts))
makeFuns fnames (Where (f, as) fds) = Where (f, as) (map (\(f, ts, t) -> (f, ts, makeFuns fnames t)) fds)

getFNames t = getFNames' [] t

getFNames' fnames (FVarApp x ts) = concatMap (getFNames' fnames) ts
getFNames' fnames (BVarApp i ts) = concatMap (getFNames' fnames) ts
getFNames' fnames (ConApp c ts) = concatMap (getFNames' fnames) ts
getFNames' fnames (Lambda x t) = getFNames' fnames t
getFNames' fnames (Let x t1 t2) = getFNames' fnames t2
getFNames' fnames (FunCall (f, ts)) = concatMap (getFNames' fnames) ts
getFNames' fnames (Where (f, as) fds) = concatMap (\(f,ts,t) -> getFNames' (f:fnames) t) fds

parseExpr = parse expr "(ERROR)"

expr = buildExpressionParser prec term

prec = []

term =   do -- Where and function/variable applications
            x <- identifier
            as <- many atom
            fds <-  do
                       reserved "where"
                       fds <- sepBy1 fundef (symbol "|")
                       return fds
                <|> do
                       spaces
                       return []
            return (makeWhere (FVarApp x as) (map (\(f,ts,t) -> (f, ts, foldl (\t x -> abstract x t) t (concatMap frees ts))) fds))
     <|> do -- ConApp
            c <- conName
            as <-  do
                      as <- bracks (sepBy1 expr comm)
                      return as
               <|> do
                      spaces
                      return []
            return (ConApp c as)
     <|> do -- Lambda
            symbol "\\"
            xs <- many1 identifier
            symbol "."
            e <- expr
            return (foldr (\x t -> (Lambda x (abstract x t))) e xs)
     <|> do -- Let
            reserved "let"
            x <- identifier
            symbol "="
            e1 <- expr
            reserved "in"
            e2 <- expr
            return (Let x e1 (abstract x e2))
     <|> do -- other expressions
            a <- atom
            return a

atom =   do -- FVarApp
            x <- identifier
            return (FVarApp x [])
     <|> do -- [list]
            symbol "["
            ts <- sepBy expr comm
            symbol "]"
            return (list2ConsList ts)
     <|> do -- (expression)
            e <- bracks expr
            return e

fundef = do
            f <- identifier
            ps <- many pattern
            symbol "="
            e <- expr
            return (f, ps, (makeFuns [f] e))

pattern =   do -- variable
               x <- identifier
               return (FVarApp x [])
        <|> do -- constructor application
               c <- conName
               as <-  do
                          as <- bracks (sepBy1 pattern comm)
                          return as
                  <|> do
                          spaces
                          return []
               return (ConApp c as)
        <|> do -- (pattern)
               pat <- bracks pattern
               return pat

{-|
    Parsers

parseExpr = parse expr "(ERROR)"

expr = buildExpressionParser prec term

prec = []

term =   do -- Where and function/variable applications
            x <- identifier
            as <- many atom
            fds <-  do
                       reserved "where"
                       fds <- sepBy1 fundef (symbol "|")
                       return fds
                <|> do
                       spaces
                       return []
            return (makeWhere (FVarApp x as) (map (\(f,ts,t) -> (f, ts, foldl (\t x -> abstract x t) t (concatMap frees ts))) fds))
     <|> do -- ConApp
            c <- conName
            as <-  do
                      as <- bracks (sepBy1 expr comm)
                      return as
               <|> do
                      spaces
                      return []
            return (ConApp c as)
     <|> do -- Lambda
            symbol "\\"
            xs <- many1 identifier
            symbol "."
            e <- expr
            return (foldr (\x t -> (Lambda x (abstract x t))) e xs)
     <|> do -- Let
            reserved "let"
            x <- identifier
            symbol "="
            e1 <- expr
            reserved "in"
            e2 <- expr
            return (Let x e1 (abstract x e2))
     <|> do -- other expressions
            a <- atom
            return a

atom =   do -- FVarApp
            x <- identifier
            return (FVarApp x [])
     <|> do -- [list]
            symbol "["
            ts <- sepBy expr comm
            symbol "]"
            return (list2ConsList ts)
     <|> do -- (expression)
            e <- bracks expr
            return e

fundef = do
            f <- identifier
            ps <- many pattern
            symbol "="
            e <- expr
            return (f, ps, e)

pattern =   do -- variable
               x <- identifier
               return (FVarApp x [])
        <|> do -- constructor application
               c <- conName
               as <-  do
                          as <- bracks (sepBy1 pattern comm)
                          return as
                  <|> do
                          spaces
                          return []
               return (ConApp c as)
        <|> do -- (pattern)
               pat <- bracks pattern
               return pat
|-}
