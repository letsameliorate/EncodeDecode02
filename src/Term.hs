module Term where

type FVar = String
type BVar = Int
type Con  = String
type FunName  = String

type FunApp = (FunName, [Term])
type FunDef = (FunName, [Term], Term)

data Term = FVarApp FVar [Term]
          | BVarApp BVar [Term]
          | ConApp Con [Term]
          | Lambda String Term
          | Let String Term Term
          | FunCall FunApp
          | Where FunApp [FunDef]
  --deriving (Show)

instance Eq Term where
  (==) (FVarApp x ts) (FVarApp x' ts') = (x == x') && (ts == ts')
  (==) (BVarApp i ts) (BVarApp i' ts') = (i == i') && (ts == ts')
  (==) (ConApp c ts) (ConApp c' ts') = (c == c') && (ts == ts')
  (==) (Lambda x t) (Lambda x' t') = (t == t')
  (==) (Let x t1 t2) (Let x' t1' t2') = (t1 == t1') && (t2 == t2')
  (==) (FunCall (f, ts)) (FunCall (f', ts')) = (f == f') && (ts == ts')
  (==) t@(Where (f, ts1) fds) t'@(Where (f', ts1') fds') | match t t' = let (fs, tss, ts) = unzip3 fds
                                                                            (fs', tss', ts') = unzip3 fds'
                                                                        in (ts1 == ts1') && (fs == fs') && (all (\(ts, ts') -> ts == ts') (zip tss tss')) && (ts == ts')
  (==) t t' = False

match (FVarApp x ts) (FVarApp x' ts') = (x == x') && (length ts == length ts')
match (BVarApp i ts) (BVarApp i' ts') = (i == i') && (length ts == length ts')
match (ConApp c ts) (ConApp c' ts') = (c == c') && (length ts == length ts')
match (Lambda x t) (Lambda x' t') = True
match (Let x t1 t2) (Let x' t1' t2') = True
match (FunCall (f, ts)) (FunCall (f', ts')) = (f == f') && (length ts == length ts')
match (Where (f, ts) fds) (Where (f', ts') fds') = (f == f') && (length ts == length ts') && (length fds == length fds')
match t t' = False
