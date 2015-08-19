module EncodeDecode where

import Term
import Aux

encode (Left a) = Left a
encode (Right a) = Right (fst (rule1 a []))

--isRecursive (Where (f, ts) fds) = any (\(f,ts,t) -> hasFunCall f t) fds
--isRecursive t = False

hasFunCall f (FVarApp x ts) = any (hasFunCall f) ts
hasFunCall f (BVarApp i ts) = any (hasFunCall f) ts
hasFunCall f (ConApp c ts) = any (hasFunCall f) ts
hasFunCall f (Lambda x t) = hasFunCall f t
hasFunCall f (Let x t1 t2) = hasFunCall f t2
hasFunCall f (FunCall (f', ts)) = (f == f') || (any (hasFunCall f) ts)
hasFunCall f (Where (f', ts) fds) = any (\(f',ts,t) -> hasFunCall f t) fds

rule1 (FVarApp x ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = rule1 t cs in (t':ts, cs')) ([],cs) ts
                          in (FVarApp x ts', cs')
rule1 (BVarApp i ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = rule1 t cs in (t':ts, cs')) ([],cs) ts
                          in (BVarApp i ts', cs')
rule1 (ConApp c ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = rule1 t cs in (t':ts, cs')) ([],cs) ts
                         in (ConApp c ts', cs')
rule1 (Lambda x t) cs = let (t', cs') = rule1 t cs
                        in (Lambda x t', cs')
rule1 (Let x t1 t2) cs = let (t2', cs') = rule1 t2 cs
                         in (Let x t1 t2', cs')
rule1 (FunCall (f, ts)) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = rule1 t cs in (t':ts, cs')) ([],cs) ts
                             in (FunCall (f, ts'), cs')
rule1 t@(Where (f, ts) fds) cs = if (hasFunCall f t)
                                 then let f' =  f ++ "_E"
                                          ts' = [FunCall ("encode_" ++ f, ts)]
                                          (fds', cs') = rule2 fds cs
                                      in (Where (f', ts') fds', cs')
                                 else let (fds', cs') = foldr (\(f,ts,t) (fds,cs) -> let (t',cs') = rule1 t cs in ((f,ts,t'):fds, cs')) ([],cs) fds
                                      in (Where (f, ts) fds', cs')


rule2 fds cs = rule2' fds cs []

rule2' fds cs ps = foldr (\(f,ts,t) (fds,cs) -> let f' = f ++ "_E"
                                                    c' = rename cs "C"
                                                    as = rule3 t
                                                    ts' = [ConApp c' as]
                                                    ps' = (ts', c'):ps
                                                    (t',cs') = rule1 t (c':cs)
                                                in ((f',ts',t'):fds, cs')) ([], cs) fds

rule3 t = []