module EncodeDecode where

import Term
import Aux

encode (Left a) = Left a
encode (Right a) = Right (fst (encode' a []))

encode' (FVarApp x ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                            in (FVarApp x ts', cs')
encode' (BVarApp i ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                            in ((BVarApp i ts'), cs')
encode' (ConApp c ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                            in ((ConApp c ts'), cs')
encode' (Lambda x t) cs = let (t', cs') = encode' t cs
                          in ((Lambda x t'), cs')
encode' (Let x t1 t2) cs = encode' (subst (FVarApp x []) t2) cs
--encode' (Let x t1 t2) cs = let (t2', cs') = encode' t2 cs
--                           in ((Let x t1 t2'), cs')
encode' (FunCall (f, ts)) cs = (FunCall ("encode_"++f, ts), cs)
encode' (Where (f, ts) fds) cs = let f' = "encode_" ++ f
                                     ts' = ts
                                     (fds', cs') = let (fs, tss, ts) = unzip3 fds
                                                       fs' = map ("encode_" ++) fs
                                                       tss' = tss
                                                       (ts', cs') = foldr (\ t (ts,cs) -> let (t', cs') = encode'' t cs in (t':ts, cs')) ([], cs) ts
                                                   in (zip3 fs' tss' ts', cs')
                                 in ((Where (f', ts') fds'), cs')

encode'' (FVarApp x ts) cs = let c' = rename cs "C"
                                 (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ((FVarApp x []):ts)
                             in ((ConApp c' ts'), cs')
encode'' (BVarApp i ts) cs = let c' = rename cs "C"
                                 (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ((BVarApp i []):ts)
                             in ((ConApp c' ts'), cs')
encode'' (ConApp c ts) cs = let c' = rename cs "C"
                                (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ts
                             in ((ConApp c' ts'), cs')
encode'' (Lambda x t) cs = let c' = rename cs "C"
                               (t', cs') = encode' t (c':cs)
                             in ((Lambda x t'), cs')
encode'' (Let x t1 t2) cs = let c' = rename cs "C"
                                (t2', cs') = encode'' (subst (FVarApp x []) t2) (c':cs)
                            in (t2', cs')
encode'' t cs = let c' = rename cs "C"
                    (t', cs') = encode' t (c':cs)
                in ((ConApp c' [t']), cs')

{-|
encode' (FVarApp x ts) cs = let (ts', cs') = encode'' (FVarApp x ts) cs
                            in ((FVarApp x ts'), cs')
encode' (BVarApp i ts) cs = let (ts', cs') = encode'' (BVarApp i ts) cs
                            in ((BVarApp i ts'), cs')
encode' (ConApp c ts) cs = let (ts', cs') = encode'' (ConApp c ts) cs
                            in ((ConApp c ts'), cs')
encode' (Lambda x t) cs = let (t', cs') = encode' t cs
                          in ((Lambda x t'), cs')
encode' (Let x t1 t2) cs = let (t2', cs') = encode' t2 cs
                           in ((Let x t1 t2'), cs')
encode' (FunCall (f, ts)) cs = (FunCall ("encode_"++f, ts), cs)
encode' (Where (f, ts) fds) cs = let f' = "encode_" ++ f
                                     ts' = ts
                                     (fds', cs') = let (fs, tss, ts) = unzip3 fds
                                                       fs' = map ("encode_" ++) fs
                                                       tss' = tss
                                                       (ts', cs') = foldr (\ t (ts,cs) -> let c' = rename cs "C"
                                                                                              (ts', cs') = encode'' t (c':cs)
                                                                                          in ((ConApp c' ts'):ts, cs')) ([], cs) ts
                                                   in (zip3 fs' tss' ts', cs')
                                 in ((Where (f', ts') fds'), cs')


encode'' (FVarApp x ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                             in (ts', cs')
encode'' (BVarApp i ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                             in (ts', cs')
encode'' (ConApp c ts) cs = let (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                            in (ts', cs')
encode'' (Lambda x t) cs = encode'' t cs
encode'' (Let x t1 t2) cs = encode'' t2 cs
encode'' t cs = let (t', cs') = encode' t cs
                in ([t'], cs')
|-}

{-|encode' (FVarApp x ts) cs = let c' = rename cs "C"
                                (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ts
                            in ((ConApp c' ts'), cs')
encode' (BVarApp i ts) cs = let c' = rename cs "C"
                                (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ts
                            in ((ConApp c' ts'), cs')
encode' (ConApp c ts) cs = let c' = rename cs "C"
                               (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], c':cs) ts
                           in ((ConApp c' ts'), cs')
encode' (Lambda x t) cs = encode' t cs
encode' (Let x t1 t2) cs = encode' t2 cs
encode' (FunCall (f, ts)) cs = let f' = "encode_" ++ f
                                   (ts', cs') = foldr (\t (ts,cs) -> let (t',cs') = encode' t cs in (t':ts, cs')) ([], cs) ts
                               in ((FunCall (f', ts')), cs')
encode' (Where (f, ts) fds) cs = let f' = "encode_" ++ f
                                     ts' = ts
                                     (fds', cs') = foldr (\(f,ts,t) (fds,cs) -> let (t',cs') = encode' t cs in (("encode_"++f,ts,t'):fds, cs')) ([], cs) fds
                                 in ((Where (f', ts') fds'), cs')|-}