module Aux where

import Term

stripLambda (Lambda x t) = let (xs, t') = stripLambda t
                           in ((x:xs), t')
stripLambda t = ([],t)

rename fvs x = if x `elem` fvs
               then rename fvs (x ++ "'")
               else x

frees t = frees' t []

frees' (FVarApp x ts) fvs = if x `elem` fvs
                            then foldr (\t fvs -> frees' t fvs) fvs ts
                            else foldr (\t fvs -> frees' t fvs) (x:fvs) ts
frees' (BVarApp i ts) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (ConApp c ts) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (Lambda x t) fvs = frees' t fvs
frees' (Let x t t') fvs = frees' t' (frees' t fvs)
frees' (FunCall (f, ts)) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (Where (f, ts) fds) fvs = foldr (\(f, ts, t) fvs -> frees' t fvs) (foldr (\t fvs -> frees' t fvs) fvs ts) fds

abstract x t = abstract' 0 x t

abstract' i x (FVarApp x' ts) = if x == x'
                                then BVarApp i (map (abstract' i x) ts)
                                else FVarApp x' (map (abstract' i x) ts)
abstract' i x (BVarApp i' ts) = if i' < i
                                then BVarApp i' (map (abstract' i x) ts)
                                else BVarApp (i'+1) (map (abstract' i x) ts)
abstract' i x (ConApp c ts) = ConApp c (map (abstract' i x) ts)
abstract' i x (Lambda x' t) = Lambda x' (abstract' (i+1) x t)
abstract' i x (Let x' t t') = Let x' (abstract' i x t) (abstract' (i+1) x t')
abstract' i x (FunCall (f, ts)) = FunCall (f, (map (abstract' i x) ts))
abstract' i x (Where (f, ts) fds) = Where (f, (map (abstract' i x) ts)) (map (\(f,ts,t) -> (f, ts, abstract' (i+(length (concatMap frees ts))) x t)) fds)

shift 0 d t = t
shift i d (FVarApp x ts) = FVarApp x (map (shift i d) ts)
shift i d (BVarApp j ts) = if j >= d
                           then BVarApp (j+i) (map (shift (j+i) d) ts)
                           else BVarApp j (map (shift j d) ts)
shift i d (ConApp c ts) = ConApp c (map (shift i d) ts)
shift i d (Lambda x t) = Lambda x (shift i (d+1) t)
shift i d (Let x t t') = Let x (shift i d t) (shift i (d+1) t')
shift i d (FunCall (f, ts)) = FunCall (f, map (shift i d) ts)
shift i d (Where (f, ts) fds) = Where (f, map (shift i d) ts) (map (\(f,ts,t) -> (f, ts, shift i (d+(length (concatMap frees ts))) t)) fds)

subst t t' = subst' 0 t t'

subst' i t (FVarApp x ts) = FVarApp x (map (subst' i t) ts)
subst' i t@(FVarApp x _) (BVarApp i' ts) = if i' == i
                                           then FVarApp x (map (subst' i t) ts)
                                           else if i' < i
                                                then BVarApp i' (map (subst' i t) ts) -- removing a variable bound before i'
                                                else BVarApp (i'-1) (map (subst' i t) ts) -- removing a vairable bound after i'
subst' i t (ConApp c ts) = ConApp c (map (subst' i t) ts)
subst' i t (Lambda x t') = Lambda x (subst' (i+1) t t')
subst' i t (Let x t1 t2) = Let x (subst' i t t1) (subst' (i+1) t t2)
subst' i t (FunCall (f, ts)) = FunCall (f, (map (subst' i t) ts))
subst' i t (Where (f, ts) fds) = Where (f, (map (subst' i t) ts)) (map (\(f',ts',t') -> (f', ts', subst' (i+(length (concatMap frees ts'))) t t')) fds)