module Printer where

import Term
import Aux
import Text.PrettyPrint.HughesPJ

instance Show Term where
    show t = render (prettyTerm t)

prettyTerm (FVarApp x ts) = if ts == []
                            then (text x)
                            else parens ((text x) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (BVarApp i ts) = if ts == []
                            then (text "#") <> (int i)
                            else parens ((text "#") <> (int i) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (ConApp c ts) = if ts == []
                           then (text c)
                           else parens ((text c) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm t@(Lambda _ _) = let (xs, t') = stripLambda t
                            in (text "\\") <> (hsep (map text xs) <> (text ".") <> (prettyTerm t'))
--prettyTerm (Let x t t') = let x' = rename (frees t') x
--                          in parens ((((text "let") <+> (text x) <+> (text "=")) <+> (prettyTerm t)) $$ (text "in") <+> (prettyTerm (subst (FVarApp x' []) t')))
prettyTerm (Let x t t') = parens ((((text "let") <+> (text x) <+> (text "=")) <+> (prettyTerm t)) $$ (text "in") <+> (prettyTerm t'))
prettyTerm (FunCall (f, ts)) = if ts == []
                               then (text f)
                               else parens ((text f) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (Where (f, ts) fds) = parens (((text f) <+> (hcat (punctuate space (map prettyTerm ts)))) $$ (text "where") $$ (vcat (map prettyFunDef fds)))
                                 where
                                       prettyFunDef (f, ts, t) = let vs = concatMap frees ts
                                                                     fvs = foldr (\v fvs -> let v' = (rename fvs v) in v':fvs) (frees t) vs
                                                                     vs' = take (length vs) fvs
                                                                     t' = foldr (\v t -> subst (FVarApp v []) t) t vs'
                                                                 in ((text f) <+> (hcat (punctuate space (map prettyTerm ts)))) <+> (text "=") <+> (prettyTerm t)
                                       --prettyFunDef (f, ts, t) = ((text f) <+> (hcat (punctuate space (map prettyTerm ts)))) <+> (text "=") <+> (prettyTerm t)

-- prettyPrint a = print a

prettyPrint (Left a) = print a
prettyPrint (Right a) = print a
