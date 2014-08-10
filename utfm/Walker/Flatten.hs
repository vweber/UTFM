module UTFM.Walker.Flatten (flatten) where

import UTFM.AG.AG

flatten :: Type -> [Int] -> Type
flatten t l = fType t l

----------------------------------------------------------------
-- Flatten cross-tree constraints to local scope
----------------------------------------------------------------
type FlatAttrs = [FlatAttr]
data FlatAttr = FlatAttr String String String Constraint

fType :: Type -> [Int] -> Type
fType (Type t) l = (Type (fFactory t l []))

fFactories :: Factories -> [Int] -> FlatAttrs -> Factories
fFactories [] l [] = []
fFactories [] l fas = []
fFactories (f:fs) l [] = (fFactory f l []) : (fFactories fs l [])
fFactories (f:fs) l fas = (fFactory f l fas) : (fFactories fs l fas)

fFactory :: Factory -> [Int] -> FlatAttrs-> Factory
fFactory (Factory ic td is) l [] = (Factory ic (fFeature td l []) is)
fFactory (Factory ic td is) l fas = (Factory ic (fFeature td l fas) is)

fFeature  :: Feature -> [Int] -> FlatAttrs -> Feature
fFeature (Child n s as cs b x) l  [] = (Child n s as cs b x)
fFeature (Parent n s as cs gc g b x) l [] = (Parent n s as cs' gc (fFactories g l fas) b x)
	where
		(cs', fas) = fConstraints cs l
fFeature (Child n s as cs b x) l fas = (Child n s as' cs' b x)
	where
		as' = addAttributesF n as fas
		cs' = addConstraintsF n cs fas
fFeature (Parent n s as cs gc g b x) l fas = (Parent n s as' cs' gc (fFactories g l newFas) b x)
	where
		as' = addAttributesF n as fas
		cs' = addConstraintsF n cs fas
		(newCs, newFas) = fConstraints cs' l

addAttributesF :: String -> Attributes -> FlatAttrs -> Attributes
addAttributesF n as (fa@(FlatAttr n' _ _ _):[]) 	| n == n' 	= [(addAttribute fa)]
																									| otherwise = as
addAttributesF n as (fa@(FlatAttr n' _ _ _):fas)	| n == n' 	= (addAttributesF n as fas) ++ [(addAttribute fa)]
																									| otherwise = addAttributesF n as fas

addAttribute :: FlatAttr -> Attribute
addAttribute (FlatAttr _ n t _) = a
	where
		a = case t of 
						"real"	-> (AttrReal n 0 False True "")
						"bool" 	-> (AttrBoolean n True False True "")

addConstraintsF :: String -> Constraints -> FlatAttrs -> Constraints
addConstraintsF n cs (fa@(FlatAttr n' _ _ c):[]) 	| n == n' 	= [c]
																									| otherwise = cs
addConstraintsF n cs (fa@(FlatAttr n' _ _ c):fas) | n == n' 	= (addConstraintsF n cs fas) ++ [c]
																									| otherwise = addConstraintsF n cs fas

fConstraints :: Constraints -> [Int] -> (Constraints, FlatAttrs)
fConstraints [] l = ([], [])
fConstraints (c:cs) l = (c':cs', fa++fas)
	where
		(c', fa, l') = (fConstraint c l)
		(cs', fas) = fConstraints cs l'

fConstraint :: Constraint -> [Int] -> (Constraint, FlatAttrs, [Int])
fConstraint (ConsBoolExpr x) l = ((ConsBoolExpr x'), fa, l')
	where
		(x', fa, l') = fBoolExpr x l
fConstraint (ConsBoolAssignExpr a x) l = ((ConsBoolAssignExpr a x'), fa, l')
	where
		(x', fa, l') = fBoolExpr x l
fConstraint (ConsBoolAssignProp a x) l = ((ConsBoolAssignProp a x'), fa, l')
	where
		(x', fa, l') = fBoolProp x l
fConstraint (ConsArithAssignExpr a x b) l = ((ConsArithAssignExpr a x' b), fa, l')
	where
		(x', fa, l') = fArithExpr x l
fConstraint (ConsArithAssignProp a x b) l = ((ConsArithAssignProp a x' b), fa, l')
 	where
 		(x', fa, l') = fArithProp x l

fBoolExpr :: BoolExpr -> [Int] -> (BoolExpr, FlatAttrs, [Int])
fBoolExpr (BoolExprFormula x) l = ((BoolExprFormula x'), fa, l')
	where 
		(x', fa, l') = fBoolFormula x l
fBoolExpr (BoolExprProp x) l = ((BoolExprProp x'), fa, l')
	where 
		(x', fa, l') = fBoolProp x l
fBoolExpr (BoolExprEq x y) l = ((BoolExprEq x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fBoolProp x l
		(y', fa', l'') = fBoolProp y l'
fBoolExpr (BoolExprIneq x y) l = ((BoolExprIneq x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fBoolProp x l
		(y', fa', l'') = fBoolProp y l'
fBoolExpr (ArithExprEq x y) l = ((ArithExprEq x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fBoolExpr (ArithExprIneqSmaller x y) l = ((ArithExprIneqSmaller x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fBoolExpr (ArithExprIneqSmallerEqual x y) l = ((ArithExprIneqSmallerEqual x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fBoolExpr (ArithExprIneqLarger x y) l = ((ArithExprIneqLarger x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fBoolExpr (ArithExprIneqLargerEqual x y) l = ((ArithExprIneqLargerEqual x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fBoolExpr (ArithExprIneqNot x y) l = ((ArithExprIneqNot x' y'), fa++fa', l'')
	where 
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'

fBoolFormula :: BoolFormula -> [Int] -> (BoolFormula, FlatAttrs, [Int])
fBoolFormula (And x y) l = ((And x' y'), fa++fa', l')
	where 
		(x', fa, l') = fBoolProp x l
		(y', fa', l'') = fBoolProp y l'
fBoolFormula (Or x y) l = ((Or x' y'), fa++fa', l')
	where 
		(x', fa, l') = fBoolProp x l
		(y', fa', l'') = fBoolProp y l'
fBoolFormula (Implies x y) l = ((Implies x' y'), fa++fa', l')
	where 
		(x', fa, l') = fBoolProp x l
		(y', fa', l'') = fBoolProp y l'
fBoolFormula (Not x) l = ((Not x'), fa, l')
	where 
		(x', fa, l') = fBoolProp x l

fBoolProp :: BoolProp -> [Int] -> (BoolProp, FlatAttrs, [Int])
fBoolProp (BoolPropOp x) l = ((BoolPropOp x'), fa, l')
	where
		(x', fa, l') = fBoolOp x l
fBoolProp (BoolPropAttr x) l = ((BoolPropAttr x), [], l)
fBoolProp (BoolPropParentAttr x) l = ((BoolPropParentAttr x), [], l)
fBoolProp (BoolPropValue x) l = ((BoolPropValue x), [], l)
fBoolProp (BoolPropExpr x) l = ((BoolPropExpr x'), fa, l')
	where
		(x', fa, l') = fBoolExpr x l

fBoolOp :: BoolOp -> [Int] -> (BoolOp, FlatAttrs, [Int])
fBoolOp (ExistsF f) l = ((ExistsF f), [], l)
fBoolOp (ExistsFO f o) l = ((ExistsFA f n), [(FlatAttr f n "bool" c)], tail l)
	where
		n = "gen" ++ (show $ head l)
		c = (ConsBoolAssignProp n (BoolPropOp o))
fBoolOp (ExistsFA f a) l = ((ExistsFA f a), [], l)
fBoolOp (ExistsFE f e) l = ((ExistsFA f n), [(FlatAttr f n "bool" c)], tail l)
	where
		n = "gen" ++ (show $ head l)
		c = (ConsBoolAssignExpr n e)
fBoolOp (ForallF f) l = ((ForallF f), [], l)
fBoolOp (ForallFO f o) l = ((ForallFA f n), [(FlatAttr f n "bool" c)], tail l)
	where
		n = "gen" ++ (show $ head l)
		c = (ConsBoolAssignProp n (BoolPropOp o))
fBoolOp (ForallFA f a) l = ((ForallFA f a), [], l)
fBoolOp (ForallFE f e) l = ((ForallFA f n), [(FlatAttr f n "bool" c)], tail l)
 	where
 		n = "gen" ++ (show $ head l)
 		c = (ConsBoolAssignExpr n e)

fArithExpr :: ArithExpr -> [Int] -> (ArithExpr, FlatAttrs, [Int])
fArithExpr (Add x y) l = ((Add x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fArithExpr (Subtract x y) l = ((Subtract x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fArithExpr (Multiply x y) l = ((Multiply x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fArithExpr (Divide x y) l = ((Divide x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'

fArithProp :: ArithProp -> [Int] -> (ArithProp, FlatAttrs, [Int])
fArithProp (ArithParent x) l = ((ArithParent x'), fa, l')
	where
		(x', fa, l') = fArithExpr x l
fArithProp (ArithOperation x) l = ((ArithOperation x'), fa, l')
	where
		(x', fa, l') = fArithOp x l
fArithProp (ArithNum x) l = ((ArithNum x), [], l)
fArithProp (ArithAttr x) l = ((ArithAttr x), [], l)
fArithProp (ArithParentAttr x) l = ((ArithParentAttr x), [], l)

fArithOp :: ArithOp -> [Int] -> (ArithOp, FlatAttrs, [Int])
fArithOp (SumFA f a) l = ((SumFA f a), [], l)
fArithOp (SumFO f o) l = ((SumFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignProp n (ArithOperation o) "")
fArithOp (SumFE f e) l = ((SumFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignExpr n e "")
fArithOp (MaxFA f a) l = ((MaxFA f a), [], l)
fArithOp (MaxFO f o) l = ((MaxFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignProp n (ArithOperation o) "")
fArithOp (MaxFE f e) l = ((MaxFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignExpr n e "")
fArithOp (MaxPP x y) l = ((MaxPP x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'
fArithOp (MinFA f a) l = ((MinFA f a), [], l)
fArithOp (MinFO f o) l = ((MinFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignProp n (ArithOperation o) "")
fArithOp (MinFE f e) l = ((MinFA f n), [(FlatAttr f n "real" c)], tail l)
	where
		n = "gen" ++ show (head l)
		c = (ConsArithAssignExpr n e "")
fArithOp (MinPP x y) l = ((MinPP x' y'), fa++fa', l'')
	where
		(x', fa, l') = fArithProp x l
		(y', fa', l'') = fArithProp y l'