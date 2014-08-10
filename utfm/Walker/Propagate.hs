module UTFM.Walker.Propagate (getChoices, setDecisions) where

--TODO:  Delte this import when getChoices is rewritten
import UU.Pretty

import UTFM.Boilerplate
import UTFM.AG.AG

getChoices :: Type -> Choices
getChoices t = cType (bType t)

setDecisions :: Type -> Decisions -> Type
setDecisions t ds = dType (bType t) ds

----------------------------------------------------------------
-- Generate Choices for Janota's algorithm
----------------------------------------------------------------

cType :: Type -> Choices
cType (Type r) = cFactory r

cFactories :: Factories -> Choices
cFactories [] = []
cFactories (f:fs) = (cFactory f) ++ (cFactories fs)

cFactory :: Factory -> Choices
cFactory (Factory _ _ is) = cFeatures is

cFeatures :: Features -> Choices
cFeatures [] = []
cFeatures (f:fs) = (cFeature f) ++ (cFeatures fs)

cFeature :: Feature -> Choices
cFeature (Parent n T a c _ g _ _) = (cFactories g) ++ (cAttributes a) ++ (cConstraints c)
cFeature (Parent _ F _ _ _ _ _ _) = []
cFeature (Parent n U a c _ g b _) = f' ++ (cAttributes a) ++ (cConstraints c) ++ (cFactories g)
	where
		f' = [(ChoiceBool (b++"."++n) ((True, "(assert (=" >#< b >|< "." >|< n >#< "true))")
												,(False, "(assert (=" >#< b >|< "." >|< n >#< "false))")))]
cFeature (Child n T a c _ _) = (cAttributes a) ++ (cConstraints c)
cFeature (Child _ F _ _ _ _) = []
cFeature (Child n U a c b _) = f' ++ (cAttributes a) ++ (cConstraints c)
	where
		f' = [(ChoiceBool n ((True, "(assert (=" >#< b >|< "." >|< n >#< "true))")
												,(False, "(assert (=" >#< b >|< "." >|< n >#< "false))")))]

cAttributes :: Attributes -> Choices
cAttributes [] = []
cAttributes (a:as) = (cAttribute a) ++ (cAttributes as)

cAttribute :: Attribute -> Choices
cAttribute (AttrBoolean n v c g b) = if (not c) 
									then [(ChoiceBoolAttr (b ++ "." ++ n)
													((True, "(assert (=" >#< b >|< "." >|< n >#< "true))")
													,(False, "(assert (=" >#< b >|< "." >|< n >#< "false))")))]
									else []
cAttribute _ = []

cConstraints :: Constraints -> Choices
cConstraints []  	= []
cConstraints (c:cs) = (cConstraint c) ++ (cConstraints cs)

cConstraint :: Constraint -> Choices
cConstraint (ConsArithAssignExpr n _ b) = [(ChoiceArithAttr (b++"."++n))]
cConstraint (ConsArithAssignProp n _ b) = [(ChoiceArithAttr (b++"."++n))]
cConstraint _ = []

----------------------------------------------------------------
-- Perform choices on tree
----------------------------------------------------------------

dType :: Type -> Decisions -> Type
dType (Type r) ds = (Type (dFactory r ds))

dFactories :: Factories -> Decisions -> Factories
dFactories [] _ = []
dFactories (f:fs) ds = (dFactory f ds) : (dFactories fs ds)

dFactory :: Factory -> Decisions -> Factory
dFactory (Factory ic tp is) ds = (Factory ic tp (dFeatures is ds))

dFeatures :: Features -> Decisions -> Features
dFeatures [] _ = []
dFeatures (f:fs) ds = (dFeature f ds) : (dFeatures fs ds)

dFeature :: Feature -> Decisions -> Feature
dFeature (Parent n s as cs c g b x) ds = (Parent n s' (dAttributes as ds) cs c (dFactories g ds) b x)
	where
		s' = case findDecision ds (b++"."++n)  of
					Just (DecisionBool _ v) -> (if v then T else F)
					Nothing -> s
dFeature (Child n s as cs b x) ds = (Child n s' (dAttributes as ds) cs b x)
	where
		s' = case findDecision ds n  of
					Just (DecisionBool _ v) -> (if v then T else F)
					Nothing -> s

dAttributes :: Attributes -> Decisions -> Attributes
dAttributes [] _ = []
dAttributes (a:as) ds =  (dAttribute a ds) : (dAttributes as ds)

dAttribute :: Attribute -> Decisions -> Attribute
dAttribute a@(AttrInt n v c g b) ds = (AttrInt n v' c' g b)
	where
		(v', c') = case findDecision ds (b++"."++n) of
								Just (DecisionIntAttr _ v'') -> (v'', True)
								Nothing -> (v, c)
dAttribute a@(AttrReal n v c g b) ds = (AttrReal n v' c' g b)
	where
		(v', c') = case findDecision ds (b++"."++n) of
								Just (DecisionRealAttr _ v'') -> (v'', True)
								Nothing -> (v, c)
dAttribute (AttrBoolean n v c g b) ds = (AttrBoolean n v' c' g b)
	where
		(v', c') = case findDecision ds (b++"."++n) of
								Just (DecisionBoolAttr _ v'') -> (v'', True)
								Nothing -> (v, c)
dAttribute a _ = a

----------------------------------------------------------------
-- Helper function
----------------------------------------------------------------

findDecision :: Decisions -> String -> Maybe Decision
findDecision [] n = Nothing
findDecision ((DecisionBool n v):ds) n' = d'
	where
		d' = if(n == n')
				then Just (DecisionBool n v)
				else findDecision ds n'
findDecision ((DecisionBoolAttr n v):ds) n' = d'
	where
		d' = if(n == n')
				then Just (DecisionBoolAttr n v)
				else findDecision ds n'
findDecision ((DecisionIntAttr n v):ds) n' = d'
	where
		d' = if(n == n')
				then Just (DecisionIntAttr n v)
				else findDecision ds n'
findDecision ((DecisionRealAttr n v):ds) n' = d'
	where
		d' = if(n == n')
				then Just (DecisionRealAttr n v)
				else findDecision ds n'

--TODO: this should not be helper functions, convert getChoices to an AG function
bType :: Type -> Type
bType (Type r) = (Type (bFactory r ""))

bFactories :: Factories -> String -> Factories
bFactories [] _ = []
bFactories (f:fs) b = (bFactory f b) : (bFactories fs b)

bFactory :: Factory -> String -> Factory
bFactory (Factory ic td is) b = (Factory ic (bFeature td b) (bFeatures is b))

bFeatures :: Features -> String -> Features
bFeatures [] _ = []
bFeatures (f:fs) b = (bFeature f b) : (bFeatures fs b)

bFeature :: Feature -> String -> Feature
bFeature (Parent n s as cs c g _ x) "" = (Parent n s (bAttributes as n) (bConstraints cs n) c (bFactories g n) n x)
bFeature (Child n s as cs _ x) "" = (Child n s (bAttributes as n) (bConstraints cs n) n x)
bFeature (Parent n s as cs c g _ x) b = (Parent n s (bAttributes as b') (bConstraints cs b') c (bFactories g b') b x)
		where
			b' = (b++"."++n)
bFeature (Child n s as cs _ x) b = (Child n s (bAttributes as b') (bConstraints cs b') b x)
		where
			b' = (b++"."++n)

bAttributes :: Attributes -> String -> Attributes 
bAttributes [] _ = []
bAttributes (a:as) b = (bAttribute a b) : (bAttributes as b)

bAttribute :: Attribute -> String -> Attribute
bAttribute (AttrInt n v c g _) b = (AttrInt n v c g b)
bAttribute (AttrReal n v c g _) b = (AttrReal n v c g b)
bAttribute (AttrString n v c _) b = (AttrString n v c b)
bAttribute (AttrBoolean n v c g _) b = (AttrBoolean n v c g b)

bConstraints :: Constraints -> String -> Constraints
bConstraints [] _	 = []
bConstraints (c:cs) b = (bConstraint c b) : (bConstraints cs b)

bConstraint :: Constraint -> String -> Constraint
bConstraint (ConsArithAssignExpr x e _) b = (ConsArithAssignExpr x e b)
bConstraint (ConsArithAssignProp x p _) b = (ConsArithAssignProp x p b)
bConstraint c _ = c