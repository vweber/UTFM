module UTFM.Walker.Unfold (unfold, deUnfold, deAttribute) where

import UTFM.Boilerplate
import UTFM.AG.AG

unfold :: Type -> [Int] -> Bool -> Type
unfold t l b = uType t l b

deUnfold :: (Maybe Type) -> (Maybe Type)
deUnfold Nothing = Nothing
deUnfold (Just t) = (Just (duType t))

deAttribute :: Type -> Type
deAttribute t = daType t

----------------------------------------------------------------
-- Unfold Type declaration
----------------------------------------------------------------

uType :: Type -> [Int] -> Bool -> Type
uType (Type r) l d = (Type r')
	where
		(r', l') = uFactory r l d

uFactories :: Factories -> [Int] -> Bool -> (Factories, [Int])
uFactories [] l _ = ([], l)
uFactories (f:fs) l d = (f' : fs', l'')
	where
		(f', l') = uFactory f l d
		(fs', l'') = uFactories fs l' d

uFactory :: Factory -> [Int] -> Bool -> (Factory, [Int])
uFactory (Factory ic tp is) l d = ((Factory ic tp is'), l')
	where 
		(is', l') = unfoldInstances tp is (length is) (getMaxCardinality ic) l d

uFeatures :: Features -> [Int] -> Bool -> (Features, [Int])
uFeatures [] l _ = ([], l)
uFeatures (f:fs) l d = (f' : fs', l'') 
	where
		(f', l') = uFeature f l d
		(fs', l'') = uFeatures fs l' d

uFeature :: Feature -> [Int] -> Bool -> (Feature, [Int])
uFeature (Parent n s as cs c g b x) l d = ((Parent n s as cs c g' b x), l')
	where
		(g', l') = uFactories g l d
uFeature c@(Child _ _ _ _ _ _) l d = (c, l)

unfoldInstances :: Feature -> Features -> Int -> Int -> [Int] -> Bool -> (Features, [Int])
unfoldInstances tp is i max l d 
	= if (i >= max) 
			then (uFeatures is l d)
			else (is' ++ [i'], l'')
	where
		(i', l') = newInstance tp (i+1) max l d
		(is', l'') = unfoldInstances tp is (i+1) max l' d

newInstance :: Feature -> Int -> Int -> [Int] -> Bool -> (Feature, [Int])
newInstance (Parent n _ a _ c g b _) i max l d = ((Parent (n ++ show (head l)) U a [] c g' b True), tail l')
	where
		(g', l') = uFactories g l d
newInstance (Child n _ a _ b _) i max l d = ((Child (n ++ show (head l)) U a [] b True), tail l)

getMaxCardinality :: Cardinality -> Int
getMaxCardinality (Cardinality _ u _) = u

----------------------------------------------------------------
-- Deunfold Type declaration
----------------------------------------------------------------

duType :: Type -> Type
duType (Type r) = (Type (duFactory r))

duFactories :: Factories -> Factories
duFactories [] = []
duFactories (f:fs) = (duFactory f) : (duFactories fs)

duFactory :: Factory -> Factory
duFactory (Factory ic tp is) = (Factory ic tp (duFeatures is))

duFeatures :: Features -> Features
duFeatures [] = []
duFeatures (f:fs) = (duFeature f) ++ (duFeatures fs)

duFeature :: Feature -> Features
duFeature p@(Parent _ _ _ _ _ _ _ True) = []
duFeature c@(Child _ _ _ _ _ True) = []
duFeature (Parent n s as cs c g b False) = [(Parent n s as cs c (duFactories g) b False)]
duFeature c@(Child _ _ _ _ _ False) = [c]

----------------------------------------------------------------
-- Deattributed Type declaration - removing generated attributes
----------------------------------------------------------------

daType :: Type -> Type
daType (Type r) = (Type (daFactory r))

daFactories :: Factories -> Factories
daFactories [] = []
daFactories (f:fs) = (daFactory f) : (daFactories fs)

daFactory :: Factory -> Factory
daFactory (Factory ic tp is) = (Factory ic tp (daFeatures is))

daFeatures :: Features -> Features
daFeatures [] = []
daFeatures (f:fs) = (daFeature f) : (daFeatures fs)

daFeature :: Feature -> Feature
daFeature (Parent n s as cs c g b x) = (Parent n s (daAttributes as) cs c (daFactories g) b x)
daFeature (Child n s as cs b x) = (Child n s (daAttributes as) cs b x)

daAttributes :: Attributes -> Attributes
daAttributes [] = []
daAttributes (a:as) = (daAttribute a) ++ (daAttributes as)

daAttribute :: Attribute -> Attributes
daAttribute (AttrInt _ _ _ True _) = []
daAttribute (AttrReal _ _ _ True _) = []
daAttribute (AttrBoolean _ _ _ True _) = []
daAttribute a = [a]