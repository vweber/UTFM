module UTFM.Walker.Merge (merge) where

import UTFM.AG.AG

merge :: Type -> (Maybe Configuration) -> (Maybe Product) -> Type
merge t (Just c) Nothing = mType t c
merge t Nothing (Just p) = mTypeP t p
----------------------------------------------------------------
-- Merge configuration in type declaration data structure
----------------------------------------------------------------

mType :: Type -> Configuration -> Type
mType (Type r) c = (Type (mFactory r [c]))

mFactories :: Factories -> Configurations -> Factories
mFactories (f:[]) cs = [(mFactory f cs)]
mFactories (f:fs) cs = (mFactory f cs) : (mFactories fs cs)

mFactory :: Factory -> Configurations -> Factory
mFactory (Factory ic td _) cs = (Factory ic td is)
	where
		is = mInstances td s
		s = selectConfigurations cs (getName td)

mInstances :: Feature -> Configurations -> Features
mInstances _ [] = []
mInstances td (c:cs) = (mInstance td c) : (mInstances td cs)

--TODO: Fixme for error handling
mInstance :: Feature -> Configuration -> Feature
mInstance (Child n' _ a' c b x) 			(Configuration s _ n a []) 	
	= (Child n (mSelected s) (mAttributesConfs a' a) c b x)
mInstance (Parent n' _ a' cs c g b x) 		(Configuration s _ n a [])	
	= (Parent n (mSelected s) (mAttributesConfs a' a) cs c g b x)
mInstance (Child _ _ _ _ _ _) 				(Configuration s _ n a g)
	= (Child "this is wrong FIXME" U [] [] "" False)
mInstance (Parent n' _ a' cs' c' g' b x) 	(Configuration s _ n a g)
	= (Parent n (mSelected s) (mAttributesConfs a' a) cs' c' (mFactories g' g) b x)

mAttributesConfs :: Attributes -> ConfAttributes -> Attributes
mAttributesConfs [] _	 		= []
mAttributesConfs (a:as) cs 	= (mAttributeConfs a cs) ++ (mAttributesConfs as cs)

mAttributeConfs :: Attribute -> ConfAttributes -> Attributes
mAttributeConfs a []												= [a]
mAttributeConfs a@(AttrInt n v _ g b) 		((ConfAttrInt n' v'):cs) 	= if (n==n') 
																		then [(AttrInt n v' True g b)]
																		else (mAttributeConfs a cs)
mAttributeConfs a@(AttrReal n v _ g b) 		((ConfAttrReal n' v'):cs) 	= if (n==n') 
																		then [(AttrReal n v' True g b)]
																		else (mAttributeConfs a cs)
mAttributeConfs a@(AttrString n v _ b) 		((ConfAttrString n' v'):cs)	= if (n==n')
																		then [(AttrString n v' True b)]
																		else (mAttributeConfs a cs)
mAttributeConfs a@(AttrBoolean n v _ g b) 	((ConfAttrBoolean n' v'):cs)= if (n==n') 
																		then [(AttrBoolean n v' True g b)]
																		else (mAttributeConfs a cs)
mAttributeConfs a (c:cs) 											= mAttributeConfs a cs

mSelected :: ConfSelected -> Selected
mSelected cs = case cs of
				CT -> T
				CF -> F
				CU -> U

----------------------------------------------------------------
-- Same functions but for products
----------------------------------------------------------------

mTypeP :: Type -> Product -> Type
mTypeP (Type r) p = (Type (mFactoryP r [p]))

mFactoriesP :: Factories -> Products -> Factories
mFactoriesP (f:[]) cs = [(mFactoryP f cs)]
mFactoriesP (f:fs) cs = (mFactoryP f cs) : (mFactoriesP fs cs)

mFactoryP :: Factory -> Products -> Factory
mFactoryP (Factory ic td _) cs = (Factory ic td is)
	where
		is = mInstancesP td s
		s = selectProducts cs (getName td)

mInstancesP :: Feature -> Products -> Features
mInstancesP _ [] = []
mInstancesP td (c:cs) = (mInstanceP td c) : (mInstancesP td cs)

--TODO: Fixme for error handling
mInstanceP :: Feature -> Product -> Feature
mInstanceP (Child n' _ a' c b x) 			(Product _ n a []) 	
	= (Child n T (mAttributesProds a' a) c b x)
mInstanceP (Parent n' _ a' cs c g b x) 		(Product _ n a [])	
	= (Parent n T (mAttributesProds a' a) cs c g b x)
mInstanceP (Child _ _ _ _ _ _) 				(Product _ n a g)
	= (Child "this is wrong FIXME" U [] [] "" False)
mInstanceP (Parent n' _ a' cs' c' g' b x) 	(Product _ n a g)
	= (Parent n T (mAttributesProds a' a) cs' c' (mFactoriesP g' g) b x)

mAttributesProds :: Attributes -> ProdAttributes -> Attributes
mAttributesProds [] _	 		= []
mAttributesProds (a:as) cs 	= (mAttributeProds a cs) ++ (mAttributesProds as cs)

mAttributeProds :: Attribute -> ProdAttributes -> Attributes
mAttributeProds a []												= [a]
mAttributeProds a@(AttrInt n v _ g b) 		((ProdAttrInt n' v'):cs) 	= if (n==n') 
																		then [(AttrInt n v' True g b)]
																		else (mAttributeProds a cs)
mAttributeProds a@(AttrReal n v _ g b) 		((ProdAttrReal n' v'):cs) 	= if (n==n') 
																		then [(AttrReal n v' True g b)]
																		else (mAttributeProds a cs)
mAttributeProds a@(AttrString n v _ b) 		((ProdAttrString n' v'):cs)	= if (n==n')
																		then [(AttrString n v' True b)]
																		else (mAttributeProds a cs)
mAttributeProds a@(AttrBoolean n v _ g b) 	((ProdAttrBoolean n' v'):cs)= if (n==n') 
																		then [(AttrBoolean n v' True g b)]
																		else (mAttributeProds a cs)
mAttributeProds a (c:cs) 											= mAttributeProds a cs




----------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------

getName :: Feature -> String
getName (Parent n _ _ _ _ _ _ _) = n
getName (Child n _ _ _ _ _) = n

selectConfigurations :: Configurations -> String -> Configurations
selectConfigurations (c@(Configuration _ t _ _ _):[]) t'
	= if(t == t') then [c] else []
selectConfigurations (c@(Configuration _ t _ _ _):cs) t'
	= if(t == t') 
		then c : selectConfigurations cs t'
		else selectConfigurations cs t'

selectProducts :: Products -> String -> Products
selectProducts (c@(Product t _ _ _):[]) t'
	= if(t == t') then [c] else []
selectProducts (c@(Product t _ _ _):cs) t'
	= if(t == t') 
		then c : selectProducts cs t'
		else selectProducts cs t'