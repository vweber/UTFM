module UTFM.Walker.Full (full) where

import UTFM.Boilerplate (Errs, noErrs)
import UTFM.AG.AG

full :: Type -> (Maybe Type, Errs)
full t = noErrs $ fType t

----------------------------------------------------------------
--Change domain to product domain, check all attributes configured
----------------------------------------------------------------

fType :: Type -> (Maybe Type, Errs)
fType (Type r) = ((Just (Type r'), e'))
	where
		(r', e') = fFactory r

fFactories :: Factories -> (Factories, Errs)
fFactories [] = ([],[])
fFactories (f:fs) = (f':fs',e++e')
	where
		(f', e) = fFactory f
		(fs', e') = fFactories fs

fFactory :: Factory -> (Factory, Errs)
fFactory (Factory ic td is) = ((Factory ic td is'), e)
	where
		(is', e) = fFeatures is

fFeatures :: Features -> (Features, Errs)
fFeatures [] = ([],[])
fFeatures (f:fs) = (f'++fs', e++e')
	where
		(f', e) = fFeature f
		(fs', e') = fFeatures fs

fFeature :: Feature -> (Features, Errs)
fFeature (Parent n T as cs c g b x) = ([(Parent n T as' cs c g' b x)], e++e')
	where
		(as', e) = fAttributes as
		(g', e') = fFactories g
fFeature (Child n T as cs b x) = ([(Child n T as' cs b x)], e)
	where
		(as', e) = fAttributes as
fFeature _ = ([],[])

fAttributes :: Attributes -> (Attributes, Errs)
fAttributes [] = ([],[])
fAttributes (a:as) = (a'++as', e++e')
	where
		(a', e) = fAttribute a
		(as', e') = fAttributes as

fAttribute :: Attribute -> (Attributes, Errs)
fAttribute a@(AttrInt _ _ True _ _) = ([a], [])
fAttribute a@(AttrReal _ _ True _ _) = ([a], [])
fAttribute a@(AttrBoolean _ _ True _ _) = ([a], [])
fAttribute a@(AttrString _ _ True _) = ([a], [])
fAttribute a@(AttrInt _ _ False True _) = ([a], [])
fAttribute a@(AttrReal _ _ False True _) = ([a], [])
fAttribute a@(AttrBoolean _ _ False True _) = ([a], [])
-- TODO: Throw error that an attribute is not configured.
fAttribute _ = ([],[])