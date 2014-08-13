module UTFM.Interpreter.Interpreter (interpretTree) where

import UTFM.Boilerplate (Errs, getRandomList, noErrs)
import UTFM.AG.AG (Type)
import UTFM.Interpreter.Z3 (validate, validateChoices)
import UTFM.Walker.Full (full)
import UTFM.Walker.Unfold (unfold, deUnfold)
import UTFM.Walker.Propagate (getChoices, setDecisions)

-- ValidateConf ValidateProd Propagate Unfold Full
interpretTree :: Bool -> Bool -> Bool -> Bool -> Bool -> (Maybe Type, Errs) -> IO (Maybe Type, Errs)
interpretTree True False propagate unfoldT full (Just tree, []) 
	= do 
			l <- getRandomList
			(tree', errs) <- validateTree ((Just (unfold tree l True)), [])
			let tree'' = if(unfoldT) then tree' else (deUnfold tree')
			interpretTree False False propagate unfoldT full (tree'', errs)

interpretTree False True False False False (Just tree, []) 
	= do 
			(tree', errs) <- validateTree (Just tree, [])
			interpretTree False False False False False (tree', errs)

interpretTree False False True unfoldT False (Just tree, []) 
	= do 
			l <- getRandomList
			let tree' = if(unfoldT) then tree else (unfold tree l True)
			let choices = getChoices (unfold tree' l True)
			decisions <- validateChoices tree' choices
			let tree'' =  setDecisions tree' decisions
			let tree''' = if(unfoldT) then (Just tree'') else (deUnfold (Just tree''))
			return (tree''', [])

-- TODO: Add error messaging if not valid product but valid conf, add function something like addError
interpretTree False False False False True (Just tree, []) 
	= do 
			let (tree', errs) = full tree
			(tree'', errs') <- validateTree (tree', errs)
			interpretTree False False False False False (tree'', errs')

interpretTree False False False False False (Just tree, []) = do return (Just tree, [])

-- TODO: Error messaging
interpretTree _ _ _ _ _ (Just tree, errs) = do return (noErrs (Just tree, errs))
interpretTree _ _ _ _ _ (Nothing, errs) = do return (Nothing, errs)

validateTree :: (Maybe Type, Errs) -> IO (Maybe Type, Errs)
validateTree (Nothing, errs) = do return (Nothing, errs)
validateTree (Just tree, []) 
	= do
			errs <- validate tree
			return (noErrs (Just tree, errs))
validateTree (_, errs) = do return (Nothing, errs)