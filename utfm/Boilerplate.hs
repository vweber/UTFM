module UTFM.Boilerplate where

import System.Random (newStdGen, randomRs)

import UU.Pretty (PP_Doc)

import UTFM.AG.AG (Type)

type Errs = [(String)]

type Choices = [Choice]
data Choice =
	  ChoiceBool String ((Bool, PP_Doc),(Bool, PP_Doc))
	| ChoiceBoolAttr String ((Bool, PP_Doc),(Bool, PP_Doc))
	| ChoiceArithAttr String

type Decisions = [Decision]
data Decision =
	  DecisionBool String Bool
	| DecisionBoolAttr String Bool
	| DecisionIntAttr String Int
	| DecisionRealAttr String Double

data Printer = CPrinter | PPrinter

getRandomList :: IO [Int]
getRandomList
	= do
		g <- newStdGen
		return (randomRs (0, 99999) g)

noErrs :: (Maybe Type, Errs) -> (Maybe Type, Errs)
noErrs (Just tree, []) = (Just tree, [])
noErrs (Just tree, errs) = (Nothing, errs)
noErrs (Nothing, errs ) = (Nothing, errs)