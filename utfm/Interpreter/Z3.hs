module UTFM.Interpreter.Z3 (validate, validateChoices) where

import Data.Char (isSpace)
import Data.List (isInfixOf)
import System.Process (readProcess)

import UU.Pretty (PP_Doc, disp, pp)

import UTFM.Boilerplate
import UTFM.AG.AG (Type, z_Syn_Type, addPP, checkSat, transformType, evalNumber)

validate :: Type -> IO Errs
validate tree
	= do 
			let zDoc = z_Syn_Type (transformType tree)
			res <- zQueryBool $ checkSat zDoc
			return (throwError res)

validateChoices :: Type -> Choices -> IO (Decisions)
validateChoices tree choices
	= do
			let zDoc = z_Syn_Type (transformType tree)
			performChoices zDoc choices

performChoices :: PP_Doc -> Choices -> IO (Decisions)
performChoices _ [] 
	= do return []
performChoices z (c:cs) 
	= do 
			decision <- (performChoice z c)
			decisions <- (performChoices z cs)
			return (decision ++ decisions)

performChoice :: PP_Doc -> Choice -> IO Decisions
performChoice z (ChoiceBool n ((t,pp1),(f,pp2))) 
	= do
			l <- zQueryBool $ checkSat $ addPP z pp1
			r <- zQueryBool $ checkSat $ addPP z pp2
			return (decisionBool n l r)
performChoice z (ChoiceBoolAttr n ((t,pp1),(f,pp2))) 
	= do
			l <- zQueryBool $ checkSat $ addPP z pp1
			r <- zQueryBool $ checkSat $ addPP z pp2
			return (decisionBoolAttr n l r)
performChoice z (ChoiceArithAttr n) 
	= do 	zQueryArith n $ evalNumber (checkSat z) (pp n)

zQuery :: PP_Doc -> IO (String, String)
zQuery z
	= do
			l <- getRandomList
			writeFile ("./tmp/test"++ show (head l) ++".smt2") (disp z 80 "")
			s <- readProcess "./lib/z3-4.3.0-x64/bin/z3" [("./tmp/test"++ show (head l) ++".smt2")] ""
			return $ span (not.isSpace) s

zQueryBool :: PP_Doc -> IO Bool
zQueryBool z 
	= do
			(l,r) <- zQuery z
			return (l == "sat")

zQueryArith :: String -> PP_Doc -> IO Decisions
zQueryArith n z
	= do
			(l,r) <- zQuery z
			return (if(isInfixOf "." r) then (decisionRealAttr n (read r :: Double)) else (decisionIntAttr n (read r :: Int)))

zQueryReal :: PP_Doc -> IO Double
zQueryReal z 
	= do
			(l,r) <-zQuery z
			return (read r :: Double)

throwError :: Bool -> Errs
throwError True = []
throwError False = ["unsat"]

decisionBool :: String -> Bool -> Bool -> Decisions
decisionBool _ True True = []
decisionBool _ False False = []
decisionBool n True False = [(DecisionBool n True)]
decisionBool n False True = [(DecisionBool n False)]

decisionBoolAttr :: String -> Bool -> Bool -> Decisions
decisionBoolAttr _ True True = []
decisionBoolAttr _ False False = []
decisionBoolAttr n True False = [(DecisionBoolAttr n True)]
decisionBoolAttr n False True = [(DecisionBoolAttr n False)]

decisionIntAttr :: String -> Int -> Decisions
decisionIntAttr n v = [(DecisionIntAttr n v)]

decisionRealAttr :: String -> Double -> Decisions
decisionRealAttr n v = [(DecisionRealAttr n v)]