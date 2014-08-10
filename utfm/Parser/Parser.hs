module UTFM.Parser.Parser (parseFiles) where

import UU.Parsing
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.GenTokenOrd()
import UU.Scanner.GenTokenSymbol()
import UU.Scanner.TokenShow()

import UTFM.Boilerplate (Errs, getRandomList, noErrs)
import UTFM.AG.AG
import UTFM.Parser.Scanner (runScanner)
import UTFM.Walker.Flatten (flatten)
import UTFM.Walker.Merge (merge)

----------------------------------------------------------------
-- UTFM Parser
----------------------------------------------------------------

parseFiles :: String -> String -> String -> IO (Maybe Type, Errs)
parseFiles "" _ _	= do return (Nothing, [])
parseFiles td "" ""	= parseTypeDecl td
parseFiles td c ""	= parseTypeDeclAndConf td c
parseFiles td "" p 	= parseTypeDeclAndProd td p

parseTypeDecl :: String -> IO (Maybe Type, Errs)
parseTypeDecl sourceType
	= do
		inputType <- readFile sourceType
		let toksType = runScanner sourceType inputType
		typedeclaration <- runParserType toksType
		return (Just typedeclaration, [])

parseTypeDeclAndConf :: String -> String -> IO (Maybe Type, Errs)
parseTypeDeclAndConf sourceType sourceConfig
	= do
		inputType <- readFile sourceType
		let toksType = runScanner sourceType inputType
		typedeclaration <- runParserType toksType
		l <- getRandomList
		let typedeclaration' = flatten typedeclaration l
		inputConfig <- readFile sourceConfig
		let toksConfig = runScanner sourceConfig inputConfig
		configuration <- runParserConfiguration toksConfig
		let typedeclaration'' = merge typedeclaration' (Just configuration) Nothing
		return (Just typedeclaration'', [])

parseTypeDeclAndProd :: String -> String -> IO (Maybe Type, Errs)
parseTypeDeclAndProd sourceType sourceProd
	= do
		inputType <- readFile sourceType
		let toksType = runScanner sourceType inputType
		typedeclaration <- runParserType toksType
		l <- getRandomList
		let typedeclaration' = flatten typedeclaration l
		inputProd <- readFile sourceProd
		let toksProd = runScanner sourceProd inputProd
		prod <- runParserProduct toksProd
		let typedeclaration'' = merge typedeclaration' Nothing (Just prod)
		return (Just typedeclaration'', [])

----------------------------------------------------------------
-- UTFM Parser additional functions
----------------------------------------------------------------

runParserType :: [Token] -> IO Type
runParserType
	= parseIOMessage show pType

runParserConfiguration :: [Token] -> IO Configuration
runParserConfiguration 
	= parseIOMessage show pConfiguration

runParserProduct :: [Token] -> IO Product
runParserProduct
	= parseIOMessage show pProduct

readIntToken :: String -> Int
readIntToken = (\a -> (read a :: Int))

readRealToken :: String -> String -> Double
readRealToken = (\a b -> (read (a ++ "." ++ b) :: Double))

----------------------------------------------------------------
-- UTFM Parse TypeDeclaration
----------------------------------------------------------------

pType :: Parser Token Type
pType
	= 	Type <$> (Factory <$> pCardinality "Instance" <*> pFeature <*> (pSucceed []))

pFactories :: Parser Token Factories
pFactories 
	= 	pList pFactory
	<|> pKey "{" *> (pList pFactory) <* pKey "}"

pFactory :: Parser Token Factory
pFactory
	= 	Factory <$> pCardinality "Instance" 
						<*> pFeature 
						<*> (pSucceed [])

pFeatures :: Parser Token Features
pFeatures 
	= 	pList pFeature
	<|> pKey "{" *> (pList pFeature) <* pKey "}"

pFeature :: Parser Token Feature
pFeature 
	= 	Parent <$> pString
						<*> (pSucceed  U)
						<*> ((pKey "attributes" *> pKey ":" *> pAttributes) <|> (pSucceed []))
						<*> ((pKey "constraints" *> pKey ":" *> pConstraints) <|> (pSucceed []))
						<*> (pKey "group" *> pCardinality "Group" <* pKey ":") 
						<*> pFactories
						<*> pSucceed ""
						<*> pSucceed False
	<|>	Child <$> pString 
						<*> (pSucceed U) 
						<*> ((pKey "attributes" *> pKey ":" *> pAttributes) <|> (pSucceed [])) 
						<*> ((pKey "constraints" *> pKey ":" *> pConstraints) <|> (pSucceed []))
						<*> pSucceed ""
						<*> pSucceed False

pCardinality :: String -> Parser Token Cardinality
pCardinality t
	= 	Cardinality <$> (pKey "[" *>  (readIntToken <$> pInteger10))
						<*> (pKey ".." *> (readIntToken <$> pInteger10) <* pKey "]")
						<*> (pSucceed t)

pAttributes :: Parser Token Attributes
pAttributes	
	= 	pList pAttribute
	<|> pKey "{" *> (pList pAttribute) <* pKey "}"

pAttribute :: Parser Token Attribute
pAttribute
	=	AttrInt <$ pKey "Int" <*> pString <*> (pSucceed 0) <*> (pSucceed False) <*> (pSucceed False) <* pKey ";" <*> (pSucceed "")
	<|> AttrReal <$ pKey "Real" <*> pString <*> (pSucceed 0) <*> (pSucceed False) <*> (pSucceed False) <* pKey ";" <*> (pSucceed "")
	<|> AttrBoolean <$ pKey "Bool" <*> pString <*> (pSucceed True) <*> (pSucceed False) <*> (pSucceed False) <* pKey ";" <*> (pSucceed "")
	<|>	AttrString <$ pKey "String" <*> pString <*> (pSucceed "") <*> (pSucceed False) <* pKey ";" <*> (pSucceed "")
	
----------------------------------------------------------------
-- UTFM Parse Constraint Language
----------------------------------------------------------------

pConstraints :: Parser Token Constraints
pConstraints 
	= 	pList pConstraint
	<|> pKey "{" *> (pList pConstraint) <* pKey "}"

pConstraint :: Parser Token Constraint
pConstraint 
	= 	ConsBoolExpr <$> pBoolExpr <* pKey ";"
	<|> ConsBoolAssignExpr <$> pString <* pKey "=" <*> pBoolExpr <* pKey ";"
	<|> ConsBoolAssignProp <$> pString <* pKey "=" <*> pBoolProp <* pKey ";"
	<|> ConsArithAssignExpr <$> pString <* pKey "=" <*> pArithExpr <* pKey ";" <*> (pSucceed "")
	<|> ConsArithAssignProp <$> pString <* pKey "=" <*> pArithProp <* pKey ";" <*> (pSucceed "")

pBoolExpr :: Parser Token BoolExpr
pBoolExpr
	= 	BoolExprFormula <$> pBoolFormula
	<|> BoolExprProp <$> pBoolProp 
	<|> BoolExprEq <$> pBoolProp <* pKey "==" <*> pBoolProp
	<|> BoolExprIneq <$> pBoolProp <* pKey "!=" <*> pBoolProp
	<|> ArithExprEq <$> pArithProp <* pKey "==" <*> pArithProp
	<|> ArithExprIneqSmaller <$> pArithProp <* pKey "<" <*> pArithProp
	<|> ArithExprIneqSmallerEqual <$> pArithProp <* pKey "<=" <*> pArithProp
	<|> ArithExprIneqLarger <$> pArithProp <* pKey ">" <*> pArithProp
	<|> ArithExprIneqLargerEqual <$> pArithProp <* pKey ">=" <*> pArithProp
	<|> ArithExprIneqNot <$> pArithProp <* pKey "!=" <*> pArithProp

pBoolFormula :: Parser Token BoolFormula
pBoolFormula
	= 	And <$> pBoolProp <* pKey "and" <*> pBoolProp
	<|> Or <$> pBoolProp <* pKey "or" <*> pBoolProp
	<|> Implies <$> pBoolProp <* pKey "implies" <*> pBoolProp
	<|> Not <$ pKey "not" <*> pBoolProp

pBoolProp :: Parser Token BoolProp
pBoolProp
	=	BoolPropOp <$> pBoolOp
	<|> BoolPropAttr <$> pString
	<|> BoolPropParentAttr <$> pParentRef
	<|> BoolPropValue <$> pBool
	<|> BoolPropExpr <$> (pParens pBoolExpr)

pBoolOp :: Parser Token BoolOp
pBoolOp
	=	ExistsF <$ pKey "exists" <*> (pParens pString)
	<|> ExistsFO <$ pKey "exists" <* pKey "(" <*> pString <* pKey "." <*> pBoolOp <* pKey ")"
	<|> ExistsFA <$ pKey "exists" <* pKey "(" <*> pString <* pKey "." <*> pString <* pKey ")"
	<|> ExistsFE <$ pKey "exists" <* pKey "(" <*> pString <* pKey "." <*> (pParens pBoolExpr) <* pKey ")"
	<|> ForallF <$ pKey "forall" <*> (pParens pString)
	<|> ForallFO <$ pKey "forall" <* pKey "(" <*> pString <* pKey "." <*> pBoolOp <* pKey ")"
	<|> ForallFA <$ pKey "forall" <* pKey "(" <*> pString <* pKey "." <*> pString <* pKey ")"
	<|> ForallFE <$ pKey "forall" <* pKey "(" <*> pString <* pKey "." <*> (pParens pBoolExpr) <* pKey ")"
 
pArithExpr :: Parser Token ArithExpr
pArithExpr
	=	Add <$> pArithProp <* pKey "+" <*> pArithProp
	<|> Subtract <$> pArithProp <* pKey "-" <*> pArithProp
	<|> Multiply <$> pArithProp <* pKey "*" <*> pArithProp
	<|> Divide <$> pArithProp <* pKey "/" <*> pArithProp

pArithProp :: Parser Token ArithProp
pArithProp
	=	ArithParent <$> (pParens pArithExpr)
	<|>	ArithOperation <$> pArithOp
	<|> ArithNum <$> pNumber
	<|> ArithAttr <$> pString
	<|> ArithParentAttr <$> pParentRef

pArithOp :: Parser Token ArithOp
pArithOp
	=	SumFA <$ pKey "sum" <* pKey "(" <*> pString <* pKey "." <*> pString <* pKey ")"
	<|> SumFO <$ pKey "sum" <* pKey "(" <*> pString <* pKey "." <*> pArithOp <* pKey ")"
	<|> SumFE <$ pKey "sum" <* pKey "(" <*> pString <* pKey "." <*> (pParens pArithExpr) <* pKey ")"
	<|> MaxFA <$ pKey "max" <* pKey "(" <*> pString <* pKey "." <*> pString <* pKey ")"
	<|> MaxFO <$ pKey "max" <* pKey "(" <*> pString <* pKey "." <*> pArithOp <* pKey ")"
	<|> MaxFE <$ pKey "max" <* pKey "(" <*> pString <* pKey "." <*> (pParens pArithExpr) <* pKey ")"
	<|> MaxPP <$ pKey "max" <* pKey "(" <*> pArithProp <* pKey "," <*> pArithProp <* pKey ")"
	<|> MinFA <$ pKey "min" <* pKey "(" <*> pString <* pKey "." <*> pString <* pKey ")"
	<|> MinFO <$ pKey "min" <* pKey "(" <*> pString <* pKey "." <*> pArithOp <* pKey ")"
	<|> MinFE <$ pKey "min" <* pKey "(" <*> pString <* pKey "." <*> (pParens pArithExpr) <* pKey ")"
	<|> MinPP <$ pKey "min" <* pKey "(" <*> pArithProp <* pKey "," <*> pArithProp <* pKey ")"

pParentRef :: Parser Token ParentRef
pParentRef
	=	ParentAttrRef <$ pKey "parent" <* pKey "." <*> pString
	<|>	ParentChain <$ pKey "parent" <* pKey "." <*> pParentRef

pNumber :: Parser Token Number
pNumber
	=	VInt <$> (readIntToken <$> pInteger10)
	<|> VReal <$> (readRealToken <$> pInteger10 <* pKey "." <*> pInteger10)

pBool :: Parser Token Bool
pBool = ((pKey "true" *> (pSucceed True)) <|> (pKey "false" *> (pSucceed False)))

----------------------------------------------------------------
-- UTFM Parse Configuration
----------------------------------------------------------------

pConfigurations :: Parser Token Configurations
pConfigurations 
	= 	pList pConfiguration
	<|> pKey "{" *> (pList pConfiguration) <* pKey "}"

pConfiguration :: Parser Token Configuration
pConfiguration
	=	Configuration <$> (pConfigurationSelected <|> (pSucceed CT))
						<*> pString <* pKey ":" <*> pString  
						<*> ((pKey "attributes" *> pKey ":" *> pConfigurationAttributes) <|> (pSucceed []))
						<*> ((pKey "group" *> pKey ":" *> pConfigurations) <|> (pSucceed []))

pConfigurationAttributes:: Parser Token ConfAttributes
pConfigurationAttributes 
	= 	pList pConfigurationAttribute
	<|> pKey "{" *> (pList pConfigurationAttribute) <* pKey "}"

pConfigurationAttribute :: Parser Token ConfAttribute
pConfigurationAttribute 	
	=	ConfAttrInt	<$> pString <* pKey "=" <*> (readIntToken <$> pInteger) <* pKey ";"
	<|> ConfAttrReal <$> pString <* pKey "=" <*> (readRealToken <$> pInteger10 <* pKey "." <*> pInteger10) <* pKey ";"
	<|>	ConfAttrString <$> pString <* pKey "=" <*> pString <* pKey ";"
	<|> ConfAttrBoolean <$> pString <* pKey "=" <*> pBool <* pKey ";"

pConfigurationSelected :: Parser Token ConfSelected
pConfigurationSelected
	= 	CT <$ (pParens (pKey "true"))
	<|> CF <$ (pParens (pKey "false"))
	<|> CU <$ (pParens (pKey "undefined"))

----------------------------------------------------------------
-- UTFM Parse Product
----------------------------------------------------------------

pProducts :: Parser Token Products
pProducts 
	= 	pList pProduct
	<|> pKey "{" *> (pList pProduct) <* pKey "}"

pProduct :: Parser Token Product
pProduct
	=	Product <$>  pString <* pKey ":" <*> pString  
				<*> ((pKey "attributes" *> pKey ":" *> pProdAttributes) <|> (pSucceed []))
				<*> ((pKey "group" *> pKey ":" *> pProducts) <|> (pSucceed []))

pProdAttributes:: Parser Token ProdAttributes
pProdAttributes 
	= 	pList pProdAttribute
	<|> pKey "{" *> (pList pProdAttribute) <* pKey "}"

pProdAttribute :: Parser Token ProdAttribute
pProdAttribute 	
	=	ProdAttrInt	<$> pString <* pKey "=" <*> (readIntToken <$> pInteger)
	<|> ProdAttrReal <$> pString <* pKey "=" <*> (readRealToken <$> pInteger10 <* pKey "." <*> pInteger10)
	<|>	ProdAttrString <$> pString <* pKey "=" <*> pString
	<|> ProdAttrBoolean <$> pString <* pKey "=" <*> pBool