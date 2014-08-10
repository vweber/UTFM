module Main (main) where

import UTFM.Boilerplate
import UTFM.Interface.CommandLine
import UTFM.Parser.Parser
import UTFM.Interpreter.Interpreter
import UTFM.Interpreter.Printer

main :: IO ()
main = do
  options <- parseArgs

  let Options {   optType = typedecl
                , optConf = conf
                , optProd = prod
                , optUnfl = unfold  
                , optProp = propagate
                , optFull = full } = options

  --toks <- parseTypeDecl typedecl
  (tree,  errs)     <- parseFiles typedecl conf prod
  (tree', errs')    <- interpretTree 
                          (not $ conf == "")  -- Validate configuration
                          (not $ prod == "")  -- Validate product
                          propagate           -- Propagate configuration
                          unfold              -- Unfold configuration
                          full                -- From configuration to product
                          (tree, errs)        -- type declaration and errors
  printTree (whichPrinter options) (tree', errs')
  --printOptions options
  --putStrLn $ show (noConfig conf prod)

whichPrinter :: Options -> (Maybe Printer)
whichPrinter options = res
  where
    Options {   optType = typedecl
                , optConf = conf
                , optProd = prod
                , optUnfl = unfold  
                , optProp = propagate
                , optFull = full } = options
    res = if(conf == "" && prod == "") then Nothing
            else(if(full) then (Just PPrinter) else (Just CPrinter))