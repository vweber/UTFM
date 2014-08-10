module UTFM.Interpreter.Printer (printTree) where

import System.IO

import UU.Pretty

import UTFM.Boilerplate
import UTFM.AG.AG
import UTFM.Walker.Unfold (deAttribute)

printTree :: (Maybe Printer) -> (Maybe Type, Errs) -> IO ()
printTree Nothing (_, errs) = do putStrLn $ show (null errs)
printTree (Just CPrinter) (Just tree, []) = hPutStrLn stderr $ disp (conf_Syn_Type $ transformType (deAttribute tree)) 80 ""
printTree (Just PPrinter) (Just tree, []) = hPutStrLn stderr $ disp (conf_Syn_Type $ transformType (deAttribute tree)) 80 ""
printTree _ (_, errs) = do putStrLn "errors"