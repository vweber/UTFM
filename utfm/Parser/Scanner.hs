module UTFM.Parser.Scanner (runScanner) where

----------------------------------------------------------------
-- UTFM Scanner
----------------------------------------------------------------

import Data.Char

import UU.Scanner.Token
import UU.Scanner.Position

runScanner :: String -> String -> [Token]
runScanner filename
	= scanBlock (initPos filename)

scanBlock :: Pos -> String -> [Token]
scanBlock _ "" = [] 

-- Scanner for Type Declarations, Configurations and Products
scanBlock p ('a' : 't' : 't' : 'r' : 'i' : 'b' : 'u' : 't' : 'e' : 's' : r)
	= reserved "attributes" p : scanBlock (advc 10 p) r
scanBlock p ('c' : 'o' : 'n' : 's' : 't' : 'r' : 'a' : 'i' : 'n' : 't' : 's' : r)
	= reserved "constraints" p : scanBlock (advc 11 p) r
scanBlock p ('g' : 'r' : 'o' : 'u' : 'p' : r)
	= reserved "group" p : scanBlock (advc 5 p) r
scanBlock p ('I' : 'n' : 't' : ' ' : r)
	= reserved "Int" p : scanBlock (advc 4 p) r
scanBlock p ('S' : 't' : 'r' : 'i' : 'n' : 'g' : ' ' : r)
	= reserved "String" p : scanBlock (advc 7 p) r
scanBlock p ('B' : 'o' : 'o' : 'l' : ' ' : r)
	= reserved "Bool" p : scanBlock (advc 5 p) r
scanBlock p ('R' : 'e' : 'a' : 'l' : ' ' : r)
	= reserved "Real" p : scanBlock (advc 5 p) r
scanBlock p ('t' : 'r' : 'u' : 'e' : r)
	= reserved "true" p : scanBlock (advc 4 p) r
scanBlock p ('f' : 'a' : 'l' : 's' : 'e' : r)
	= reserved "false" p : scanBlock (advc 5 p) r
scanBlock p ('u' : 'n' : 'd' : 'e' : 'f' : 'i' : 'n' : 'e' : 'd' : r)
 	= reserved "undefined" p : scanBlock (advc 9 p) r
scanBlock p ('.' : '.' : r)
	= reserved ".." p : scanBlock (advc 2 p) r

-- Scanner for Constraint Language
scanBlock p ('a' : 'n' : 'd' : r)
	= reserved "and" p : scanBlock (advc 3 p) r
scanBlock p ('o' : 'r' : r)
	= reserved "or" p : scanBlock (advc 2 p) r
scanBlock p ('n' : 'o' : 't' : r)
	= reserved "not" p : scanBlock (advc 3 p) r
scanBlock p ('e' : 'x' : 'i' : 's' : 't' : 's' : r)
	= reserved "exists" p : scanBlock (advc 6 p) r
scanBlock p ('f' : 'o' : 'r' : 'a' : 'l' : 'l' : r)
	= reserved "forall" p : scanBlock (advc 6 p) r
scanBlock p ('i' : 'm' : 'p' : 'l' : 'i' : 'e' : 's' : r)
	= reserved "implies" p : scanBlock (advc 7 p) r
scanBlock p ('>' : '=' : r)
	= reserved ">=" p : scanBlock (advc 2 p) r
scanBlock p ('<' : '=' : r)
	= reserved "<=" p : scanBlock (advc 2 p) r
scanBlock p ('>' : r)
	= reserved ">" p : scanBlock (advc 1 p) r
scanBlock p ('<' : r)
	= reserved "<" p : scanBlock (advc 1 p) r
scanBlock p ('s' : 'u' : 'm' : r)
	= reserved "sum" p : scanBlock (advc 3 p) r
scanBlock p ('m' : 'a' : 'x' : r)
	= reserved "max" p : scanBlock (advc 3 p) r
scanBlock p ('m' : 'i' : 'n' : r)
	= reserved "min" p : scanBlock (advc 3 p) r
scanBlock p ('a' : 'v' : 'e' : 'r' : 'a' : 'g' : 'e' : r)
	= reserved "average" p : scanBlock (advc 7 p) r
scanBlock p ('p' : 'a' : 'r' : 'e' : 'n' : 't' : r)
	= reserved "parent" p : scanBlock (advc 6 p) r

scanBlock p ('\"' : r)
	= valueToken TkString (trim s) p' : scanBlock (advc 1 p') (tail r')
		where
			(p',r',s) = untilChar False '\"' (advc 1 p) r
scanBlock p (c : r)
  | isLetter c = valueToken TkString s' p' : scanBlock (advc (length s') p') r'
  | isSpace c = scanBlock (adv p c) r
  | isNumber c = valueToken TkInteger10 d' p'' : scanBlock (advc 1 p) r''
  | c `elem` "[](){}.,:;=+-*/" = reserved [c] p : scanBlock (advc 1 p) r
  | otherwise = errToken ("Unexpected character " ++ show c) p : scanBlock (adv p c) r
  		where
  			(p',r',s) = untilEndString True (advc 1 p) r
  			s' = (c:s)
  			(p'',r'',d) = untilEndInteger True (advc 1 p) r
  			d' = (c:d)

----------------------------------------------------------------
-- UTFM Scanner additional functions
----------------------------------------------------------------

untilChar :: Bool -> Char -> Pos -> String -> (Pos, String, String)
untilChar eofAllowed c p s
	= 	let (l,r) = span (/= c) s
		in if null r && not eofAllowed
			then error "Unexpected EOF"
			else (foldl adv p l, r, l)

untilEndString :: Bool -> Pos -> String -> (Pos, String, String)
untilEndString eofAllowed p s
	= 	let (l,r) = span (isAlphaNum) s
		in if null r && not eofAllowed
			then error "Unexpected EOF"
			else (foldl adv p l, r, l)

untilEndInteger :: Bool -> Pos -> String -> (Pos, String, String)
untilEndInteger eofAllowed p s
	=	let (l,r) = span (isNumber) s
		in if null r && not eofAllowed
			then error "Unexpected EOF"
			else (foldl adv p l, r, l)

trim :: String -> String
trim = (reverse . trimAtFront . reverse) . trimAtFront
	where trimAtFront = dropWhile isSpace