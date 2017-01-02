module Parser(parse) where

import CommonFunctions
import Types
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import Lexppdate
import Parppdate
import Skelppdate
import Printppdate
import Absppdate
import ErrM

type ParseFun a = [Token] -> Err a

run :: (Print a, Show a) => ParseFun a -> String -> Err a
run p s = let ts = myLexer s 
          in case p ts of
                Bad s    -> fail s
                Ok  tree -> return tree

parse :: String -> Err AbsPPDATE
parse = run pAbsPPDATE
