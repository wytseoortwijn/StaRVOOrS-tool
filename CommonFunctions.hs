module CommonFunctions where

import Data.Char
import Data.List
import Data.List.Split
import Data.Either
import Types
import ErrM
import qualified Data.Map as Map

readIdentifier :: String -> Err (String, String)
readIdentifier text 
  | identifier == "" = fail "Error Parsing: Expecting identifier but not found.\n"
  | otherwise        = return (identifier, text')
  where
    (identifier, text') = break (\c -> c == '}' || c == '{') text

isIdentifierSymbol :: Char -> Bool
isIdentifierSymbol c = isAlphaNum c || c == '_'

clean :: String -> String
clean = dropWhile (\ c -> isSpace c || c == '{' || c == '}')

trim :: String -> String
trim = reverse . clean . reverse . clean

splitAtIdentifier :: Char -> String -> (String, String)
splitAtIdentifier iden s = (takeWhile (\c -> not (c == iden)) s, dropWhile (\c -> not (c == iden)) s)

splitAtClosingParen :: Int -> String -> (String, String)
splitAtClosingParen n ""       = ("","")
splitAtClosingParen n ('(':xs) = 
 let (a,b) = splitAtClosingParen (n+1) xs
 in ('(':a,b)
splitAtClosingParen n (')':xs) = 
 if n > 0
 then let (a,b) = splitAtClosingParen (n-1) xs
      in (')':a,b)
 else ("",xs)
splitAtClosingParen n (x:xs) = 
 let (a,b) = splitAtClosingParen n xs
 in (x:a,b)

splitOnIdentifier :: String -> String -> [String]
splitOnIdentifier = splitOn

checkIfParseErrors :: [Either a b] -> Either [a] [b]
checkIfParseErrors es = let (ls, rs) = partitionEithers es
                        in if ((not.null) ls)
                           then Left ls
                           else Right rs


lookForExitTrigger :: Triggers -> MethodName -> Trigger
lookForExitTrigger [] mn     = error $ "Missing exit event for method " ++ mn ++ ".\n"
lookForExitTrigger (e:es) mn = 
 case (compTrigger e) of 
      NormalEvent _ id _ evar -> 
              case evar of
                   EVExit _ -> if (mn == id)
                               then tName e
                               else lookForExitTrigger es mn
                   _        -> lookForExitTrigger es mn
      otherwise -> lookForExitTrigger es mn

lookForEntryTrigger :: Triggers -> MethodName -> Trigger
lookForEntryTrigger [] mn     = error $ "Missing entry event for method " ++ mn ++ ".\n"
lookForEntryTrigger (e:es) mn = 
 case (compTrigger e) of 
      NormalEvent _ id _ evar -> 
              case evar of
                   EVEntry -> if (mn == id)
                               then tName e
                               else lookForEntryTrigger es mn
                   _       -> lookForEntryTrigger es mn
      otherwise -> lookForEntryTrigger es mn

openingBracket :: String -> Bool
openingBracket "" = False
openingBracket s  = null (clean s) && elem '{' s

args2Str :: [Bind] -> String
args2Str []       = ""
args2Str (bn:bns) = case bn of
                         BindType t id -> t ++ " " ++ id ++ "," ++ args2Str bns
                         _ -> error "argst2Str: An event has a wrongly defined argument.\n"

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = if (elem x xs)
                          then removeDuplicates xs
                          else x:removeDuplicates xs

removeDuplicates' :: Eq a => [(a,a)] -> [(a,a)]
removeDuplicates' []            = []
removeDuplicates' (p@(xs,ys):xss) = 
 let zs = map fst xss
 in if (elem xs zs)
    then removeDuplicates' xss 
    else p:removeDuplicates' xss


getListOfTypesAndVars :: ClassInfo -> [(String, ClassInfo, [(Type, Id)])] -> [(Type, Id)]
getListOfTypesAndVars cl []                  = []
getListOfTypesAndVars cl ((main, cl',ts):xs) = if (cl == cl') 
                                               then ts
                                               else getListOfTypesAndVars cl xs

getListOfTypesAndMethods :: ClassInfo -> [(String, ClassInfo, [(Type, Id,[String])])] -> [(Type, Id)]
getListOfTypesAndMethods cl []                  = []
getListOfTypesAndMethods cl ((main, cl',ts):xs) = if (cl == cl') 
                                                  then [(x,y) | (x,y,_) <- ts]
                                                  else getListOfTypesAndMethods cl xs

getListOfArgs :: MethodName -> [(Type, Id,[String])] -> [String]
getListOfArgs mn []                  = []
getListOfArgs mn ((t,mn',ts):xs) = if (mn == mn') 
                                   then ts
                                   else getListOfArgs mn xs

addComma :: [String] -> String
addComma []       = ""
addComma [xs]     = xs
addComma (xs:xss) = xs ++ "," ++ addComma xss


getConstTnv :: HT -> OldExprM -> Variables
getConstTnv c oldExpM = 
  if Map.null oldExpM 
  then []
  else case Map.lookup (htName c) oldExpM of
            Nothing -> []
            Just xs -> let cn    = htName c
                           vdec  = VarDecl cn VarInitNil
                           typE  = "Old_" ++ cn
                       in if null xs
                          then []
                          else [Var VarModifierNil typE [vdec]]

getOldExpr :: OldExprM -> HTName -> String
getOldExpr oldExpM cn = 
 case Map.lookup cn oldExpM of
      Nothing -> ""
      Just xs -> if null xs 
                 then ""
                 else "," ++ cn

---------------------------------------
-- Manipulating the parsed .xml file --
---------------------------------------

removeNoneHTs :: [Proof] -> [HTName] -> [Proof]
removeNoneHTs [] _       = []
removeNoneHTs (p:ps) cns = let cn = getHTNameErrConst (contractText p) (typee p) in
                                 if ((cn /= "") && elem cn cns)
                                 then p:removeNoneHTs ps cns
                                 else removeNoneHTs ps cns


getInfoFromProof :: Proof -> (MethodName, HTName, [String],String)
getInfoFromProof proof = let mn    = getMethodName' (target proof)
                             path  = typee proof
                             cn    = getHTNameErrConst (contractText proof) (typee proof)
                             npres = getNewPreConds (executionPath proof)
                             aoft  = length $ filter (=="true") npres
                         in if (aoft >= 1)
                            then if (aoft == 1) 
                                 then if (length npres == 1)
                                      then (mn, cn, npres,path) -- KeY has nothing to say about the proof
                                      else (mn, cn, filter (not.(=="true")) npres,path)
                                 else if (aoft == length npres)
                                      then (mn, cn, ["true"],path) -- KeY has nothing to say about the proof
                                      else (mn, cn, filter (not.(=="true")) npres,path)
                            else (mn, cn, npres,path)

getMethodName' :: Target -> MethodName
getMethodName' t = fst $ splitAtIdentifier '(' t                   

getHTNameErrConst :: ContractText -> Type -> HTName
getHTNameErrConst ctext t = let (_, xs) = splitAtIdentifier ':' ctext
                                ys      = splitOnIdentifier t (tail xs)
                            in if (length ys == 1)
                               then ""
                               else trim $ fst $ splitAtIdentifier '=' $ (tail.head.tail) ys

getHTNameErrVar :: ContractText -> HTName
getHTNameErrVar ctext = 
 let (_, xs) = splitAtIdentifier ':' ctext
     (_, ys) = splitAtIdentifier '.' $ tail xs
 in if (null ys)
    then ""
    else let (cn, _) = splitAtIdentifier '=' $ tail ys 
         in trim cn

getNewPreConds :: [EPath] -> [String]
getNewPreConds []       = []
getNewPreConds (ep:eps) = if (verified ep == "false") 
                          then pathCondition ep:getNewPreConds eps
                          else getNewPreConds eps

introduceOr :: [String] -> String
introduceOr [x]    = x
introduceOr (x:xs) = x ++ " || " ++ introduceOr xs


getAllTriggers :: Global -> Triggers
getAllTriggers (Global (Ctxt vars ies trigs prop []))                  = trigs
getAllTriggers (Global (Ctxt vars ies trigs prop [Foreach args ctxt])) = trigs ++ getTriggersCtxt ctxt

getTriggersCtxt :: Context -> Triggers
getTriggersCtxt (Ctxt vars ies trigs prop [])                  = trigs
getTriggersCtxt (Ctxt vars ies trigs prop [Foreach args ctxt]) = trigs ++ getTriggersCtxt ctxt


makeAddFile :: Import -> IO (String, ClassInfo)
makeAddFile (Import s) = let xs = splitOnIdentifier "." s
                         in if (length xs == 1)
                            then return ("", head xs)
                            else let val = last xs
                                     ys = (init $ foldr (\ xs xss -> xs ++ "/" ++ xss) "" (init xs))
                                 in return (ys, val)

