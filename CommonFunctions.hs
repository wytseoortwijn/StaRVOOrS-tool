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


lookForExitEvent :: Events -> MethodName -> Event
lookForExitEvent [] mn     = error $ "Missing exit event for method " ++ mn ++ ".\n"
lookForExitEvent (e:es) mn = 
 case (compEvent e) of 
      NormalEvent _ id _ evar -> 
              case evar of
                   EVExit _ -> if (mn == id)
                               then eName e
                               else lookForExitEvent es mn
                   _        -> lookForExitEvent es mn
      otherwise -> lookForExitEvent es mn

lookForEntryEvent :: Events -> MethodName -> Event
lookForEntryEvent [] mn     = error $ "Missing entry event for method " ++ mn ++ ".\n"
lookForEntryEvent (e:es) mn = 
 case (compEvent e) of 
      NormalEvent _ id _ evar -> 
              case evar of
                   EVEntry -> if (mn == id)
                               then eName e
                               else lookForEntryEvent es mn
                   _       -> lookForEntryEvent es mn
      otherwise -> lookForEntryEvent es mn

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


getConstTnv :: Contract -> OldExprM -> Variables
getConstTnv c oldExpM = 
  if Map.null oldExpM 
  then []
  else case Map.lookup (contractName c) oldExpM of
            Nothing -> []
            Just xs -> let cn    = contractName c
                           vdec  = VarDecl cn VarInitNil
                           typE  = "Old_" ++ cn
                       in if null xs
                          then []
                          else [Var VarModifierNil typE [vdec]]

getOldExpr :: OldExprM -> ContractName -> String
getOldExpr oldExpM cn = 
 case Map.lookup cn oldExpM of
      Nothing -> ""
      Just xs -> if null xs 
                 then ""
                 else "," ++ cn


---------------------------------------
-- Manipulating the parsed .xml file --
---------------------------------------

removeNoneContracts :: [Proof] -> [ContractName] -> [Proof]
removeNoneContracts [] _       = []
removeNoneContracts (p:ps) cns = let cn = getContractNameErrConst (contractText p) (typee p) in
                                 if ((cn /= "") && elem cn cns)
                                 then p:removeNoneContracts ps cns
                                 else removeNoneContracts ps cns


getInfoFromProof :: Proof -> (MethodName, ContractName, [String],String)
getInfoFromProof proof = let mn    = getMethodName' (target proof)
                             path  = typee proof
                             cn    = getContractNameErrConst (contractText proof) (typee proof)
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

getContractNameErrConst :: ContractText -> Type -> ContractName
getContractNameErrConst ctext t = let (_, xs) = splitAtIdentifier ':' ctext
                                      ys      = splitOnIdentifier t (tail xs)
                                  in if (length ys == 1)
                                     then ""
                                     else trim $ fst $ splitAtIdentifier '=' $ (tail.head.tail) ys

getContractNameErrVar :: ContractText -> ContractName
getContractNameErrVar ctext = let (_, xs) = splitAtIdentifier ':' ctext
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


getAllEvents :: Global -> Events
getAllEvents (Global (Ctxt vars es prop [])) = es 
getAllEvents (Global (Ctxt vars es prop [Foreach args ctxt])) = es ++ getEventsCtxt ctxt

getEventsCtxt :: Context -> Events
getEventsCtxt (Ctxt vars es prop [])    = es
getEventsCtxt (Ctxt vars es prop [Foreach args ctxt]) = es ++ getEventsCtxt ctxt


makeAddFile :: Import -> IO (String, ClassInfo)
makeAddFile (Import s) = let xs = splitOnIdentifier "." s
                         in if (length xs == 1)
                            then return ("", head xs)
                            else let val = last xs
                                     ys = (init $ foldr (\ xs xss -> xs ++ "/" ++ xss) "" (init xs))
                                 in return (ys, val)

