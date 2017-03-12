module CommonFunctions where

import Data.Char
import Data.List
import Data.List.Split
import Data.Either
import Types
import ErrM
import qualified Data.Map as Map
import Data.Maybe (fromJust)

readIdentifier :: String -> Err (String, String)
readIdentifier text 
  | identifier == "" = fail "Error Parsing: Expecting identifier, but none was found.\n"
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
splitAtClosingParen _ ""       = ("","")
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


lookForExitTrigger :: [(Id,MethodName,ClassInfo,TriggerVariation,[Bind],Maybe TriggerDef)] -> MethodName -> ClassInfo -> [Trigger]
lookForExitTrigger [] _ _                        = []
lookForExitTrigger ((tr,mn',ci',e,_,_):es) mn ci = 
    case e of
        EVExit _ -> if (mn == mn' && ci == ci')
                    then tr:lookForExitTrigger es mn ci
                    else lookForExitTrigger es mn ci
        _        -> lookForExitTrigger es mn ci

lookForEntryTrigger :: [(Id,MethodName,ClassInfo,TriggerVariation,[Bind],Maybe TriggerDef)] -> MethodName -> ClassInfo -> [Trigger]
lookForEntryTrigger [] _ _                         = []
lookForEntryTrigger ((tr,mn',ci',e,_,_):es) mn ci  = 
    case e of
        EVEntry -> if (mn == mn' && ci == ci')
                   then tr:lookForEntryTrigger es mn ci
                   else lookForEntryTrigger es mn ci
        _       -> lookForEntryTrigger es mn ci

openingBracket :: String -> Bool
openingBracket "" = False
openingBracket s  = null (clean s) && elem '{' s

args2Str :: [Bind] -> String
args2Str []       = ""
args2Str (bn:bns) = case bn of
                         BindType t id -> t ++ " " ++ id ++ "," ++ args2Str bns
                         _ -> error "argst2Str: A trigger has a wrongly defined argument.\n"

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

getListOfTypesAndMethods :: ClassInfo -> [(String, ClassInfo, [(Type, Id,[String],MethodInvocations)])] -> [(Type, Id)]
getListOfTypesAndMethods cl []                  = []
getListOfTypesAndMethods cl ((main, cl',ts):xs) = if (cl == cl') 
                                                  then [(x,y) | (x,y,_,_) <- ts]
                                                  else getListOfTypesAndMethods cl xs

getMethodInvocations :: MethodCN -> [(String, ClassInfo, [(Type, Id,[String],MethodInvocations)])] -> MethodInvocations
getMethodInvocations _ []                    = []
getMethodInvocations mcn ((main, cl',ts):xs) = 
 let mn  = mname mcn
     cl  = clinf mcn
     ov  = overl mcn
 in if (cl == cl') 
    then let ys = [ (t,id,args,minvs) | (t,id,args,minvs) <- ts, id==mn]
         in if (not.null) ys
            then case ov of 
                 OverNil  -> (\(_,_,_,x) -> x) $ head ys 
                 Over ovs -> let zs = [ minvs | (_,_,args,minvs) <- ys, map (head.words) args == ovs]
                             in if null zs
                                then getMethodInvocations mcn xs
                                else head zs
            else getMethodInvocations mcn xs
    else getMethodInvocations mcn xs

getListOfArgs :: MethodName -> [(Type, Id,[String],MethodInvocations)] -> [String]
getListOfArgs mn []                = []
getListOfArgs mn ((t,mn',ts,_):xs) = if (mn == mn') 
                                     then ts
                                     else getListOfArgs mn xs

addComma :: [String] -> String
addComma = addComma'


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

getBindArgs' :: [Bind] -> String
getBindArgs' []                     = ""
getBindArgs' [BindType t id]        = t ++ " " ++ id
getBindArgs' ((BindType t id):y:ys) = t ++ " " ++ id ++ "," ++ getBindArgs' (y:ys)
getBindArgs' _                      = ""

getInfoTrigger :: (Trigger,MethodName,ClassInfo,TriggerVariation,[Bind],Maybe TriggerDef) -> Maybe (Trigger, [String])
getInfoTrigger (tr,mn',ci,e,bs,_) = 
 case e of
     EVExit _ -> Just (tr,splitOnIdentifier "," $ getBindArgs' bs)
     EVEntry  -> Just (tr,splitOnIdentifier "," $ getBindArgs' bs)
     _        -> Nothing

getTriggerDef :: Overloading -> HT -> [(Trigger,MethodName,ClassInfo,TriggerVariation,[Bind],Maybe TriggerDef)] -> TriggerDef 
getTriggerDef OverNil c xs = 
 let mnc = methodCN c
     cl  = clinf mnc
     tr  = mn ++ "_ppdex"
     mn  = mname mnc
     xs'  = [ tdef | (tr',_,cl',_,_,tdef) <- xs, isInfixOf tr tr',cl == cl']
     xs'' = filter (\ c -> c /= Nothing) xs'
 in case xs'' of
         []     -> error $ "Error: Problem when generating the exit trigger for the Hoare triple " ++ htName c ++ ".\n"
         tdef:_ -> fromJust tdef
getTriggerDef (Over ts) c xs = 
 let mnc  = methodCN c
     cl   = clinf mnc
     tr   = mn ++ "_ppdex"
     mn   = mname mnc
     xs'  = [ tdef | (tr',_,cl',_,_,tdef) <- xs, isInfixOf tr tr',cl == cl']
     xs'' = filter (\ c -> c /= Nothing) xs'
 in if length xs'' == 1
    then case head xs' of
         Nothing   -> error $ "Error: Problem when generating the exit trigger for the Hoare triple " ++ htName c ++ ".\n"
         Just tdef -> tdef
    else let xs''' = map fromJust xs''
         in case [ x | (ts',x) <- zip (map (map getBindTypeType.getCTArgs.compTrigger) xs''') xs''', ts == ts'] of
                 []  -> error $ "Error: Problem when generating the exit trigger for the Hoare triple " ++ htName c ++ ".\n"
                 [x] -> x

lookfor :: [(Trigger, [String])] -> Trigger -> [String]
lookfor [] _     = []
lookfor (x:xs) e = if (fst x==e)
                   then snd x
                   else lookfor xs e

lookforClVar :: PropertyName -> [(PropertyName, ClassInfo, String)] -> String
lookforClVar pn []              = ""
lookforClVar pn ((pn',_,cl):xs) = if pn == pn'
                                  then cl
                                  else lookforClVar pn xs

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

introduceAnd :: [String] -> String
introduceAnd [x]    = x
introduceAnd (x:xs) = x ++ " && " ++ introduceAnd xs

getAllTriggers :: Global -> Triggers
getAllTriggers (Global (Ctxt vars ies trigs prop fors)) = trigs ++ getTriggersFors fors

getTriggersFors :: Foreaches -> Triggers
getTriggersFors []     = []
getTriggersFors (f:fs) = getTriggersCtxt (getCtxtForeach f) ++ getTriggersFors fs

getTriggersCtxt :: Context -> Triggers
getTriggersCtxt (Ctxt vars ies trigs prop fors) = trigs ++ getTriggersFors fors


makeAddFile :: Import -> IO (String, ClassInfo)
makeAddFile (Import s) = let xs = splitOnIdentifier "." s
                         in if (length xs == 1)
                            then return ("", head xs)
                            else let val = last xs
                                     ys = (init $ foldr (\ xs xss -> xs ++ "/" ++ xss) "" (init xs))
                                 in return (ys, val)

