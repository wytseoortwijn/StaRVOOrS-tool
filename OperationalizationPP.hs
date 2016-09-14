module OperationalizationPP (operationalizeOldResultBind, operationalizeForall, operationalizeExists) where

import Types
import CommonFunctions
import DL2JML
import RefinementPPDATE
import Data.Char
import UpgradePPDATE
import ErrM
import Data.List
import qualified Data.Map as Map
import Data.Maybe


--TODO: Possible problem when operationalising nested properties
operationalizeOldResultBind :: UpgradePPD PPDATE -> Map.Map ContractName [(String,Type)] -> (UpgradePPD PPDATE, [(Contract, Variables)])
operationalizeOldResultBind ppd oldExprTypesM =
 let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
     global   = globalGet ppdate
     consts   = contractsGet ppdate
     mfiles   = methodsInFiles env
     methods  = map (\(x,y,z) -> (x,y,map (\(x,y,z) -> y) z)) mfiles
     es       = getAllEvents global
     xs       = map (\ c -> operationalizePrePostORB c (varsInFiles env) es methods oldExprTypesM) consts
     oldExpT  = Map.unions $ map (\(x,y,z) -> z) xs
     consts'  = map (\(x,y,z) -> x) xs
     newvars  = genNewVarsOld global (concat (map (\(x,y,z) -> y) xs))
     global'  = updateGlobal global (Ctxt newvars (events $ ctxtGet global) (property $ ctxtGet global) (foreaches $ ctxtGet global))
     ppdate'  = ppd >>= (\x -> do put env { oldExpTypes = oldExpT } ; return $ PPDATE (importsGet ppdate) global' (cinvariantsGet ppdate) consts' (methodsGet ppdate))
 in (ppdate', map (\(x,y,z) -> (x,y)) xs)


operationalizePrePostORB :: Contract -> [(String, ClassInfo, [(Type, Id)])] -> Events -> [(String, ClassInfo, [String])] -> Map.Map ContractName [(String,Type)] -> (Contract, Variables,OldExprM)
operationalizePrePostORB c vars events methods oldExprTypesM = 
 let cn                  = contractName c
     p                   = pre c
     p'                  = post c
     (xsPost, oldExprl)  = operationalizeOld p' cn
     ysPost              = operationalizeResult xsPost
     (oldExprl', tvars)  = addType2NewVars cn oldExprTypesM oldExprl
     const'              = updatePost c ysPost
     oldExprl''          = bindOldExp c vars events methods oldExprl'
 in (bindCV const' vars events methods, tvars,Map.singleton cn oldExprl'')


--------------------------
-- bind class variables --
--------------------------

-- TODO: if classes with the same name in different folders, then fix this method
bindCV :: Contract -> [(String, ClassInfo, [(String, String)])] -> Events -> [(String, ClassInfo, [String])] -> Contract
bindCV c vars es methods =
 let bindEntry = getClassVar c es EVEntry
     bindExit  = getClassVar c es (EVExit [])
     varsc  = getVarsToControl (fst $ methodCN c) vars
     mnames = getMethodsToControl (fst $ methodCN c) methods
     pre'   = concat $ bindVars bindEntry varsc (pre c)
     post'  = concat $ bindVars bindExit varsc (post c)
     pre''  = concat $ bindMethods bindEntry mnames pre'
     post'' = concat $ bindMethods bindExit mnames post'
 in updatePre (updatePost c post'') pre''

bindOldExp :: Contract -> [(String, ClassInfo, [(String, String)])] -> Events -> [(String, ClassInfo, [String])] -> OldExprL -> OldExprL
bindOldExp c vars es _ []             = []
bindOldExp c vars es ms ((x,y,z):xss) = 
 let bindExit  = getClassVar c es (EVExit [])
     varsc     = getVarsToControl (fst $ methodCN c) vars
     x'        = concat $ bindVars bindExit varsc x
     ms'       = getMethodsToControl (fst $ methodCN c) ms
     x''       = concat $ bindMethods bindExit ms' x'
 in (x'',y,z):bindOldExp c vars es ms xss


bindMethods :: String -> [String] -> String -> [String]
bindMethods bindn mnames s = 
 let exps = getExpressions s
 in bindExpsM bindn mnames exps

getMethodsToControl :: ClassInfo -> [(String, ClassInfo, [String])] -> [String]
getMethodsToControl cl []                       = []
getMethodsToControl cl ((main, cl', mnames):xs) = if (cl == cl')
                                                  then mnames
                                                  else getMethodsToControl cl xs

bindExpsM :: String -> [String] -> [String] -> [String]
bindExpsM bindn _ []           = []
bindExpsM bindn nms (exp:exps) = if (elem exp nms)
                                 then (bindn ++ "." ++ exp):bindExpsM bindn nms exps
                                 else exp : bindExpsM bindn nms exps


bindVars :: String -> [String] -> String -> [String]
bindVars bindn varsc s = 
 let exps = getExpressions s
 in bindExps bindn varsc exps

bindExps :: String -> [String] -> [String] -> [String]
bindExps bindn _ []          = []
bindExps bindn vars (exp:exps) = (bindExp bindn exp vars) : bindExps bindn vars exps

bindExp :: String -> String -> [String] -> String
bindExp bindn exp []     = exp
bindExp bindn exp (v:vs) = if ((exp == v) || (v == fst (splitAtIdentifier '.' exp)))
                           then bindn ++ "." ++ exp
                           else bindExp bindn exp vs

getExpressions :: String -> [String]
getExpressions s = let (exp, rest) = getExpression s
                   in if (exp == "" && rest == "")
                      then []
                      else exp : getExpressions' rest

getExpressions' :: String -> [String]
getExpressions' s = let (exp, rest) = getExpression' s
                     in if (exp == "" && rest == "")
                        then []
                        else exp : getExpressions rest

getExpression' :: String -> (String, String)
getExpression' s = (takeWhile (not.isCharExpression) s, dropWhile (not.isCharExpression) s)

getExpression :: String -> (String, String)
getExpression s = (takeWhile isCharExpression s, dropWhile isCharExpression s)

isCharExpression :: Char -> Bool
isCharExpression = (\c -> isIdentifierSymbol c || c == '.')

getVarsToControl :: ClassInfo -> [(String, ClassInfo, [(String, String)])] -> [String]
getVarsToControl cl []                     = []
getVarsToControl cl ((main, cl', vars):xs) = if (cl == cl')
                                             then map snd vars
                                             else getVarsToControl cl xs
----------
-- \old --
----------

-- returns the operationalized post and a map storing the expressions in old operators
operationalizeOld :: String -> ContractName -> (String, OldExprL)
operationalizeOld post cn = 
 let xs = splitOnIdentifier "\\old(" post
 in if (length xs == 1)
    then (post, [])
    else let begin = head xs
             ys    = tail xs
             zs    = map ((\(x,y) -> (trim (tail x), clean $ tail y)) . (splitAtClosingParen 0)) ys
             fszs  = foldr (\(x,y) xs -> (x,"","e" ++ show y):xs) [] $ zip (removeDuplicates (map fst zs)) [1..length zs]
             s'    = begin ++ flattenOld zs cn fszs
         in (s',fszs)

flattenOld :: [(String, String)] -> ContractName -> OldExprL -> String
flattenOld [] cn _             = ""
flattenOld ((xs,ys):xss) cn zs = 
 let xs' = getExpName zs xs cn
 in xs' ++ " " ++ ys ++ flattenOld xss cn zs

getExpName :: OldExprL -> String -> ContractName -> String
getExpName [] exp _             = error "Error: Cannot get type to operationalise \\old expresion"
getExpName ((a,_,c):xss) exp cn = if exp == a
                                  then cn ++ "_"  ++ c ++ "_nyckelord"
                                  else getExpName xss exp cn

addType2NewVars :: ContractName -> Map.Map ContractName [(String,Type)] -> OldExprL -> (OldExprL, Variables)
addType2NewVars cn _ []                      = ([],[])
addType2NewVars cn mtypes oexpr@((v,t,e):vs) = 
 let vdec  = VarDecl (cn ++ "_" ++ e ++ "_nyckelord") VarInitNil
     typE  = getType v cn mtypes
     tvar  = Var VarModifierNil typE [vdec]                                          
     (a,b) = addType2NewVars cn mtypes vs
 in ((v,typE,e):a, tvar:b)

genNewVarsOld :: Global -> Variables -> Variables
genNewVarsOld global ss = 
  let vars = (variables.ctxtGet) global
  in if (null vars)
     then ss
     else ss ++ vars

getType :: Id -> ContractName -> Map.Map ContractName [(String,Type)] -> Type
getType var cn oldExprTypesM = 
 let xs = fromJust $ Map.lookup cn oldExprTypesM 
     ys = [t | (y,t) <- xs, y==var]
 in if (null ys)
    then ""
    else head ys


-------------
-- \result --
-------------

operationalizeResult :: String -> String
operationalizeResult s =
 let xs = splitOnIdentifier "\\result" s
 in if (length xs == 1)
    then s
    else let begin = head xs
             ys = map ("ret"++) $ tail xs
         in begin ++ (concat ys)

-------------
-- \forall --
-------------

operationalizeForall :: Contract -> Env -> [(Contract, Variables)] -> ([Either (String, String) String], [Either (String, String) String])
operationalizeForall c env ctnv = 
 let mn                 = snd $ methodCN c
     cinfo              = fst $ methodCN c
     p                  = pre c
     (enargs, enargswt) = lookForAllEntryEventArgs env cinfo mn
     p'                 = post c 
     (exargs, exargswt) = lookForAllExitEventArgs env cinfo mn
     xs                 = splitInQuantifiedExpression p "\\forall"
     ys                 = splitInQuantifiedExpression p' "\\forall"
     tnewvars           = lookForNewVarsConst c ctnv 
     xs_pre             = applyGenMethodForall xs (contractName c) 1 enargs enargswt "_pre" []
     ys_post            = applyGenMethodForall ys (contractName c) 1 exargs exargswt "_post" tnewvars
 in (xs_pre, ys_post)

lookForNewVarsConst :: Contract -> [(Contract, Variables)] -> Variables
lookForNewVarsConst c' []            = []
lookForNewVarsConst c' ((c, tnv):cs) = if (c == c') 
                                       then tnv
                                       else lookForNewVarsConst c' cs

applyGenMethodForall :: [Either String String] -> ContractName -> Int -> String -> String -> String -> Variables -> [Either (String, String) String]
applyGenMethodForall [] _ _ _ _ _ _                        = []
applyGenMethodForall ((Right x):xs) cn n args argswt s tnv = Right x:applyGenMethodForall xs cn n args argswt s tnv
applyGenMethodForall ((Left x):xs) cn n args argswt s  tnv = 
 let y  = splitQuantifiedExpression x
     z  = generateMethodForall cn n y args argswt s tnv
 in Left z:applyGenMethodForall xs cn (n+1) args argswt s tnv

generateMethodForall :: ContractName -> Int -> (String, String, String) -> String -> String -> String -> Variables -> (String, String)
generateMethodForall cn n (var, range, body) args argswt s [] = 
 let mn = cn ++ s ++ "_opF_" ++ show n
 in (mn ++ "(" ++ argswt ++ ")", methodForall mn (var, range, body) args)
generateMethodForall cn n (var, range, body) args argswt s tnv = 
 let mn   = cn ++ s ++ "_opF_" ++ show n
     tnv' = auxNewVars tnv
     tnewvars = addComma tnv'
     newargs  = addComma $ map (last.words) tnv'
     argswt'  = if null newargs then argswt else argswt ++ "," ++ newargs
     args'    = if null tnewvars then args else args ++ "," ++ tnewvars
 in (mn ++ "(" ++ argswt' ++ ")", methodForall mn (var, range, body) args')


auxNewVars :: Variables -> [String]
auxNewVars []                          = []
auxNewVars (Var _ t [VarDecl id _]:xs) = (t ++ " " ++ id):auxNewVars xs

methodForall :: String -> (String, String, String) -> String -> String
methodForall mn (tvar, range, body) args = 
 let var   = last $ words tvar 
     low   = getLowBoundary range var
     top   = getTopBoundary range var
 in
    "public static boolean " ++ mn ++ "(" ++ args ++ ") {\n" 
 ++ "      boolean r = true;\n\n"
 ++ "      for (" ++ tvar ++ " = " ++ low ++ " ; " ++ var ++ " <= " ++ top ++ " ; " ++ var ++ "++) {\n"
 ++ "          if (true) {\n"
 ++ "             if (!(" ++ body ++ ")) {\n"
 ++ "                 r = false ; break;\n"
 ++ "             }\n"
 ++ "          }\n"
 ++ "      }\n"
 ++ "      return r;\n"          
 ++ "    }\n\n"

getLowBoundary :: String -> String -> String
getLowBoundary s var = 
 let xs = splitOnIdentifier "<" s
     ys = splitOnIdentifier "<=" s
     zs = splitOnIdentifier ">" s
     ts = splitOnIdentifier ">=" s
 in if (length ys > 1 && (auxTake.trim.head.tail) ys == var)
    then reverse.auxTake $ (clean.reverse.head) ys
    else if (length xs > 1 && (auxTake.trim.head.tail) xs == var)
         then (reverse.auxTake $ (clean.reverse.head) xs) ++ " + 1"
         else if (length ts > 1 && (auxTake.reverse.trim.head) ts == var)
              then auxTake $ (clean.head.tail) ts 
              else if (length zs > 1 && (auxTake.reverse.trim.head) zs == var)
                   then (auxTake $ (clean.head.tail) zs) ++ " + 1"
                   else ""

getTopBoundary :: String -> String -> String
getTopBoundary s var = 
 let xs = splitOnIdentifier "<" s
     ys = splitOnIdentifier "<=" s
     zs = splitOnIdentifier ">" s
     ts = splitOnIdentifier ">=" s
 in if (length ys > 1 && (auxTake.reverse.trim.head) ys == var)
    then auxTake $ (clean.head.tail) ys
    else if (length xs > 1 && (auxTake.reverse.trim.head) xs == var)
         then (auxTake $ (clean.head.tail) xs) ++ " - 1"
         else if (length ts > 1 && (auxTake.trim.head.tail) ts == var)
              then reverse.auxTake $ (clean.reverse.head) ts
              else if (length zs > 1 && (auxTake.trim.head.tail) zs == var)
                   then (reverse.auxTake $ (clean.reverse.head) zs) ++ " - 1"
                   else ""

auxTake :: String -> String
auxTake = takeWhile (not.isSpace)


splitQuantifiedExpression :: String -> (String, String, String)
splitQuantifiedExpression s = 
 let xs = splitOnIdentifier ";" s
 in (trim $ head xs, (trim.head.tail) xs, (trim.last) xs)

splitInQuantifiedExpression :: String -> String -> [Either String String]
splitInQuantifiedExpression s qexpident = 
 let xs = splitOnIdentifier qexpident s
 in if (length xs == 1)
    then [Right $ head xs]
    else let begin = Right $ head xs
             ys    = map (lookforEnd 0 []) $ tail xs
             zs    = concat $ map foo ys
          in zs
                  where foo (a,b) = [Left a, Right b]

lookforEnd :: Int -> String -> String -> (String ,String)
lookforEnd _ acum []     = (acum, [])
lookforEnd n acum (x:xs) = if (x == ')')
                           then case n of 
                                     0 -> (acum, xs)
                                     n -> lookforEnd (n-1) (acum ++ [x]) xs
                           else if (x == '(')
                                then lookforEnd (n+1) (acum ++ [x]) xs
                                else lookforEnd n (acum ++ [x]) xs

------------
-- \exist --
------------

operationalizeExists :: Contract -> Env -> [(Contract, Variables)] -> ([Either (String, String) String], [Either (String, String) String])
operationalizeExists c es ctnv = 
 let mn                 = snd $ methodCN c
     cinfo              = fst $ methodCN c
     p                  = pre c
     (enargs, enargswt) = lookForAllEntryEventArgs es cinfo mn
     p'                 = post c 
     (exargs, exargswt) = lookForAllExitEventArgs es cinfo mn
     xs                 = splitInQuantifiedExpression p "\\exists"
     ys                 = splitInQuantifiedExpression p' "\\exists"
     tnewvars           = lookForNewVarsConst c ctnv
     xs_pre             = applyGenMethodExists xs (contractName c) 1 enargs enargswt "_pre" []
     ys_post            = applyGenMethodExists ys (contractName c) 1 exargs exargswt "_post" tnewvars
 in (xs_pre, ys_post)


applyGenMethodExists :: [Either String String] -> ContractName -> Int -> String -> String -> String -> Variables -> [Either (String, String) String]
applyGenMethodExists [] _ _ _ _ _ _                        = []
applyGenMethodExists ((Right x):xs) cn n args argswt s tnv = Right x:applyGenMethodExists xs cn n args argswt s tnv
applyGenMethodExists ((Left x):xs) cn n args argswt s tnv  = 
 let y  = splitQuantifiedExpression x
     z  = generateMethodExists cn n y args argswt s tnv
 in Left z:applyGenMethodExists xs cn (n+1) args argswt s tnv

generateMethodExists :: ContractName -> Int -> (String, String, String) -> String -> String -> String -> Variables -> (String, String)
generateMethodExists cn n (var, range, body) args argswt s []  = 
 let mn = cn ++ s ++ "_opE_" ++ show n
 in (mn ++ "(" ++ argswt ++ ")", methodExists mn (var, range, body) args "")
generateMethodExists cn n (var, range, body) args argswt s tnv = 
 let mn = cn ++ s ++ "_opE_" ++ show n
     tnv' = auxNewVars tnv
     tnewvars = addComma tnv'
     newargs  = addComma $ map (last.words) tnv'
     args'    = if null newargs then argswt else argswt ++ "," ++ newargs
 in (mn ++ "(" ++ args' ++ ")", methodExists mn (var, range, body) args tnewvars)


methodExists :: String -> (String, String, String) -> String -> String -> String
methodExists mn (tvar, range, body) args tnv = 
 let var = last $ words tvar 
     low = getLowBoundary range var
     top = getTopBoundary range var
     args' = if null tnv then args else args ++ "," ++ tnv
 in
    "public static boolean " ++ mn ++ "(" ++ args' ++ ") {\n" 
 ++ "      boolean r = false;\n\n"
 ++ "      for (" ++ tvar ++ " = " ++ low ++ " ; " ++ var ++ " <= " ++ top ++ " ; " ++ var ++ "++) {\n"
 ++ "          if (true) {\n"
 ++ "             if (" ++ body ++ ") {\n"
 ++ "                 r = true ; break;\n"
 ++ "             }\n"
 ++ "          }\n"
 ++ "      }\n"
 ++ "      return r;\n"
 ++ "    }\n\n"

