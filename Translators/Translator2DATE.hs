module Translator2DATE(translate) where

import Types
import CommonFunctions
import ReplicatedAutomataGenerator
import RefinementPPDATE
import UpgradePPDATE
import ErrM
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Language.Java.Syntax hiding(VarDecl)


translate :: UpgradePPD PPDATE -> FilePath -> IO ()
translate ppd fpath =
 do let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
    putStrLn "Translating ppDATE to DATE."
    writeFile fpath (writeImports (importsGet ppdate) (htsGet ppdate))
    let consts = assocChannel2HTs 1 $ (htsGet ppdate)
    let ppdate' = updateHTsPP ppdate consts
    appendFile fpath (writeGlobal ppdate' env)
    appendFile fpath (writeMethods (methodsGet ppdate')) 
    putStrLn $ "Translation completed."


assocChannel2HTs :: Int -> HTriples -> HTriples
assocChannel2HTs _ []     = []
assocChannel2HTs n (c:cs) = updateCH c n:assocChannel2HTs (n+1) cs

---------------------
-- IMPORTS section --
---------------------

getImports :: Imports -> String
getImports []            = ""
getImports (Import s:xs) = "import " ++ s ++ ";\n" ++ getImports xs

writeImports :: Imports -> HTriples -> String
writeImports xss const = let newImp = "import ppArtifacts.*;\n"
                         in if null const 
                            then "IMPORTS {\n" ++ getImports xss ++ "}\n\n"
                            else "IMPORTS {\n" ++ getImports xss ++ newImp  ++ "}\n\n"

--------------------
-- GLOBAL section --
--------------------

writeGlobal :: PPDATE -> Env -> String
writeGlobal ppdate env = 
 let global = ctxtGet $ globalGet ppdate
     consts = htsGet ppdate
     vars   = variables global
     acts   = actevents global
     trs    = triggers global
     prop   = property global
     fors   = foreaches global                      
 in "GLOBAL {\n\n"
    ++ writeVariables vars consts acts
    ++ writeTriggers trs consts acts env
    ++ writeProperties prop consts env
    ++ writeForeach fors consts env
    ++ generateReplicatedAutomata consts trs env  
    ++ "}\n" 

---------------
-- Variables --
---------------

writeVariables :: Variables -> HTriples -> ActEvents -> String
writeVariables vars consts acts = 
 let actChann = if (null acts) then "" else concatMap (makeChannelsAct.show) acts in
 if (null consts)
 then if (null vars) 
      then "" 
      else "VARIABLES {\n" ++ actChann ++ writeVariables' vars ++ "}\n\n"
 else "VARIABLES {\n" ++ makeChannels (length consts) "h" ++ actChann ++ writeVariables' vars ++ "}\n\n"

writeVariables' :: Variables -> String
writeVariables' []     = ""
writeVariables' (v:vs) = writeVariable v ++ ";\n" ++ writeVariables' vs

writeVariable :: Variable -> String
writeVariable (Var vm t vdec) = writeVarModifier vm ++ " "
                                ++ t ++ " "
                                ++ flattenVariables (map writeVarDecl vdec)

writeVarModifier :: VarModifier -> String
writeVarModifier VarModifierNil   = ""
writeVarModifier VarModifierFinal = "final"

writeVarDecl :: VarDecl -> String
writeVarDecl (VarDecl id (VarInit ve)) = id ++ " = " ++ ve
writeVarDecl (VarDecl id VarInitNil)   = id

flattenVariables :: [String] -> String
flattenVariables []       = ""
flattenVariables [x]      = x
flattenVariables (x:y:xs) = x ++ "," ++ flattenVariables (y:xs)

makeChannels :: Int -> String -> String
makeChannels n s = generateChannels n s ++ "\n"

generateChannels :: Int -> String -> String
generateChannels 0 s = "\n"
generateChannels n s = generateChannels (n-1) s ++ " Channel " ++ s ++ show n ++ " = new Channel();\n"

makeChannelsAct :: String -> String
makeChannelsAct s = " Channel " ++ s ++ " = new Channel();\n"


--------------
-- Triggers --
--------------

writeTriggers :: Triggers -> HTriples -> ActEvents -> Env -> String
writeTriggers [] _ _ _           = ""
writeTriggers es consts acts env = 
 "EVENTS {\n"
 ++ writeTriggersActs acts
 ++ writeAllTriggers (instrumentTriggers es consts env)
 ++ "}\n\n"

writeTriggersActs :: ActEvents -> String
writeTriggersActs []         = "" 
writeTriggersActs (act:acts) =
 'r':show act ++ "() = {" ++ show act ++ ".receive()}" ++ "\n" ++ writeTriggersActs acts 


writeAllTriggers :: Triggers -> String
writeAllTriggers []     = ""
writeAllTriggers (e:es) = (getTrigger e) ++ writeAllTriggers es

getTrigger :: TriggerDef -> String
getTrigger (TriggerDef e arg cpe wc) =
 let wc' = if (wc == "") 
           then "" 
           else " where {" ++ wc ++ "}"
 in e ++ "(" ++ getBindArgs' arg ++ ") = " ++ getCpe cpe ++ wc' ++ "\n"


getCpe :: CompoundTrigger -> String
getCpe (Collection (CECollection xs)) = "{" ++ getCollectionCpeCompoundTrigger xs ++ "}"
getCpe ce@(OnlyIdPar _)               = getCpeCompoundTrigger ce
getCpe ce@(OnlyId _)                  = getCpeCompoundTrigger ce
getCpe ce@(ClockEvent _ _)            = getCpeCompoundTrigger ce
getCpe ce@(NormalEvent _ _ _ _)       = getCpeCompoundTrigger ce


getCollectionCpeCompoundTrigger :: [CompoundTrigger] -> String
getCollectionCpeCompoundTrigger []        = ""
getCollectionCpeCompoundTrigger [ce]      = getCpeCompoundTrigger ce
getCollectionCpeCompoundTrigger (ce:y:ys) = getCpeCompoundTrigger ce ++ " | " ++ getCollectionCpeCompoundTrigger (y:ys)

getCpeCompoundTrigger :: CompoundTrigger -> String
getCpeCompoundTrigger (OnlyIdPar id)                 = "{" ++ id ++ "()" ++ "}"
getCpeCompoundTrigger (OnlyId id)                    = "{" ++ id ++ "}"
getCpeCompoundTrigger (ClockEvent id n)              = "{" ++ id ++ "@" ++ show n ++ "}"
getCpeCompoundTrigger (NormalEvent bind id bs ev)    = "{" ++ getBinding bind ++ id 
                                                        ++ "(" ++ getBindArgs bs ++ ")"
                                                        ++ getTriggerVariation' ev ++ "}"
getCpeCompoundTrigger _                              = ""

getBindArgs :: [Bind] -> String
getBindArgs []                 = ""
getBindArgs [BindId id]        = id
getBindArgs ((BindId id):y:ys) = id ++ "," ++ getBindArgs (y:ys)
getBindArgs (BindStar:ys)      = "*," ++ getBindArgs ys

getBinding :: Binding -> String
getBinding (BindingVar BindStar)        = "*."
getBinding (BindingVar (BindType t id)) = t ++ " " ++ id ++ "."
getBinding (BindingVar (BindId id))     = id ++ "."

getTriggerVariation' :: TriggerVariation -> String
getTriggerVariation' EVEntry      = ""
getTriggerVariation' (EVExit xs)  = "uponReturning(" ++ auxGetTriggerVariation' xs ++ ")"
getTriggerVariation' (EVThrow xs) = "uponThrowing(" ++ auxGetTriggerVariation' xs ++ ")"
getTriggerVariation' (EVHadle xs) = "uponHandling(" ++ auxGetTriggerVariation' xs ++ ")"

auxGetTriggerVariation' :: [Bind] -> String
auxGetTriggerVariation' []           = ""
auxGetTriggerVariation' [BindId ret] = ret



-- Checks if the trigger to control has to be the auxiliary one (in case of optimisation by key)
instrumentTriggers :: Triggers -> HTriples -> Env -> Triggers
instrumentTriggers [] cs _       = []
instrumentTriggers (e:es) cs env = 
 let (b,mn,bs) = lookupHTForTrigger e (getClassInfo e env) cs 
 in if b
    then let e'  = updateMethodCallName e (mn++"Aux")
             e'' = updateTriggerArgs e' ((args e') ++ [BindType "Integer" "id"])
         in updateMethodCallBody e'' (bs++[BindId "id"]):instrumentTriggers es cs env
    else e:instrumentTriggers es cs env

lookupHTForTrigger :: TriggerDef -> ClassInfo -> HTriples -> (Bool, MethodName, [Bind])
lookupHTForTrigger _ _ []      = (False,"", [])
lookupHTForTrigger e ci (c:cs) = case compTrigger e of
                                      NormalEvent _ id bs _ -> if (id == mname (methodCN c) && clinf (methodCN c) == ci)
                                                               then (True, id, bs)
                                                               else lookupHTForTrigger e ci cs
                                      _                     -> lookupHTForTrigger e ci cs

getClassInfo :: TriggerDef -> Env -> ClassInfo
getClassInfo e env = 
 let trs = [tr | tr <- allTriggers env, ((\(x,_,_,_,_,_) -> x) tr) == tName e]
 in head $ map (\(_,_,x,_,_,_) -> x) trs

----------------
-- Properties --
----------------

writeProperties :: Property -> HTriples -> Env -> String
writeProperties PNIL _ _        = ""
writeProperties prop consts env = getProperties prop consts env

getProperties :: Property -> HTriples -> Env -> String
getProperties = writeProperty

writeProperty :: Property -> HTriples -> Env -> String
writeProperty PNIL _ _                                   = ""
writeProperty (Property name states trans props) cs  env =
  "PROPERTY " ++ name ++ " \n{\n\n"
  ++ writeStates states
  ++ writeTransitions name trans cs states env
  ++ "}\n\n"
  ++ writeProperty props cs env

writeStates :: States -> String
writeStates (States start acc bad normal) =
 let acc'    = getStates acc
     bad'    = getStates bad
     normal' = getStates normal
     start'  = getStates start
 in "STATES \n{\n"
    ++ writeState "ACCEPTING" acc'
    ++ writeState "BAD" bad'
    ++ writeState "NORMAL" normal'
    ++ writeState "STARTING" start'
    ++ "}\n\n"

writeState :: String -> String -> String
writeState iden xs = if (clean xs == "")
                     then ""
                     else iden ++ " { " ++ xs ++ "}\n"

getStates :: [State] -> String
getStates []              = ""
getStates (State n ic _:xs) = n ++ " " ++ getInitCode' ic ++ " " ++ getStates xs

getInitCode' :: InitialCode -> String
getInitCode' InitNil         = ""
getInitCode' (InitProg java) = "{" ++ init java ++ "}"

--HTriples [] -> Replicated Automata
--HTriples non-empty -> Property transitions instrumentation
writeTransitions :: PropertyName -> Transitions -> HTriples -> States -> Env -> String
writeTransitions _ ts [] _ env =
 "TRANSITIONS \n{ \n"
 ++ concat (map (getTransition env) ts)
 ++ "}\n\n"
writeTransitions pn ts (c:cs) states env =
 "TRANSITIONS \n{ \n"
  ++ (concat (map (getTransition env) (getTransitionsGeneral (c:cs) states ts env pn)))
  ++ "}\n\n"

getTransition :: Env -> Transition -> String
getTransition env (Transition q (Arrow e c act) q') =
 let e' = if elem e (map (\ a -> a ++ "?") $ actes env)
          then "r" ++ init e
          else e
 in q ++ " -> " ++ q' ++ " [" ++ e' ++ " \\ "  ++ c ++ " \\ " ++ (concat.lines) act ++ "]\n"

getTransitionsGeneral :: HTriples -> States -> Transitions -> Env -> PropertyName -> Transitions
getTransitionsGeneral cs (States star acc bad nor) ts env pn =
 let ts1 = generateAllTransitions star cs ts env pn
     ts2 = generateAllTransitions acc cs ts1 env pn
     ts3 = generateAllTransitions bad cs ts2 env pn
     ts4 = generateAllTransitions nor cs ts3 env pn
 in ts4

generateAllTransitions :: [State] -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
generateAllTransitions [] _ ts _ _                             = ts
generateAllTransitions ((State ns ic []):xs) cs ts env pn      = generateAllTransitions xs cs ts env pn
generateAllTransitions ((State ns ic l@(_:_)):xs) cs ts env pn = 
 let ts' = accumTransitions' l ns cs ts env pn
 in generateAllTransitions xs cs ts' env pn

accumTransitions :: [HTName] -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
accumTransitions [] _ _ ts _ _                = ts
accumTransitions (cn:cns) ns consts ts env pn =
 let nts = generateTransitions cn ns consts ts env pn
 in accumTransitions cns ns consts nts env pn

accumTransitions' :: [HTName] -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
accumTransitions' cns ns consts ts env pn = removeDuplicates $ concatMap (\cn -> generateTransitions cn ns consts ts env pn) cns


generateTransitions :: HTName -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
generateTransitions p ns cs ts env pn = 
 let c             = lookForHT p cs ns
     cl            = clinf $ methodCN c
     mn            = mname $ methodCN c
     entrs         = lookForEntryTrigger (allTriggers env) mn cl
     entrs'        = [tr | tr <- entrs, isInfixOf (mn++"_ppden") tr]
     (lts, nonlts) = foldr (\x xs -> (fst x ++ fst xs,snd x ++ snd xs)) ([],[]) $ map (\e -> lookForLeavingTransitions e ns ts) entrs'
 in if null entrs
    then error $ "Translation: Missing entry trigger for method " ++ mn ++ ".\n"
    else if (null lts)
         then ts ++ makeTransitionAlg1 ns c env pn mn entrs
         else let lts' = avoidTriviallyFalseCond lts
                  ext  = map (\e -> makeExtraTransitionAlg2 lts' c e ns env pn) entrs'
                  xs   = concat [map (\x -> instrumentTransitionAlg2 c x tr env pn) lts | tr <- entrs']
              in nonlts ++ xs ++ ext

--Method used to avoid generating transitions with conditions trivially false
avoidTriviallyFalseCond :: Transitions -> Transitions
avoidTriviallyFalseCond = filter (check.trim.cond.arrow)
                                    where check = \ t -> t /= "true" && (not.null) t

-- Implementation of Algorithm 1 --

makeTransitionAlg1 :: NameState -> HT -> Env -> PropertyName -> MethodName -> [Trigger] -> Transitions
makeTransitionAlg1 ns c env pn mn entrs = 
 let entrs' = [tr | tr <- entrs, isInfixOf (mn++"_ppden") tr]
 in if null entrs'
    then map (\e -> makeTransitionAlg1Cond ns e c env pn) entrs
    else map (\e -> makeTransitionAlg1Cond ns e c env pn) entrs'


makeTransitionAlg1Cond :: NameState -> Trigger -> HT -> Env -> PropertyName -> Transition
makeTransitionAlg1Cond ns e c env pn =
 let cn      = htName c
     oldExpM = oldExpTypes env
     esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
     esinf'  = filter (/="") $ lookfor esinf e
     arg     = if null esinf' 
               then ""
               else init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail.words) $ esinf'
     c'      = "HoareTriplesPPD." ++ cn ++ "_pre(" ++ arg ++ ")"
     zs      = getExpForOld oldExpM cn
     act     = " h" ++ show (chGet c) ++ ".send(" ++ msg ++ ");"
     type_   = if null zs then "PPD" else "Old_" ++ cn
     old     = if null zs then "" else "," ++ zs
     ident   = lookforClVar pn (propInForeach env)
     ident'  = if null ident then "(id" else "(" ++ ident ++ "::id"
     msg     = "new Messages" ++ type_ ++ ident' ++ old ++ ")"
 in Transition ns (Arrow e c' act) ns

getExpForOld :: OldExprM -> HTName -> String
getExpForOld oldExpM cn = 
 case Map.lookup cn oldExpM of
      Nothing -> ""
      Just xs -> if null xs 
                 then ""
                 else initOldExpr xs cn

initOldExpr :: OldExprL -> HTName -> String
initOldExpr oel cn = 
 "new Old_" ++ cn ++ "(" ++ addComma (map (\(x,_,_) -> x) oel) ++ ")"


-- Implementation of Algorithm 2 --

makeExtraTransitionAlg2 :: Transitions -> HT -> Trigger -> NameState -> Env -> PropertyName -> Transition
makeExtraTransitionAlg2 ts c e ns env pn = 
 let esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
     oldExpM = oldExpTypes env
     cn      = htName c
     zs      = getExpForOld oldExpM cn
     esinf'  = filter (/="") $ lookfor esinf e
     arg     = if null esinf' 
               then ""
               else init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail.words) $ esinf'
     pre'    = "HoareTriplesPPD." ++ (htName c) ++ "_pre(" ++ arg ++ ")"
     type_   = if null zs then "PPD" else "Old_" ++ cn
     old     = if null zs then "" else "," ++ zs
     ident   = lookforClVar pn (propInForeach env)
     ident'  = if null ident then "(id" else "(" ++ ident ++ "::id"
     msg     = "new Messages" ++ type_ ++ ident' ++ old ++ ")"
     act     = " h" ++ show (chGet c) ++ ".send(" ++ msg ++ ");"
     c'      = makeExtraTransitionAlg2Cond ts pn ++ pre'
 in Transition ns (Arrow e c' act) ns

makeExtraTransitionAlg2Cond :: Transitions -> PropertyName -> String
makeExtraTransitionAlg2Cond [] _      = ""
makeExtraTransitionAlg2Cond (t:ts) pn =
 let cond'  = cond $ arrow t
     cond'' = if (null $ clean cond') then "true" else cond'
 in
 "!("++ cond'' ++ ") && " ++ makeExtraTransitionAlg2Cond ts pn


instrumentTransitionAlg2 :: HT -> Transition -> Trigger -> Env -> PropertyName -> Transition
instrumentTransitionAlg2 c t@(Transition q (Arrow e' c' act) q') e env pn =
 let cn      = htName c
     oldExpM = oldExpTypes env
     esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
     esinf'  = filter (/="") $ lookfor esinf e
     arg     = if null esinf' 
               then ""
               else init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail.words) $ esinf'
     zs      = getExpForOld oldExpM cn
     type_   = if null zs then "PPD" else "Old_" ++ cn
     old     = if null zs then "" else "," ++ zs
     ident   = lookforClVar pn (propInForeach env)
     ident'  = if null ident then "(id" else "(" ++ ident ++ "::id"
     msg     = "new Messages" ++ type_ ++ ident' ++ old ++ ")"
     act'    = " if (HoareTriplesPPD." ++ cn ++ "_pre(" ++ arg ++ ")) { h" ++ show (chGet c) ++ ".send(" ++ msg ++ "); " ++ "}"
 in Transition q (Arrow e' c' (act ++ act')) q'


lookForHT :: PropertyName -> HTriples -> NameState -> HT
lookForHT p []     ns = error $ "Error: Could not find property "++ p ++ " on state " ++ ns ++ ".\n"
lookForHT p (c:cs) ns =
 if (htName c == p)
 then c
 else lookForHT p cs ns

lookForLeavingTransitions :: Trigger -> NameState -> Transitions -> (Transitions, Transitions)
lookForLeavingTransitions e ns []                                        = ([],[])
lookForLeavingTransitions e ns (t@(Transition q (Arrow e' c act) q'):ts) = 
 if (e == e')
 then if (ns == q)
      then (t:a, b)
      else (a, t:b)
 else (a,t:b)
     where (a, b) = lookForLeavingTransitions e ns ts

-------------
-- Foreach --
-------------

writeForeach :: Foreaches -> HTriples -> Env -> String
writeForeach [] _ _                    = ""
writeForeach (foreach:fors) consts env =
 let ctxt   = getCtxtForeach foreach
     args   = getArgsForeach foreach 
     vars   = variables ctxt
     es     = triggers ctxt
     prop   = property ctxt
     acts   = actevents ctxt
     fors'  = foreaches ctxt
 in "FOREACH (" ++ getForeachArgs args ++ ") {\n\n"
    ++ writeVariables vars [] []
    ++ writeTriggers es consts [] env
    ++ writeProperties prop consts env
    ++ writeForeach fors' consts env
    ++ "}\n\n"
    ++ writeForeach fors consts env

getForeachArgs :: [Args] -> String
getForeachArgs []               = ""
getForeachArgs [Args t id]      = t ++ " " ++ id
getForeachArgs (Args t id:y:ys) = t ++ " " ++ id ++ "," ++ getForeachArgs (y:ys)


---------------------
-- Methods section --
---------------------

writeMethods :: Methods -> String
writeMethods methods = "\n" ++  methods

-------------------------
-- Replicated Automata --
-------------------------

generateReplicatedAutomata :: HTriples -> Triggers -> Env -> String
generateReplicatedAutomata cs es env = 
 let n        = length cs
     ys       = zip cs [1..n]
     esinf    = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
 in generateProp esinf es ys env []


generateProp :: [(Trigger, [String])] -> Triggers -> [(HT,Int)] -> Env -> [(MethodCN,Bool)] -> String
generateProp _ _ [] _ _                  = ""
generateProp esinf es ((c,n):ys) env acc = 
 let (ra,acc') = genRA c n esinf es env acc
 in ra ++ generateProp esinf es ys env acc'

genRA :: HT -> Int -> [(Trigger, [String])] -> Triggers -> Env -> [(MethodCN,Bool)] -> (String,[(MethodCN,Bool)])
genRA c n esinf es env acc = 
 let (b,acc') = checkIfRec (methodCN c) env acc
 in if b 
    then (generatePropRec c n esinf es env, acc')
    else (generatePropNonRec c n esinf es env, acc')

--Generates automaton to control a postcondition
generatePropRec :: HT -> Int -> [(Trigger, [String])] -> Triggers -> Env -> String
generatePropRec c n esinf es env = 
 let tr      = generateTriggerRA env c n ("idPPD = msgPPD.id;")
     prop    = generateRAString esinf es env c n Nothing
     cn      = snd prop
     oldExpM = oldExpTypes env
     zs      = getOldExpr oldExpM cn
     nvar    = if null zs then "" else "Old_" ++ cn ++ " oldExpAux = new " ++ "Old_" ++ cn ++ "();\n" 
 in "FOREACH (Integer idPPD) {\n\n"
    ++ "VARIABLES {\n" ++ " Integer idAuxPPD = new Integer(0);\n " ++ nvar ++ "}\n\n"
    ++ "EVENTS {\n" ++ tr ++ "}\n\n"
    ++ fst prop
    ++ "}\n\n"

checkIfRec :: MethodCN -> Env -> [(MethodCN,Bool)] -> (Bool,[(MethodCN,Bool)])
checkIfRec mcn env acc = 
 let xs = [b | (mcn',b) <- acc , mcn == mcn']
 in if null xs
    then let minvs = getInvocationsInMethodBody mcn env
             rec   = True
         in if null minvs 
            then (False,(mcn,False):acc)
            else if directRec (mname mcn) minvs
                 then (True,(mcn,True):acc)
                 else (rec,(mcn,rec):acc)
    else (head xs,acc)

directRec :: MethodName -> [Exp] -> Bool
directRec mn []           = False
directRec mn (minv:minvs) = 
 case minv of
      MethodInv (MethodCall (Name [Ident id]) _)        -> id == mn
      MethodInv (PrimaryMethodCall This _ (Ident id) _) -> id == mn
      _                                                 -> directRec mn minvs

{- case minv of
      MethodCall name args                    -> undefined
      PrimaryMethodCall exp _ (Ident id) args -> undefined
      SuperMethodCall _ (Ident id) args       -> undefined
      ClassMethodCall _ _ (Ident id) args     -> undefined
      TypeMethodCall _ _ (Ident id) args      -> undefined
-}

getInvocationsInMethodBody :: MethodCN -> Env -> MethodInvocations
getInvocationsInMethodBody mcn env = 
 let mns = methodsInFiles env
 in getMethodInvocations mcn mns


generateTriggerRA :: Env -> HT -> Int -> String -> String
generateTriggerRA env c n w =
 let cn       = htName c
     mnc      = methodCN c
     mn       = mname mnc
     ci       = clinf mnc
     ov       = overl mnc
     oldExpM  = oldExpTypes env
     zs       = getOldExpr oldExpM cn
     nvar     = if null zs then "PPD" else "Old_" ++ cn     
 in "rh" ++ show n ++ "(Messages" ++ nvar  
    ++ " msgPPD) = {h"++ show n ++ ".receive(msgPPD)} where {" ++ w ++  "}\n"
    ++ getTrigger (generateExitTrigger ov c env)


generateExitTrigger :: Overloading -> HT -> Env -> TriggerDef
generateExitTrigger OverNil c env = 
 let mnc = methodCN c
     cl  = clinf mnc
     tr  = mn ++ "_ppdex"
     mn  = mname mnc
 in case head [ tdef | (tr',_,cl',_,_,tdef) <- allTriggers env, isInfixOf tr tr',cl == cl'] of
         Nothing   -> error $ "Error: Problem when generating the exit trigger for the Hoare triple " ++ htName c ++ "\n."
         Just tdef -> tdef
generateExitTrigger (Over ts) c env = undefined


generateRAString :: [(Trigger, [String])] -> Triggers -> Env -> HT -> Int -> Maybe () -> (String,HTName)
generateRAString esinf es env c n rec =
  let ra = if rec == Nothing then generateRA c n env else generateRAOptimised c n env
      cn = pName ra
  in ("PROPERTY " ++ cn ++ "\n{\n\n"
     ++ writeStates (pStates ra)
     ++ writeTransitions cn (pTransitions ra) [] (States [] [] [] []) emptyEnv
     ++ "}\n\n",cn)

--Optimisation: If the method is not recursive, then use optimised automaton
generatePropNonRec :: HT -> Int -> [(Trigger, [String])] -> Triggers -> Env -> String
generatePropNonRec c n esinf es env = 
 let tr      = generateTriggerRA env c n ("idPPD"++ show n ++ " = null;")
     prop    = generateRAString esinf es env c n (Just ())
     cn      = snd prop
     oldExpM = oldExpTypes env
     zs      = getOldExpr oldExpM cn
     nvar    = "VARIABLES {\n"
     nvar'   = if null zs 
               then "" 
               else nvar ++ "Old_" ++ cn ++ " oldExpAux = new " ++ "Old_" ++ cn ++ "();\n }\n\n"
 in "FOREACH (Integer idPPD" ++ show n ++ ") {\n\n"
    ++ nvar'
    ++ "EVENTS {\n" ++ tr ++ "}\n\n"
    ++ fst prop
    ++ "}\n\n"


