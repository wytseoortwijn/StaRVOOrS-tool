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
import qualified AbsActions as Act

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
     acts   = actes env
     trs    = triggers global
     prop   = property global
     fors   = foreaches global         
     temps  = templatesGet ppdate            
 in "GLOBAL {\n\n" ++ writeVariables vars consts acts (allCreateAct env)
    ++ writeTriggers trs consts acts env
    ++ writeProperties prop consts env
    ++ writeForeach fors consts env
    ++ generateReplicatedAutomata consts trs env  
    ++ writeTemplates temps env
    ++ "}\n" 
 
---------------
-- Variables --
---------------

writeVariables :: Variables -> HTriples -> [Id] -> [CreateActInfo] -> String
writeVariables vars [] [] [] = 
 if null vars
 then ""
 else "VARIABLES {\n" ++ writeVariables' vars ++ "}\n\n"
writeVariables vars consts acts creates = 
 let actChann    = if (null acts) then "" else concatMap makeChannelsAct (removeDuplicates acts) 
     createChann = if (null creates) then "" else concatMap (makeChannelsAct.caiCh) creates
     constsChann = if (null consts) then "" else makeChannels (length consts) "h"
     extraChann  = actChann ++ createChann ++ constsChann
 in if (null vars) 
    then if (null extraChann)
         then ""
         else "VARIABLES {\n" ++ extraChann ++ "}\n\n"
    else "VARIABLES {\n" ++ extraChann ++ writeVariables' vars ++ "}\n\n"

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

writeTriggers :: Triggers -> HTriples -> [Id] -> Env -> String
writeTriggers [] _ _ _           = ""
writeTriggers es consts acts env = 
 "EVENTS {\n"
 ++ writeTriggersActs acts
 ++ writeAllTriggers (instrumentTriggers es consts env)
 ++ "}\n\n"

writeTriggersActs :: [Id] -> String
writeTriggersActs []         = "" 
writeTriggersActs (act:acts) =
 'r':show act ++ "() = {" ++ act ++ ".receive()}" ++ "\n" ++ writeTriggersActs acts 


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
 let trs = [tr | tr <- allTriggers env, tiTN tr == tName e]
 in head $ map tiCI trs

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
 let ts' = accumTransitions l ns cs ts env pn
 in generateAllTransitions xs cs ts' env pn

accumTransitions :: [HTName] -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
accumTransitions [] _ _ ts _ _                = ts
accumTransitions (cn:cns) ns consts ts env pn =
 let (nonlts,gentrans,lts) = generateTransitions cn ns consts ts env pn
     instrans = instrumentTransitions cn ns consts lts env pn
 in accumTransitions cns ns consts (nonlts++instrans) env pn ++ gentrans

generateTransitions :: HTName -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> (Transitions,Transitions,Transitions)
generateTransitions p ns cs ts env pn = 
 let c             = lookForHT p cs ns
     mn            = mname $ methodCN c
     entrs         = lookForEntryTrigger (allTriggers env) (methodCN c)
     entrs'        = [tr | tr <- entrs, not(isInfixOf (mn++"_ppden") tr)]
     (lts, nonlts) = foldr (\x xs -> (fst x ++ fst xs,snd x ++ snd xs)) ([],[]) $ map (\e -> lookForLeavingTransitions e ns ts) entrs'
 in if null entrs
    then error $ "Translation: Missing entry trigger for method " ++ mn ++ ".\n"
    else if (null lts)
         then (ts,makeTransitionAlg1 ns c env pn mn entrs,[])
         else let ext  = map (\e -> makeExtraTransitionAlg2 lts c e ns env pn) entrs'
                  ext' = map fromJust $ filter (\c -> c /= Nothing) ext
              in (nonlts,ext',lts)            

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
     (_,arg) = getValue $ lookForAllEntryTriggerArgs env c 
     c'      = "HoareTriplesPPD." ++ cn ++ "_pre(" ++ arg ++ ")"
     zs      = getExpForOld oldExpM cn
     act     = " h" ++ show (chGet c) ++ ".send(" ++ msg ++ ");"
     type_   = if null zs then "PPD" else "Old<Old_" ++ cn ++ ">"
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

makeExtraTransitionAlg2 :: Transitions -> HT -> Trigger -> NameState -> Env -> PropertyName -> Maybe Transition
makeExtraTransitionAlg2 [] _ _ _ _ _     = Nothing
makeExtraTransitionAlg2 ts c e ns env pn = 
 let oldExpM = oldExpTypes env
     cn      = htName c
     zs      = getExpForOld oldExpM cn
     (_,arg) = getValue $ lookForAllEntryTriggerArgs env c 
     pre'    = "HoareTriplesPPD." ++ (htName c) ++ "_pre(" ++ arg ++ ")"
     type_   = if null zs then "PPD" else "Old<Old_" ++ cn ++ ">"
     old     = if null zs then "" else "," ++ zs
     ident   = lookforClVar pn (propInForeach env)
     ident'  = if null ident then "(id" else "(" ++ ident ++ "::id"
     msg     = "new Messages" ++ type_ ++ ident' ++ old ++ ")"
     act     = " h" ++ show (chGet c) ++ ".send(" ++ msg ++ ");"
     c'      = makeExtraTransitionAlg2Cond ts 
 in case c' of
         Nothing  -> Nothing 
         Just c'' -> Just $ Transition ns (Arrow e (c''++ pre') act) ns

makeExtraTransitionAlg2Cond :: Transitions -> Maybe String
makeExtraTransitionAlg2Cond ts = 
 case avoidTriviallyFalseCond ts of 
      [] -> Nothing 
      xs -> Just $ getConditionsInTransitions xs

getConditionsInTransitions :: Transitions -> String
getConditionsInTransitions []     = ""
getConditionsInTransitions (t:ts) =
 let cond'  = cond $ arrow t
     cond'' = if (null $ clean cond') then "true" else cond'
 in "!("++ cond'' ++ ") && " ++ getConditionsInTransitions ts

--Method used to avoid generating transitions with conditions trivially false
avoidTriviallyFalseCond :: Transitions -> Transitions
avoidTriviallyFalseCond = filter (check.trim.cond.arrow)
                                    where check = \ t -> t /= "true" && (not.null.trim) t

instrumentTransitions :: HTName -> NameState -> HTriples -> Transitions -> Env -> PropertyName -> Transitions
instrumentTransitions p ns cs ts env pn = 
 let c             = lookForHT p cs ns
     mn            = mname $ methodCN c
     entrs         = lookForEntryTrigger (allTriggers env) (methodCN c)
     entrs'        = [tr | tr <- entrs, not(isInfixOf (mn++"_ppden") tr)]
     (lts, nonlts) = foldr (\x xs -> (fst x ++ fst xs,snd x ++ snd xs)) ([],[]) $ map (\e -> lookForLeavingTransitions e ns ts) entrs'
 in if null entrs
    then error $ "Translation: Missing entry trigger for method " ++ mn ++ ".\n"
    else if (null lts)
         then []
         else concat [map (\x -> instrumentTransitionAlg2 c x tr env pn) lts | tr <- entrs']

instrumentTransitionAlg2 :: HT -> Transition -> Trigger -> Env -> PropertyName -> Transition
instrumentTransitionAlg2 c t@(Transition q (Arrow e' c' act) q') e env pn =
 let cn      = htName c
     oldExpM = oldExpTypes env
     (_,arg) = getValue $ lookForAllEntryTriggerArgs env c 
     zs      = getExpForOld oldExpM cn
     type_   = if null zs then "PPD" else "Old<Old_" ++ cn ++ ">"
     old     = if null zs then "" else "," ++ zs
     ident   = lookforClVar pn (propInForeach env)
     ident'  = if null ident then "(id" else "(" ++ ident ++ "::id"
     msg     = "new Messages" ++ type_ ++ ident' ++ old ++ ")"
     act'    = " if (HoareTriplesPPD." ++ cn ++ "_pre(" ++ arg ++ ")) { h" ++ show (chGet c) ++ ".send(" ++ msg ++ "); " ++ "} ;"
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
    ++ writeVariables vars [] [] []
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
 in generateProp es ys env []


generateProp :: Triggers -> [(HT,Int)] -> Env -> [(MethodCN,Bool)] -> String
generateProp _ [] _ _              = ""
generateProp es ((c,n):ys) env acc = 
 let (ra,acc') = genRA c n es env acc
 in ra ++ generateProp es ys env acc'

genRA :: HT -> Int -> Triggers -> Env -> [(MethodCN,Bool)] -> (String,[(MethodCN,Bool)])
genRA c n es env acc = 
 let (b,acc') = checkIfRec (methodCN c) env acc
 in if True --TODO:replace by b once the optimisation is implemented
    then (generatePropRec c n es env, acc')
    else (generatePropNonRec c n es env, acc')

--Generates automaton to control a postcondition
generatePropRec :: HT -> Int -> Triggers -> Env -> String
generatePropRec c n es env = 
 let tr      = generateTriggerRA env c n ("idPPD = msgPPD.id;","idPPD = id;")
     prop    = generateRAString es env c n Nothing
     cn      = snd prop
     oldExpM = oldExpTypes env
     zs      = getOldExpr oldExpM cn
     nvar    = if null zs then "" else "Old_" ++ cn ++ " oldExpAux = new " ++ "Old_" ++ cn ++ "();\n" 
 in "FOREACH (Integer idPPD) {\n\n"
    ++ "VARIABLES {\n" ++ " Integer idAuxPPD = new Integer(0);\n" ++ nvar ++ "}\n\n"
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

generateTriggerRA :: Env -> HT -> Int -> (String,String) -> String
generateTriggerRA env c n w =
 let cn       = htName c
     mnc      = methodCN c
     mn       = mname mnc
     ci       = clinf mnc
     ov       = overl mnc
     oldExpM  = oldExpTypes env
     zs       = getOldExpr oldExpM cn
     nvar     = if null zs then "PPD" else "Old<Old_" ++ cn ++ ">"     
     wtr      = if null (snd w) then "" else " where { " ++ snd w ++ "}\n"
     ntr      = getTriggerDef ov c (allTriggers env)
 in "rh" ++ show n ++ "(Messages" ++ nvar  
    ++ " msgPPD) = {h"++ show n ++ ".receive(msgPPD)} where {" ++ fst w ++  "}\n"
    ++ init (concatMap getTrigger (instrumentTriggers [ntr] [c] env)) ++ wtr


generateRAString :: Triggers -> Env -> HT -> Int -> Maybe () -> (String,HTName)
generateRAString es env c n rec =
  let ra = if rec == Nothing then generateRA c n env else generateRAOptimised c n env
      cn = pName ra
  in ("PROPERTY " ++ cn ++ "\n{\n\n"
     ++ writeStates (pStates ra)
     ++ writeTransitions cn (pTransitions ra) [] (States [] [] [] []) emptyEnv
     ++ "}\n\n",cn)

--Optimisation: If the method is not recursive, then use optimised automaton
generatePropNonRec :: HT -> Int -> Triggers -> Env -> String
generatePropNonRec c n es env = 
 let tr      = generateTriggerRA env c n ("idPPD"++ show n ++ " = null;","idPPD"++ show n ++ " = null;")
     prop    = generateRAString es env c n (Just ())
     cn      = snd prop
     oldExpM = oldExpTypes env
     zs      = getOldExpr oldExpM cn
     nvar    = "VARIABLES {\n"
     nvar'   = if null zs 
               then "" 
               else nvar ++ "Old_" ++ cn ++ " oldExpAux = new " ++ "Old_" ++ cn ++ "();\n}\n\n"
 in "FOREACH (Integer idPPD" ++ show n ++ ") {\n\n"
    ++ nvar'
    ++ "EVENTS {\n" ++ tr ++ "}\n\n"
    ++ fst prop
    ++ "}\n\n"

---------------
-- Templates --
---------------

writeTemplates :: Templates -> Env -> String
writeTemplates TempNil _        = ""
writeTemplates (Temp temps) env = 
 let creates = removeDuplicates $ allCreateAct env
     skell   = map generateRAtmp temps
     xs      = [ instantiateTemp for id args (caiCh cai) | (id,args,for) <- skell , cai <- creates, id == caiId cai ] 
 in writeForeach xs [] env

generateRAtmp :: Template -> (Id, [Args], Foreach)
generateRAtmp temp = (tempId temp, tempBinds temp, Foreach (filterRefTypes $ tempBinds temp) (generateCtxtForTemp temp) (ForId (tempId temp)))

generateCtxtForTemp :: Template -> Context
generateCtxtForTemp temp = Ctxt (tempVars temp) (tempActEvents temp) (tempTriggers temp) (tempProp temp) []

instantiateTemp :: Foreach -> Id -> [Args] -> Channel -> Foreach
instantiateTemp for id args ch = 
 let ctxt   = getCtxtForeach for 
     ctxt'  = updateCtxtTrs ctxt ((genTriggerForCreate id ch args):triggers ctxt)
     ctxt'' = updateCtxtProps ctxt' (instantiateProp (property ctxt) args ch)
 in Foreach (getArgsForeach for) ctxt'' (ForId (show (getIdForeach for) ++ ch))

instantiateProp :: Property -> [Args] -> Channel -> Property
instantiateProp PNIL _ _                              = PNIL
instantiateProp (Property id sts trans props) args ch =  
 let sts'   = addNewInitState sts
     trans' = (Transition "start" (Arrow ("r"++ch) "" "") (head $ map getNameState $ getStarting sts)):trans
 in Property (id++"_"++ch) sts' trans' props

genTriggerForCreate :: Id -> Channel -> [Args] -> TriggerDef
genTriggerForCreate id ch args = 
 let tr    = "r"++ch
     args' = BindType ("Tmp_"++id) "obj"
     ce    = NormalEvent (BindingVar (BindId ch)) "receive" [BindId "obj"] EVEntry
     w     = concat [ getArgsId arg ++ " = obj." ++ getArgsId arg ++ "; " | arg <- filterRefTypes args ] 
 in TriggerDef tr [args'] ce w

addNewInitState :: States -> States
addNewInitState (States start accep bad norm) = States [State "start" InitNil []] accep bad (norm++start)
