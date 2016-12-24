module Translator2DATE(translate) where

import Types
import CommonFunctions
import ReplicatedAutomataGenerator
import RefinementPPDATE
import UpgradePPDATE
import ErrM
import qualified Data.Map as Map
import Data.Maybe


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
     es     = triggers global
     prop   = property global
     fors   = foreaches global                             
 in "GLOBAL {\n\n"
    ++ writeVariables vars consts
    ++ writeTriggers es consts
    ++ writeProperties prop consts es env
    ++ writeForeach fors consts env
    ++ "}\n"

---------------
-- Variables --
---------------

writeVariables :: Variables -> HTriples -> String
writeVariables vars consts = 
 if (null consts)
 then if (null vars) 
      then "" 
      else "VARIABLES {\n" ++ writeVariables' vars ++ "}\n\n"
 else "VARIABLES {\n" ++ makeChannels (length consts) ++ writeVariables' vars ++ "}\n\n"

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

makeChannels :: Int -> String
makeChannels n = generateChannels n ++ "\n"

generateChannels :: Int -> String
generateChannels 0 = "\n"
generateChannels n = generateChannels (n-1) ++ " Channel h" ++ show n ++ " = new Channel();\n"

--------------
-- Triggers --
--------------

writeTriggers :: Triggers -> HTriples -> String
writeTriggers [] _      = ""
writeTriggers es consts = "EVENTS {\n"
                        ++ writeAllTriggers (instrumentTriggers es consts)
                        ++ "}\n\n"

writeAllTriggers :: Triggers -> String
writeAllTriggers []     = ""
writeAllTriggers (e:es) = (getTrigger e) ++ writeAllTriggers es

getTrigger :: TriggerDef -> String
getTrigger (TriggerDef e arg cpe wc) =
 let wc' = if (wc == "") then "" else " where {" ++ wc ++ "}"
 in e ++ "(" ++ getBindArgs' arg ++ ") = " ++ getCpe cpe ++ wc' ++ "\n"


getCpe :: CompoundTrigger -> String
getCpe (Collection (CECollection xs)) = "{" ++ getCollectionCpeCompoundTrigger xs ++ "}"
getCpe ce@(OnlyIdPar _)               = getCpeCompoundTrigger ce
getCpe ce@(OnlyId _)                  = getCpeCompoundTrigger ce
getCpe ce@(ClockEvent _ _)            = getCpeCompoundTrigger ce
getCpe ce@(NormalEvent _ _ _ _)       = getCpeCompoundTrigger ce


getCollectionCpeCompoundTrigger :: [CompoundTrigger] -> String
getCollectionCpeCompoundTrigger []   = ""
getCollectionCpeCompoundTrigger [ce] = getCpeCompoundTrigger ce
getCollectionCpeCompoundTrigger (ce:y:ys) = getCpeCompoundTrigger ce ++ " | " ++ getCollectionCpeCompoundTrigger (y:ys)

getCpeCompoundTrigger :: CompoundTrigger -> String
getCpeCompoundTrigger (OnlyIdPar id)                 = "{" ++ id ++ "()" ++ "}"
getCpeCompoundTrigger (OnlyId id)                    = "{" ++ id ++ "}"
getCpeCompoundTrigger (ClockEvent id n)              = "{" ++ id ++ "@" ++ show n ++ "}"
getCpeCompoundTrigger (NormalEvent bind id bs ev)    = "{" ++ getBinding bind ++ id ++ "(" ++ getBindArgs bs ++ ")"
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



-- Checks if the trigger to control has to be the auxiliary one (in case of optimization by key)
instrumentTriggers :: Triggers -> HTriples -> Triggers
instrumentTriggers [] cs     = []
instrumentTriggers (e:es) cs = let (b,mn,bs) = lookupHTForTrigger e cs in
                             if b
                             then let e'  = updateMethodCallName e (mn++"Aux")
                                      e'' = updateTriggerArgs e' ((args e') ++ [BindType "Integer" "id"])
                                  in updateMethodCallBody e'' (bs++[BindId "id"]):instrumentTriggers es cs
                             else e:instrumentTriggers es cs

lookupHTForTrigger :: TriggerDef -> HTriples -> (Bool, MethodName, [Bind])
lookupHTForTrigger e []     = (False,"", [])
lookupHTForTrigger e (c:cs) = case compTrigger e of
                                       NormalEvent _ id bs _ -> if (id == snd (methodCN c))
                                                                then (True, id, bs)
                                                                else lookupHTForTrigger e cs
                                       otherwise             -> lookupHTForTrigger e cs

----------------
-- Properties --
----------------

writeProperties :: Property -> HTriples -> Triggers -> Env -> String
writeProperties PNIL _ _ _         = ""
writeProperties prop consts es env =
 let fors   = forsVars env --list of foreach variables
     xs     = getProperties prop consts es env
     ra     = generateReplicatedAutomata consts fors es env
 in xs ++ ra


getProperties :: Property -> HTriples -> Triggers -> Env -> String
getProperties = writeProperty

writeProperty :: Property -> HTriples -> Triggers -> Env -> String
writeProperty PNIL _ _ _                                   = ""
writeProperty (Property name states trans props) cs es env =
  "PROPERTY " ++ name ++ " \n{\n\n"
  ++ writeStates states
  ++ writeTransitions trans cs states es env
  ++ "}\n\n"
  ++ writeProperty props cs es env

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
writeTransitions :: Transitions -> HTriples -> States -> Triggers -> Env -> String
writeTransitions ts [] _ _ _ =
 "TRANSITIONS \n{ \n"
 ++ concat (map getTransition ts)
 ++ "}\n\n"
writeTransitions ts (c:cs) states es env =
 "TRANSITIONS \n{ \n"
  ++ (concat (map getTransition (getTransitionsGeneral (c:cs) states ts es env)))
  ++ "}\n\n"

getTransition :: Transition -> String
getTransition (Transition q (Arrow e c act) q') =
     q ++ " -> " ++ q' ++ " [" ++ e ++ " \\ "  ++ c ++ " \\ " ++ act ++ "]\n"

getTransitionsGeneral :: HTriples -> States -> Transitions -> Triggers -> Env -> Transitions
getTransitionsGeneral cs (States star acc bad nor) ts es env =
 let ts1 = generateTransitions star cs ts es env
     ts2 = generateTransitions acc cs ts1 es env 
     ts3 = generateTransitions bad cs ts2 es env
     ts4 = generateTransitions nor cs ts3 es env
 in ts4

generateTransitions :: [State] -> HTriples -> Transitions -> Triggers -> Env -> Transitions
generateTransitions [] _ ts _ _                              = ts
generateTransitions ((State ns ic []):xs) cs ts es env       = generateTransitions xs cs ts es env
generateTransitions ((State ns ic l@(_:_)):xs) cs ts es env  = let ts' = accumTransitions l ns cs ts es env
                                                               in generateTransitions xs cs ts' es env

accumTransitions :: [HTName] -> NameState -> HTriples -> Transitions -> Triggers -> Env -> Transitions
accumTransitions [] _ _ ts _ _                = ts
accumTransitions (cn:cns) ns consts ts es env =
 let ts' = generateTransition cn ns consts ts es env
 in accumTransitions cns ns consts ts' es env

generateTransition :: HTName -> NameState -> HTriples -> Transitions -> Triggers -> Env -> Transitions
generateTransition p ns cs ts es env = let c             = lookForHT p cs
                                           mn            = snd $ methodCN c
                                           e             = lookForEntryTrigger (allTriggers env) mn
                                           (lts, nonlts) = lookForLeavingTransitions e ns ts
                                       in if (null lts)
                                          then ts ++ [(makeTransitionAlg1Cond ns e c env)]
                                          else let ext = makeExtraTransitionAlg2 lts c e ns env
                                                   xs  = map (\x -> instrumentTransitionAlg2 c x e env) lts
                                               in nonlts ++ xs ++ [ext]


makeTransitionAlg1Cond :: NameState -> Trigger -> HT -> Env -> Transition
makeTransitionAlg1Cond ns e c env =
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
     msg     = "new Messages" ++ type_ ++ "(id" ++ old ++ ")"
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



makeExtraTransitionAlg2Cond :: Transitions -> String
makeExtraTransitionAlg2Cond []     = ""
makeExtraTransitionAlg2Cond (t:ts) =
 let cond'  = cond $ arrow t
     cond'' = if (null $ clean cond') then "true" else cond'
 in
 "!("++ cond'' ++ ") && " ++ makeExtraTransitionAlg2Cond ts

makeExtraTransitionAlg2 :: Transitions -> HT -> Trigger -> NameState -> Env -> Transition
makeExtraTransitionAlg2 ts c e ns env = let esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
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
                                            msg     = "new Messages" ++ type_ ++ "(id" ++ old ++ ")"
                                            act     = " h" ++ show (chGet c) ++ ".send(" ++ msg ++ ");"
                                            c'      = makeExtraTransitionAlg2Cond ts ++ pre'
                                        in Transition ns (Arrow e c' act) ns


instrumentTransitionAlg2 :: HT -> Transition -> Trigger -> Env -> Transition
instrumentTransitionAlg2 c t@(Transition q (Arrow e' c' act) q') e env =
 let cn      = htName c
     oldExpM = oldExpTypes env
     esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
     esinf'  = filter (/="") $ lookfor esinf e
     arg     = if null esinf' 
               then ""
               else init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail.words) $ esinf'
     semicol = if (act == "") then "" else ";"     
     zs      = getExpForOld oldExpM cn
     type_   = if null zs then "PPD" else "Old_" ++ cn
     old     = if null zs then "" else "," ++ zs
     msg     = "new Messages" ++ type_ ++ "(id" ++ old ++ ")"
     act'    = " if (HoareTriplesPPD." ++ cn ++ "_pre(" ++ arg ++ ")) { h" ++ show (chGet c) ++ ".send(" ++ msg ++ "); " ++ "}"
 in Transition q (Arrow e' c' (act ++ semicol ++ act')) q'

lookForHT :: PropertyName -> HTriples -> HT
lookForHT p []     = error $ "Wow! The impossible happened when checking the property "++ p ++ " on a state.\n"
lookForHT p (c:cs) = if (htName c == p)
                     then c
                     else lookForHT p cs

lookForLeavingTransitions :: Trigger -> NameState -> Transitions -> (Transitions, Transitions)
lookForLeavingTransitions e ns []                                        = ([],[])
lookForLeavingTransitions e ns (t@(Transition q (Arrow e' c act) q'):ts) = 
 if (e == e')
 then if (ns == q)
      then (t:a, b)
      else (a, t:b)
 else (a,t:b)
     where (a, b) = lookForLeavingTransitions e ns ts

-------------------------
-- Replicated Automata --
-------------------------

generateReplicatedAutomata :: HTriples -> [Id] -> Triggers -> Env -> String
generateReplicatedAutomata cs fs es env = 
 let n      = length cs
     ys     = zip cs [1..n]
     esinf  = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
     props  = map (uncurry (generateRAString esinf es env)) ys
     fors   = generateWhereInfo fs
     triggers = generateTriggersRA fors (map (\(x,y) -> (htName x,y)) ys) env
     eps      = zip triggers props
 in generateProp eps env


generateProp :: [(String,(String,HTName))] -> Env -> String
generateProp [] _               = ""
generateProp ((es,ps):eps)  env = 
 let cn       = snd ps
     oldExpM  = oldExpTypes env
     zs       = getOldExpr oldExpM cn
     nvar     = if null zs then "" else "Old_" ++ cn ++ " oldExpAux = new " ++ "Old_" ++ cn ++ "();\n" 
 in "FOREACH (Integer idPPD) {\n\n"
    ++ "VARIABLES {\n" ++ " Integer idAux = new Integer(0);\n " ++ nvar ++ "}\n\n"
    ++ "EVENTS {\n" ++ es ++ "}\n\n"
    ++ fst ps
    ++ "}\n\n"
    ++ generateProp eps env

generateWhereInfo :: [Id] -> String
generateWhereInfo []     = ""
generateWhereInfo (f:fs) = f ++ "=null;" ++ generateWhereInfo fs

generateTriggersRA :: String -> [(HTName,Int)] -> Env -> [String]
generateTriggersRA fs ns env = map (uncurry (generateTriggerRA fs env)) ns

generateTriggerRA :: String -> Env -> HTName -> Int -> String
generateTriggerRA fs env cn n =
 let oldExpM  = oldExpTypes env
     zs       = getOldExpr oldExpM cn
     nvar     = if null zs then "PPD" else "Old_" ++ cn
 in "rh" ++ show n ++ "(Messages" ++ nvar ++ " msg) = {h"++ show n ++ ".receive(msg)} where {idPPD=msg.id;"
    ++ fs ++  "}\n"

generateRAString :: [(Trigger, [String])] -> Triggers -> Env -> HT -> Int -> (String,HTName)
generateRAString esinf es env c n =
  let ra = generateRA c n env
      cn = pName ra
  in ("PROPERTY " ++ cn ++ "\n{\n\n"
     ++ writeStates (pStates ra)
     ++ writeTransitions (pTransitions ra) [] (States [] [] [] []) [] emptyEnv
     ++ "}\n\n",cn)


-------------
-- Foreach --
-------------

writeForeach :: Foreaches -> HTriples -> Env -> String
writeForeach [] _ _                              = ""
writeForeach (Foreach args ctxt:fors) consts env =
 let vars   = variables ctxt
     es     = triggers ctxt
     prop   = property ctxt
     fors   = foreaches ctxt
 in "FOREACH (" ++ getForeachArgs args ++ ") {\n\n"
    ++ writeVariables vars []
    ++ writeTriggers es consts
    ++ writeProperties prop consts es env
    ++ writeForeach fors consts env
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
