module Translator2DATE(translate) where

import Types
import CommonFunctions
import ReplicatedAutomataGenerator
import RefinementPPDATE
import UpgradePPDATE
import ErrM


translate :: UpgradePPD PPDATE -> FilePath -> IO ()
translate ppd fpath =
 do let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
    putStrLn "Translating ppDATE to DATE."
    writeFile fpath (writeImports (importsGet ppdate))
    let consts = assocChannel2Contracts 1 $ (contractsGet ppdate)
    let ppdate' = updateContractsPP ppdate consts
    appendFile fpath (writeGlobal ppdate' env)
    putStrLn $ "Translation complete."


assocChannel2Contracts :: Int -> Contracts -> Contracts
assocChannel2Contracts _ []     = []
assocChannel2Contracts n (c:cs) = updateCH c n:assocChannel2Contracts (n+1) cs

---------------------
-- IMPORTS section --
---------------------

getImports :: Imports -> String
getImports []            = ""
getImports (Import s:xs) = "import " ++ s ++ ";\n" ++ getImports xs

writeImports :: Imports -> String
writeImports xss = let newImp = "import ppArtifacts.Contracts;\nimport ppArtifacts.Id;\n"
                   in "IMPORTS {\n" ++ getImports xss ++ newImp  ++ "}\n\n"

--------------------
-- GLOBAL section --
--------------------

writeGlobal :: PPDATE -> Env -> String
writeGlobal ppdate env = let global = ctxtGet $ globalGet ppdate
                             consts = contractsGet ppdate
                             vars   = variables global
                             es     = events global
                             prop   = property global
                             fors   = foreaches global
                             vars'  = if (null consts)
                                      then if (null vars) then "" else "VARIABLES {\n" ++ writeVariables vars ++ "}\n\n"
                                      else "VARIABLES {\n" ++ makeChannels (length consts) ++ writeVariables vars ++ "}\n\n"
                         in "GLOBAL {\n\n"
                            ++ vars'
                            ++ writeEvents es consts
                            ++ writeProperties prop consts es env
                            ++ writeForeach fors consts env
                            -- ++ "}\n"

---------------
-- Variables --
---------------

writeVariables :: Variables -> String
writeVariables []     = ""
writeVariables (v:vs) = writeVariable v ++ ";\n" ++ writeVariables vs

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

------------
-- Events --
------------

writeEvents :: Events -> Contracts -> String
writeEvents [] _      = ""
writeEvents es consts = "EVENTS {\n"
                        ++ writeAllEvents (instrumentEvents es consts)
                        ++ "}\n\n"

writeAllEvents :: Events -> String
writeAllEvents []     = ""
writeAllEvents (e:es) = (getEvent e) ++ writeAllEvents es

getEvent :: EventDef -> String
getEvent (EventDef e arg cpe wc) =
 let wc' = if (wc == "") then "" else " where {" ++ wc ++ "}"
 in e ++ "(" ++ getBindArgs' arg ++ ") = " ++ getCpe cpe ++ wc' ++ "\n"


getBindArgs' :: [Bind] -> String
getBindArgs' []                     = ""
getBindArgs' [BindType t id]        = t ++ " " ++ id
getBindArgs' ((BindType t id):y:ys) = t ++ " " ++ id ++ "," ++ getBindArgs' (y:ys)
getBindArgs' _                      = ""


getCpe :: CompoundEvent -> String
getCpe (Collection (CECollection xs)) = "{" ++ getCollectionCpeCompoundEvent xs ++ "}"
getCpe ce@(OnlyIdPar _)               = getCpeCompoundEvent ce
getCpe ce@(OnlyId _)                  = getCpeCompoundEvent ce
getCpe ce@(ClockEvent _ _)            = getCpeCompoundEvent ce
getCpe ce@(NormalEvent _ _ _ _)       = getCpeCompoundEvent ce


getCollectionCpeCompoundEvent :: [CompoundEvent] -> String
getCollectionCpeCompoundEvent []   = ""
getCollectionCpeCompoundEvent [ce] = getCpeCompoundEvent ce
getCollectionCpeCompoundEvent (ce:y:ys) = getCpeCompoundEvent ce ++ " | " ++ getCollectionCpeCompoundEvent (y:ys)

getCpeCompoundEvent :: CompoundEvent -> String
getCpeCompoundEvent (OnlyIdPar id)                 = "{" ++ id ++ "()" ++ "}"
getCpeCompoundEvent (OnlyId id)                    = "{" ++ id ++ "}"
getCpeCompoundEvent (ClockEvent id n)              = "{" ++ id ++ "@" ++ show n ++ "}"
getCpeCompoundEvent (NormalEvent bind id bs ev)    = "{" ++ getBinding bind ++ id ++ "(" ++ getBindArgs bs ++ ")"
                                                     ++ getEventVariation' ev ++ "}"
getCpeCompoundEvent _                              = ""

getBindArgs :: [Bind] -> String
getBindArgs []                 = ""
getBindArgs [BindId id]        = id
getBindArgs ((BindId id):y:ys) = id ++ "," ++ getBindArgs (y:ys)

getBinding :: Binding -> String
getBinding (BindingVar BindStar)        = "*."
getBinding (BindingVar (BindType t id)) = t ++ " " ++ id ++ "."
getBinding (BindingVar (BindId id))     = id ++ "."

getEventVariation' :: EventVariation -> String
getEventVariation' EVEntry      = ""
getEventVariation' (EVExit xs)  = "uponReturning(" ++ auxGetEventVariation' xs ++ ")"
getEventVariation' (EVThrow xs) = "uponThrowing(" ++ auxGetEventVariation' xs ++ ")"
getEventVariation' (EVHadle xs) = "uponHandling(" ++ auxGetEventVariation' xs ++ ")"

auxGetEventVariation' :: [Bind] -> String
auxGetEventVariation' []           = ""
auxGetEventVariation' [BindId ret] = ret



-- Checks if the event to control has to be the auxiliary one (in case of optimization by key)
instrumentEvents :: Events -> Contracts -> Events
instrumentEvents [] cs     = []
instrumentEvents (e:es) cs = let (b,mn,bs) = lookupContractForEvent e cs in
                             if b
                             then let e'  = updateMethodCallName e (mn++"Aux")
                                      e'' = updateEventArgs e' ((args e') ++ [BindType "Integer" "id"])
                                  in updateMethodCallBody e'' (bs++[BindId "id"]):instrumentEvents es cs
                             else e:instrumentEvents es cs

lookupContractForEvent :: EventDef -> Contracts -> (Bool, MethodName, [Bind])
lookupContractForEvent e []     = (False,"", [])
lookupContractForEvent e (c:cs) = case compEvent e of
                                       NormalEvent _ id bs _ -> if (id == snd (methodCN c))
                                                                then (True, id, bs)
                                                                else lookupContractForEvent e cs
                                       otherwise             -> lookupContractForEvent e cs

----------------
-- Properties --
----------------

writeProperties :: Property -> Contracts -> Events -> Env -> String
writeProperties PNIL _ _ _         = ""
writeProperties prop consts es env =
 let fors   = forsVars env --list of foreach variables
     xs     = getProperties prop consts es
     ra     = generateReplicatedAutomata consts fors es
 in if (null fors)
    then xs ++ ra ++ "\n}\n"
    else xs ++ ra ++ "\n}\n}\n"


getProperties :: Property -> Contracts -> Events -> String
getProperties prop cs es = writeProperty prop cs es

writeProperty :: Property -> Contracts -> Events -> String
writeProperty PNIL _ _                                 = ""
writeProperty (Property name states trans props) cs es =
  "PROPERTY " ++ name ++ " \n{\n\n"
  ++ writeStates states
  ++ writeTransitions trans cs states es
  ++ "}\n\n"
  ++ writeProperty props cs es

writeStates :: States -> String
writeStates (States acc bad normal start) =
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
getStates (State n _:xs) = n ++ " " ++ getStates xs

--Contracts [] -> Replicated Automata
--Contracts non-empty -> Property transitions instrumentation
writeTransitions :: Transitions -> Contracts -> States -> Events -> String
writeTransitions ts [] _ _ =
 "TRANSITIONS \n{ \n"
 ++ concat (map getTransition ts)
 ++ "}\n\n"
writeTransitions ts (c:cs) states es =
 "TRANSITIONS \n{ \n"
  ++ (concat (map getTransition (getTransitionsGeneral (c:cs) states ts es)))
  ++ "}\n\n"

getTransition :: Transition -> String
getTransition (Transition q (Arrow e c act) q') =
     q ++ " -> " ++ q' ++ " [" ++ e ++ " \\ "  ++ c ++ " \\ " ++ act ++ "]\n"

getTransitionsGeneral :: Contracts -> States -> Transitions -> Events -> Transitions
getTransitionsGeneral cs (States acc bad nor star) ts es =
 let ts1 = generateTransitions acc cs ts es
     ts2 = generateTransitions bad cs ts1 es
     ts3 = generateTransitions nor cs ts2 es
     ts4 = generateTransitions star cs ts3 es
 in ts4

generateTransitions :: [State] -> Contracts -> Transitions -> Events -> Transitions
generateTransitions [] _ ts _                         = ts
generateTransitions ((State ns []):xs) cs ts es       = generateTransitions xs cs ts es
generateTransitions ((State ns l@(_:_)):xs) cs ts es  = let ts' = accumTransitions l ns cs ts es
                                                        in generateTransitions xs cs ts' es

accumTransitions :: [ContractName] -> NameState -> Contracts -> Transitions -> Events -> Transitions
accumTransitions [] _ _ ts _              = ts
accumTransitions (cn:cns) ns consts ts es =
 let ts' = generateTransition cn ns consts ts es
 in accumTransitions cns ns consts ts' es

generateTransition :: ContractName -> NameState -> Contracts -> Transitions -> Events -> Transitions
generateTransition p ns cs ts es = let c             = lookForContract p cs
                                       mn            = snd $ methodCN c
                                       e             = lookForEntryEvent es mn
                                       (lts, nonlts) = lookForLeavingTransitions e ns ts
                                   in if (null lts)
                                      then ts ++ [(makeTransitionAlg1Cond ns e es c)]
                                      else let ext = makeExtraTransitionAlg2 lts c e es ns
                                               xs  = map (\x -> instrumentTransitionAlg2 c x e es) lts
                                           in nonlts ++ xs ++ [ext]


makeTransitionAlg1Cond :: NameState -> Event -> Events -> Contract -> Transition
makeTransitionAlg1Cond ns e events c =
 let p     = pre c
     p'    = post c
     cn    = contractName c
     xs    = splitOnIdentifier cn p'
     esinf = map getInfoEvent events
     arg   = init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf e
     c'    = "Contracts." ++ cn ++ "_pre(" ++ arg ++ ")"
     act   = "h" ++ show (chGet c) ++ ".send(id);"
 in if (length xs == 1)
    then Transition ns (Arrow e c' act) ns
    else let ident = "_nyckelord"
             ys    = map (splitOnIdentifier ident) (tail xs)
             bindn = getClassVar c events
             ys'   = removeDuplicates $ map head ys
             zs    = map (\xs -> cn ++ xs ++ ident ++ " = " ++ bindn ++ "." ++ (tail xs) ++ ";") ys'
             act'  = act ++ " " ++ concat zs
         in Transition ns (Arrow e c' act') ns

makeExtraTransitionAlg2Cond :: Transitions -> String
makeExtraTransitionAlg2Cond []     = ""
makeExtraTransitionAlg2Cond (t:ts) =
 let cond'  = cond $ arrow t
     cond'' = if (null $ clean cond') then "true" else cond'
 in
 "!("++ cond'' ++ ") && " ++ makeExtraTransitionAlg2Cond ts

makeExtraTransitionAlg2 :: Transitions -> Contract -> Event -> Events -> NameState -> Transition
makeExtraTransitionAlg2 ts c e es ns = let esinf = map getInfoEvent es
                                           arg   = init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf e
                                           pre'  = "Contracts." ++ (contractName c) ++ "_pre(" ++ arg ++ ")"
                                           c'    = makeExtraTransitionAlg2Cond ts ++ pre'
                                       in Transition ns (Arrow e c' ("h" ++ show (chGet c) ++ ".send(id);")) ns


instrumentTransitionAlg2 :: Contract -> Transition -> Event -> Events -> Transition
instrumentTransitionAlg2 c t@(Transition q (Arrow e' c' act) q') e events =
 let p  = pre c
     p' = post c
     cn = contractName c
     xs = splitOnIdentifier cn p'
     esinf = map getInfoEvent events
     arg = init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf e
 in if (length xs == 1)
    then let semicol = if (act == "") then "" else ";"
             act'    = " if (Contracts." ++ cn ++ "_pre(" ++ arg ++ ")) { h" ++ show (chGet c) ++ ".send(id);}"
         in Transition q (Arrow e' c' (act ++ semicol ++ act')) q'
    else let ident   = "_nyckelord"
             ys      = map (splitOnIdentifier ident) (tail xs)
             bindn   = getClassVar c events
             ys'     = removeDuplicates $ map head ys
             zs      = map (\xs -> cn ++ xs ++ ident ++ " = " ++ bindn ++ "." ++ (tail xs) ++ ";") ys'
             semicol = if (act == "") then "" else ";"
             act'    = " if (Contracts." ++ cn ++ "_pre(" ++ arg ++ ")) { h" ++ show (chGet c) ++ ".send(id); " ++ concat zs ++ "}"
         in Transition q (Arrow e' c' (act ++ semicol ++ act')) q'

lookForContract :: PropertyName -> Contracts -> Contract
lookForContract p []     = error $ "Wow! The impossible happened when checking the property "++ p ++ " on a state.\n"
lookForContract p (c:cs) = if (contractName c == p)
                             then c
                             else lookForContract p cs

lookForLeavingTransitions :: Event -> NameState -> Transitions -> (Transitions, Transitions)
lookForLeavingTransitions e ns []                                        = ([],[])
lookForLeavingTransitions e ns (t@(Transition q (Arrow e' c act) q'):ts) = if (e == e')
                                                                           then if (ns == q && ns /= q')
                                                                                then (t:a, b)
                                                                                else (a, t:b)
                                                                           else (a,t:b)
                                                                                where (a, b) = lookForLeavingTransitions e ns ts

-------------------------
-- Replicated Automata --
-------------------------

generateReplicatedAutomata :: Contracts -> [Id] -> Events -> String
generateReplicatedAutomata cs fs es = let n      = length cs
                                          ys     = zip cs [1..n]
                                          esinf  = map getInfoEvent es
                                          props  = map (uncurry (generateRAString esinf es)) ys
                                          fors   = generateWhereInfo fs
                                          events = generateEventsRA fors n
                                          eps    = zip events props
                                      in generateProp eps


generateProp :: [(String,String)] -> String
generateProp []            = ""
generateProp ((es,ps):eps) = "FOREACH (Integer id) {\n\n"
                             ++ "VARIABLES {\n" ++ "Integer idAux ;\n " ++ "}\n\n"
                             ++ "EVENTS {\n" ++ es ++ "}\n\n"
                             ++ ps
                             ++ "}\n\n"
                             ++ generateProp eps

getInfoEvent :: EventDef -> (Event, [String])
getInfoEvent (EventDef en args ce w) = case ce of
                                            NormalEvent (BindingVar bind) _ _ _ ->
                                                case bind of
                                                     BindType t id -> (en, splitOnIdentifier "," $ getBindArgs' ((BindType t id):args))
                                                     otherwise     -> (en, splitOnIdentifier "," $ getBindArgs' args)
                                            otherwise -> error "Error: Trying to generate a replicated automaton for an incorrect event.\n"



generateWhereInfo :: [Id] -> String
generateWhereInfo []     = ""
generateWhereInfo (f:fs) = f ++ "=null;" ++ generateWhereInfo fs

generateEventsRA :: String -> Int -> [String]
generateEventsRA fs n = map (generateEventRA fs) [1..n]

generateEventRA :: String -> Int -> String
generateEventRA fs n =
  "rh" ++ show n ++ "(Integer idfrom) = {h"++ show n ++ ".receive(idfrom)} where {id=idfrom;"
   ++ fs ++  "}\n"

generateRAString :: [(Event, [String])] -> Events -> Contract -> Int -> String
generateRAString esinf es c n =
  let ra = generateRA esinf es c n
      cn = pName ra
  in "PROPERTY " ++ cn ++ "\n{\n\n"
     ++ writeStates (pStates ra)
     ++ writeTransitions (pTransitions ra) [] (States [] [] [] []) []
     ++ "}\n\n"


-------------
-- Foreach --
-------------

writeForeach :: Foreaches -> Contracts -> Env -> String
writeForeach [] _ _                         = ""
writeForeach [Foreach args ctxt] consts env =
 let vars   = variables ctxt
     es     = events ctxt
     prop   = property ctxt
     fors   = foreaches ctxt
     vars'  = if (null vars)
              then ""
              else "VARIABLES {\n" ++ writeVariables vars ++ "}\n\n"
 in "FOREACH (" ++ getForeachArgs args ++ ") {\n\n"
    ++ vars'
    ++ writeEvents es consts
    ++ writeProperties prop consts es env
    ++ writeForeach fors consts env
    -- ++ "}\n"

getForeachArgs :: [Args] -> String
getForeachArgs []               = ""
getForeachArgs [Args t id]      = t ++ " " ++ id
getForeachArgs (Args t id:y:ys) = t ++ " " ++ id ++ "," ++ getForeachArgs (y:ys)


---------------------
-- Methods section --
---------------------

writeMethods :: Methods -> String
writeMethods methods = methods
