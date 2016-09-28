module UpgradePPDATE where

import Types
import CommonFunctions
import Control.Monad
import Control.Monad.Writer
import qualified Control.Monad.State as CM
import qualified Data.Map as Map
import qualified Absppdate as Abs
import ErrM
import Printppdate
import Data.List


upgradePPD :: Abs.AbsPPDATE -> UpgradePPD PPDATE
upgradePPD (Abs.AbsPPDATE imports global cinvs consts methods) =
 do let imports' = genImports imports
    let cinvs'   = genClassInvariants cinvs
    let methods' = genMethods methods
    case runStateT (genHTs consts) emptyEnv of
         Bad s             -> fail s
         Ok (consts', env) -> let cns = htsNames env
                                  dcs = getDuplicate cns
                              in case runStateT (genGlobal global) env of
                                      Bad s              -> if (not.null) dcs
                                                            then fail $ s ++ duplicateHT dcs
                                                            else fail s
                                      Ok (global', env') -> if (not.null) dcs
                                                            then fail $ duplicateHT dcs
                                                            else do put env'
                                                                    return (PPDATE imports' global' cinvs' consts' methods')


duplicateHT :: [HTName] -> String
duplicateHT []     = ""
duplicateHT (c:cs) = "Error: Multiple definitions for Hoare triple " ++ c ++ ".\n" ++ duplicateHT cs

getDuplicate :: [HTName] -> [HTName]
getDuplicate []     = []
getDuplicate (c:cs) = if elem c cs
                      then c:getDuplicate cs
                      else getDuplicate cs

-------------
-- Imports --
-------------

genImports :: Abs.Imports -> Imports
genImports (Abs.Imports imps) =
 let jfss = map getImportAbs imps
 in map (Import . joinImport . map (getIdAbs.getJFAbs)) jfss

------------
-- Global --
------------

genGlobal :: Abs.Global -> UpgradePPD Global
genGlobal (Abs.Global ctxt) =
 do ctxt' <- getCtxt ctxt
    return (Global ctxt')

-- Context --

getCtxt :: Abs.Context -> UpgradePPD Context
getCtxt (Abs.Ctxt vars ies trigs prop foreaches) =
 do env <- get
    let cns = htsNames env
    trigs' <- getTriggers trigs
    fors <- getForeaches foreaches
    let vars' = getVars vars
    let prop' = getProperty prop (map tName trigs')
    let ies' = getIEvents ies
    case runWriter prop' of
         (PNIL,_)                              -> return (Ctxt vars' ies' trigs' PNIL fors)
         (Property pname states trans props,s) -> let accep  = checkAllHTsExist (getAccepting states) cns pname
                                                      bad    = checkAllHTsExist (getBad states) cns pname
                                                      normal = checkAllHTsExist (getNormal states) cns pname
                                                      start  = checkAllHTsExist (getStarting states) cns pname
                                                      errs   = concat $ accep ++ bad ++ normal ++ start
                                                      s'     = if (not.null) s
                                                               then "Error: Triggers [" ++ s ++ "] are used in the transitions, but are not defined in section TRIGGERS.\n" ++ errs
                                                               else errs
                                                  in if (null s')
                                                     then return (Ctxt vars' ies' trigs' (Property pname states trans props) fors)
                                                     else fail s'


checkAllHTsExist :: [State] -> [HTName] -> PropertyName -> [String]
checkAllHTsExist [] _ _        = []
checkAllHTsExist (s:ss) cns pn = 
 let ns   = getNS s
     cns' = getCNList s
     aux  = [x | x <- cns' , not (elem x cns)]
 in if (null aux)
    then checkAllHTsExist ss cns pn
    else ("Error: On property " ++ pn
         ++ ", in state " ++ ns ++ ", the Hoare triples(s) "
         ++ commaAdd aux
         ++ " do(es) not exist.\n") : checkAllHTsExist ss cns pn
                              where commaAdd []       = ""
                                    commaAdd [xs]     = xs
                                    commaAdd (xs:xss) = xs ++ "," ++ commaAdd xss
-- Variables --

getVars :: Abs.Variables -> Variables
getVars Abs.VarNil        = []
getVars (Abs.VarDef vars) = map getVariable vars

getVariable :: Abs.Variable -> Variable
getVariable (Abs.Var varm t vdecls) =
 let varm' = getVarModif varm
     t'    = getTypeAbs t
     vs    = map getVarDecl vdecls
 in Var varm' t' vs

getVarModif :: Abs.VarModifier -> VarModifier
getVarModif Abs.VarModifierFinal = VarModifierFinal
getVarModif Abs.VarModifierNil   = VarModifierNil

getVarDecl :: Abs.VarDecl -> VarDecl
getVarDecl (Abs.VarDecl id varinit) = VarDecl (getIdAbs id) (getVarInitAbs varinit)

-- IEvents --

getIEvents :: Abs.IEvents -> IEvents
getIEvents Abs.IEventsNil       = []
getIEvents (Abs.IEventsDef ies) = map (\(Abs.IEvent ie) -> IEvent (getIdAbs ie)) ies

-- Triggers --

getTriggers :: Abs.Triggers -> UpgradePPD Triggers
getTriggers Abs.TriggersNil      = return []
getTriggers (Abs.TriggersDef es) =
 do env <- get
    let xs = map getTrigger' es
    let (ls, rs) = partitionErr (map (\e -> CM.evalStateT e env) xs)
    if (null ls)
    then sequence xs
    else fail $ foldr joinBad "" ls

joinBad :: Err a -> String -> String
joinBad (Bad s1) s2 = s1 ++ s2
joinBad _ _         = error "ppDATE refinement failure: joinBad \n"

getTrigger' :: Abs.Trigger -> UpgradePPD TriggerDef
getTrigger' (Abs.Trigger id binds ce wc)      =
 do env  <- get
    let id'' = getIdAbs id
    let err  = if (elem id'' (allTriggersId env)) then ("Error: Multiple definitions for trigger " ++ id'' ++ ".\n") else ""
    do case runWriter (getBindsArgs binds) of
         (bs, s) ->
           let err0 = if (not.null) s then (err ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ s ++ "].\n") else err
           in case runWriter (getCompTriggers ce) of
                 (ce',s') ->
                   let err1 = if (not.null) s'
                              then err0 ++ ("Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ s' ++ "] in the method component.\n")
                              else err0
                       argss = map getBindTypeId bs
                   in case ce' of
                          NormalEvent (BindingVar bind) mn args' eventv
                               -> let allArgs = map getBindIdId (filter (\ c -> c /= BindStar) args') in
                                  case eventv of
                                       EVEntry  -> let id  = getIdBind bind
                                                       wc' = getWhereClause wc
                                                       wcs = [x | x <- argss, not(elem x allArgs)]
                                                       vs  = filter (\ x -> x /= id) $ checkVarsInitialisation wcs (getVarsWC wc)
                                                    in if ((not.null) vs) then fail (err1 ++ "Error: Missing Initialization of variable(s) " ++ show vs ++  " in trigger declaration [" ++ id'' ++ "].\n")
                                                       else
                                                         case runWriter ((checkAllArgs argss allArgs bind)) of
                                                          (b,zs) ->
                                                            if b
                                                             then if (not.null) err1 then fail err1 else
                                                                 do let env' = updateEntryTriggersInfo env (id'', getTriggerClass bind, (map bindToArgs bs)) bs mn bind
                                                                    put env' { allTriggersId = id'' : allTriggersId env }
                                                                    return TriggerDef { tName = id''
                                                                                      , args  = bs
                                                                                      , compTrigger = ce'
                                                                                      , whereClause = getWhereClause wc
                                                                                    }
                                                            else fail (err1 ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ addComma zs ++ "] in the method component.\n")
                                       EVExit rs -> let id  = getIdBind bind
                                                        wc' = getWhereClause wc
                                                        wcs = [x | x <- argss, not(elem x allArgs)]
                                                        rs' = map getIdBind rs
                                                        vs  = filter (\ x -> (not (elem x rs')) && (x /= id)) $ checkVarsInitialisation wcs (getVarsWC wc)
                                                    in if ((not.null) vs) then fail (err1 ++ "Error: Missing Initialization of variable(s) " ++ show vs ++  " in trigger declaration [" ++ id'' ++ "].\n")
                                                       else
                                                        case runWriter ((checkAllArgs argss allArgs bind)) of
                                                          (b,zs) ->
                                                            if (b && (checkRetVar rs argss))
                                                            then if (not.null) err1 then fail err1 else
                                                                 do let env' = updateExitTriggersInfo env (id'', getTriggerClass bind, (map bindToArgs bs)) bs mn bind
                                                                    put env' { allTriggersId = id'' : allTriggersId env }
                                                                    return TriggerDef { tName = id''
                                                                                      , args  = bs
                                                                                      , compTrigger = ce'
                                                                                      , whereClause = wc'
                                                                                      }
                                                            else fail (err1 ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ addComma zs ++ "] in the method component.\n")
                                       _        -> return TriggerDef { tName = id''
                                                                     , args  = bs
                                                                     , compTrigger = ce'
                                                                     , whereClause = getWhereClause wc
                                                                     }
                          _  -> if (not.null) err1 then fail err1 else
                                do put env { allTriggersId = id'' : allTriggersId env }
                                   return TriggerDef { tName = id''
                                                     , args  = bs
                                                     , compTrigger = ce'
                                                     , whereClause = getWhereClause wc
                                                     }

checkAllArgs :: [Id] -> [Id] -> Bind -> Writer [String] Bool
checkAllArgs argss allArgs bind =
 case bind of
      BindId id -> if (elem id argss)
                   then checkArgs argss allArgs
                   else return False
      _         -> checkArgs argss allArgs


checkArgs :: [Id] -> [Id] -> Writer [String] Bool
checkArgs _ []              = return True
checkArgs argss (a:allArgs) =
 do b <- checkArgs argss allArgs
    let s = if not(elem a argss) then [a] else []
    writer ((elem a argss && b),s)

checkRetVar :: [Bind] -> [Id] -> Bool
checkRetVar xs ids = case length xs of
                          0 -> True
                          1 -> elem (getBindIdId (head xs)) ids
                          otherwise -> False

getVarsWC :: Abs.WhereClause -> [Id]
getVarsWC Abs.WhereClauseNil      = []
getVarsWC (Abs.WhereClauseDef xs) = map (\ (Abs.WhereExp bind _) -> (getBindIdId.getBind_) bind) xs

checkVarsInitialisation :: [Id] -> [Id] -> [Id]
checkVarsInitialisation [] _      = []
checkVarsInitialisation (x:xs) wc = if (elem x wc)
                                    then checkVarsInitialisation xs wc
                                    else x:checkVarsInitialisation xs wc

getIdBind :: Bind -> Id
getIdBind (BindType _ id) = id
getIdBind (BindId id)     = id
getIdBind _                 = ""

getCompTrigger :: Abs.CompoundTrigger -> Writer String CompoundTrigger
getCompTrigger ce =
 case ce of
     Abs.NormalEvent (Abs.BindingVar bind) id binds trv ->
        case runWriter (getBindsBody (map getVarsAbs binds)) of
             (bs, s) -> do let id' = getIdAbs id
                           let trv' = getTriggerVariation trv
                           tell s
                           return (NormalEvent (BindingVar (getBind_ bind)) id' bs trv')
     Abs.ClockEvent id int -> do let id' = getIdAbs id
                                 return (ClockEvent id' int)
     Abs.OnlyId id         -> do let id' = getIdAbs id
                                 return (OnlyId id')
     Abs.OnlyIdPar id      -> do let id' = getIdAbs id
                                 return (OnlyIdPar id')

getCompTriggers :: Abs.CompoundTrigger -> Writer String CompoundTrigger
getCompTriggers ce@(Abs.NormalEvent _ _ _ _)            = getCompTrigger ce
getCompTriggers ce@(Abs.ClockEvent _ _)                 = getCompTrigger ce
getCompTriggers ce@(Abs.OnlyId _)                       = getCompTrigger ce
getCompTriggers ce@(Abs.OnlyIdPar _)                    = getCompTrigger ce
getCompTriggers (Abs.Collection (Abs.CECollection esl)) = do
                                                           let xs = map getCompTrigger esl
                                                           ce <- sequence xs
                                                           return (Collection (CECollection ce))
getBindsArgs :: [Abs.Bind] -> Writer String [Bind]
getBindsArgs []     = return []
getBindsArgs (b:bs) =
 case runWriter (getBindsArgs bs) of
      (bs', s) -> case b of
                       Abs.BindType t id -> do tell s
                                               return ((BindType (getTypeAbs t) (getIdAbs id)):bs')
                       _                 -> do tell (mAppend (printTree b) s)
                                               return bs'

getBindsBody :: [Abs.Bind] -> Writer String [Bind]
getBindsBody []     = return []
getBindsBody (b:bs) =
 case runWriter (getBindsBody bs) of
      (xs, s) -> case b of
                       Abs.BindId id -> do tell s
                                           return ((BindId (getIdAbs id)):xs)
                       Abs.BindStar  -> do tell s
                                           return (BindStar:xs)
                       _             -> do tell (mAppend (printTree b) s)
                                           return xs

getBind_ :: Abs.Bind -> Bind
getBind_ Abs.BindStar        = BindStar
getBind_ (Abs.BindType t id) = BindType (getTypeAbs t) (getIdAbs id)
getBind_ (Abs.BindId id)     = BindId (getIdAbs id)

getTriggerVariation :: Abs.TriggerVariation -> TriggerVariation
getTriggerVariation Abs.EVEntry        = EVEntry
getTriggerVariation (Abs.EVExit vars)  = EVExit (map (getBind_.getVarsAbs) vars)
getTriggerVariation (Abs.EVThrow vars) = EVThrow (map (getBind_.getVarsAbs) vars)
getTriggerVariation (Abs.EVHadle vars) = EVHadle (map (getBind_.getVarsAbs) vars)

-- Also removes white spaces added by printTree after ';'
getWhereClause :: Abs.WhereClause -> WhereClause
getWhereClause Abs.WhereClauseNil        = ""
getWhereClause (Abs.WhereClauseDef wexp) = (concat.lines.printTree) wexp


-- Properties --

getProperty :: Abs.Properties -> [Id] -> Writer String Property
getProperty Abs.PropertiesNil _                           = return PNIL
getProperty (Abs.ProperiesDef id states trans props) enms =
 let props' = getProperty props enms
     trans' = getTransitions trans
     ts     = map (trigger.arrow) trans' in
 case runWriter props' of
      (p, s)    -> let xs = [x | x <- ts, not(elem x enms)]
                   in do tell (mAppend (addComma xs) s)
                         return (Property { pName        = getIdAbs id
                                          , pStates      = getStates' states
                                          , pTransitions = trans'
                                          , pProps       = p
                                          })

mAppend :: String -> String -> String
mAppend [] []     = ""
mAppend [] (y:ys) = y:ys
mAppend (x:xs) [] = x:xs
mAppend xs ys     = xs ++ "," ++ ys

getStates' :: Abs.States -> States
getStates' (Abs.States accep bad norm start) = States { getAccepting = getAccepting' accep
                                                      , getBad = getBad' bad
                                                      , getNormal = getNormal' norm
                                                      , getStarting = getStarting' start
                                                      }

getAccepting' :: Abs.Accepting -> Accepting
getAccepting' Abs.AcceptingNil      = []
getAccepting' (Abs.AcceptingDef ss) = map getState ss

getBad' :: Abs.Bad -> Bad
getBad' Abs.BadNil      = []
getBad' (Abs.BadDef ss) = map getState ss

getNormal' :: Abs.Normal -> Normal
getNormal' Abs.NormalNil      = []
getNormal' (Abs.NormalDef ss) = map getState ss

getStarting' :: Abs.Starting -> Starting
getStarting' (Abs.StartingDef ss) = map getState ss

getState :: Abs.State -> State
getState (Abs.State (Abs.NameState id) ic Abs.CNSNil) = State (getIdAbs id) (getInitCode ic) []
getState (Abs.State (Abs.NameState id) ic (Abs.CNS cns)) = State (getIdAbs id) (getInitCode ic) (map (getIdAbs.getConstNameAbs) cns)

getInitCode :: Abs.InitialCode -> InitialCode
getInitCode Abs.InitNil      = InitNil
getInitCode (Abs.InitProg p) = InitProg (getJava p)

getTransitions :: Abs.Transitions -> Transitions
getTransitions (Abs.Transitions ts) = map getTransition' ts

getTransition' :: Abs.Transition -> Transition
getTransition' (Abs.Transition (Abs.NameState q1) (Abs.NameState q2) ar) = Transition { fromState = getIdAbs q1
                                                                                      , arrow = getArrow ar
                                                                                      , toState = getIdAbs q2
                                                                                      }
getArrow :: Abs.Arrow -> Arrow
getArrow (Abs.Arrow id Abs.Cond1)        = Arrow { trigger = getIdAbs id, cond = "", action = "" }
getArrow (Abs.Arrow id (Abs.Cond2 cond)) = case cond of
                                                Abs.CondExpDef cexp     -> Arrow { trigger = getIdAbs id, cond = printTree cexp, action = "" }
                                                Abs.CondAction cexp act -> Arrow { trigger = getIdAbs id, cond = printTree cexp, action = (trim.printTree) act }



-- Foreaches --

getForeaches :: Abs.Foreaches -> UpgradePPD Foreaches
getForeaches Abs.ForeachesNil             = return []
getForeaches (Abs.ForeachesDef args ctxt) =
    do ctxt' <- getCtxt ctxt
       let args' = map getArgs args
       env <- get
       put env { forsVars = forsVars env ++ map getArgsId args' }
       return [Foreach args' ctxt']

getArgs :: Abs.Args -> Args
getArgs (Abs.Args t id) = Args (getTypeAbs t) (getIdAbs id)


-----------------
-- CInvariants --
-----------------

genClassInvariants :: Abs.CInvariants -> CInvariants
genClassInvariants Abs.CInvempty           = []
genClassInvariants (Abs.CInvariants cinvs) = getCInvs cinvs

--TODO: Modify if ppDATE operators are added to the JML specification
getCInvs :: [Abs.CInvariant] -> [CInvariant]
getCInvs []                    = []
getCInvs (Abs.CI id jml:cinvs) = CI (getIdAbs id) (getJML jml):getCInvs cinvs


-------------------
-- Hoare Triples --
-------------------

genHTs :: Abs.HTriples -> UpgradePPD HTriples
genHTs Abs.HTempty     = return []
genHTs (Abs.HTriples cs) = sequence (map getHT cs)

getHT :: Abs.HT -> UpgradePPD HT
getHT (Abs.HT id pre' method post' (Abs.Assignable ass)) =
 do env <- get
    let cns = htsNames env
    put env { htsNames = (getIdAbs id):(htsNames env) }
    return (HT { htName       = getIdAbs id
               , methodCN     = (getMethodClassInfo method, getMethodMethodName method)
               , pre          = filter (/='\n') $ getPre pre'
               , post         = filter (/='\n') $ getPost post'
               , assignable   = joinAssignable $ map assig ass
               , optimized    = []
               , chGet        = 0
               , path2it      = ""
               })

assig :: Abs.Assig -> String
assig (Abs.AssigJML jml) = getJML jml
assig Abs.AssigE         = "\\everything"
assig Abs.AssigN         = "\\nothing"

joinAssignable [x]    = x
joinAssignable (x:xs) = x ++ "," ++ joinAssignable xs

-------------
-- Methods --
-------------

genMethods :: Abs.Methods -> Methods
genMethods m = printTree m


------------------------
-- Selector functions --
------------------------

getSymbolsAbs :: Abs.Symbols -> String
getSymbolsAbs (Abs.Symbols s) = s

getIdAbs :: Abs.Id -> String
getIdAbs (Abs.Id s) = s

getTypeAbs :: Abs.Type -> String
getTypeAbs (Abs.Type (Abs.Id id)) = id

getJFAbs :: Abs.JavaFiles -> Abs.Id
getJFAbs (Abs.JavaFiles id) = id

getImportAbs :: Abs.Import -> [Abs.JavaFiles]
getImportAbs (Abs.Import jfs) = jfs

getVarInitAbs :: Abs.VariableInitializer -> VariableInitializer
getVarInitAbs Abs.VarInitNil     = VarInitNil
getVarInitAbs (Abs.VarInit vexp) = VarInit (printTree vexp)

getVarsAbs :: Abs.Vars -> Abs.Bind
getVarsAbs (Abs.Vars bind) = bind

getConstNameAbs :: Abs.HTName -> Abs.Id
getConstNameAbs (Abs.CN id) = id

getJML :: Abs.JML -> JML
getJML jml = printTree jml

getJava :: Abs.Java -> Java
getJava java = printTree java

getMethodClassInfo :: Abs.Method -> ClassInfo
getMethodClassInfo (Abs.Method ci _) = getIdAbs ci

getMethodMethodName :: Abs.Method -> MethodName
getMethodMethodName (Abs.Method _ mn) = getIdAbs mn

getAssig :: Abs.Assig -> Abs.JML
getAssig (Abs.AssigJML jml) = jml

getPre :: Abs.Pre -> Pre
getPre (Abs.Pre pre) = getJML pre

getPost :: Abs.Post -> Post
getPost (Abs.Post post) = getJML post

-------------------------
-- Auxiliary functions --
-------------------------

-- Imports are considered to be separated by '.'
joinImport :: [String] -> String
joinImport []     = ""
joinImport [ys]   = ys
joinImport (xs:ys:iss) = xs ++ "." ++ joinImport (ys:iss)


lookForAllEntryTriggerArgs :: Env -> ClassInfo -> MethodName -> (String, String)
lookForAllEntryTriggerArgs env cinf mn =
 case Map.lookup cinf (entryTriggersInfo env) of
      Nothing -> case Map.lookup "*" (entryTriggersInfo env) of
                      Nothing -> error $ "Error: Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                      Just m' -> case Map.lookup mn m' of
                                      Nothing -> error $ "Error: Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                      Just _  -> error $ "Error: Cannot associated a class variable to the entry trigger for method " ++ mn ++ ". It is associated to '*' on its definition" ++ ".\n"                           
      Just m  -> case Map.lookup mn m of
                      Nothing -> case Map.lookup "*" (entryTriggersInfo env) of
                                      Nothing -> error $ "Error: Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                      Just m' -> case Map.lookup mn m' of
                                                 Nothing ->  error $ "Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                                 Just _  -> error $ "Error: Cannot associated a class variable to the entry trigger for method " ++ mn ++ ". It is associated to '*' on its definition" ++ ".\n"
                      Just (_, varClass', argsPre) ->
                           let classPre     = words $ varClass'
                               varClass     = last classPre
                               argsPrewt    = map getArgsId argsPre
                               argsPrewt'   = addComma argsPrewt
                               argsPrewt''  = if (elem varClass argsPrewt)
                                              then argsPrewt'
                                              else varClass ++ "," ++ argsPrewt'
                               flatArgsPre  = flattenArgs argsPre
                               argsPre'     = if (elem varClass argsPrewt)
                                              then flattenArgs argsPre
                                              else if (null flatArgsPre)
                                                   then trim varClass'
                                                   else (trim varClass')  ++ "," ++ flattenArgs argsPre
                           in (argsPre', argsPrewt'')

lookForAllExitTriggerArgs :: Env -> ClassInfo -> MethodName -> (String, String)
lookForAllExitTriggerArgs env cinf mn =
 case Map.lookup cinf (exitTriggersInfo env) of
      Nothing -> case Map.lookup "*" (entryTriggersInfo env) of
                      Nothing -> error $ "Error: Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                      Just m' -> case Map.lookup mn m' of
                                      Nothing -> error $ "Error: Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                      Just _  -> error $ "Error: Cannot associated a class variable to the exit trigger for method " ++ mn ++ ". It is associated to '*' on its definition" ++ ".\n"
      Just m  -> case Map.lookup mn m of
                      Nothing -> case Map.lookup "*" (entryTriggersInfo env) of
                                      Nothing -> error $ "Error: Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                      Just m' -> case Map.lookup mn m' of
                                                 Nothing ->  error $ "Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
                                                 Just _  -> error $ "Error: Cannot associated a class variable to the exit trigger for method " ++ mn ++ ". It is associated to '*' on its definition" ++ ".\n"
                      Just (_, varClass', argsPost) ->
                           let classPost    = words $ varClass'
                               varClass     = last classPost
                               argsPostwt   = map getArgsId argsPost
                               argsPostwt'  = addComma argsPostwt
                               argsPostwt'' = if (elem varClass argsPostwt)
                                              then argsPostwt'
                                              else varClass ++ "," ++ argsPostwt'
                               flatArgsPost = flattenArgs argsPost
                               argsPost'    = if (elem varClass argsPostwt)
                                              then flatArgsPost
                                              else if (null flatArgsPost)
                                                   then trim varClass'
                                                   else (trim varClass')  ++ "," ++ flattenArgs argsPost
                           in (argsPost', argsPostwt'')

flattenArgs :: [Args] -> String
flattenArgs []               = ""
flattenArgs [(Args t id)]    = t ++ " " ++ id
flattenArgs ((Args t id):xs) = t ++ " " ++ id ++ "," ++ flattenArgs xs

-- Get the variable name of a bind
-- It is used to get the name of the class variable associated to an entry/exit Trigger
getTriggerClass :: Bind -> String
getTriggerClass bn = case bn of
                        BindType t id' -> t ++ " " ++ id'
                        BindId id'     -> id'
                        BindStar       -> "*"

--------------------------------------------------------------------
-- Environment with variables, triggers and foreaches information --
--------------------------------------------------------------------

type MapTrigger = Map.Map MethodName (Id, String, [Args]) --(trigger_name,type class_variable,trigger_arguments)

--Triggers associated to methods in Hoare triples should include: type class_variable
data Env = Env
 { forsVars            :: [Id]
 , entryTriggersInfo     :: Map.Map ClassInfo MapTrigger
 , exitTriggersInfo      :: Map.Map ClassInfo MapTrigger
 , allTriggersId         :: [Id]
 , htsNames      :: [HTName]
 , varsInFiles         :: [(String, ClassInfo, [(Type, Id)])]
 , methodsInFiles      :: [(String, ClassInfo, [(Type,Id,[String])])] --[(path_to_class,class_name,[(returned_type,method_name,arguments)])]
 , oldExpTypes         :: OldExprM
 }
  deriving (Show)

type UpgradePPD a = CM.StateT Env Err a

emptyEnv :: Env
emptyEnv = Env { forsVars            = []
               , entryTriggersInfo   = Map.empty
               , exitTriggersInfo    = Map.empty
               , allTriggersId       = []
               , htsNames      = []
               , varsInFiles         = []
               , methodsInFiles      = []
               , oldExpTypes         = Map.empty
               }

updateEntryTriggersInfo :: Env -> (Id, String, [Args]) -> [Bind] -> [Char] -> Bind -> Env
updateEntryTriggersInfo env einfo args mn BindStar        = 
 let t = "*" in
 case Map.lookup t (entryTriggersInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { entryTriggersInfo = Map.insert t mapeinfo' (entryTriggersInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { entryTriggersInfo = Map.insert t mapeinfo' (entryTriggersInfo env) }
updateEntryTriggersInfo env einfo args mn (BindType t id) = 
 case Map.lookup t (entryTriggersInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { entryTriggersInfo = Map.insert t mapeinfo' (entryTriggersInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { entryTriggersInfo = Map.insert t mapeinfo' (entryTriggersInfo env) }
updateEntryTriggersInfo env einfo args mn (BindId id)     = 
 let ts = [getBindTypeType arg | arg <- args, getBindTypeId arg == id ]
 in if (length ts /= 1)
    then error $ "The entry trigger associated to method " ++ mn ++ " does not include a class variable declaration.\n"
    else case Map.lookup (head ts) (entryTriggersInfo env) of
              Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                         in env { entryTriggersInfo = Map.insert (head ts) mapeinfo' (entryTriggersInfo env) }
              Just mapeinfo -> 
                   let mapeinfo' = Map.insert mn einfo mapeinfo
                   in env { entryTriggersInfo = Map.insert (head ts) mapeinfo' (entryTriggersInfo env) }


updateExitTriggersInfo :: Env -> (Id, String, [Args]) -> [Bind] -> [Char] -> Bind -> Env
updateExitTriggersInfo env einfo args mn BindStar        = 
 let t = "*" in
 case Map.lookup t (exitTriggersInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { exitTriggersInfo = Map.insert t mapeinfo' (exitTriggersInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { exitTriggersInfo = Map.insert t mapeinfo' (exitTriggersInfo env) }
updateExitTriggersInfo env einfo args mn (BindType t id) = 
 case Map.lookup t (exitTriggersInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { exitTriggersInfo = Map.insert t mapeinfo' (exitTriggersInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { exitTriggersInfo = Map.insert t mapeinfo' (exitTriggersInfo env) }
updateExitTriggersInfo env einfo args mn (BindId id)     = 
 let ts = [getBindTypeType arg | arg <- args, getBindTypeId arg == id ]
 in if (length ts /= 1)
    then error $ "The exit trigger associated to method " ++ mn ++ " does not include a class variable declaration.\n"
    else case Map.lookup (head ts) (exitTriggersInfo env) of
              Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                         in env { exitTriggersInfo = Map.insert (head ts) mapeinfo' (exitTriggersInfo env) }
              Just mapeinfo -> 
                   let mapeinfo' = Map.insert mn einfo mapeinfo
                   in env { exitTriggersInfo = Map.insert (head ts) mapeinfo' (exitTriggersInfo env) }

 
----------------------------
-- Monad State operations --
----------------------------

get = CM.get
put = CM.put
runStateT = CM.runStateT

getValue :: UpgradePPD a -> a
getValue uppd = fst . (\(Ok x) -> x) $ runStateT uppd emptyEnv

getEnvVal :: UpgradePPD a -> Env
getEnvVal uppd = snd . (\(Ok x) -> x) $ runStateT uppd emptyEnv

getEntryTriggersEnv :: UpgradePPD a -> Map.Map ClassInfo MapTrigger
getEntryTriggersEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> Map.empty
                                Ok fs -> entryTriggersInfo fs

getExitTriggersEnv :: UpgradePPD a -> Map.Map ClassInfo MapTrigger
getExitTriggersEnv ppd = let env = CM.execStateT ppd emptyEnv
                       in case env of
                               Bad _ -> Map.empty
                               Ok fs -> exitTriggersInfo fs

getForeachVarsEnv :: UpgradePPD a -> [Id]
getForeachVarsEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> []
                                Ok fs -> forsVars fs

getHTNamesEnv :: UpgradePPD a -> [HTName]
getHTNamesEnv ppd = let env = CM.execStateT ppd emptyEnv
                          in case env of
                                Bad _ -> []
                                Ok fs -> htsNames fs

getOldExpTypesEnv :: UpgradePPD a -> OldExprM
getOldExpTypesEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> Map.empty
                                Ok fs -> oldExpTypes fs

