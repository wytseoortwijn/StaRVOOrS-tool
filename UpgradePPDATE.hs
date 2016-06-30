module UpgradePPDATE where

import Types
import CommonFunctions
import Control.Monad
import Data.Maybe
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
    case runStateT (genContracts consts) emptyEnv of
              Bad s             -> fail s
              Ok (consts', env) -> case runStateT (genGlobal global) env of
                                         Bad s              -> fail s
                                         Ok (global', env') -> do put env'
                                                                  return (PPDATE imports' global' cinvs' consts' methods')


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
getCtxt (Abs.Ctxt vars es prop foreaches) =
 do env <- get
    let cns = contractsNames env
    es' <- getEvents es
    fors <- getForeaches foreaches
    let vars' = getVars vars
    let prop' = getProperty prop (allEventsId env)
    case prop' of
         Just PNIL                                -> return (Ctxt vars' es' PNIL fors)
         Just (Property pname states trans props) -> let pn = pName (fromJust prop')
                                                         accep  = checkAllContractsExist (getAccepting states) cns pn
                                                         bad    = checkAllContractsExist (getBad states) cns pn
                                                         normal = checkAllContractsExist (getNormal states) cns pn
                                                         start  = checkAllContractsExist (getStarting states) cns pn 
                                                         errs   = accep ++ bad ++ normal ++ start
                                                     in if (null errs)
                                                        then return (Ctxt vars' es' (fromJust prop') fors)
                                                        else fail $ concat errs


checkAllContractsExist :: [State] -> [ContractName] -> PropertyName -> [String]
checkAllContractsExist [] _ _        = []
checkAllContractsExist (s:ss) cns pn = let ns   = getNS s
                                           cns' = getCNList s
                                           aux  = [x | x <- cns' , not (elem x cns)]
                                       in if (null aux)
                                          then checkAllContractsExist ss cns pn
                                          else ("Error: On property " ++ pn 
                                                ++ ", in state " ++ ns ++ ", the contract(s) " 
                                                ++ commaAdd aux
                                                ++ " do(es) not exist.\n") : checkAllContractsExist ss cns pn
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

-- Triggers --

getEvents :: Abs.Events -> UpgradePPD Events
getEvents Abs.EventsNil      = return []
getEvents (Abs.EventsDef es) = 
 do env <- get
    let xs = map getEvent' es
    let (ls, rs) = partitionErr (map (\e -> CM.evalStateT e env) xs)
    if (null ls)
    then sequence xs
    else fail $ foldr joinBad "" ls
    
joinBad :: Err a -> String -> String
joinBad (Bad s1) s2 = s1 ++ s2
joinBad _ _         = error "ppDATE refinement failure: joinBad \n"
 
getEvent' :: Abs.Event -> UpgradePPD EventDef
getEvent' (Abs.Event id binds ce wc)      = 
 do env  <- get
    let id'' = getIdAbs id
    if (elem id'' (allEventsId env))
    then fail $ "Error: Multiple definitions for event " ++ id'' ++ ".\n"
    else case runStateT (getBindsArgs binds) env of
            Bad s         -> fail (s ++ "Event " ++ id'' ++ ".\n")
            Ok (bs, env') -> 
               case runStateT (getCompEvents ce) env' of
                    Bad s             -> fail (s ++ "Event " ++ id'' ++ ".\n")
                    Ok (ce', env'') -> 
                       let argss = map getBindTypeId bs in
                       case ce' of 
                            NormalEvent (BindingVar bind) mn args' eventv
                                 -> let allArgs = map getBindIdId (filter (\ c -> c /= BindStar) args') in
                                    case eventv of
                                         EVEntry  -> let id  = getIdBind bind
                                                         wc' = getWhereClause wc 
                                                         wcs = [x | x <- argss, not(elem x allArgs)]
                                                         vs  = filter (\ x -> (x /= "ret") && (x /= id)) $ checkVarsInitialisation wcs (getVarsWC wc)
                                                      in if ((not.null) vs) then fail ("Error: Missing Initialization of variable(s): " ++ show vs ++  ".\n" ++ "Event " ++ id'' ++ ".\n")
                                                         else if (checkAllArgs argss allArgs bind)
                                                              then do put env'' { entryEventsInfo = Map.insert mn (id'', getEventClass bind, (map bindToArgs bs)) (entryEventsInfo env'') 
                                                                                , allEventsId = id'' : allEventsId env'' }
                                                                      return EventDef { eName = id''
                                                                                      , args  = bs
                                                                                      , compEvent = ce'
                                                                                      , whereClause = getWhereClause wc
                                                                                      }
                                                              else fail ("Error: Wrong value is used as an argument in an event declaration.\n" ++ "Event " ++ id'' ++ ".\n")
                                         EVExit rs -> let id  = getIdBind bind
                                                          wc' = getWhereClause wc 
                                                          wcs = [x | x <- argss, not(elem x allArgs)]
                                                          vs  = filter (\ x -> (x /= "ret") && (x /= id)) $ checkVarsInitialisation wcs (getVarsWC wc)
                                                      in if ((not.null) vs) then fail ("Error: Missing Initialization of variable(s): " ++ show vs ++  ".\n" ++ "Event " ++ id'' ++ ".\n")
                                                         else 
                                                          if ((checkAllArgs argss allArgs bind) && (checkRetVar rs argss))
                                                          then do put env'' { exitEventsInfo = Map.insert mn (id'', getEventClass bind, (map bindToArgs bs)) (exitEventsInfo env'')  
                                                                            , allEventsId = id'' : allEventsId env'' }
                                                                  return EventDef { eName = id''
                                                                                  , args  = bs
                                                                                  , compEvent = ce'
                                                                                  , whereClause = wc'
                                                                                  }
                                                          else fail (show argss ++ "\n" ++ "Error: Wrong value is used as an argument in an event declaration.\n" ++ "Event " ++ id'' ++ ".\n")
                                         _        -> return EventDef { eName = id''
                                                                     , args  = bs
                                                                     , compEvent = ce'
                                                                     , whereClause = getWhereClause wc
                                                                     }
                            _  -> do put env'' { allEventsId = id'' : allEventsId env'' }
                                     return EventDef { eName = id''
                                                     , args  = bs
                                                     , compEvent = ce'
                                                     , whereClause = getWhereClause wc
                                                     }

checkAllArgs :: [Id] -> [Id] -> Bind -> Bool
checkAllArgs argss allArgs bind = 
 case bind of
      BindId id -> if (elem id argss)
                   then checkArgs argss allArgs
                   else False
      _         -> checkArgs argss allArgs


checkArgs :: [Id] -> [Id] -> Bool
checkArgs _ []              = True
checkArgs argss (a:allArgs) = elem a argss && checkArgs argss allArgs

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

getCompEvent :: Abs.CompoundEvent -> UpgradePPD CompoundEvent
getCompEvent ce = 
 case ce of
     Abs.NormalEvent (Abs.BindingVar bind) id binds eventv -> 
        do 
           bs <- getBindsBody (map getVarsAbs binds)
           let id' = getIdAbs id
           let eventv' = getEventVariation eventv
           return (NormalEvent (BindingVar (getBind_ bind)) id' bs eventv')
     Abs.ClockEvent id int -> do let id' = getIdAbs id
                                 return (ClockEvent id' int)
     Abs.OnlyId id         -> do let id' = getIdAbs id
                                 return (OnlyId id')
     Abs.OnlyIdPar id      -> do let id' = getIdAbs id
                                 return (OnlyIdPar id')
      
getCompEvents :: Abs.CompoundEvent -> UpgradePPD CompoundEvent
getCompEvents ce@(Abs.NormalEvent _ _ _ _)            = getCompEvent ce
getCompEvents ce@(Abs.ClockEvent _ _)                 = getCompEvent ce
getCompEvents ce@(Abs.OnlyId _)                       = getCompEvent ce
getCompEvents ce@(Abs.OnlyIdPar _)                    = getCompEvent ce
getCompEvents (Abs.Collection (Abs.CECollection esl)) = do 
                                                           let xs = map getCompEvent esl
                                                           ce <- sequence xs
                                                           return (Collection (CECollection ce))
getBindsArgs :: [Abs.Bind] -> UpgradePPD [Bind]
getBindsArgs []     = return []
getBindsArgs (b:bs) = case b of                      
                          Abs.BindType t id -> do xs <- getBindsArgs bs
                                                  return ((BindType (getTypeAbs t) (getIdAbs id)):xs)
                          _                 -> fail "Error: Wrong value is used as an argument in an event declaration.\n"

getBindsBody :: [Abs.Bind] -> UpgradePPD [Bind]
getBindsBody []     = return []
getBindsBody (b:bs) = case b of                      
                          Abs.BindId id -> do xs <- getBindsBody bs
                                              return ((BindId (getIdAbs id)):xs)
                          Abs.BindStar  -> do xs <- getBindsBody bs
                                              return (BindStar:xs)
                          _             -> fail "Error: Wrong value is used as an argument in an event declaration.\n"

getBind_ :: Abs.Bind -> Bind
getBind_ Abs.BindStar        = BindStar
getBind_ (Abs.BindType t id) = BindType (getTypeAbs t) (getIdAbs id)
getBind_ (Abs.BindId id)     = BindId (getIdAbs id)

getEventVariation :: Abs.EventVariation -> EventVariation
getEventVariation Abs.EVEntry        = EVEntry
getEventVariation (Abs.EVExit vars)  = EVExit (map (getBind_.getVarsAbs) vars)
getEventVariation (Abs.EVThrow vars) = EVThrow (map (getBind_.getVarsAbs) vars)
getEventVariation (Abs.EVHadle vars) = EVHadle (map (getBind_.getVarsAbs) vars)

-- Also removes white spaces added by printTree after ';'
getWhereClause :: Abs.WhereClause -> WhereClause
getWhereClause Abs.WhereClauseNil        = ""
getWhereClause (Abs.WhereClauseDef wexp) = (concat.lines.printTree) wexp


-- Properties --

getProperty :: Abs.Properties -> [Id] -> Maybe Property
getProperty Abs.PropertiesNil _                           = Just PNIL
getProperty (Abs.ProperiesDef id states trans props) enms = 
 let props' = getProperty props enms
     trans' = getTransitions trans in
 if (props' == Nothing) then Nothing
 else 
    Just (Property { pName        = getIdAbs id
                   , pStates      = getStates' states
                   , pTransitions = trans'
                   , pProps       = fromJust props'
                   })

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
getArrow (Abs.Arrow id Abs.Cond1)        = Arrow { event = getIdAbs id, cond = "", action = "" }
getArrow (Abs.Arrow id (Abs.Cond2 cond)) = case cond of
                                                Abs.CondExpDef cexp     -> Arrow { event = getIdAbs id, cond = printTree cexp, action = "" }
                                                Abs.CondAction cexp act -> Arrow { event = getIdAbs id, cond = printTree cexp, action = (trim.printTree) act }



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

--TODO: Modify when ppDATE operators are added to the JML specification
getCInvs :: [Abs.CInvariant] -> [CInvariant]
getCInvs []                    = []
getCInvs (Abs.CI id jml:cinvs) = CI (getIdAbs id) (getJML jml):getCInvs cinvs


-------------------
-- Hoare Triples --
-------------------

genContracts :: Abs.Contracts -> UpgradePPD Contracts
genContracts Abs.Constempty     = return []
genContracts (Abs.Contracts cs) = sequence (map getContract cs)

getContract :: Abs.Contract -> UpgradePPD Contract
getContract (Abs.Contract id pre' method post' (Abs.Assignable ass)) = 
 do env <- get
    let cns = contractsNames env
    if (elem (getIdAbs id) cns)
    then fail $ "Error: Multiple definitions for contract " ++ getIdAbs id ++ ".\n"
    else do put env { contractsNames = (getIdAbs id):(contractsNames env) }
            return (Contract { contractName = getIdAbs id
                             , methodCN     = (getMethodClassInfo method, getMethodMethodName method)
                             , pre          = filter (/='\n') $ getPre pre'
                             , post         = filter (/='\n') $ getPost post'
                             , assignable   = joinAssignable ass
                             , optimized    = []
                             , chGet        = 0
                             })

--joinAssignable :: 
joinAssignable [x]    = (getJML.getAssig) x
joinAssignable (x:xs) = (getJML.getAssig) x ++ ", " ++ joinAssignable xs

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

getConstNameAbs :: Abs.ContractName -> Abs.Id
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
getAssig (Abs.Assig jml) = jml

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


lookForAllEntryEventArgs :: Env -> MethodName -> (String, String)
lookForAllEntryEventArgs env mn = 
 case Map.lookup mn (entryEventsInfo env) of
      Nothing -> error $ "Problem when looking for arguments of an entry event associated to method " ++ mn ++ ".\n" 
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

lookForAllExitEventArgs :: Env -> MethodName -> (String, String)
lookForAllExitEventArgs env mn =
 case Map.lookup mn (exitEventsInfo env) of
      Nothing -> error $ "Problem when looking for arguments of an exit event associated to method " ++ mn ++ ".\n" 
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
-- It is used to get the name of the class variable associated to an entry/exit event
getEventClass :: Bind -> String
getEventClass bn = case bn of 
                        BindType t id' -> t ++ " " ++ id'
                        BindId id'     -> id'
                        BindStar       -> error "Error: Missing class name in an event definition.\n"
 
------------------------------------------------------------------
-- Environment with variables, events and foreaches information --
------------------------------------------------------------------

data Env = Env
 { forsVars            :: [Id]
 , entryEventsInfo     :: Map.Map MethodName (Id, String, [Args])
 , exitEventsInfo      :: Map.Map MethodName (Id, String, [Args])
 , allEventsId         :: [Id]
 , contractsNames      :: [ContractName]
 , varsInFiles         :: [(String, ClassInfo, [(Type, Id)])]
 }
  deriving (Show)

type UpgradePPD a = CM.StateT Env Err a

emptyEnv :: Env
emptyEnv = Env { forsVars            = []
               , entryEventsInfo     = Map.empty
               , exitEventsInfo      = Map.empty
               , allEventsId         = []
               , contractsNames      = []
               , varsInFiles         = []
               }

----------------------------
-- Monad State operations --
----------------------------

get = CM.get
put = CM.put
runStateT = CM.runStateT

getValue :: UpgradePPD a -> a
getValue uppd = fst . (\(Ok x) -> x) $ runStateT uppd emptyEnv

getEntryEventsEnv :: UpgradePPD a -> Map.Map MethodName (Id, String, [Args])
getEntryEventsEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> Map.empty
                                Ok fs -> entryEventsInfo fs

getExitEventsEnv :: UpgradePPD a -> Map.Map MethodName (Id, String, [Args])
getExitEventsEnv ppd = let env = CM.execStateT ppd emptyEnv
                       in case env of
                               Bad _ -> Map.empty
                               Ok fs -> exitEventsInfo fs

getForeachVarsEnv :: UpgradePPD a -> [Id]
getForeachVarsEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> []
                                Ok fs -> forsVars fs

getContractNamesEnv :: UpgradePPD a -> [ContractName]
getContractNamesEnv ppd = let env = CM.execStateT ppd emptyEnv
                          in case env of
                                Bad _ -> []
                                Ok fs -> contractsNames fs

