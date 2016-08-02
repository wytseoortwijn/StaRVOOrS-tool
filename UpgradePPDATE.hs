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
    case runStateT (genContracts consts) emptyEnv of
         Bad s             -> fail s
         Ok (consts', env) -> let cns = contractsNames env
                                  dcs = getDuplicate cns
                              in case runStateT (genGlobal global) env of
                                      Bad s              -> if (not.null) dcs
                                                            then fail $ s ++ duplicateHT dcs
                                                            else fail s
                                      Ok (global', env') -> if (not.null) dcs
                                                            then fail $ duplicateHT dcs
                                                            else do put env'
                                                                    return (PPDATE imports' global' cinvs' consts' methods')


duplicateHT :: [ContractName] -> String
duplicateHT []     = ""
duplicateHT (c:cs) = "Error: Multiple definitions for Hoare triple " ++ c ++ ".\n" ++ duplicateHT cs

getDuplicate :: [ContractName] -> [ContractName]
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
getCtxt (Abs.Ctxt vars es prop foreaches) =
 do env <- get
    let cns = contractsNames env
    es' <- getEvents es
    fors <- getForeaches foreaches
    let vars' = getVars vars
    let prop' = getProperty prop (map eName es')
    case runWriter prop' of
         (PNIL,_)                              -> return (Ctxt vars' es' PNIL fors)
         (Property pname states trans props,s) -> let accep  = checkAllContractsExist (getAccepting states) cns pname
                                                      bad    = checkAllContractsExist (getBad states) cns pname
                                                      normal = checkAllContractsExist (getNormal states) cns pname
                                                      start  = checkAllContractsExist (getStarting states) cns pname
                                                      errs   = concat $ accep ++ bad ++ normal ++ start
                                                      s'     = if (not.null) s
                                                               then "Error: Triggers [" ++ s ++ "] are used in the transitions, but are not defined in section TRIGGERS.\n" ++ errs
                                                               else errs
                                                  in if (null s')
                                                     then return (Ctxt vars' es' (Property pname states trans props) fors)
                                                     else fail s'


checkAllContractsExist :: [State] -> [ContractName] -> PropertyName -> [String]
checkAllContractsExist [] _ _        = []
checkAllContractsExist (s:ss) cns pn = let ns   = getNS s
                                           cns' = getCNList s
                                           aux  = [x | x <- cns' , not (elem x cns)]
                                       in if (null aux)
                                          then checkAllContractsExist ss cns pn
                                          else ("Error: On property " ++ pn
                                                ++ ", in state " ++ ns ++ ", the Hoare triples(s) "
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
    let err  = if (elem id'' (allEventsId env)) then ("Error: Multiple definitions for trigger " ++ id'' ++ ".\n") else ""
    do case runWriter (getBindsArgs binds) of
         (bs, s) ->
           let err0 = if (not.null) s then (err ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ s ++ "].\n") else err
           in case runWriter (getCompEvents ce) of
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
                                                                 do let env' = updateEntryEventsInfo env (id'', getEventClass bind, (map bindToArgs bs)) bs mn bind
                                                                    put env' { allEventsId = id'' : allEventsId env }
                                                                    return EventDef { eName = id''
                                                                                    , args  = bs
                                                                                    , compEvent = ce'
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
                                                                 do let env' = updateExitEventsInfo env (id'', getEventClass bind, (map bindToArgs bs)) bs mn bind
                                                                    put env' { allEventsId = id'' : allEventsId env }
                                                                    return EventDef { eName = id''
                                                                                    , args  = bs
                                                                                    , compEvent = ce'
                                                                                    , whereClause = wc'
                                                                                    }
                                                            else fail (err1 ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ addComma zs ++ "] in the method component.\n")
                                       _        -> return EventDef { eName = id''
                                                                   , args  = bs
                                                                   , compEvent = ce'
                                                                   , whereClause = getWhereClause wc
                                                                   }
                          _  -> if (not.null) err1 then fail err1 else
                                do put env { allEventsId = id'' : allEventsId env }
                                   return EventDef { eName = id''
                                                   , args  = bs
                                                   , compEvent = ce'
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

getCompEvent :: Abs.CompoundEvent -> Writer String CompoundEvent
getCompEvent ce =
 case ce of
     Abs.NormalEvent (Abs.BindingVar bind) id binds eventv ->
        case runWriter (getBindsBody (map getVarsAbs binds)) of
             (bs, s) -> do let id' = getIdAbs id
                           let eventv' = getEventVariation eventv
                           tell s
                           return (NormalEvent (BindingVar (getBind_ bind)) id' bs eventv')
     Abs.ClockEvent id int -> do let id' = getIdAbs id
                                 return (ClockEvent id' int)
     Abs.OnlyId id         -> do let id' = getIdAbs id
                                 return (OnlyId id')
     Abs.OnlyIdPar id      -> do let id' = getIdAbs id
                                 return (OnlyIdPar id')

getCompEvents :: Abs.CompoundEvent -> Writer String CompoundEvent
getCompEvents ce@(Abs.NormalEvent _ _ _ _)            = getCompEvent ce
getCompEvents ce@(Abs.ClockEvent _ _)                 = getCompEvent ce
getCompEvents ce@(Abs.OnlyId _)                       = getCompEvent ce
getCompEvents ce@(Abs.OnlyIdPar _)                    = getCompEvent ce
getCompEvents (Abs.Collection (Abs.CECollection esl)) = do
                                                           let xs = map getCompEvent esl
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

getProperty :: Abs.Properties -> [Id] -> Writer String Property
getProperty Abs.PropertiesNil _                           = return PNIL
getProperty (Abs.ProperiesDef id states trans props) enms =
 let props' = getProperty props enms
     trans' = getTransitions trans
     ts     = map (event.arrow) trans' in
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
    put env { contractsNames = (getIdAbs id):(contractsNames env) }
    return (Contract { contractName = getIdAbs id
                     , methodCN     = (getMethodClassInfo method, getMethodMethodName method)
                     , pre          = filter (/='\n') $ getPre pre'
                     , post         = filter (/='\n') $ getPost post'
                     , assignable   = joinAssignable $ map assig ass
                     , optimized    = []
                     , chGet        = 0
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


lookForAllEntryEventArgs :: Env -> ClassInfo -> MethodName -> (String, String)
lookForAllEntryEventArgs env cinf mn =
 case Map.lookup cinf (entryEventsInfo env) of
      Nothing -> error $ "Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
      Just m  -> case Map.lookup mn m of
                      Nothing -> error $ "Problem when looking for arguments of an entry trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
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

lookForAllExitEventArgs :: Env -> ClassInfo -> MethodName -> (String, String)
lookForAllExitEventArgs env cinf mn =
 case Map.lookup cinf (exitEventsInfo env) of
      Nothing -> error $ "Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
      Just m  -> case Map.lookup mn m of
                      Nothing -> error $ "Problem when looking for arguments of an exit trigger associated to method " ++ mn ++ " in class " ++ cinf ++ ".\n"
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
                        BindStar       -> error "Error: Missing class name in a trigger definition.\n"

--------------------------------------------------------------------
-- Environment with variables, triggers and foreaches information --
--------------------------------------------------------------------

type MapTrigger = Map.Map MethodName (Id, String, [Args]) --(trigger_name,type class_variable,trigger_arguments)

--Triggers associated to methods in Hoare triples should include: type class_variable
data Env = Env
 { forsVars            :: [Id]
 , entryEventsInfo     :: Map.Map ClassInfo MapTrigger
 , exitEventsInfo      :: Map.Map ClassInfo MapTrigger
 , allEventsId         :: [Id]
 , contractsNames      :: [ContractName]
 , varsInFiles         :: [(String, ClassInfo, [(Type, Id)])]
 , methodsInFiles      :: [(String, ClassInfo, [(Id,String,[String])])] --[(path_to_class,class_name,[(method_name,returned_type,arguments)])]
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
               , methodsInFiles      = []
               }

updateEntryEventsInfo :: Env -> (Id, String, [Args]) -> [Bind] -> [Char] -> Bind -> Env
updateEntryEventsInfo env einfo args mn BindStar        = env
updateEntryEventsInfo env einfo args mn (BindType t id) = 
 case Map.lookup t (entryEventsInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { entryEventsInfo = Map.insert t mapeinfo' (entryEventsInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { entryEventsInfo = Map.insert t mapeinfo' (entryEventsInfo env) }
updateEntryEventsInfo env einfo args mn (BindId id)     = 
 let ts = [getBindTypeType arg | arg <- args, getBindTypeId arg == id ]
 in if (length ts /= 1)
    then error $ "Problem when looking for arguments of a trigger associated to method " ++ mn ++ " .\n"
    else case Map.lookup (head ts) (entryEventsInfo env) of
              Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                         in env { entryEventsInfo = Map.insert (head ts) mapeinfo' (entryEventsInfo env) }
              Just mapeinfo -> 
                   let mapeinfo' = Map.insert mn einfo mapeinfo
                   in env { entryEventsInfo = Map.insert (head ts) mapeinfo' (entryEventsInfo env) }


updateExitEventsInfo :: Env -> (Id, String, [Args]) -> [Bind] -> [Char] -> Bind -> Env
updateExitEventsInfo env einfo args mn BindStar        = env
updateExitEventsInfo env einfo args mn (BindType t id) = 
 case Map.lookup t (exitEventsInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                 in env { exitEventsInfo = Map.insert t mapeinfo' (exitEventsInfo env) }
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn einfo mapeinfo
           in env { exitEventsInfo = Map.insert t mapeinfo' (exitEventsInfo env) }
updateExitEventsInfo env einfo args mn (BindId id)     = 
 let ts = [getBindTypeType arg | arg <- args, getBindTypeId arg == id ]
 in if (length ts /= 1)
    then error $ "Problem when looking for arguments of a trigger associated to method " ++ mn ++ " .\n"
    else case Map.lookup (head ts) (exitEventsInfo env) of
              Nothing -> let mapeinfo' =  Map.insert mn einfo Map.empty
                         in env { exitEventsInfo = Map.insert (head ts) mapeinfo' (exitEventsInfo env) }
              Just mapeinfo -> 
                   let mapeinfo' = Map.insert mn einfo mapeinfo
                   in env { exitEventsInfo = Map.insert (head ts) mapeinfo' (exitEventsInfo env) }

 
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

getEntryEventsEnv :: UpgradePPD a -> Map.Map ClassInfo MapTrigger
getEntryEventsEnv ppd = let env = CM.execStateT ppd emptyEnv
                        in case env of
                                Bad _ -> Map.empty
                                Ok fs -> entryEventsInfo fs

getExitEventsEnv :: UpgradePPD a -> Map.Map ClassInfo MapTrigger
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
