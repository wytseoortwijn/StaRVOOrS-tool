module UpgradePPDATE where

import Types
import CommonFunctions
import Control.Monad
import Control.Monad.Writer
import qualified Control.Monad.State as CM
import qualified Data.Map.Strict as Map
import qualified AbsPpdate as Abs
import qualified AbsActions as Act
import ErrM
import PrintPpdate
import qualified PrintActions as PrintAct
import qualified PrintJml as PrintJML
import Parser
import qualified ParserAct as ParAct
import qualified ParserJML as ParJML
import Data.List
import Data.Either
import Data.Maybe


upgradePPD :: Abs.AbsPPDATE -> UpgradePPD PPDATE
upgradePPD (Abs.AbsPPDATE imports global temps cinvs consts methods) =
 do let imports' = genImports imports
    let methods' = genMethods methods
    case runStateT (genHTs consts imports') emptyEnv of
         Bad s             -> fail s
         Ok (consts', env) ->
            let cns = htsNames env
                dcs = getDuplicate cns
            in case runStateT (genTemplates temps) env of
                    Bad s             -> fail s
                    Ok (temps',env') ->
                       case runStateT (genGlobal global) env' of
                            Bad s               -> fail $ s ++ duplicateHT dcs
                            Ok (global', env'') ->  
                               let trs     = map tiTN $ allTriggers env''
                                   noneTrs = [x | x <- triggersInTemps env'', not $ elem x trs] 
                               in  if (not.null.trim.concat) noneTrs
                                   then fail $ "Error: The trigger(s) [" ++ addComma noneTrs ++ "] are used in the definition of a template, but do(es) not exist(s).\n"
                                   else case runStateT (genClassInvariants cinvs) env'' of
                                             Bad s             -> fail s
                                             Ok (cinvs',env''') -> 
                                                do put env'''
                                                   return (PPDATE imports' global' temps' cinvs' consts' methods')

   
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
 do ctxt' <- getCtxt ctxt TopLevel    
    return (Global ctxt')

-- Context --

getCtxt :: Abs.Context -> Scope -> UpgradePPD Context
getCtxt (Abs.Ctxt _ _ _ Abs.PropertiesNil Abs.ForeachesNil) _ = fail $ "Error: No properties were defined in section GLOBAL\n."
getCtxt (Abs.Ctxt Abs.VarNil Abs.ActEventsNil Abs.TriggersNil Abs.PropertiesNil foreaches@(Abs.ForeachesDef _ _ _)) TopLevel = getForeaches foreaches (Ctxt [] [] [] PNIL []) (InFor (ForId "TopLevel"))
getCtxt (Abs.Ctxt vars ies Abs.TriggersNil prop foreaches) scope =
 getCtxt (Abs.Ctxt vars ies (Abs.TriggersDef []) prop foreaches) scope
getCtxt (Abs.Ctxt vars ies trigs prop foreaches) scope =
 do vars' <- getVars vars
    trigs' <- getTriggers trigs scope
    env <- get
    let prop' = getProperty prop (map tiTN (allTriggers env)) env
    let cns   = htsNames env
    let ies'  = getActEvents ies   
    case runWriter prop' of
         ((PNIL,env'),_)                              -> do put env'
                                                            getForeaches foreaches (Ctxt vars' ies' trigs' PNIL []) scope
         ((PINIT pname id xs props,env'),s)           -> 
                  let trs = (addComma.removeDuplicates) [tr | tr <- (splitOnIdentifier "," (fst s)), not (elem tr (map ((\x -> x ++ "?").show) ies'))]
                      s'  = if (not.null) trs
                            then "Error: Trigger(s) [" ++ trs 
                                 ++ "] is(are) used in the transitions, but is(are) not defined in section TRIGGERS.\n" 
                                 ++ snd s
                            else snd s
                      s'' = if elem id (map fst $ tempsInfo env)
                            then ""
                            else "Error: In the definition of property " ++ pname
                                 ++ ". The template " ++ id ++ " does not exist\n." 
                  in if (null (s'++s''))
                     then do put env' 
                             getForeaches foreaches (Ctxt vars' ies' trigs' (PINIT pname id xs props) []) scope
                     else fail s'
         ((Property pname states trans props,env'),s) -> 
                  let accep  = checkAllHTsExist (getAccepting states) cns pname
                      bad    = checkAllHTsExist (getBad states) cns pname
                      normal = checkAllHTsExist (getNormal states) cns pname
                      start  = checkAllHTsExist (getStarting states) cns pname
                      errs   = concat $ start ++ accep ++ bad ++ normal
                      trs    = (addComma.removeDuplicates) [tr | tr <- (splitOnIdentifier "," (fst s)), not (elem tr (map ((\x -> x ++ "?").show) ies'))]
                      s'     = if (not.null) trs
                               then "Error: Trigger(s) [" ++ trs ++ "] is(are) used in the transitions, but is(are) not defined in section TRIGGERS.\n" 
                                     ++ snd s ++ errs
                               else snd s ++ errs 
                  in if (null s')
                     then do put env' 
                             getForeaches foreaches (Ctxt vars' ies' trigs' (Property pname states trans props) []) scope
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

getVars :: Abs.Variables -> UpgradePPD Variables
getVars Abs.VarNil        = return []
getVars (Abs.VarDef vars) = sequence $ map getVariable vars

getVariable :: Abs.Variable -> UpgradePPD Variable
getVariable (Abs.Var varm t vdecls) =
 let varm' = getVarModif varm
     t'    = getTypeAbs t
     vs    = map getVarDecl vdecls
 in do env <- get
       put env { varsInPPD = Var varm' t' vs:varsInPPD env}
       return $ Var varm' t' vs

getVarModif :: Abs.VarModifier -> VarModifier
getVarModif Abs.VarModifierFinal = VarModifierFinal
getVarModif Abs.VarModifierNil   = VarModifierNil

getVarDecl :: Abs.VarDecl -> VarDecl
getVarDecl (Abs.VarDecl id varinit) = VarDecl (getIdAbs id) (getVarInitAbs varinit)

-- ActEvents --

getActEvents :: Abs.ActEvents -> ActEvents
getActEvents Abs.ActEventsNil       = []
getActEvents (Abs.ActEventsDef ies) = map (\(Abs.ActEvent ie) -> ActEvent (getIdAbs ie)) ies

-- Triggers --

getTriggers :: Abs.Triggers -> Scope -> UpgradePPD Triggers
getTriggers Abs.TriggersNil _          = return []
getTriggers (Abs.TriggersDef es) scope =
 do env <- get
    let xs = map (getTrigger' scope) es
    let (ls, rs) = partitionErr (map (\e -> CM.evalStateT e env) xs)
    if (null ls)
    then sequence xs
    else fail $ foldr joinBad "" ls

joinBad :: Err a -> String -> String
joinBad (Bad s1) s2 = s1 ++ s2
joinBad _ _         = error "ppDATE refinement failure: joinBad \n"

getTrigger' :: Scope -> Abs.Trigger -> UpgradePPD TriggerDef
getTrigger' scope (Abs.Trigger id binds ce wc) =
 do env  <- get
    let id'' = getIdAbs id
    let xs   = [ x | x <- allTriggers env, id'' == tiTN x, tiScope x == scope ]
    let err  = if (not.null) xs
               then "Error: Multiple definitions for trigger " ++ id'' ++ ".\n" ++ show scope ++ "\n" 
               else ""
    do case runWriter (getBindsArgs binds) of
         (bs, s) ->
           let err0 = if (not.null) s 
                      then (err ++ "Error: Trigger declaration [" ++ id'' ++ "] uses wrong argument(s) [" ++ s ++ "].\n")
                      else err
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
                                                    in if ((not.null) vs) 
                                                       then fail (err1 ++ "Error: Missing Initialization of variable(s) " ++ show vs ++  " in trigger declaration [" ++ id'' ++ "].\n")
                                                       else
                                                         case runWriter ((checkAllArgs argss allArgs bind)) of
                                                          (b,zs) ->
                                                            case runWriter (checkSpecialCases b mn bind bs [] zs id'' env scope) of 
                                                             (b',s'') -> 
                                                                if b' then
                                                                  if (not.null) err1 then fail err1 else
                                                                  do let einfo = if null s''
                                                                                 then show bind
                                                                                 else s''
                                                                     let ov = generateOverloading bs (getCTArgs ce')
                                                                     (ci,cinm) <- getClassVarName id'' mn bs bind s'' scope
                                                                     let tr = TriggerDef { tName = id''
                                                                                         , args  = bs
                                                                                         , compTrigger = ce'
                                                                                         , whereClause = getWhereClause wc
                                                                                         }
                                                                     put env { allTriggers = TI id'' mn ci cinm EVEntry bs (Just tr) scope ov: allTriggers env }
                                                                     return tr
                                                                else fail (err1 ++ s'')
                                       EVExit rs -> let id  = getIdBind bind
                                                        wc' = getWhereClause wc
                                                        wcs = [x | x <- argss, not(elem x allArgs)]
                                                        rs' = map getIdBind rs
                                                        vs  = filter (\ x -> (not (elem x rs')) && (x /= id)) $ checkVarsInitialisation wcs (getVarsWC wc)
                                                    in if ((not.null) vs) 
                                                       then fail (err1 ++ "Error: Missing Initialization of variable(s) " ++ show vs ++  " in trigger declaration [" ++ id'' ++ "].\n")
                                                       else
                                                        case runWriter ((checkAllArgs argss allArgs bind)) of
                                                          (b,zs) ->
                                                            case runWriter (checkSpecialCases b mn bind bs rs zs id'' env scope) of 
                                                              (b',s'') -> 
                                                                if (b' && (checkRetVar rs argss))
                                                                then if (not.null) err1 then fail err1 else
                                                                     do (ci,cinm) <- getClassVarName id'' mn bs bind s'' scope
                                                                        let ov = generateOverloading bs (getCTArgs ce')
                                                                        let tr = TriggerDef { tName = id''
                                                                                            , args  = bs
                                                                                            , compTrigger = ce'
                                                                                            , whereClause = wc'
                                                                                            }
                                                                        put env { allTriggers = TI id'' mn ci cinm (EVExit rs) bs (Just tr) scope ov: allTriggers env }
                                                                        return tr
                                                                else fail (err1 ++ s'')
                                       _        -> return TriggerDef { tName = id''
                                                                     , args  = bs
                                                                     , compTrigger = ce'
                                                                     , whereClause = getWhereClause wc
                                                                     }
                          _  -> if (not.null) err1 then fail err1 else
                                do put env { allTriggers = TI id'' "" "" "" EVNil bs Nothing scope OverNil: allTriggers env }
                                   return TriggerDef { tName = id''
                                                     , args  = bs
                                                     , compTrigger = ce'
                                                     , whereClause = getWhereClause wc
                                                     }

generateOverloading :: [Bind] -> [Bind] -> Overloading
generateOverloading bs [] = Over []
generateOverloading bs ms = Over $ map (getTypeForOverLoading bs) (map getIdBind ms)

getTypeForOverLoading :: [Bind] -> Id -> Type
getTypeForOverLoading [] _                     = ""
getTypeForOverLoading ((BindType t id):bs) id' = if id == id' then t else getTypeForOverLoading bs id'

--Method handling the special cases of the use of new and channels
checkSpecialCases :: Bool -> MethodName -> Bind -> [Bind] -> [Bind] -> [String] -> Trigger -> Env -> Scope -> Writer String Bool
checkSpecialCases b mn bind bs rs zs id env scope = 
 if b
 then return b
 else case runWriter (checkMNforNew mn bind bs rs id zs) of
           (True,_)  -> return True
           (False,s) -> if null s
                        then case runWriter (checkForChannel bind env) of
                                  (True,s)  -> writer (True,s)
                                  (False,s) -> if tempScope scope
                                               then writer (True,s)
                                               else fail $ "Error: Trigger declaration [" ++ id ++ "] uses wrong argument(s) [" 
                                                           ++ addComma zs ++ "] in the method component.\n"                             
                        else fail s

--Method handling new
checkMNforNew :: MethodName -> Bind -> [Bind] -> [Bind] -> Trigger ->  [String] -> Writer String Bool
checkMNforNew mn bind bs rs id zs = 
 case bind of
      BindId id -> if (mn /= "new")
                   then return False
                   else if null rs
                        then fail $ "Error: new cannot be associated to an entry trigger.\n"
                        else if elem (BindType id (getBindIdId (head rs))) bs
                             then if null zs 
                                  then return True
                                  else fail $ "Error: Trigger declaration [" ++ id ++ "] uses wrong argument(s) [" ++ addComma zs ++ "] in the method component.\n"
                             else do tell $ "Error: Wrong exit object in trigger " ++ id ++ "\n."
                                     return False
      _         -> return False

--Method handling channels
checkForChannel :: Bind -> Env -> Writer String Bool
checkForChannel bind env = 
 case bind of
      BindId id -> let vars = map varDecId $ concatMap getVarDeclVar 
                              $ filter (\v -> getTypeVar v == "Channel") $ varsInPPD env
                   in if elem id vars
                      then writer (True,id)
                      else return False
      _         -> return False

properBind :: Bind -> [Bind]
properBind (BindType t id) = [BindType t id]
properBind _               = []

checkAllArgs :: [Id] -> [Id] -> Bind -> Writer [String] Bool
checkAllArgs argss allArgs bind =
 case bind of
      BindId id -> if (elem id argss)
                   then checkArgs argss allArgs
                   else case runWriter (checkArgs argss allArgs) of
                             (b,zs) -> writer (False,zs)
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
                          _ -> False

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
getIdBind _               = ""

getCompTrigger :: Abs.CompoundTrigger -> Writer String CompoundTrigger
getCompTrigger ce =
 case ce of
     Abs.NormalEvent (Abs.BindingVar bind) id binds trv ->
        case runWriter (getBindsBody (map getVarsAbs binds)) of
             (bs, s) -> do let id'  = getIdAbs id
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
getCompTriggers (Abs.Collection (Abs.CECollection esl)) = 
 do let xs = map getCompTrigger esl
    ce <- sequence xs
    return (Collection (CECollection ce))
getCompTriggers ce                                      = getCompTrigger ce

--Checks if the arguments in the triggers have the right form
getBindsArgs :: [Abs.Bind] -> Writer String [Bind]
getBindsArgs []     = return []
getBindsArgs (b:bs) =
 case runWriter (getBindsArgs bs) of
      (bs', s) -> case b of
                       Abs.BindType t id -> do tell s
                                               return ((BindType (getTypeAbs t) (getIdAbs id)):bs')
                       _                 -> do tell (mAppend (printTree b) s)
                                               return bs'

--Checks if the arguments in the method call on a trigger have the right form
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

-- Removes white spaces added by printTree after ';'
getWhereClause :: Abs.WhereClause -> WhereClause
getWhereClause Abs.WhereClauseNil        = ""
getWhereClause (Abs.WhereClauseDef wexp) = (concat.lines.printTree) wexp

--
-- Properties --
--

getProperty :: Abs.Properties -> [Id] -> Env -> Writer (String,String) (Property,Env)
getProperty Abs.PropertiesNil _ env                                           = return (PNIL,env)
getProperty (Abs.ProperiesDef id (Abs.PropKindPinit id' ids') props) enms env = 
 let props' = getProperty props enms env
 in case runWriter props' of
      ((p,env'), s) -> 
                do tell s
                   return (PINIT { piName  = getIdAbs id
                                 , tmpId   = getIdAbs id'
                                 , bounds  = map getIdAbs ids'
                                 , piProps = p
                                 },env')
getProperty (Abs.ProperiesDef id (Abs.PropKindNormal states trans) props) enms env =
 let trans' = getTransitions (getIdAbs id) trans env in
 case runWriter trans' of
      ((t,env'),s') ->
           let props' = getProperty props enms env'
               ts = map (trigger.arrow) t
           in case runWriter props' of
                   ((p,env''), s) -> 
                              do let xs = [x | x <- ts, not(elem x enms)]
                                 tell $ mkErrPair s (addComma xs) s'
                                 return (Property { pName        = getIdAbs id
                                                  , pStates      = getStates' states
                                                  , pTransitions = t
                                                  , pProps       = p },env'')

mkErrPair :: (String, String) -> String -> String -> (String,String)
mkErrPair s xs s' = ((mAppend xs (fst s)), s' ++ snd s)

mAppend :: String -> String -> String
mAppend [] []     = ""
mAppend [] (y:ys) = y:ys
mAppend (x:xs) [] = x:xs
mAppend xs ys     = xs ++ "," ++ ys

getStates' :: Abs.States -> States
getStates' (Abs.States start accep bad norm) = States { getStarting = getStarting' start
                                                      , getAccepting = getAccepting' accep 
                                                      , getBad = getBad' bad
                                                      , getNormal = getNormal' norm
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

getTransitions :: PropertyName -> Abs.Transitions -> Env -> Writer String (Transitions,Env)
getTransitions id (Abs.Transitions ts) env = 
 case runWriter (sequence $ map (getTransition' id env) ts) of
      (xs,s) -> do let trans = map fst xs
                   let envs  = map snd xs
                   let env'  = joinEnvsCreate envs emptyEnv
                   writer ((trans,env'),s)

getTransition' :: PropertyName -> Env -> Abs.Transition -> Writer String (Transition,Env)
getTransition' id env (Abs.Transition (Abs.NameState q1) (Abs.NameState q2) ar) = 
 case runWriter (getArrow ar env) of
      ((xs,env'),s) -> 
                do let err = "Error: Parsing error in an action of a transition from state " ++ getIdAbs q1 ++ " to state " 
                             ++ getIdAbs q2 ++ " in property " ++ id ++ ".\n"
                   let s' = if null s then "" else err ++ s
                   tell s'                  
                   return (Transition { fromState = getIdAbs q1
                                      , arrow = xs
                                      , toState = getIdAbs q2
                                      },env')

getArrow :: Abs.Arrow -> Env -> Writer String (Arrow,Env)
getArrow (Abs.Arrow id mark Abs.Cond1) env        = 
 return $ (Arrow { trigger = getIdAbs id ++ addQuestionMark mark, cond = "", action = "" },env)
getArrow (Abs.Arrow id mark (Abs.Cond2 cond)) env = 
 case cond of
      Abs.CondExpDef cexp     -> return $ (Arrow { trigger = getIdAbs id ++ addQuestionMark mark, cond = printTree cexp, action = "" },env)
      Abs.CondAction cexp act -> 
        let act' = (trim.printTree) act in
        case ParAct.parse act' of 
             Bad s -> do tell s
                         return $ (Arrow { trigger = getIdAbs id ++ addQuestionMark mark, cond = printTree cexp, action = "Parse error" },env)
             Ok (Act.Actions ac) -> 
                      case runWriter $ sequence (map (\a -> checkTempInCreate a env) ac) of
                           (ac',s') -> do tell s'
                                          let ac'' = filter isCreateAct ac'
                                          let acts = [(y,z,"",x) | (x,(y,z)) <- zip ac'' (map getIdAndArgs ac'')]
                                          let env' = env { allCreateAct = acts ++ (allCreateAct env)}
                                          return $ (Arrow { trigger = getIdAbs id ++ addQuestionMark mark, cond = printTree cexp, 
                                                           action = foldr (\ x xs -> x ++ "; " ++ xs) [] $ map PrintAct.printTree ac' },env')
      

isCreateAct :: Act.Action -> Bool
isCreateAct (Act.ActCreate _ _) = True
isCreateAct _                   = False

getIdAndArgs :: Act.Action -> (Id,[Act.Args])
getIdAndArgs (Act.ActCreate (Act.Temp (Act.IdAct id)) args) = (id,args)

joinEnvsCreate :: [Env] -> Env -> Env
joinEnvsCreate [] env          = env
joinEnvsCreate (env:envs) env' = joinEnvsCreate envs (join env env')
                                      where join env env' = env { allCreateAct = (allCreateAct env) ++ (allCreateAct env')}


addQuestionMark :: Abs.Actmark -> String
addQuestionMark Abs.ActMarkNil  = ""
addQuestionMark Abs.ActMark     = "?"
            
checkTempInCreate :: Act.Action -> Env -> Writer String Act.Action
checkTempInCreate ac@(Act.ActCreate (Act.Temp (Act.IdAct id) ) _) env = 
 let tmpids = map fst $ tempsInfo env
 in if not (elem id tmpids)
    then do tell $ "Template " ++ id ++ ", which is used in an action create does not exist.\n"
            return ac
    else return ac
checkTempInCreate (Act.ActCond conds act) env = 
 case runWriter $ checkTempInCreate act env of
      (ac,s) -> do tell s
                   return $ Act.ActCond conds ac
checkTempInCreate (Act.ActBlock (Act.Actions acts)) env     = 
 case runWriter $ sequence $ map (\act -> checkTempInCreate act env) acts of
      (ac,s) -> do tell s
                   return $ Act.ActBlock (Act.Actions ac)
checkTempInCreate act env                     = return act

---------------
-- Foreaches --
---------------

getForeaches :: Abs.Foreaches -> Context -> Scope -> UpgradePPD Context
getForeaches Abs.ForeachesNil ctxt _ = 
 do env <- get    
    put env { actes = actes env ++ map show (actevents ctxt)} 
    return ctxt
getForeaches (Abs.ForeachesDef _ (Abs.Ctxt _ _ _ _ (Abs.ForeachesDef args actxt fors)) afors) _ _ = 
 fail "Error: StaRVOOrS does not support nested Foreaches.\n"
getForeaches afors@(Abs.ForeachesDef _ (Abs.Ctxt _ _ _ _ Abs.ForeachesNil) _) ctxt scope = 
 let afors' = prepareForeaches afors
 in do env <- get
       put env { actes = actes env ++ map show (actevents ctxt)}
       fors <- sequence $ map (uncurry getForeach) (zip afors' (createIds scope afors'))
       env' <- get
       return (updateCtxtFors ctxt fors)


prepareForeaches :: Abs.Foreaches -> [Abs.Foreaches]
prepareForeaches Abs.ForeachesNil                  = []
prepareForeaches (Abs.ForeachesDef args ctxt fors) = 
 (Abs.ForeachesDef args ctxt Abs.ForeachesNil):prepareForeaches fors

createIds :: Scope -> [Abs.Foreaches] -> [ForId]
createIds TopLevel afors          = map (ForId . show) [1..length afors]
createIds (InFor (ForId s)) afors = 
 if s == "TopLevel" 
 then [ForId s]
 else map (ForId . (s++) . show) [1..length afors]

getForeach :: Abs.Foreaches -> ForId -> UpgradePPD Foreach
getForeach (Abs.ForeachesDef args ctxt Abs.ForeachesNil) id = 
 do ctxt' <- getCtxt ctxt (InFor id)
    env <- get    
    case foreaches ctxt' of
      [] -> do let args'     = map getArgs args
               let propn     = pName $ property ctxt'
               let Args t cl = head $ args'               
               put env { forsVars      = forsVars env ++ map getArgsId args'
                       , propInForeach = (propn,t,cl):propInForeach env }               
               return $ Foreach args' ctxt' id
      _  -> fail $ "Error: StaRVOOrS does not support nested Foreaches.\n"


---------------
-- Templates --
---------------

getTemplate :: Templates -> Id -> Template  
getTemplate (Temp tmps) id = getTemplate' tmps id

getTemplate' :: [Template] -> Id -> Template
getTemplate' [x] _     = x
getTemplate' (x:xs) id = if tempId x == id then x else getTemplate' xs id

genTemplates :: Abs.Templates -> UpgradePPD Templates
genTemplates Abs.TempsNil   = return TempNil
genTemplates (Abs.Temps xs) = 
 do xs <- sequence $ map genTemplate xs
    env <- get
    put env { tempsInfo = tempsInfo env ++ foldr (\ x xs -> (tempId x,tempBinds x) : xs) [] xs}
    return $ Temp xs 
 

genTemplate :: Abs.Template -> UpgradePPD Template
genTemplate (Abs.Temp id args (Abs.Body vars ies trs prop)) = 
 do trigs' <- getTriggers trs (InTemp (getIdAbs id))
    env <- get
    let cns   = htsNames env
    let prop' = getProperty prop (map tName trigs') env
    let extrs = getExitTrsInfo trigs'
    case runWriter prop' of
         ((PNIL,env'),_)                      -> fail $ "Error: The template " ++ getIdAbs id ++ " does not have a PROPERTY section.\n"
         ((PINIT pname id' xs props,env'),s)  -> 
                  let temptrs = splitOnIdentifier "," $ fst s
                      s'      = snd s ++ if props /= PNIL 
                                         then "Error: In template " ++ getIdAbs id ++ ", a template should describe only one property.\n"
                                         else "" 
                  in if ((not.null) s')
                     then fail s'
                     else do put env' { triggersInTemps = triggersInTemps env' ++ temptrs 
                                      , actes           = actes env' ++ map show (getActEvents ies)}
                             return $ Template { tempId        = getIdAbs id
                                               , tempBinds     = map ((uncurry makeArgs).getArgsAbs) args
                                               , tempVars      = getValue $ getVars vars
                                               , tempActEvents = getActEvents ies
                                               , tempTriggers  = trigs'
                                               , tempProp      = PINIT pname id' xs props
                                               }
         ((Property pname states trans props,env'),s) -> 
                  let accep   = checkAllHTsExist (getAccepting states) cns pname
                      bad     = checkAllHTsExist (getBad states) cns pname
                      normal  = checkAllHTsExist (getNormal states) cns pname
                      start   = checkAllHTsExist (getStarting states) cns pname
                      errs    = concat $ start ++ accep ++ bad ++ normal
                      s'      = snd s ++ errs ++ if props /= PNIL 
                                                 then "Error: In template " ++ getIdAbs id ++ ", a template should describe eonly one property.\n"
                                                 else ""
                      temptrs = splitOnIdentifier "," $ fst s
                  in if ((not.null) s')
                     then fail s'
                     else do put env' { triggersInTemps = triggersInTemps env' ++ temptrs 
                                      , actes           = actes env' ++ map show (getActEvents ies)}
                             return $ Template { tempId        = getIdAbs id
                                               , tempBinds     = map ((uncurry makeArgs).getArgsAbs) args
                                               , tempVars      = getValue $ getVars vars
                                               , tempActEvents = getActEvents ies
                                               , tempTriggers  = trigs'
                                               , tempProp      = Property pname states trans props
                                               }

getExitTrsInfo :: Triggers -> [(ClassInfo,MethodName)]
getExitTrsInfo [] = []
getExitTrsInfo ((TriggerDef tr args (NormalEvent (BindingVar (BindId id)) id' _ (EVExit _)) _):ts) = 
 case runStateT (getClassVarArgs args id) emptyEnv of
      Bad s    -> error $ "Error: In trigger " ++ tr ++ ", " ++ s
      Ok (t,_) -> (t,id'):getExitTrsInfo ts
getExitTrsInfo ((TriggerDef _ _ (NormalEvent (BindingVar (BindType t _)) id _ (EVExit _)) _):ts) = 
 (t,id):getExitTrsInfo ts
getExitTrsInfo (_:ts) = getExitTrsInfo ts


getClassVarArgs :: [Bind] -> Id -> UpgradePPD ClassInfo
getClassVarArgs [] id                    = fail $ id ++ " is not associated to any argument.\n" 
getClassVarArgs (BindType t id':args) id = 
 if id == id'
 then return t
 else getClassVarArgs args id
getClassVarArgs (_:args) id              = getClassVarArgs args id

-----------------
-- CInvariants --
-----------------

genClassInvariants :: Abs.CInvariants -> UpgradePPD CInvariants
genClassInvariants absinvs =
 case runWriter (genClassInvariants' absinvs) of
      (cinvs,s) -> if null s
                   then return cinvs
                   else fail s

genClassInvariants' :: Abs.CInvariants -> Writer String CInvariants
genClassInvariants' Abs.CInvempty           = return []
genClassInvariants' (Abs.CInvariants cinvs) = sequence $ map getCInv cinvs

getCInv :: Abs.CInvariant -> Writer String CInvariant
getCInv (Abs.CI cn jml) = 
 case runWriter (getJML jml "") of
      (jml',s) -> if null s 
                  then return $ CI (getIdAbs cn) jml'
                  else do tell $ "Error: Parse on error on class invariant [" ++ printTree jml ++ "] for the class " ++ getIdAbs cn ++ ".\n"
                          return CInvNil

-------------------
-- Hoare Triples --
-------------------

genHTs :: Abs.HTriples -> Imports -> UpgradePPD HTriples
genHTs Abs.HTempty _          = return []
genHTs (Abs.HTriples cs) imps = checkOverl $ sequence $ map (getHT imps) cs

getHT :: Imports -> Abs.HT -> UpgradePPD HT
getHT imps (Abs.HT id pre' method post' (Abs.Assignable ass)) =
 do let mcn = MCN { clinf = getMethodClassInfo method, mname = getMethodMethodName method, overl = getMethodOverloading method }
    env <- get
    case checkImports (clinf mcn) imps of
         []     -> fail $ "Error: Hoare triple " ++ getIdAbs id ++ " is associated to class " ++ clinf mcn ++ ", but the class is not imported.\n"
         (x:xs) -> if (not.null) xs 
                   then fail $ "Error: Multiple imports for class " ++ clinf mcn
                   else do let cns = htsNames env
                           let ys  = map checkJML $ [getPre pre',getPost post'] ++ (map assig ass)
                           joinErrorJML ys (getIdAbs id)
                           put env { htsNames = (getIdAbs id):(htsNames env) }
                           return (HT { htName   = getIdAbs id
                                  , methodCN     = mcn
                                  , pre          = filter (/='\n') $ getJMLExp $ getPre pre'
                                  , post         = filter (/='\n') $ getJMLExp $ getPost post'
                                  , assignable   = joinAssignable $ map (getJMLExp.assig) ass
                                  , optimized    = []
                                  , chGet        = 0
                                  , path2it      = ""
                                  })

checkJML :: Writer String JMLExp -> Either JMLExp String
checkJML wjml =
 case runWriter wjml of
      (jml', s) -> if null s
                   then Left jml'
                   else Right s

getJMLExp :: Writer String JMLExp -> JMLExp
getJMLExp wjml = 
 case runWriter wjml of
      (jml', s) -> jml'
                   
joinErrorJML :: [Either JMLExp String] -> String -> UpgradePPD ()
joinErrorJML xs str = 
 do let ys = rights xs 
    if null ys 
    then return ()
    else fail $ concatMap (\s -> s ++ " of Hoare triple " ++ str ++ ".\n") ys 

assig :: Abs.Assig -> Writer String JMLExp
assig (Abs.AssigJML jml) = return $ printTree jml
assig Abs.AssigE         = return "\\everything"
assig Abs.AssigN         = return "\\nothing"

joinAssignable [x]    = x
joinAssignable (x:xs) = x ++ "," ++ joinAssignable xs

checkImports :: ClassInfo -> Imports -> Imports
checkImports _ []             = []
checkImports cn (Import s:xs) = 
 let ys = splitOnIdentifier "." s
 in if (trim $ last ys) == cn
    then (Import s):checkImports cn xs
    else checkImports cn xs

checkOverl :: UpgradePPD HTriples -> UpgradePPD HTriples
checkOverl hts = 
 case runStateT hts emptyEnv of
      Bad s         -> fail s
      Ok (hts',env) -> let ys  = zip (map htName hts') (map methodCN hts')
                           ys' = [(htn,mnc') | (htn,mnc') <- ys, overl mnc' /= OverNil]
                       in if null ys'
                          then hts
                          else case runWriter $ sequence $ map (checkOV ys') [p | p <- ys, not (elem p ys')] of
                                    (b,s) -> if and b
                                             then hts
                                             else fail s

checkOV :: [(HTName,MethodCN)] -> (HTName,MethodCN) -> Writer String Bool
checkOV xs (htn,mnc) =
 let ys = [ mnc' | (_,mnc') <- xs, mname mnc == mname mnc', clinf mnc == clinf mnc']
 in if null ys
    then return True
    else do tell $ "Error: Type missing for the method " ++ mname mnc ++ " of the class "
                   ++ clinf mnc ++ " in the Hoare triple " ++ htn ++ ".\n"
            return False
    

-------------
-- Methods --
-------------

genMethods :: Abs.Methods -> Methods
genMethods m = printTree m

--------------------
-- PINIT property --
--------------------

replacePInit :: UpgradePPD PPDATE -> UpgradePPD PPDATE
replacePInit ppd = 
 let env    = getEnvVal ppd
     ppdate = getValue ppd
     global = ctxtGet $ globalGet ppdate
     es     = triggers global
     vars   = variables global
     acts   = actevents global
     prop   = property global
     fors   = foreaches global 
 in if or $ map checkPInitForeach fors
    then fail "Error: It is not possible to define a PINIT property within a FOREACH.\n"
    else case getPInit prop of
              (p:ps) ->  
                  let templates = templatesGet ppdate 
                      prop'     = removePInit prop
                      tmpFors   = map (\pi -> pinit2foreach pi templates) (p:ps)
                      fors'     = tmpFors ++ fors
                      global'   = updateGlobal (globalGet ppdate) (Ctxt vars acts es prop' fors')
                      propns    = map piName (p:ps)
                      pif       = map (\(x,Args t cl) -> (x,t,cl)) $ zip propns (map (head.getArgsForeach) tmpFors)
                  in do put env { forsVars = forsVars env ++ map getArgsId (concatMap getArgsForeach tmpFors) 
                                , propInForeach = pif ++ propInForeach env  }
                        return $ updateGlobalPP ppdate global'
              []     -> ppd

getPInit :: Property -> [Property]
getPInit PNIL                        = []
getPInit (PINIT id temp bound props) = (PINIT id temp bound PNIL):getPInit props
getPInit (Property _ _ _ props)      = getPInit props

removePInit :: Property -> Property
removePInit PNIL                         = PNIL
removePInit (PINIT _ _ _ props)          = removePInit props
removePInit (Property name st trs props) = Property name st trs (removePInit props)


checkPInitForeach :: Foreach -> Bool
checkPInitForeach foreach = 
 (not.null) $ getPInit $ property $ getCtxtForeach foreach

pinit2foreach :: Property -> Templates -> Foreach
pinit2foreach (PINIT id tempid bound PNIL) templates = 
 let temp = getTemplate templates tempid
     args = tempBinds temp
     ctxt = Ctxt (tempVars temp) (tempActEvents temp) (tempTriggers temp) (tempProp temp) []
 in Foreach args ctxt (ForId "pinit")


------------------------------
-- ppDATE to input language --
------------------------------

writePPD :: UpgradePPD PPDATE -> String
writePPD ppd = 
 case getValue ppd of
      PPDATE imps global temps cinvs hts ms ->
            writePPDImports imps
            ++ writePPDGlobal global
            ++ writePPDTemps temps
            ++ writePPDCinvs cinvs
            ++ writePPDHts hts
            ++ writePPDMethods ms

writePPDImports :: Imports -> String
writePPDImports imps = 
 "IMPORTS {\n"
 ++ unlines (map show imps)
 ++ "}\n\n"

writePPDGlobal :: Global -> String
writePPDGlobal (Global ctxt) = 
 "GLOBAL {\n\n"
 ++ writePPDContext ctxt
 ++ "}\n\n"

writePPDContext :: Context -> String
writePPDContext (Ctxt vars acts trs props fors) =
 writePPDVariables vars
 ++ writePPDActs acts
 ++ writePPDTriggers trs
 ++ writePPDProps props
 ++ writePPDForeaches fors

writePPDVariables :: Variables -> String
writePPDVariables []   = ""
writePPDVariables vars =
 "VARIABLES {\n"
 ++ unlines (map show vars)
 ++ "}\n\n"

writePPDActs :: ActEvents -> String
writePPDActs []   = ""
writePPDActs acts = 
 "ACTEVENTS {\n"
 ++ addComma (map show acts) ++ "\n"
 ++ "}\n\n"

writePPDTriggers :: Triggers -> String
writePPDTriggers []  = ""
writePPDTriggers trs = 
 "TRIGGERS {\n"
 ++ unlines (map show trs)
 ++ "}\n\n"

writePPDProps :: Property -> String
writePPDProps PNIL                    = ""
writePPDProps (PINIT nm tmp bds prop) = 
 "PROPERTY " ++ nm ++ " {\n"
 ++ "PINIT { (" ++ tmp ++ "," ++ addComma bds ++ ") }\n"
 ++ "}\n\n"
 ++ writePPDProps prop
writePPDProps (Property nm sts trans prop) = 
 "PROPERTY " ++ nm ++ " {\n\n"
 ++ writePPDStates sts
 ++ writePPDTransitions trans
 ++ "}\n\n"
 ++ writePPDProps prop

writePPDStates :: States -> String
writePPDStates (States start acc bad normal) =
 let acc'    = map show acc
     bad'    = map show bad
     normal' = map show normal
     start'  = map show start
 in "STATES \n{\n"
    ++ writePPDState "STARTING" start'
    ++ writePPDState "ACCEPTING" acc'
    ++ writePPDState "BAD" bad'
    ++ writePPDState "NORMAL" normal'    
    ++ "}\n\n"

writePPDState :: String -> [String] -> String
writePPDState iden xs = if null (filter (\c -> c /= "") xs)
                        then ""
                        else iden ++ " { " ++ concat xs ++ "}\n"
 

writePPDTransitions :: Transitions -> String
writePPDTransitions []    = ""
writePPDTransitions trans = 
 "TRANSITIONS {\n"
 ++ unlines (map show trans)
 ++ "}\n\n"

writePPDForeaches :: Foreaches -> String
writePPDForeaches []          = ""
writePPDForeaches fors@(f:fs) = unlines (map writePPDForeach fors)
 
writePPDForeach :: Foreach -> String
writePPDForeach foreach = 
 "FOREACH (" ++ addComma (map show (getArgsForeach foreach)) ++ ") {\n\n"
 ++ writePPDContext (getCtxtForeach foreach)
 ++ "}\n\n"

writePPDTemps :: Templates -> String
writePPDTemps TempNil      = ""
writePPDTemps (Temp temps) =
 "TEMPLATES {\n\n"
 ++ unlines (map writePPDTemp temps)
 ++ "}\n\n" 

writePPDTemp :: Template -> String
writePPDTemp (Template id bnds vars acts trs prop) = 
 "TEMPLATE " ++ id ++ " (" ++ addComma (map show bnds) ++ ")" ++ " {\n\n"
 ++ writePPDVariables vars
 ++ writePPDActs acts
 ++ writePPDTriggers trs
 ++ writePPDProps prop
 ++ "}\n\n"


writePPDCinvs :: CInvariants -> String
writePPDCinvs []    = ""
writePPDCinvs cinvs = 
 "CINVARIANTS {\n"
 ++ writePPDCinvs' cinvs
 ++ "}\n\n"

writePPDCinvs' :: CInvariants -> String
writePPDCinvs' []                 = ""
writePPDCinvs' (CInvNil:cinvs)    = writePPDCinvs' cinvs
writePPDCinvs' (inv:cinvs)        =
 show inv ++ "\n" ++ writePPDCinvs' cinvs

writePPDHts :: HTriples -> String
writePPDHts []  = ""
writePPDHts hts =
 "HTRIPLES {\n\n"
 ++ writePPDHts' hts
 ++ "}\n\n"

writePPDHts' :: HTriples -> String
writePPDHts' []      = ""
writePPDHts' (h:hts) = 
 "HT " ++ htName h ++ " {\n"
 ++ "PRE { " ++ pre h ++" }\n"
 ++ "METHOD { " ++ clinf (methodCN h) ++ "." ++ mname (methodCN h) ++" }\n"
 ++ "POST { " ++ post h ++" }\n"
 ++ "ASSIGNABLE { " ++ assignable h ++" }\n"
 ++ "}\n\n"
 ++ writePPDHts' hts

writePPDMethods :: Methods -> String
writePPDMethods [] = ""
writePPDMethods xs = xs

------------------------
-- Selector functions --
------------------------

getArgs :: Abs.Args -> Args
getArgs (Abs.Args t id) = Args (getTypeAbs t) (getIdAbs id)

getArgsAbs :: Abs.Args -> (Type,Id)
getArgsAbs (Abs.Args t id) = (getTypeAbs t, getIdAbs id)

getSymbolsAbs :: Abs.Symbols -> String
getSymbolsAbs (Abs.Symbols s) = s

getIdAbs :: Abs.Id -> String
getIdAbs (Abs.Id s) = s

getTypeAbs :: Abs.Type -> String
getTypeAbs (Abs.Type (Abs.TypeDef (Abs.Id id))) = id
getTypeAbs (Abs.Type (Abs.TypeGen (Abs.Id id) (Abs.Symbols s1) (Abs.Id id') (Abs.Symbols s2))) = id ++ s1 ++ id' ++ s2
getTypeAbs (Abs.Type (Abs.TypeArray (Abs.Id id))) = id

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

getJML :: Abs.JML -> String -> Writer String JMLExp
getJML jml str = 
 let jml' = printTree jml in
 case ParJML.parse jml' of
      Bad s -> do tell $ "Parse error on the " ++ str
                  return "Parse error"
      Ok _  -> return jml' 

getJava :: Abs.Java -> Java
getJava java = printTree java

getMethodClassInfo :: Abs.Method -> ClassInfo
getMethodClassInfo (Abs.Method ci _ _) = getIdAbs ci

getMethodMethodName :: Abs.Method -> MethodName
getMethodMethodName (Abs.Method _ mn _) = getIdAbs mn

getMethodOverloading :: Abs.Method -> Overloading
getMethodOverloading (Abs.Method _ _ over) = getOverloading over

getOverloading :: Abs.Overloading -> Overloading 
getOverloading Abs.OverNil = OverNil
getOverloading (Abs.Over xs) = Over $ map getTypeAbs xs

getAssig :: Abs.Assig -> Abs.JML
getAssig (Abs.AssigJML jml) = jml

getPre :: Abs.Pre -> Writer String JMLExp
getPre (Abs.Pre pre) = getJML pre "precondition"

getPost :: Abs.Post -> Writer String JMLExp
getPost (Abs.Post post) = getJML post "postcondition"

-------------------------
-- Auxiliary functions --
-------------------------

duplicateHT :: [HTName] -> String
duplicateHT []     = ""
duplicateHT (c:cs) = "Error: Multiple definitions for Hoare triple " ++ c ++ ".\n" ++ duplicateHT cs

getDuplicate :: [HTName] -> [HTName]
getDuplicate []     = []
getDuplicate (c:cs) = if elem c cs
                      then c:getDuplicate cs
                      else getDuplicate cs

-- Imports are considered to be separated by '.'
joinImport :: [String] -> String
joinImport []     = ""
joinImport [ys]   = ys
joinImport (xs:ys:iss) = xs ++ "." ++ joinImport (ys:iss)


lookForAllEntryTriggerArgs :: Env -> HT -> UpgradePPD (String, String)
lookForAllEntryTriggerArgs env c =
 let trs = allTriggers env
 in do tinfo <- getEntryTriggers (methodCN c) trs
       let args  = map (\(BindType t id) -> Args t id) (tiBinds tinfo)
       return (getArgsGenMethods (tiCI tinfo,tiCVar tinfo, args))   

getEntryTriggers :: MethodCN -> [TriggersInfo] -> UpgradePPD TriggersInfo
getEntryTriggers mnc []         = fail $ "Error: Could not find an entry trigger associated to method "
                                         ++ mname mnc ++ show (overl mnc) ++ " of the class " ++ clinf mnc ++ ".\n"
getEntryTriggers mnc (tinfo:ts) =
 if (mname mnc) == (tiMN tinfo) && (clinf mnc) == (tiCI tinfo) 
    && (cmpOverloading (overl mnc) (tiOver tinfo))
    && tiTrvar tinfo == EVEntry
 then return tinfo
 else getEntryTriggers mnc ts


lookForAllExitTriggerArgs :: Env -> HT -> UpgradePPD (String, String)
lookForAllExitTriggerArgs env c =
 let mnc   = methodCN c
     over  = overl mnc
     tr    = getTriggerDef over c (allTriggers env) 
     tinfo = head [ t | t <- allTriggers env , Just tr == tiTrDef t]
     args  = map (\(BindType t id) -> Args t id) (tiBinds tinfo)
 in return (getArgsGenMethods (tiCI tinfo, tiCVar tinfo, args))

getArgsGenMethods :: (ClassInfo, String, [Args]) -> (String,String)
getArgsGenMethods (ci, cvar, args) = 
 let varClass' = ci ++ " " ++ cvar
     argswt    = map getArgsId args
     argswt'   = addComma argswt
     argswt''  = if (elem cvar argswt) 
                 then argswt'
                 else if null argswt' then cvar else cvar ++ "," ++ argswt'
     flatArgs  = flattenArgs args
     args'     = if (elem cvar argswt)
                 then flatArgs
                 else if (null flatArgs)
                      then trim varClass'
                      else (trim varClass') ++ if null args then "" else "," ++ flattenArgs args
 in (args', argswt'')

getAllTriggers :: Global -> Env -> Triggers
getAllTriggers (Global (Ctxt vars ies trigs prop fors)) env = 
 let trs = trigs ++ getTriggersFors fors 
 in trs ++ [ fromJust (tiTrDef t) | t <- allTriggers env, tiTrDef t /= Nothing, not (elem (fromJust $ tiTrDef t) trs)]


--------------------------------------------------------------------
-- Environment with variables, triggers and foreaches information --
--------------------------------------------------------------------

data Env = Env
 { forsVars        :: [Id] --foreach bounded variable names 
 , allTriggers     :: [TriggersInfo]
 , htsNames        :: [HTName]
 , varsInFiles     :: [(String, ClassInfo, [(Type, Id)])]
 , varsInPPD       :: Variables
 , methodsInFiles  :: [(String, ClassInfo, [(Type,Id,[String],MethodInvocations)])] --[(path_to_class,class_name,[(returned_type,method_name,arguments,methodsInvokedIn_method_name_body)])]
 , oldExpTypes     :: OldExprM
 , tempsInfo       :: [(Id,[Args])]
 , triggersInTemps :: [Trigger] --is used to check whether the triggers in the transitions of the templates are  
                                --defined in the triggers of the ppDATE
 , propInForeach   :: [(PropertyName, ClassInfo, String)]-- is used to avoid ambigous reference to variable id in foreaches
 , actes           :: [Id] --list of all defined action events
 , allCreateAct    :: [CreateActInfo]--list of all actions \create used in the transitions of the ppDATE
 }
  deriving (Show)

type UpgradePPD a = CM.StateT Env Err a

emptyEnv :: Env
emptyEnv = Env { forsVars        = []
               , allTriggers     = []
               , htsNames        = []
               , varsInFiles     = []
               , varsInPPD       = []
               , methodsInFiles  = []
               , oldExpTypes     = Map.empty
               , tempsInfo       = []
               , triggersInTemps = []
               , propInForeach   = []
               , actes           = []
               , allCreateAct    = []
               }

getClassVarName :: Trigger -> MethodName -> [Bind] -> Bind -> String -> Scope -> UpgradePPD (ClassInfo,String)
getClassVarName _ _ args BindStar _ _          = return ("*","")
getClassVarName _ _ args (BindType t id) _ _   = return (t,id)
getClassVarName tr mn args (BindId id) s scope = 
 let ts' = [getBindTypeType arg | arg <- args, getBindTypeId arg == id ]
     ts  = if (null ts') then prepareValUpd mn s id s else ts'
 in if (length ts /= 1)
    then if tempScope scope
         then return ("",id)
         else fail $ "The trigger " ++ tr ++ " does not include a class variable declaration.\n" 
    else return (head ts,id)
 
prepareValUpd :: MethodName -> String -> String -> String -> [String]
prepareValUpd mn s id chan = 
 if mn == "new"
 then [id]
 else if null s
      then []
      else [chan]

updateInfo :: Map.Map MethodName [(Id, String, [Args],Scope)] -> MethodName -> (Id, String, [Args],Scope) -> Map.Map MethodName [(Id, String, [Args],Scope)]
updateInfo m mn einfo = 
 case Map.lookup mn m of
      Nothing -> Map.insert mn [einfo] m
      Just xs -> Map.insert mn (einfo:xs) m


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

getHTNamesEnv :: UpgradePPD a -> [HTName]
getHTNamesEnv ppd = let env = CM.execStateT ppd emptyEnv
                    in case env of
                            Bad _ -> []
                            Ok fs -> htsNames fs
