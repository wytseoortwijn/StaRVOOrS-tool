module RefinementPPDATE (refinePPDATE, getClassVar,generateNewTriggers,filterDefinedTriggers) where

import Types
import CommonFunctions
import DL2JML
import qualified Data.Map as Map
import UpgradePPDATE

-----------------------
-- ppDATE refinement --
-----------------------

refinePPDATE :: UpgradePPD PPDATE -> [Proof] -> UpgradePPD PPDATE
refinePPDATE ppd proofs = 
 let ppdate    = getValue ppd 
     consts    = htsGet ppdate
     nproved   = filter (\(x,y,z,t) -> (not.null) z) $ map getInfoFromProof proofs
     proved    = filter (\(x,y,z,t) -> (null z)) $ map getInfoFromProof proofs
     consts'   = [c | c <- consts, (x,y,z,t) <- nproved, htName c == y]  
     cproved   = [c | c <- consts, (x,y,z,t) <- proved, htName c == y]  
     ppd'      = generateNewTriggers ppd consts'
     ppdate'   = getValue ppd'
     global    = globalGet ppdate'
     triggers' = getAllTriggers global 
     consts''  = updateHTs nproved consts' triggers'
     env'      = getEnvVal ppd'     
     global'   = optimizedProvenHTs cproved refinePropertyOptGlobal global
     temps     = templatesGet ppdate'
     temps'    = optimizedProvenHTs cproved refinePropertyOptTemplates temps
 in do put env'
       return $ updateTemplatesPP (updateHTsPP (updateGlobalPP ppdate' global') consts'') temps'

--------------------------------------------------
-- Remove Hoare triples which were fully proved --
--------------------------------------------------

optimizedProvenHTs :: HTriples -> (HTName -> a -> a) -> a -> a
optimizedProvenHTs [] f ps     = ps
optimizedProvenHTs (c:cs) f ps = f (htName c) $ optimizedProvenHTs cs f ps

refinePropertyOptTemplates :: HTName -> Templates -> Templates
refinePropertyOptTemplates cn (Temp temps) = Temp $ map (refineTemplate cn) temps

refineTemplate :: HTName -> Template -> Template
refineTemplate cn temp = updateTemplateProp temp (removeStatesProp cn $ tempProp temp)

refinePropertyOptGlobal :: HTName -> Global -> Global
refinePropertyOptGlobal cn (Global ctxt) = Global $ refineContext cn ctxt

refineContext :: HTName -> Context -> Context
refineContext cn (Ctxt vars ies trigs prop fors) = 
 let prop' = removeStatesProp cn prop
 in case fors of
         []                  -> Ctxt vars ies trigs prop' [] 
         [Foreach args ctxt] -> Ctxt vars ies trigs prop' [Foreach args (refineContext cn ctxt)] 

removeStatesProp :: HTName -> Property -> Property
removeStatesProp _ PNIL               = PNIL
removeStatesProp _ p@(PINIT _ _ _ _)  = p
removeStatesProp cn prop = let States acc bad nor star = pStates prop
                               acc'  = map (\s -> removeStateProp s cn) acc
                               bad'  = map (\s -> removeStateProp s cn) bad
                               nor'  = map (\s -> removeStateProp s cn) nor
                               star' = map (\s -> removeStateProp s cn) star
                               states = States acc' bad' nor' star'
                           in Property (pName prop) states (pTransitions prop) (removeStatesProp cn (pProps prop))

removeStateProp :: State -> HTName -> State
removeStateProp (State ns ic cns) cn = State ns ic (removePropInState cn cns)

removePropInState :: HTName -> [HTName] -> [HTName]
removePropInState cn []        = []
removePropInState cn (cn':cns) = if (cn == cn')
                                 then cns
                                 else cn':removePropInState cn cns


------------------------------------------------------
-- Get information from the results produced by KeY --
------------------------------------------------------

updateHTs :: [(MethodName, HTName, [Pre],String)] -> HTriples -> Triggers -> HTriples
updateHTs [] consts _      = consts
updateHTs (x:xs) consts es = updateHTs xs (updateHT x consts es) es


updateHT :: (MethodName, HTName, [Pre],String) -> HTriples -> Triggers -> HTriples
updateHT (mn,cn,pres,path) [] _      = []
updateHT (mn,cn,pres,path) (c:cs) es = 
 if (htName c == cn && (snd.methodCN) c == mn)
 then if (null pres)
      then c:updateHT (mn,cn,pres,path) cs es
      else let clvar = getClassVar c es EVEntry
               pres' = removeDuplicates pres
               opt'  = map (addParenthesisNot.(replaceSelfWith clvar).removeDLstrContent) pres'
               opt'' = '(':introduceOr opt' ++ [')']
               c'    = updatePath (updateOpt c [opt'']) path
               c''   = updatePre c' $ removeDLstrContent (pre c)
               c'''  = updatePost c'' $ removeDLstrContent (post c)
           in c''':cs
 else c:updateHT (mn,cn,pres,path) cs es

getClassVar :: HT -> Triggers -> TriggerVariation -> String
getClassVar c es ev = lookupClassVar es c ev

-- returns variable name used to instantiate the class in the ppDATE
lookupClassVar :: Triggers -> HT -> TriggerVariation -> String
lookupClassVar [] _ _      = ""
lookupClassVar (e:es) c ev = 
 case (compTrigger e) of
      NormalEvent (BindingVar b) id _ ev' -> 
                  if (id == snd (methodCN c) && compareEV ev ev')
                  then case b of
                            BindStar      -> ""
                            BindType _ id -> id
                            BindId id     -> id
                  else lookupClassVar es c ev
      ClockEvent id _        -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c ev
      OnlyId id              -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c ev
      OnlyIdPar id           -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c ev
      otherwise              -> lookupClassVar es c ev

compareEV :: TriggerVariation -> TriggerVariation -> Bool
compareEV EVEntry EVEntry         = True
compareEV (EVExit _) (EVExit _)   = True
compareEV _ _                     = False

-----------------------------------------------------------------------------------------
-- Generate triggers whenever a method to be runtime verified is not associated to one --
-----------------------------------------------------------------------------------------

--If trigger associated to *, it will be considered as defined even if the the class is wrong
generateNewTriggers :: UpgradePPD PPDATE -> HTriples -> UpgradePPD PPDATE
generateNewTriggers ppd consts =
  do let env     = getEnvVal ppd
     let ppdate  = getValue ppd     
     let mfiles  = methodsInFiles env
     let mns     = removeDuplicates [mn | mn <- map methodCN consts]
     let entry   = filterDefinedTriggers (entryTriggersInfo env) mns
     let exit    = filterDefinedTriggers (exitTriggersInfo env) mns
     let entry'  = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- entry, (_,d,z) <- mfiles,d==x]
     let exit'   = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- exit, (_,d,z) <- mfiles,d==x]
     let (env',ppdate')   = addNewTriggerEntry env ppdate 0 entry'
     let (env'',ppdate'') = addNewTriggerExit env' ppdate' (length entry') exit'
     put env''
     return ppdate''


filterDefinedTriggers :: Map.Map ClassInfo MapTrigger -> [(ClassInfo,MethodName)] -> [(ClassInfo,MethodName)]
filterDefinedTriggers mci []           = []
filterDefinedTriggers mci ((ci,mn):xs) = 
 case Map.lookup ci mci of
      Nothing -> case Map.lookup "*" mci of
                      Nothing -> (ci,mn):filterDefinedTriggers mci xs
                      Just m'  -> case Map.lookup mn m' of
                                       Nothing -> (ci,mn):filterDefinedTriggers mci xs
                                       Just _  -> filterDefinedTriggers mci xs
      Just m  -> case Map.lookup mn m of
                      Nothing -> case Map.lookup "*" mci of
                                      Nothing -> (ci,mn):filterDefinedTriggers mci xs
                                      Just m'  -> case Map.lookup mn m' of
                                                       Nothing -> (ci,mn):filterDefinedTriggers mci xs
                                                       Just _  -> filterDefinedTriggers mci xs
                      Just _  -> filterDefinedTriggers mci xs

--Creates the info to be added in the environment and the ppDATE about the new entry trigger
createTriggerEntry :: (ClassInfo,MethodName,(String,MethodName,[String])) -> Int -> ((ClassInfo,MethodName,(Id, String, [Args])),TriggerDef)
createTriggerEntry (cn,mn,(rt,mn',xs)) n = 
 if (mn == mn')
 then let trnm = mn ++ "_ppden"
          nvar = "cv" ++ "_" ++ (show n)
          cn'  = cn ++ " " ++ nvar
          cpe  = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words) xs) EVEntry
          tr   = TriggerDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
      in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)),tr)
 else error $ "Problem when creating an entry trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerEntry :: Env -> PPDATE -> Int -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env,PPDATE)
addNewTriggerEntry env ppdate _ []     = (env,ppdate)
addNewTriggerEntry env ppdate n (x:xs) =
 let (p,tr) = createTriggerEntry x n
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
     cl     = makeBind $ (\(x,y,z) -> y) v
  in case Map.lookup cn (entryTriggersInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerEntry (env { entryTriggersInfo = Map.insert cn mapeinfo' (entryTriggersInfo env) 
                                            , allTriggers = (tName tr,mn,EVEntry,cl:args tr):allTriggers env}) ppdate' (n+1) xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerEntry (env { entryTriggersInfo = Map.insert cn mapeinfo' (entryTriggersInfo env) 
                                      , allTriggers = (tName tr,mn,EVEntry, cl:args tr):allTriggers env}) ppdate' (n+1) xs

--Creates the info to be added in the environment and the ppDATE about the new exit trigger
createTriggerExit:: (ClassInfo,MethodName,(String,MethodName,[String])) -> Int -> ((ClassInfo,MethodName,(Id, String, [Args])), TriggerDef)
createTriggerExit (cn,mn,(rt,mn',xs)) n = 
 let trnm = mn ++ "_ppdex" 
     nvar = "cv" ++ "_" ++ (show n)
     cn'  = cn ++ " " ++ nvar
     ret  = "ret_ppd" ++ (show n) in
 if (mn == mn')
 then if (rt == "void")
      then let cpe  = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [])
               tr   = TriggerDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
           in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)), tr)
      else let cpe = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [BindId ret]) 
               tr  = TriggerDef trnm (map ((\[x,y] -> BindType x y).words) xs ++ [BindType rt ret]) cpe ""
           in ((cn,mn,(trnm, cn', (map ((\[x,y] -> Args x y).words) xs) ++ [Args rt ret])),tr)
 else error $ "Problem when creating an exit trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerExit :: Env -> PPDATE -> Int -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env, PPDATE)
addNewTriggerExit env ppdate _ []     = (env,ppdate)
addNewTriggerExit env ppdate n (x:xs) =
 let (p,tr) = createTriggerExit x n
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
     cl     = makeBind $ (\(x,y,z) -> y) v
 in case Map.lookup cn (exitTriggersInfo env) of
      Nothing -> let mapeinfo' = Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerExit (env { exitTriggersInfo = Map.insert cn mapeinfo' (exitTriggersInfo env)
                                           , allTriggers = (tName tr,mn, getCTVariation (compTrigger tr),cl:args tr):allTriggers env }) ppdate' (n+1) xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerExit (env { exitTriggersInfo = Map.insert cn mapeinfo' (exitTriggersInfo env)
                                     , allTriggers = (tName tr,mn, getCTVariation (compTrigger tr),cl:args tr):allTriggers env }) ppdate' (n+1) xs

addTrigger2ppDATE :: TriggerDef -> PPDATE -> PPDATE
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt [] [] [] PNIL (f:fs))) temps ci consts ms) =
 PPDATE imp (Global (Ctxt [] [] [] PNIL (addTrigger2Foreach (f:fs) tr))) temps ci consts ms
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt vars ies trs p for)) temps ci consts ms) =
 PPDATE imp (Global (Ctxt vars ies (tr:trs) p for)) temps ci consts ms

addTrigger2Foreach :: Foreaches -> TriggerDef -> Foreaches
addTrigger2Foreach [Foreach [Args t id] (Ctxt vars ies trs p for)] tr = 
 [Foreach [Args t id] (Ctxt vars ies (trs ++ [updateWhereTr tr (Args t id)]) p for)]

updateWhereTr :: TriggerDef -> Args -> TriggerDef
updateWhereTr (TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) w) (Args t id) = 
 if t == cn
 then TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) (id ++ " = " ++ id' ++ ";")
 else TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) (id ++ " = null ;")

makeBind :: String -> Bind
makeBind [] = error "f"
makeBind s  = (\[x,y] -> BindType x y) $ words s

