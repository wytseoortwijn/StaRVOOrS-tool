module RefinementPPDATE (refinePPDATE, getClassVar,generateNewTriggers) where

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
 let ppdate                = getValue ppd 
     consts                = contractsGet ppdate
     nproved               = filter (\(x,y,z) -> (not.null) z) $ map getInfoFromProof proofs
     proved                = filter (\(x,y,z) -> (null z)) $ map getInfoFromProof proofs
     consts'               = [c | c <- consts, (x,y,z) <- nproved, contractName c == y]  
     cproved               = [c | c <- consts, (x,y,z) <- proved, contractName c == y]  
     ppd'                  = generateNewTriggers ppd consts'
     ppdate'               = getValue ppd'
     global                = globalGet ppdate'
     events'               = getAllEvents global 
     consts''              = updateContracts nproved consts' events'
     env'                  = getEnvVal ppd'     
     global'               = globalGet ppdate'
     (consts''', global'') = optimizedProvenContracts cproved global'
 in do put env'
       return $ PPDATE (importsGet ppdate') global'' (cinvariantsGet ppdate') (consts'''++consts'') (methodsGet ppdate')

----------------------------------------------
-- Remove contracts which were fully proved --
----------------------------------------------

optimizedProvenContracts :: Contracts -> Global -> (Contracts, Global)
optimizedProvenContracts [] ps     = ([], ps)
optimizedProvenContracts (c:cs) ps = if (null $ optimized c)
                                     then (a, refinePropertyOpt (contractName c) b)
                                     else (c:a, b)
                                           where (a, b) = optimizedProvenContracts cs ps

refinePropertyOpt :: ContractName -> Global -> Global
refinePropertyOpt cn (Global (Ctxt vars es prop fors)) = 
 let prop' = removeStatesProp cn prop
 in case fors of
         []                  -> Global (Ctxt vars es prop' [])
         [Foreach args ctxt] -> Global (Ctxt vars es prop' [Foreach args (refineContext cn ctxt)])

refineContext :: ContractName -> Context -> Context
refineContext cn (Ctxt vars es prop fors) = 
 let prop' = removeStatesProp cn prop
 in case fors of
         []                  -> Ctxt vars es prop' [] 
         [Foreach args ctxt] -> Ctxt vars es prop' [Foreach args (refineContext cn ctxt)] 

removeStatesProp :: ContractName -> Property -> Property
removeStatesProp _ PNIL  = PNIL
removeStatesProp cn prop = let States acc bad nor star = pStates prop
                               acc'  = map (\s -> removeStateProp s cn) acc
                               bad'  = map (\s -> removeStateProp s cn) bad
                               nor'  = map (\s -> removeStateProp s cn) nor
                               star' = map (\s -> removeStateProp s cn) star
                               states = States acc' bad' nor' star'
                           in Property (pName prop) states (pTransitions prop) (removeStatesProp cn (pProps prop))

removeStateProp :: State -> ContractName -> State
removeStateProp (State ns ic cns) cn = State ns ic (removePropInState cn cns)

removePropInState :: ContractName -> [ContractName] -> [ContractName]
removePropInState cn []        = []
removePropInState cn (cn':cns) = if (cn == cn')
                                       then cns
                                       else cn':removePropInState cn cns


------------------------------------------------------
-- Get information from the results produced by KeY --
------------------------------------------------------

updateContracts :: [(MethodName, ContractName, [Pre])] -> Contracts -> Events -> Contracts
updateContracts [] consts _      = consts
updateContracts (x:xs) consts es = updateContracts xs (updateContract x consts es) es


updateContract :: (MethodName, ContractName, [Pre]) -> Contracts -> Events -> Contracts
updateContract (mn,cn,pres) [] _      = []
updateContract (mn,cn,pres) (c:cs) es = if (contractName c == cn && (snd.methodCN) c == mn)
                                          then if (null pres)
                                               then c:updateContract (mn,cn,pres) cs es
                                               else let clvar = getClassVar c es EVEntry
                                                        pres' = removeDuplicates pres
                                                        opt'  = map (addParenthesisNot.(replaceSelfWith clvar)) pres'
                                                        opt'' = '(':introduceOr opt' ++ [')']
                                                    in (updateOpt c [opt'']):cs
                                          else c:updateContract (mn,cn,pres) cs es

getClassVar :: Contract -> Events -> EventVariation -> String
getClassVar c es ev = lookupClassVar es c ev

-- returns variable name used to instantiate the class in the ppDATE
lookupClassVar :: Events -> Contract -> EventVariation -> String
lookupClassVar [] _ _      = ""
lookupClassVar (e:es) c ev = 
 case (compEvent e) of
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

compareEV :: EventVariation -> EventVariation -> Bool
compareEV EVEntry EVEntry         = True
compareEV (EVExit []) (EVExit []) = True
compareEV _ _                     = False

-----------------------------------------------------------------------------------------
-- Generate triggers whenever a method to be runtime verified is not associated to one --
-----------------------------------------------------------------------------------------

--TODO: Fix this method if classes with the same name in different paths are allowed
generateNewTriggers :: UpgradePPD PPDATE -> Contracts -> UpgradePPD PPDATE
generateNewTriggers ppd consts =
  do let env     = getEnvVal ppd
     let ppdate  = getValue ppd     
     let mfiles  = methodsInFiles env
     let mns     = removeDuplicates [mn | mn <- map methodCN consts]
     let entry   = filterDefinedTriggers (entryEventsInfo env) mns
     let exit    = filterDefinedTriggers (exitEventsInfo env) mns
     let entry'  = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- entry, (_,d,z) <- mfiles,d==x]
     let exit'   = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- exit, (_,d,z) <- mfiles,d==x]
     let (env',ppdate')   = addNewTriggerEntry env ppdate entry'
     let (env'',ppdate'') = addNewTriggerExit env' ppdate' 0 exit'
     put env''
     return ppdate''

     
filterDefinedTriggers :: Map.Map ClassInfo MapTrigger -> [(ClassInfo,MethodName)] -> [(ClassInfo,MethodName)]
filterDefinedTriggers mci []           = []
filterDefinedTriggers mci ((ci,mn):xs) = 
 case Map.lookup ci mci of
      Nothing -> (ci,mn):filterDefinedTriggers mci xs
      Just m  -> case Map.lookup mn m of
                      Nothing -> (ci,mn):filterDefinedTriggers mci xs
                      Just _  -> filterDefinedTriggers mci xs

--Creates the info to be added in the environment and the ppDATE about the new entry trigger
createTriggerEntry :: (ClassInfo,MethodName,(String,MethodName,[String])) -> ((ClassInfo,MethodName,(Id, String, [Args])),EventDef)
createTriggerEntry (cn,mn,(rt,mn',xs)) = 
 if (mn == mn')
 then let trnm = mn ++ "_en"
          cn'  = cn ++ " cv"
          cpe  = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) EVEntry
          tr   = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
      in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)),tr)
 else error $ "Problem when creating an entry trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerEntry :: Env -> PPDATE -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env,PPDATE)
addNewTriggerEntry env ppdate []     = (env,ppdate)
addNewTriggerEntry env ppdate (x:xs) =
 let (p,tr) = createTriggerEntry x
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
  in case Map.lookup cn (entryEventsInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerEntry (env { entryEventsInfo = Map.insert cn mapeinfo' (entryEventsInfo env) }) ppdate' xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerEntry (env { entryEventsInfo = Map.insert cn mapeinfo' (entryEventsInfo env) }) ppdate' xs

--Creates the info to be added in the environment and the ppDATE about the new exit trigger
createTriggerExit:: (ClassInfo,MethodName,(String,MethodName,[String])) -> Integer -> ((ClassInfo,MethodName,(Id, String, [Args])), EventDef)
createTriggerExit (cn,mn,(rt,mn',xs)) n = 
 let trnm = mn ++ "_ex" 
     cn'  = cn ++ " cv" 
     ret  = "r" ++ (show n) in
 if (mn == mn')
 then if (rt == "void")
      then let cpe  = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [])
               tr   = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
           in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)), tr)
      else let cpe = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [BindId ret]) 
               tr  = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
           in ((cn,mn,(trnm, cn', (map ((\[x,y] -> Args x y).words) xs) ++ [Args rt ret])),tr)
 else error $ "Problem when creating an exit trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerExit :: Env -> PPDATE -> Integer -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env, PPDATE)
addNewTriggerExit env ppdate _ []     = (env,ppdate)
addNewTriggerExit env ppdate n (x:xs) =
 let (p,tr) = createTriggerExit x n
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
 in case Map.lookup cn (exitEventsInfo env) of
      Nothing -> let mapeinfo' = Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerExit (env { exitEventsInfo = Map.insert cn mapeinfo' (exitEventsInfo env) }) ppdate' (n+1) xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerExit (env { exitEventsInfo = Map.insert cn mapeinfo' (exitEventsInfo env) }) ppdate' (n+1) xs

addTrigger2ppDATE :: EventDef -> PPDATE -> PPDATE
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt vars trs p for)) ci consts ms) =
 PPDATE imp (Global (Ctxt vars (tr:trs) p for)) ci consts ms

