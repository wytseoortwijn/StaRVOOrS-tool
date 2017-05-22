module RefinementPPDATE (specRefinement,refinePPDATE, getClassVar,generateNewTriggers) where

import Types
import CommonFunctions
import DL2JML
import qualified Data.Map as Map
import UpgradePPDATE
import Data.List
import Data.Maybe
import Optimisations
import qualified ParserAct as ParAct
import ErrM
import qualified PrintActions as PrintAct
import TranslatorActions


specRefinement :: UpgradePPD PPDATE -> Either [Proof] [Proof] -> Filename -> FilePath -> IO (UpgradePPD PPDATE)
specRefinement ppdate (Left []) _ _  = return ppdate
specRefinement ppdate (Right []) _ _ =
 if (null (htsGet $ getValue ppdate))
 then return ppdate
 else let ppdref = generateNewTriggers ppdate (htsGet $ getValue ppdate)  
      in return $ translateActions $ replacePInit $ namedCreateActPPD ppdref
specRefinement ppdate (Right proofs) fn output_addr =
 do let ppdref  = refinePPDATE ppdate proofs
    let ppdref' = prepareRefPPD ppdref
    let ppdate' = translateActions $ replacePInit $ namedCreateActPPD ppdref
    let refFile = output_addr ++ generateRefPPDFileName fn
    writeFile refFile (writePPD ppdref')
    return ppdate'

-------------------------
-- Auxiliary functions --
-------------------------

generateRefPPDFileName :: Filename -> Filename
generateRefPPDFileName fn = 
 let (ext, _:name) = break ('.' ==) $ reverse fn 
     xs = splitOnIdentifier "/" name     
 in if (length xs == 1)
    then reverse name ++ "_optimised.ppd"
    else reverse (head xs) ++ "_optimised.ppd"

prepareRefPPD :: UpgradePPD PPDATE -> UpgradePPD PPDATE
prepareRefPPD = removeGeneratedTriggers

removeGeneratedTriggers :: UpgradePPD PPDATE -> UpgradePPD PPDATE
removeGeneratedTriggers ppd = 
 do ppdate <- ppd
    return $ remGeneratedTriggers ppdate

remGeneratedTriggers :: PPDATE -> PPDATE
remGeneratedTriggers ppdate@(PPDATE _ (Global ctxt@(Ctxt [] [] [] PNIL (foreach:fors))) _ _ _ _) = 
 let ctxt''  = removeFromTrsCtxt (getCtxtForeach foreach)
     fors'   = (updCtxtForeach foreach ctxt''):fors
     ctxt''' = updateCtxtFors ctxt fors'
     global' = Global ctxt'''
 in updateGlobalPP ppdate global'
remGeneratedTriggers ppdate@(PPDATE _ (Global ctxt) _ _ _ _) = 
 let ctxt'   = removeFromTrsCtxt ctxt
     global' = Global ctxt'
 in updateGlobalPP ppdate global'

removeFromTrsCtxt :: Context -> Context 
removeFromTrsCtxt ctxt@(Ctxt _ _ trs _ _) = updateCtxtTrs ctxt (removeFromTriggers trs)

removeFromTriggers :: Triggers -> Triggers
removeFromTriggers []       = []
removeFromTriggers (tr:trs) = 
 if isInfixOf "_ppden" (tName tr) || isInfixOf "_ppdex" (tName tr)
 then removeFromTriggers trs
 else tr:removeFromTriggers trs

-----------------------
-- ppDATE refinement --
-----------------------

refinePPDATE :: UpgradePPD PPDATE -> [Proof] -> UpgradePPD PPDATE
refinePPDATE ppd proofs = 
 let ppdate    = getValue ppd 
     env       = getEnvVal ppd
     consts    = htsGet ppdate
     nproved   = filter (\(x,y,z,t) -> (not.null) z) $ map getInfoFromProof proofs
     proved    = filter (\(x,y,z,t) -> (null z)) $ map getInfoFromProof proofs
     consts'   = [c | c <- consts, (x,y,z,t) <- nproved, htName c == y]  
     cproved   = [c | c <- consts, (x,y,z,t) <- proved, htName c == y]  
     ppd'      = generateNewTriggers ppd consts'
     ppdate'   = getValue ppd'
     global    = globalGet ppdate'
     triggers' = getAllTriggers global env
     consts''  = strengthenPre $ updateHTs nproved consts' triggers'
     env'      = getEnvVal ppd'     
     global'   = optimisedProvenHTs cproved refinePropertyOptGlobal global
     temps     = templatesGet ppdate'
     temps'    = optimisedProvenHTs cproved refinePropertyOptTemplates temps
     ppdate''  = updateTemplatesPP (updateHTsPP (updateGlobalPP ppdate' global') consts'') temps'
 in do put env'
       return ppdate''

-----------------------------------------------------------------
-- Generate name for the channels associated to actions create --
-----------------------------------------------------------------

namedCreateActPPD :: UpgradePPD PPDATE -> UpgradePPD PPDATE
namedCreateActPPD ppd = 
 do let env = getEnvVal ppd
    put (namedCreateAct env)
    return (getValue ppd)

namedCreateAct :: Env -> Env
namedCreateAct env =
 let xs = allCreateAct env 
     ys = genChannelNames $ zip xs [1..length xs]
 in env { allCreateAct = ys}

genChannelNames :: [(CreateActInfo,Int)] -> [CreateActInfo]
genChannelNames []                 = []
genChannelNames ((cai,n):xs) = (cai {caiCh = "cact"++show n}):genChannelNames xs

---
--
---

translateActions :: UpgradePPD PPDATE -> UpgradePPD PPDATE
translateActions ppd =
 do ppdate <- ppd
    return $ translateActInPPD ppdate (getEnvVal ppd)

translateActInPPD :: PPDATE -> Env -> PPDATE
translateActInPPD (PPDATE imps global temps cinvs hts ms) env = 
 PPDATE imps (translateActInGlobal global env) (translateActInTemps temps env) cinvs hts ms


translateActInGlobal :: Global -> Env -> Global
translateActInGlobal (Global ctxt) env = Global (translateActInCtxt ctxt env)

translateActInCtxt :: Context -> Env -> Context
translateActInCtxt ctxt env = 
 let prop' = translateActInProps (property ctxt) env
     fors' = translateActInFors env (foreaches ctxt)
 in updateCtxtProps (updateCtxtFors ctxt fors') prop'
 
translateActInProps :: Property -> Env -> Property
translateActInProps PNIL _                            = PNIL
translateActInProps (PINIT nm tmp bnds props) env     = PINIT nm tmp bnds (translateActInProps props env)
translateActInProps (Property nm sts trans props) env = Property nm sts (translateActInTrans env trans) (translateActInProps props env)

translateActInFors :: Env -> Foreaches -> Foreaches
translateActInFors env = map (translateActInFor env)

translateActInFor :: Env -> Foreach -> Foreach
translateActInFor env foreach = updCtxtForeach foreach (translateActInCtxt (getCtxtForeach foreach) env)

translateActInTrans :: Env -> Transitions -> Transitions
translateActInTrans env = map (translateActInTran env)

translateActInTran :: Env -> Transition -> Transition
translateActInTran env (Transition q (Arrow tr cond act) q') =
 Transition q (Arrow tr cond (translateAction act env)) q'

translateAction :: Action -> Env -> Action
translateAction [] _    = ""
translateAction act env = 
 case ParAct.parse act of 
      Ok ac -> PrintAct.printTree (translateAct ac env)

translateActInTemps :: Templates -> Env -> Templates
translateActInTemps TempNil _       = TempNil
translateActInTemps (Temp tmps) env = Temp $ map (translateActInTemp env) tmps

translateActInTemp :: Env -> Template -> Template
translateActInTemp env tmp = 
 updateTemplateProp tmp (translateActInProps (tempProp tmp) env)

------------------------------------------------------
-- Get information from the results produced by KeY --
------------------------------------------------------

updateHTs :: [(MethodName, HTName, [Pre],String)] -> HTriples -> Triggers -> HTriples
updateHTs [] consts _      = consts
updateHTs (x:xs) consts es = updateHTs xs (updateHT x consts es) es

updateHT :: (MethodName, HTName, [Pre],String) -> HTriples -> Triggers -> HTriples
updateHT (mn,cn,pres,path) [] _      = []
updateHT (mn,cn,pres,path) (c:cs) es = 
 if (htName c == cn && (mname.methodCN) c == mn)
 then if (null pres)
      then c:updateHT (mn,cn,pres,path) cs es
      else let pres' = removeDuplicates pres
               opt'  = simplify $ map refineNewCond pres'
               opt'' = '(':introduceOr opt' ++ [')']
               c'    = updatePath (updateNewPre c [opt'']) path
               c''   = updatePre c' $ refineCond (pre c)
               c'''  = updatePost c'' $ refineCond (post c)
           in c''':cs
 else c:updateHT (mn,cn,pres,path) cs es

getClassVar :: HT -> Triggers -> TriggerVariation -> String
getClassVar c es ev = lookupClassVar es c ev

refineNewCond :: Pre -> Pre
refineNewCond = addParenthesisNot . removeDLstrContent

refineCond :: Pre -> Pre
refineCond = removeDLstrContent


-- returns variable name used to instantiate the class in the ppDATE
lookupClassVar :: Triggers -> HT -> TriggerVariation -> String
lookupClassVar trs c ev = 
 let trs' = filterTriggers trs c ev
 in case getGeneratedExTr trs' c of
         []   -> getExTr trs' c ev
         [id] -> id

filterTriggers :: Triggers -> HT -> TriggerVariation -> Triggers
filterTriggers [] _ _      = []
filterTriggers (e:es) c ev = 
 case (compTrigger e) of
      NormalEvent (BindingVar b) id _ ev' -> 
           let mn = mname (methodCN c) 
               ov = overl (methodCN c)
           in if (id == mn && compareEV ev ev'
                 && checkArgsOver (args e) (getCTArgs (compTrigger e)) (overl $ methodCN c))
              then e:filterTriggers es c ev
              else filterTriggers es c ev
      _                                   -> filterTriggers es c ev

getGeneratedExTr :: Triggers -> HT -> [String]
getGeneratedExTr [] _       = []
getGeneratedExTr (tr:trs) c = 
  case (compTrigger tr) of
      NormalEvent (BindingVar b) id _ ev' -> 
                  if (isInfixOf "_ppdex" (tName tr) && checkArgsOver (args tr) (getCTArgs (compTrigger tr)) (overl $ methodCN c)) 
                  then case b of
                            BindStar      -> []
                            BindType _ id -> [id]
                            BindId id     -> [id]
                  else getGeneratedExTr trs c

getExTr :: Triggers -> HT -> TriggerVariation -> String
getExTr [] _ _      = ""
getExTr (e:es) c ev = 
 case (compTrigger e) of
      NormalEvent (BindingVar b) id _ ev' -> 
                  if (id == mname (methodCN c) && compareEV ev ev')
                  then case b of
                            BindStar      -> ""
                            BindType _ id -> id
                            BindId id     -> id
                  else getExTr es c ev
      _                                   -> getExTr es c ev

checkArgsOver :: [Bind] -> [Bind] -> Overriding -> Bool
checkArgsOver _ _ OverNil  = True
checkArgsOver bs ceargs ov =
 let ov' = generateOverloading bs ceargs
 in ov == ov'
 

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
  do let env    = getEnvVal ppd
     let ppdate = getValue ppd     
     let mfiles = javaFilesInfo env
     let mns    = removeDuplicates $ map methodCN consts
     let entry  = filterDefEntryTriggers mns (allTriggers env) 
     let entry' = [(mnc, checkOverloading (filter (\(_,mn,_,_) -> mname mnc == mn) (methodsInFiles z)) (overl mnc)) | mnc <- entry, (_,d,z) <- mfiles,d == clinf mnc]
     let exit   = [(mnc, checkOverloading (filter (\(_,mn,_,_) -> mname mnc == mn) (methodsInFiles z)) (overl mnc)) | mnc <- mns, (_,d,z) <- mfiles,d == clinf mnc]
     let scope  = properScope ppdate
     let (env',ppdate') = addNewTriggerEntry env 0 entry' ppdate scope
     let env''  = addNewTriggerExit env' (length entry') exit scope     
     put env''
     return ppdate'

checkOverloading :: [(String,MethodName,[String],MethodInvocations)] -> Overriding -> (String,MethodName,[String],MethodInvocations)
checkOverloading [] _                          = error "Problem with overloading.\n"
checkOverloading (val@(_,_,args,_):xs) OverNil = val
checkOverloading (val@(_,_,args,_):xs) ov      = 
 if Over (map (getBindTypeType.makeBind) args) == ov
 then val
 else checkOverloading xs ov

--Filter methods associated to entry triggers defined at top-level
filterDefEntryTriggers :: [MethodCN] -> [TriggersInfo] -> [MethodCN]
filterDefEntryTriggers [] _                                = []
filterDefEntryTriggers (mnc@(MCN ci mn OverNil):mncs) ts   = 
 if null [ t | t <- ts , tiCI t == ci, tiMN t == mn, tiTrvar t == EVEntry]
 then mnc:filterDefEntryTriggers mncs ts
 else filterDefEntryTriggers mncs ts
filterDefEntryTriggers (mnc@(MCN ci mn (Over xs)):mncs) ts = 
 if null [ t | t <- ts , tiCI t == ci, tiMN t == mn, tiTrvar t == EVEntry, tiOver t == (Over xs)]
 then mnc:filterDefEntryTriggers mncs ts
 else filterDefEntryTriggers mncs ts

--Creates the info to be added in the environment
createTriggerEntry :: (MethodCN,(String,MethodName,[String],MethodInvocations)) -> Int -> Scope -> TriggersInfo
createTriggerEntry (mnc,(rt,mn',xs,_)) n scope = 
 let mn = mname mnc
     cn = clinf mnc 
     ov = overl mnc
 in if (mn == mn')
    then let trnm = mn ++ "_ppden"++ (show n)
             nvar = "cv" ++ "_" ++ (show n)
             cn'  = cn ++ " " ++ nvar
             cpe  = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words.remGenerics') xs) EVEntry
             tr   = TriggerDef trnm (map ((\[x,y] -> BindType x y).words.remGenerics') xs) cpe ""
             bs   = map ((\[x,y] -> BindType x y).words.remGenerics') xs             
         in TI trnm mn cn nvar EVEntry bs (Just tr) scope ov
    else error $ "Problem when creating an entry trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerEntry :: Env -> Int -> [(MethodCN,(String,MethodName,[String],MethodInvocations))] -> PPDATE -> Scope -> (Env,PPDATE)
addNewTriggerEntry env _ [] ppd _         = (env,ppd)
addNewTriggerEntry env n (x:xs) ppd scope =
 let tinfo = createTriggerEntry x n scope
     ppd'  = addTrigger2ppDATE (fromJust (tiTrDef tinfo)) ppd
 in addNewTriggerEntry (env { allTriggers = tinfo:allTriggers env}) (n+1) xs ppd' scope


--Creates the info to be added in the environment
createTriggerExit:: (MethodCN,(String,MethodName,[String],MethodInvocations)) -> Int -> Scope -> TriggersInfo
createTriggerExit (mnc,(rt,mn',xs',_)) n scope = 
 let mn = mname mnc
     cn = clinf mnc
     ov = overl mnc
     trnm = mn ++ "_ppdex"
     nvar = "cv" ++ "_" ++ (show n)
     cn'  = cn ++ " " ++ nvar
     ret  = "ret_ppd" ++ (show n)
     xs   = map remGenerics' xs' in
 if (mn == mn')
 then if (rt == "void")
      then let bs  = map ((\[x,y] -> BindType x y).words.remGenerics') xs
               cpe = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words.remGenerics') xs) (EVExit [])
               tr  = TriggerDef trnm bs cpe ""
           in TI trnm mn cn nvar (EVExit []) bs (Just tr) scope ov
      else let bs  = (map ((\[x,y] -> BindType x y).words) xs ++ [BindType rt ret])
               cpe = NormalEvent (BindingVar (BindType cn nvar)) mn (map ((\[x,y] -> BindId y).words.remGenerics') xs) (EVExit [BindId ret]) 
               tr  = TriggerDef trnm bs cpe ""
           in TI trnm mn cn nvar (EVExit [BindId ret]) bs (Just tr) scope ov
 else error $ "Problem when creating an exit trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerExit :: Env -> Int -> [(MethodCN,(String,MethodName,[String],MethodInvocations))] -> Scope -> Env
addNewTriggerExit env _ [] _         = env
addNewTriggerExit env n (x:xs) scope =
 let tinfo  = createTriggerExit x n scope
 in addNewTriggerExit (env { allTriggers = tinfo:allTriggers env }) (n+1) xs scope


addTrigger2ppDATE :: TriggerDef -> PPDATE -> PPDATE
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt [] [] [] PNIL (f:fs))) temps ci consts ms) =
 PPDATE imp (Global (Ctxt [] [] [] PNIL (addTrigger2Foreach (f:fs) tr))) temps ci consts ms
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt vars ies trs p for)) temps ci consts ms) =
 PPDATE imp (Global (Ctxt vars ies (tr:trs) p for)) temps ci consts ms

addTrigger2Foreach :: Foreaches -> TriggerDef -> Foreaches
addTrigger2Foreach [Foreach [Args t id] (Ctxt vars ies trs p for) id'] tr = 
 [Foreach [Args t id] (Ctxt vars ies (trs ++ [updateWhereTr tr (Args t id)]) p for) id']

updateWhereTr :: TriggerDef -> Args -> TriggerDef
updateWhereTr (TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) w) (Args t id) = 
 if t == cn
 then TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) (id ++ " = " ++ cl ++ ";")
 else TriggerDef tn args (NormalEvent (BindingVar (BindType cn cl)) id' xs v) (id ++ " = null ;")

makeBind :: String -> Bind
makeBind [] = error "Cannot make bind\n."
makeBind s  = (\[x,y] -> BindType x y) $ words s

--Removes generics from the type of the arguments
remGenerics' :: String -> String
remGenerics' s = 
 let ys = splitAtIdentifier '<' s
 in if (not.null.snd) ys 
    then let xs = (splitAtIdentifier '>'.reverse.tail.snd) ys in
         if (not.null.snd) xs
         then fst ys ++ (reverse $ fst xs)
         else error "Problem with generics.\n"
    else s

--Returns the scope where the generated triggers should be placed
properScope :: PPDATE -> Scope
properScope ppd = 
 let global = ctxtGet $ globalGet ppd
     vars   = variables global
     actes  = actevents global
     trs    = triggers global
     props  = property global
 in if and [null vars,null actes,null trs,PNIL == props]
    then InFor (ForId "TopLevel")
    else TopLevel

