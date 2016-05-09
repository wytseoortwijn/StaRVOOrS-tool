module RefinementPPDATE (refinePPDATE, getClassVar) where

import Types
import CommonFunctions
import DL2JML


-----------------------
-- ppDATE refinement --
-----------------------

refinePPDATE :: PPDATE -> [Proof] -> PPDATE
refinePPDATE (PPDATE imports global cinvs consts methods) proofs = 
 let events'             = getAllEvents global 
     newinfo             = map getInfoFromProof proofs
     consts'             = updateContracts newinfo consts events'
     (consts'', global') = optimizedProvenContracts consts' global
 in PPDATE imports global' cinvs consts'' methods


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
                                               else let clvar = getClassVar c es
                                                        pres' = removeDuplicates pres
                                                        opt'  = map (addParenthesisNot.(replaceSelfWith clvar)) pres'
                                                        opt'' = '(':introduceOr opt' ++ [')']
                                                    in (updateOpt c [opt'']):cs
                                          else c:updateContract (mn,cn,pres) cs es

getClassVar :: Contract -> Events -> String
getClassVar c es = lookupClassVar es c

-- returns variable name used to instantiate the class in the ppDATE
lookupClassVar :: Events -> Contract -> String
lookupClassVar [] _     = ""
lookupClassVar (e:es) c =
 case (compEvent e) of
      NormalEvent (BindingVar b) id _ _ -> 
                  if (id == snd (methodCN c))
                  then case b of
                            BindStar      -> ""
                            BindType _ id -> id
                            BindId id     -> id
                  else lookupClassVar es c
      ClockEvent id _        -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c
      OnlyId id              -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c
      OnlyIdPar id           -> if (id == snd (methodCN c))
                                then ""
                                else lookupClassVar es c
      otherwise              -> lookupClassVar es c

