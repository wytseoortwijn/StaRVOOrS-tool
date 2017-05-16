module Optimisations where 


import Types
import CommonFunctions
import UpgradePPDATE
import DL2JML
import OptAvoidTrivialCond
import OptRecAway

--------------------------------------------------------------
-- Deductive verification to partially verify Hoare triples --
--------------------------------------------------------------

-- Remove Hoare triples which were fully proved --

optimisedProvenHTs :: HTriples -> (HTName -> a -> a) -> a -> a
optimisedProvenHTs [] f ps     = ps
optimisedProvenHTs (c:cs) f ps = f (htName c) $ optimisedProvenHTs cs f ps

refinePropertyOptTemplates :: HTName -> Templates -> Templates
refinePropertyOptTemplates _ TempNil       = TempNil
refinePropertyOptTemplates cn (Temp temps) = Temp $ map (refineTemplate cn) temps

refineTemplate :: HTName -> Template -> Template
refineTemplate cn temp = updateTemplateProp temp (removeStatesProp cn $ tempProp temp)

refinePropertyOptGlobal :: HTName -> Global -> Global
refinePropertyOptGlobal cn (Global ctxt) = Global $ refineContext cn ctxt

refineContext :: HTName -> Context -> Context
refineContext cn (Ctxt vars ies trigs prop fors) = 
 let prop' = removeStatesProp cn prop
 in Ctxt vars ies trigs prop' (map (refineForeach cn) fors)

refineForeach :: HTName -> Foreach -> Foreach
refineForeach cn foreach = updCtxtForeach foreach (refineContext cn (getCtxtForeach foreach))

removeStatesProp :: HTName -> Property -> Property
removeStatesProp _ PNIL               = PNIL
removeStatesProp _ p@(PINIT _ _ _ _)  = p
removeStatesProp cn prop = let States acc bad nor star = pStates prop
                               acc'   = map (\s -> removeStateProp s cn) acc
                               bad'   = map (\s -> removeStateProp s cn) bad
                               nor'   = map (\s -> removeStateProp s cn) nor
                               star'  = map (\s -> removeStateProp s cn) star
                               states = States acc' bad' nor' star'
                           in Property (pName prop) states (pTransitions prop) (removeStatesProp cn (pProps prop))

removeStateProp :: State -> HTName -> State
removeStateProp (State ns ic cns) cn = State ns ic (removePropInState cn cns)

removePropInState :: HTName -> [HTName] -> [HTName]
removePropInState cn []        = []
removePropInState cn (cn':cns) = if (cn == cn')
                                 then cns
                                 else cn':removePropInState cn cns


-- Strengthen preconditions --

strengthenPre :: HTriples -> HTriples
strengthenPre []      = []
strengthenPre (h:hts) = 
 let newpre = (removeSelf.head.newPRe) h
     pre'   = "(" ++ pre h ++ ") && " ++ newpre
 in if ((head.newPRe) h == "(true)")
    then h:strengthenPre hts
    else updatePre h pre':strengthenPre hts

---------------------------------------------------------------
-- Avoid strengthening Hoare triples with trivial conditions --
---------------------------------------------------------------

simplify :: [String] -> [String]
simplify = avoidStrengthening 

-----------------------------------------------------------------------
-- If a method is recursive, then translate away replicated automata --
-----------------------------------------------------------------------

checkIfRec :: MethodCN -> Env -> [(MethodCN,Bool)] -> (Bool,[(MethodCN,Bool)])
checkIfRec = checkIfRec'

generateRAOptimised :: HT -> Int -> Env -> Property
generateRAOptimised = generateRAOptimised'

