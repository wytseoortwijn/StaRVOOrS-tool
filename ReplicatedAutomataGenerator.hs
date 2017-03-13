module ReplicatedAutomataGenerator(generateRA,generateRAOptimised) where

import Types
import CommonFunctions
import RefinementPPDATE
import UpgradePPDATE
import qualified Data.Map as Map
import Data.Maybe


generateRA :: HT -> Int -> Env -> Property
generateRA = makeReplicateAutomaton

makeReplicateAutomaton :: HT -> Int -> Env -> Property
makeReplicateAutomaton c n env = 
 Property (htName c)
          (States [State "start" InitNil []] 
                  [State "postOK" InitNil []] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []])
          (makeTransitions c n env)
          PNIL

makeTransitions :: HT -> Int -> Env -> Transitions
makeTransitions c n env =
   let mn      = mname $ methodCN c
       cl      = clinf $ methodCN c
       trig    = tName $ getTriggerDef (overl $ methodCN c) c (allTriggers env) 
       bs      = snd $ getValue $ lookForAllExitTriggerArgs env c
       cn      = htName c
       oldExpM = oldExpTypes env
       checkId = "id.equals(idAuxPPD) && "
       zs      = if getOldExpr oldExpM cn == "" then "" else ",oldExpAux"
       nvar    = if null zs then "" else "CopyUtilsPPD.copy(MessagesOld_" ++ cn ++ ".getOldExpr(msgPPD),oldExpAux); "
       idle_to_postok = Arrow trig (checkId ++ "HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow trig (checkId ++ "!HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" ("CopyUtilsPPD.copy(MessagesPPD.getId(msgPPD),idAuxPPD) ; " ++ nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "postOK",
      Transition "idle" idle_to_bad "bad"
      ]


generateRAOptimised :: HT -> Int -> Env -> Property
generateRAOptimised c n env = 
  Property (htName c)
           (States [State "start" InitNil []] 
                  [] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []])
           (makeTransitions' c n env)
           PNIL

makeTransitions' :: HT -> Int -> Env -> Transitions
makeTransitions' c n env =
   let mn      = mname $ methodCN c
       cl      = clinf $ methodCN c
       trig    = tName $ getTriggerDef (overl $ methodCN c) c (allTriggers env) 
       bs      = snd $ getValue $ lookForAllExitTriggerArgs env c
       cn      = htName c
       oldExpM = oldExpTypes env
       zs      = if getOldExpr oldExpM cn == "" then "" else ",oldExpAux"
       nvar    = if null zs then "" else "CopyUtilsPPD.copy(MessagesOld_" ++ cn ++ ".getOldExpr(msgPPD),oldExpAux); "
       idle_to_postok = Arrow trig ("HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow trig ("!HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" (nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "start",
      Transition "idle" idle_to_bad "bad"
      ]
