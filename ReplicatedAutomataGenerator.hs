module ReplicatedAutomataGenerator(generateRA, lookfor) where

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
          (States [State "postOK" InitNil []] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []] 
                  [State "start" InitNil []])
          (makeTransitions c n env)
          PNIL

makeTransitions :: HT -> Int -> Env -> Transitions
makeTransitions c n env =
   let trig    = lookForExitTrigger (allTriggers env) (snd $ methodCN c)
       esinf   = map fromJust $ filter (/= Nothing) $ map getInfoTrigger (allTriggers env)
       cn      = htName c
       oldExpM = oldExpTypes env
       esinf'  = filter (/="") $ lookfor esinf trig
       arg     = if null esinf' 
                 then ""
                 else init $ foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail.words) $ esinf'
       checkId = "id.equals(idAux) && "
       zs      = if getOldExpr oldExpM cn == "" then "" else ",oldExpAux"
       nvar    = if null zs then "" else "CopyUtilsPPD.copy(MessagesOld_" ++ cn ++ ".getOldExpr(msg),oldExpAux); "
       idle_to_postok = Arrow trig (checkId ++ "HoareTriplesPPD." ++ cn ++ "_post(" ++ arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow trig (checkId ++ "!HoareTriplesPPD." ++ cn ++ "_post(" ++ arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" ("CopyUtilsPPD.copy(MessagesPPD.getId(msg),idAux) ; " ++ nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "postOK",
      Transition "idle" idle_to_bad "bad"
      ]

