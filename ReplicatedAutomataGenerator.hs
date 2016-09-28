module ReplicatedAutomataGenerator(generateRA, lookfor) where

import Types
import CommonFunctions
import RefinementPPDATE
import UpgradePPDATE
import qualified Data.Map as Map


generateRA :: [(Trigger, [String])] -> Triggers -> HT -> Int -> Env -> Property
generateRA = makeReplicateAutomaton

makeReplicateAutomaton :: [(Trigger, [String])] -> Triggers -> HT -> Int -> Env -> Property
makeReplicateAutomaton esinf es c n env = 
 Property (htName c)
          (States [State "postOK" InitNil []] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []] 
                  [State "start" InitNil []])
          (makeTransitions c n esinf es env)
          PNIL


lookfor :: [(Trigger, [String])] -> Trigger -> [String]
lookfor [] _     = []
lookfor (x:xs) e = if (fst x==e)
                   then snd x
                   else lookfor xs e

makeTransitions :: HT -> Int -> [(Trigger, [String])] -> Triggers -> Env -> Transitions
makeTransitions c n esinf es env =
   let trig     = lookForExitTrigger es (snd $ methodCN c)
       cn       = htName c
       oldExpM  = oldExpTypes env
       arg      = foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf trig
       checkId  = "id.equals(idAux) && "
       zs       = getOldExpr oldExpM cn  
       nvar     = if null zs then "" else "CopyUtilsPPD.copy(MessagesOld_" ++ cn ++ ".getOldExpr(msg),oldExpAux); "
       idle_to_postok = Arrow trig (checkId ++ "HoareTriplesPPD." ++ cn ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow trig (checkId ++ "!HoareTriplesPPD." ++ cn ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" ("CopyUtilsPPD.copy(MessagesPPD.getId(msg),idAux) ; " ++ nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "postOK",
      Transition "idle" idle_to_bad "bad"
      ]


