module ReplicatedAutomataGenerator(generateRA, lookfor) where

import Types
import CommonFunctions
import RefinementPPDATE
import UpgradePPDATE
import qualified Data.Map as Map


generateRA :: [(Event, [String])] -> Events -> Contract -> Int -> Env -> Property
generateRA = makeReplicateAutomaton

makeReplicateAutomaton :: [(Event, [String])] -> Events -> Contract -> Int -> Env -> Property
makeReplicateAutomaton esinf es c n env = 
 Property (contractName c)
          (States [State "postOK" InitNil []] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []] 
                  [State "start" InitNil []])
          (makeTransitions c n esinf es env)
          PNIL


lookfor :: [(Event, [String])] -> Event -> [String]
lookfor [] _     = []
lookfor (x:xs) e = if (fst x==e)
                   then snd x
                   else lookfor xs e

makeTransitions :: Contract -> Int -> [(Event, [String])] -> Events -> Env -> Transitions
makeTransitions c n esinf es env =
   let event    = lookForExitEvent es (snd $ methodCN c)
       cn       = contractName c
       oldExpM  = oldExpTypes env
       arg      = foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf event
       checkId  = "id.equals(idAux) && "
       zs       = getOldExpr oldExpM cn  
       nvar     = if null zs then "" else "oldExpAux = MessagesOld_" ++ cn ++ ".getOldExpr(msg); "
       idle_to_postok = Arrow event (checkId ++ "HoareTriples." ++ cn ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow event (checkId ++ "!HoareTriples." ++ cn ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" ("idAux = Messages.getId(msg) ; " ++ nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "postOK",
      Transition "idle" idle_to_bad "bad"
      ]


