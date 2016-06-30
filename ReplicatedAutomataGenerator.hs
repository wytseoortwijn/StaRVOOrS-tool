module ReplicatedAutomataGenerator(generateRA, lookfor) where

import Types
import CommonFunctions
import RefinementPPDATE


lookfor :: [(Event, [String])] -> Event -> [String]
lookfor [] _     = []
lookfor (x:xs) e = if (fst x==e)
                   then snd x
                   else lookfor xs e

makeTransitions :: Contract -> Int -> [(Event, [String])] -> Events -> Transitions
makeTransitions c n esinf es =
   let event    = lookForExitEvent es mn
       p'       = post c
       consName = (contractName c)
       mn       = (snd $ methodCN c)
       arg      = foldr (\x xs -> x ++ "," ++ xs) "" $ map (head.tail) $ map words $ lookfor esinf event
       xs       = splitOnIdentifier consName p'
   in if (length xs == 1)
      then let checkId = "id.equals(idAux) && "
               idle_to_postok = Arrow event (checkId ++ "HoareTriples." ++ consName ++ "_post(" ++ init arg ++ ")") ("System.out.println(\"    " ++ consName ++ "_postOK \\n \");")
               idle_to_bad    = Arrow event (checkId ++ "!HoareTriples." ++ consName ++ "_post(" ++ init arg ++ ")") ("System.out.println(\"    " ++ consName ++ "_bad \\n \");")
               start_to_idle  = Arrow ("rh" ++ show n) "" ("idAux = new Integer(idfrom) ; " ++ "System.out.println(\"    " ++ consName ++ "_preOK \\n\");")
           in [Transition "start" start_to_idle "idle",
              Transition "idle" idle_to_postok "postOK",
              Transition "idle" idle_to_bad "bad"
              ]
      else let ident = "_nyckelord"
               ys    = map (splitOnIdentifier ident) (tail xs)
               ys'   = removeDuplicates $ map head ys
               zs    = concat $ map (\xs ->  "," ++ consName ++ xs ++ ident) ys'
               zs'   = if (null zs) then zs else init zs
               idle_to_postok = Arrow event ("HoareTriples." ++ consName ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ consName ++ "_postOK \\n \");")
               idle_to_bad    = Arrow event ("!HoareTriples." ++ consName ++ "_post(" ++ init arg ++ zs ++ ")") ("System.out.println(\"    " ++ consName ++ "_bad \\n \");")
               start_to_idle  = Arrow ("rh" ++ show n) "" ("System.out.println(\"    " ++ consName ++ "_preOK \\n\");")
           in [Transition "start" start_to_idle "idle",
              Transition "idle" idle_to_postok "postOK",
              Transition "idle" idle_to_bad "bad"
              ]


makeReplicateAutomaton :: [(Event, [String])] -> Events -> Contract -> Int -> Property
makeReplicateAutomaton esinf es c n = Property (contractName c)
                                               (States [State "postOK" InitNil []] [State "bad" InitNil []] [State "idle" InitNil []] [State "start" InitNil []])
                                               (makeTransitions c n esinf es)
                                               PNIL

generateRA :: [(Event, [String])] -> Events -> Contract -> Int -> Property
generateRA = makeReplicateAutomaton
