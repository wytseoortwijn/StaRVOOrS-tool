module OptRecAway(checkIfRec',generateRAOptimised') where 


import Types
import CommonFunctions
import UpgradePPDATE
import Language.Java.Syntax hiding(VarDecl)

-----------------------------------------------------------------------
-- If a method is recursive, then translate away replicated automata --
-----------------------------------------------------------------------

checkIfRec' :: MethodCN -> Env -> [(MethodCN,Bool)] -> (Bool,[(MethodCN,Bool)])
checkIfRec' mcn env acc = 
 let xs = [b | (mcn',b) <- acc , mcn == mcn']
 in if null xs
    then let minvs = getInvocationsInMethodBody mcn env
             rec   = False
         in if null minvs 
            then (False,(mcn,False):acc)
            else if directRec (mname mcn) minvs
                 then (True,(mcn,True):acc)
                 else (rec,(mcn,rec):acc)
    else (head xs,acc)

directRec :: MethodName -> [Exp] -> Bool
directRec mn []           = False
directRec mn (minv:minvs) = 
 case minv of
      MethodInv (MethodCall (Name [Ident id]) _)        -> id == mn
      MethodInv (PrimaryMethodCall This _ (Ident id) _) -> id == mn
      _                                                 -> directRec mn minvs

{- case minv of
      MethodCall name args                    -> undefined
      PrimaryMethodCall exp _ (Ident id) args -> undefined
      SuperMethodCall _ (Ident id) args       -> undefined
      ClassMethodCall _ _ (Ident id) args     -> undefined
      TypeMethodCall _ _ (Ident id) args      -> undefined
-}

getInvocationsInMethodBody :: MethodCN -> Env -> MethodInvocations
getInvocationsInMethodBody mcn env = 
 let mns = methodsInFiles env
 in getMethodInvocations mcn mns



generateRAOptimised' :: HT -> Int -> Env -> Property
generateRAOptimised' c n env = 
  Property (htName c)
           (States [State "start" InitNil []] 
                  [] 
                  [State "bad" InitNil []] 
                  [State "idle" InitNil []])
           (makeTransitions c n env)
           PNIL

makeTransitions :: HT -> Int -> Env -> Transitions
makeTransitions c n env =
   let trig    = tName $ getTriggerDef (overl $ methodCN c) c (allTriggers env) 
       bs      = snd $ getValue $ lookForAllExitTriggerArgs env c
       cn      = htName c
       oldExpM = oldExpTypes env
       zs      = if getOldExpr oldExpM cn == "" then "" else ",oldExpAux"
       nvar    = if null zs then "" else "CopyUtilsPPD.copy(msgPPD.getOldExpr(),oldExpAux); "
       idle_to_postok = Arrow trig ("HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_postOK \\n \");")
       idle_to_bad    = Arrow trig ("!HoareTriplesPPD." ++ cn ++ "_post(" ++ bs ++ zs ++ ")") ("System.out.println(\"    " ++ cn ++ "_bad \\n \");")
       start_to_idle  = Arrow ("rh" ++ show n) "" (nvar ++ "System.out.println(\"    " ++ cn ++ "_preOK \\n\");")
   in [Transition "start" start_to_idle "idle",
      Transition "idle" idle_to_postok "start",
      Transition "idle" idle_to_bad "bad"
      ]
