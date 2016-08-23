module ParserJML where

import Types
import UpgradePPDATE
import ErrM

----------------------
-- Class Invariants --
----------------------

getCInvariants :: CInvariants -> String
getCInvariants []           = ""
getCInvariants (cinv:cinvs) = fromCInvariant2JML cinv ++ getCInvariants cinvs

fromCInvariant2JML :: CInvariant -> String
fromCInvariant2JML (CI _ body) =
  "\n  /*@ public invariant\n" 
  ++ "    @ " ++ body ++ ";\n"
  ++ "    @*/ \n" 

-------------------
-- Hoare Triples --
-------------------

-- For analysis of all the Hoare triples at once version

getContracts' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, String)]
getContracts' = genJMLConstsAll'

genJMLConstsAll' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, String)]
genJMLConstsAll' ppd = 
 let (ppdate, _) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     cs          = contractsGet ppdate
 in map (\(x,z,y) -> (x, z,"  /*@ public normal_behaviour\n" ++ y ++ "    @*/\n")) $ genJMLConsts' cs []

genJMLConsts' :: Contracts -> [(MethodName, ClassInfo, String)] -> [(MethodName, ClassInfo, String)]
genJMLConsts' [] xs     = xs
genJMLConsts' (c:cs) xs = let mn = snd $ methodCN c
                              cl = fst $ methodCN c  
                          in genJMLConsts' cs (updateJMLForM' c mn cl xs)

updateJMLForM' :: Contract -> MethodName -> ClassInfo -> [(MethodName, ClassInfo, String)] -> [(MethodName, ClassInfo, String)]
updateJMLForM' c mn cl []                   = [(mn, cl, fromContract2JML' c)]
updateJMLForM' c mn cl ((mn', cl', jml):xs) = if (mn == mn' && cl == cl')
                                              then (mn', cl', jml ++ "    @\n    @ also\n    @\n" ++ fromContract2JML' c):xs
                                              else (mn', cl', jml):updateJMLForM' c mn cl xs

fromContract2JML' :: Contract -> String
fromContract2JML' (Contract cn _ precon postcon assig _ _ _) =  
  "    @ " ++ requires' precon cn
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness

-- bool variable added to be able to identify the contract
-- associated to the jml specification when analysing the .xml file
requires' :: Pre -> ContractName -> String
requires' p cn = "requires " ++ cn ++ " && " ++ p ++ ";\n"


----------------------------------------------------------
-- For one by one analysis of the Hoare triples version --
----------------------------------------------------------

getContract :: Contract -> String
getContract (Contract _ _ precon postcon assig _ _ _) =
  "  /*@ public normal_behaviour\n"
  ++ "    @ " ++ requires precon 
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness  
  ++ "    @*/\n"

getContracts :: Contracts -> [(MethodName, String)]
getContracts = genJMLConstsAll

genJMLConstsAll :: Contracts -> [(MethodName, String)]
genJMLConstsAll cs = map (\(x,y) -> (x, "  /*@ public normal_behaviour\n" ++ y ++ "    @*/\n")) $ genJMLConsts cs []


genJMLConsts :: Contracts -> [(MethodName, String)] -> [(MethodName, String)]
genJMLConsts [] xs     = xs
genJMLConsts (c:cs) xs = let mn = snd $ methodCN c                         
                         in genJMLConsts cs (updateJMLForM c mn xs)

updateJMLForM :: Contract -> MethodName -> [(MethodName, String)] -> [(MethodName, String)]
updateJMLForM c mn []              = [(mn, fromContract2JML c)]
updateJMLForM c mn ((mn', jml):xs) = if (mn == mn')
                                     then (mn', jml ++ "    @\n  @ also\n" ++ fromContract2JML c):xs
                                     else (mn', jml):updateJMLForM c mn xs

fromContract2JML :: Contract -> String
fromContract2JML (Contract _ _ precon postcon assig _ _ _) =  
  "    @ " ++ requires precon 
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness


requires :: Pre -> String
requires p = "requires " ++ p ++ ";\n"

ensures :: Post -> String
ensures p = "ensures " ++ p ++ ";\n"

assign :: Assignable -> String
assign ass = "assignable " ++ ass ++ ";\n"


