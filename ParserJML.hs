module ParserJML where

import Types
import UpgradePPDATE
import ErrM

----------------------
-- Class Invariants --
----------------------

getCInvariants :: CInvariants -> JML
getCInvariants []           = ""
getCInvariants (cinv:cinvs) = fromCInvariant2JML cinv ++ getCInvariants cinvs

fromCInvariant2JML :: CInvariant -> JML
fromCInvariant2JML (CI _ body) =
  "\n  /*@ public invariant\n" 
  ++ "    @ " ++ body ++ ";\n"
  ++ "    @*/ \n" 

-----------------
-- Contracts --
-----------------

-- For analysis of all the contracts at once version

getContracts' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, JML)]
getContracts' = genJMLConstsAll'

genJMLConstsAll' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, JML)]
genJMLConstsAll' ppd = 
 let (ppdate, _) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     cs          = contractsGet ppdate
 in map (\(x,z,y) -> (x, z,"  /*@ public normal_behaviour\n" ++ y ++ "    @*/\n")) $ genJMLConsts' cs []

genJMLConsts' :: Contracts -> [(MethodName, ClassInfo, JML)] -> [(MethodName, ClassInfo, JML)]
genJMLConsts' [] xs     = xs
genJMLConsts' (c:cs) xs = let mn = snd $ methodCN c
                              cl = fst $ methodCN c  
                          in genJMLConsts' cs (updateJMLForM' c mn cl xs)

updateJMLForM' :: Contract -> MethodName -> ClassInfo -> [(MethodName, ClassInfo, JML)] -> [(MethodName, ClassInfo, JML)]
updateJMLForM' c mn cl []                   = [(mn, cl, fromContract2JML' c)]
updateJMLForM' c mn cl ((mn', cl', jml):xs) = if (mn == mn' && cl == cl')
                                              then (mn', cl', jml ++ "    @\n    @ also\n    @\n" ++ fromContract2JML' c):xs
                                              else (mn', cl', jml):updateJMLForM' c mn cl xs

fromContract2JML' :: Contract -> JML
fromContract2JML' (Contract cn _ precon postcon assig _ _) =  
  "    @ " ++ requires' precon cn
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness

-- bool variable added to be able to identify the contract
-- associated to the jml specification when analysing the .xml file
requires' :: Pre -> ContractName -> String
requires' p cn = "requires " ++ cn ++ " && " ++ p ++ ";\n"


------------------------------------------------------
-- For one by one analysis of the contracts version --
------------------------------------------------------

getContract :: Contract -> JML
getContract (Contract _ _ precon postcon assig _ _) =
  "  /*@ public normal_behaviour\n"
  ++ "    @ " ++ requires precon 
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness  
  ++ "    @*/\n"

getContracts :: Contracts -> [(MethodName, JML)]
getContracts = genJMLConstsAll

genJMLConstsAll :: Contracts -> [(MethodName, JML)]
genJMLConstsAll cs = map (\(x,y) -> (x, "  /*@ public normal_behaviour\n" ++ y ++ "    @*/\n")) $ genJMLConsts cs []


genJMLConsts :: Contracts -> [(MethodName, JML)] -> [(MethodName, JML)]
genJMLConsts [] xs     = xs
genJMLConsts (c:cs) xs = let mn = snd $ methodCN c                         
                         in genJMLConsts cs (updateJMLForM c mn xs)

updateJMLForM :: Contract -> MethodName -> [(MethodName, JML)] -> [(MethodName, JML)]
updateJMLForM c mn []              = [(mn, fromContract2JML c)]
updateJMLForM c mn ((mn', jml):xs) = if (mn == mn')
                                     then (mn', jml ++ "    @\n  @ also\n" ++ fromContract2JML c):xs
                                     else (mn', jml):updateJMLForM c mn xs

fromContract2JML :: Contract -> JML
fromContract2JML (Contract _ _ precon postcon assig _ _) =  
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


