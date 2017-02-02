module JMLGenerator where

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

getHTs' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, String)]
getHTs' = genJMLConstsAll'

genJMLConstsAll' :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, String)]
genJMLConstsAll' ppd = 
 let ppdate = getValue ppd
     cs     = htsGet ppdate
 in map (\(x,z,y) -> (x, z,"  /*@ \n" ++ y ++ "    @*/\n")) $ genJMLConsts' cs []

genJMLConsts' :: HTriples -> [(MethodName, ClassInfo, String)] -> [(MethodName, ClassInfo, String)]
genJMLConsts' [] xs     = xs
genJMLConsts' (c:cs) xs = let mn = snd $ methodCN c
                              cl = fst $ methodCN c  
                          in genJMLConsts' cs (updateJMLForM' c mn cl xs)

updateJMLForM' :: HT -> MethodName -> ClassInfo -> [(MethodName, ClassInfo, String)] -> [(MethodName, ClassInfo, String)]
updateJMLForM' c mn cl []                   = [(mn, cl, fromHT2JML' c)]
updateJMLForM' c mn cl ((mn', cl', jml):xs) = if (mn == mn' && cl == cl')
                                              then (mn', cl', jml ++ "    @\n    @ also\n    @\n" ++ fromHT2JML' c):xs
                                              else (mn', cl', jml):updateJMLForM' c mn cl xs

fromHT2JML' :: HT -> String
fromHT2JML' (HT cn _ precon postcon assig _ _ _) =  
  "    @ public normal_behaviour\n"
  ++ "    @ " ++ requires' precon cn
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness

-- bool variable added to be able to identify the Hoare triple
-- associated to the jml specification when analysing the .xml file
requires' :: Pre -> HTName -> String
requires' p cn = "requires " ++ cn ++ " && " ++ p ++ ";\n"


----------------------------------------------------------
-- For one by one analysis of the Hoare triples version --
----------------------------------------------------------

getHT :: HT -> String
getHT (HT _ _ precon postcon assig _ _ _) =
  "  /*@ public normal_behaviour\n"
  ++ "    @ " ++ requires precon 
  ++ "    @ " ++ ensures postcon
  ++ "    @ " ++ assign assig
  ++ "    @ diverges true;\n" -- partial correctness  
  ++ "    @*/\n"

getHTs :: HTriples -> [(MethodName, String)]
getHTs = genJMLConstsAll

genJMLConstsAll :: HTriples -> [(MethodName, String)]
genJMLConstsAll cs = map (\(x,y) -> (x, "  /*@ public normal_behaviour\n" ++ y ++ "    @*/\n")) $ genJMLConsts cs []


genJMLConsts :: HTriples -> [(MethodName, String)] -> [(MethodName, String)]
genJMLConsts [] xs     = xs
genJMLConsts (c:cs) xs = let mn = snd $ methodCN c                         
                         in genJMLConsts cs (updateJMLForM c mn xs)

updateJMLForM :: HT -> MethodName -> [(MethodName, String)] -> [(MethodName, String)]
updateJMLForM c mn []              = [(mn, fromHT2JML c)]
updateJMLForM c mn ((mn', jml):xs) = if (mn == mn')
                                     then (mn', jml ++ "    @\n  @ also\n" ++ fromHT2JML c):xs
                                     else (mn', jml):updateJMLForM c mn xs

fromHT2JML :: HT -> String
fromHT2JML (HT _ _ precon postcon assig _ _ _) =  
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


