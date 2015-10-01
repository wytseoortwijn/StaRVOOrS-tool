module ReportGen(report) where

import Types
import CommonFunctions
import DL2JML


type PProof = (MethodName, ContractName, [Pre])

report :: [Proof] -> IO String
report proofs = do let xs    = map getInfoFromProof proofs
                   let pinfo = splitAccording2Proof [] [] [] xs
                   return $ makeReport pinfo


splitAccording2Proof :: [PProof] -> [PProof] -> [PProof] -> [PProof] -> ([PProof], [PProof], [PProof])
splitAccording2Proof xs ys zs []     = (xs, ys, zs)
splitAccording2Proof xs ys zs (p:ps) = let pres = (\(x,y,z) -> z) p
                                       in if (null pres)
                                          then splitAccording2Proof (p:xs) ys zs ps
                                          else if (elem "true" pres) 
                                               then splitAccording2Proof xs ys (p:zs) ps -- KeY has nothing to say about the proof
                                               else splitAccording2Proof xs (p:ys) zs ps

makeReport :: ([PProof], [PProof], [PProof]) -> String
makeReport (ps, pps, nps) = "Results of the Static Verification of contracts\n\n"
                            ++ show (sum (map length [ps, pps, nps])) ++ " contract(s) were analysed:\n\n"
                            ++ fullyProvedInfo ps ++ "\n"
                            ++ partiallyProvedInfo pps ++ "\n" 
                            ++ notProvedInfo nps


fullyProvedInfo :: [PProof] -> String
fullyProvedInfo ps = if (null ps)
                     then "* No contract(s) were fully proved.\n"
                     else "* " ++ show (length ps) ++ " contract(s) were fully proved:\n"
                          ++ genListContractsName (map (\(x,y,z) -> y) ps)                          

partiallyProvedInfo :: [PProof] -> String
partiallyProvedInfo pps = if (null pps)
                          then ""
                          else "* " ++ show (length pps) ++ " contract(s) were partially proved:\n"
                               ++ genPartiallyInfo pps

notProvedInfo :: [PProof] -> String
notProvedInfo nps = if (null nps)
                    then ""
                    else "* " ++ show (length nps) ++ " contract(s) were not proved:\n"
                         ++ genListContractsName (map (\(x,y,z) -> y) nps)

genListContractsName :: [ContractName] -> String
genListContractsName []     = ""
genListContractsName (c:cs) = "  " ++ c ++ "\n" ++ genListContractsName cs

genPartiallyInfo :: [PProof] -> String
genPartiallyInfo []                  = ""
genPartiallyInfo ((mn, cn, pres):ps) = "  " ++ cn ++ " --> New condition added to its precondition is "  ++ (introduceOr $ map (addParenthesisNot.removeSelf) (removeDuplicates pres)) ++ "\n" 
                                       ++ genPartiallyInfo ps
