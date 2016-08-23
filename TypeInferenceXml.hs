module TypeInferenceXml(generateXmlFile) where

import Types
import Text.XML.HaXml hiding(path)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import CommonFunctions
import UpgradePPDATE


--inferTypesOldExprs :: UpgradePPD PPDATE -> IO [OldExpr]
inferTypesOldExprs ppdate = 
 ppdate >>= (\ppd -> generateXmlFile $ contractsGet ppd)

-------------------------
-- Generating XML file --
-------------------------

--generateXmlFile :: Contracts -> 
generateXmlFile []     = []
generateXmlFile (c:cs) = 
 let cn     = contractName c
     p      = post c
     type'  = fst $ methodCN c
     target = snd $ methodCN c
     xs     = splitOnIdentifier "\\old(" p
 in if (length xs == 1)
    then generateXmlFile cs
    else let ys    = tail xs
             zs    = removeDuplicates $ map ((\(x,y) -> trim (tail x)) . (splitAtClosingParen 0)) ys
             xs'   = foldr (\x xs -> (OExpr x ""):xs) [] zs
         in (OldExpr cn type' target xs'):generateXmlFile cs


----------------------
-- Reading XML file --
----------------------

parse :: XML -> IO [OldExpr]
parse xml_fn = 
  do r <- readFile xml_fn
     let xml = xmlParse "(No Document)" r
     let (Document _ _ root _) = xml
     let rootElem = CElem root noPos
     let xs = (tag "result" /> tag "contract" $ rootElem)
     let ys = map (tag "contract" /> tag "oldExpr") xs
     let oldExpr_info = map getContractInfo xs
     let oldExpr_expr = map (map getExpr) ys
     let oldExpr_type = map (map getType) ys
     let components   = zip oldExpr_expr oldExpr_type
     let oldExpr      = map (foldr (\p xs -> (makeOldExpr p):xs) [].(\(x, y) -> zip x y)) components
     let proof        = foldr foo [] $ zip oldExpr_info oldExpr
     return proof
          where foo (x,oexpr) xs  = (OldExpr { contractID    = fst' x
                                             , path          = snd' x
                                             , methoD        = trd' x
                                             , oldExprs      = oexpr
                                             }) : xs
                fst' (x,y,z) = x
                snd' (x,y,z) = y
                trd' (x,y,z) = z


getContractInfo :: Content i -> (ContractId, Type, Target)
getContractInfo (CElem (Elem name as _) _) =
  if (getFromQN name == "contract")
  then let cid = lookForVal "ID" as
           t   = lookForVal "type" as 
           tar = lookForVal "target" as 
       in (cid, t, tar)
  else ("","","")


getExpr :: Content i -> String
getExpr (CElem (Elem name attributes _) _) =
  if (getFromQN name == "oldExpr")
  then lookForVal "expr" attributes
  else ""

getType :: Content i -> String
getType (CElem (Elem name attributes _) _) = 
  if (getFromQN name == "oldExpr")
  then lookForVal "type" attributes
  else ""


getFromQN :: QName -> Name
getFromQN (N s) = s 

lookForVal :: String -> [Attribute] -> String
lookForVal s []             = ""
lookForVal s ((an, ref):as) = if (getFromQN an == s) 
                              then show ref
                              else lookForVal s as

makeOldExpr :: (String, String) -> OExpr
makeOldExpr (a, b) = OExpr a b
