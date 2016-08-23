module TypeInferenceXml(inferTypesOldExprs) where

import Types
import Text.XML.HaXml hiding(path)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import CommonFunctions
import UpgradePPDATE
import System.Directory
import System.Environment
import System.Process
import System.FilePath
import qualified Data.Map as Map
import Data.List


--inferTypesOldExprs :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO (Map.Map ContractName [(String,Type)])
--inferTypesOldExprs :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO [OldExpr]
inferTypesOldExprs ppd jpath output_add = 
 do let ppdate  = getValue ppd
    let env     = getEnvVal ppd
    let toXml   = generateOldExpr (contractsGet ppdate)
    let vars    = varsInFiles env
    let mfiles  = methodsInFiles env
    let types   = removeDuplicates [(classInf c, getTypes (classInf c) vars mfiles) | c <- toXml]
    let toXml'  = map (\oexpr -> addType oexpr types) toXml
    let xml_add = output_add ++ "tmp.xml"
    generateXmlFile jpath xml_add
    return toXml'
                 where getTypes c vs ms = getListOfTypesAndVars c vs ++ getListOfTypesAndMethods c ms


--------------------
-- Type inference --
--------------------

addType :: OldExpr -> [(ClassInfo,[(Type,String)])] -> OldExpr
addType oexpr ts = 
 let xs = map (\(OExpr e t) -> OExpr e (checkType ts (classInf oexpr) e)) (oldExprs oexpr)
 in updateOldExprs oexpr xs

checkType :: [(ClassInfo,[(Type,String)])] -> ClassInfo -> String -> Type
checkType [] cn s             = ""
checkType ((cn',ts):xss) cn s = 
 if (cn == cn')
 then let ys = [ t | (t,s') <- ts, s == s' || isPrefixOf (s'++"(") s]
      in if null ys 
         then checkType' ts s
         else head ys
 else checkType xss cn s


checkType' :: [(Type,String)] -> String -> Type
checkType' ts s 
 | (or.map (\c -> isInfixOf c s)) boolSymbols   = "boolean"
 | (or.map (\c -> isInfixOf c s)) intSymbols    = "int"
 | (or.map (\c -> isInfixOf c s)) mathSymbols 
   && (or.map (\c -> isInfixOf c s)) intSymbols = "int"
 | (or.map (\c -> isInfixOf c s)) mathSymbols   = 
   let ys = [x | x <- words s,not $ elem x mathSymbols]
       zs = removeDuplicates [t | z <- ys, (t,s) <- ts, z == s || isPrefixOf (z++"(") s]
   in if null zs 
      then ""
      else head zs
 | otherwise                                    = ""
 
boolSymbols :: [String]
boolSymbols = ["<","<=","==",">",">=","&&","||","!"]

mathSymbols :: [String]
mathSymbols = ["+","-","*"]

intSymbols :: [String]
intSymbols = [".intValue()","1"]

-------------------------
-- Generating XML file --
-------------------------

generateXmlFile :: FilePath -> FilePath -> IO ()
generateXmlFile jpath output_add = 
 do writeFile output_add "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    appendFile output_add "<result>\n"
    appendFile output_add "</result>" 


generateOldExpr :: Contracts -> [OldExpr]
generateOldExpr []     = []
generateOldExpr (c:cs) = 
 let cn     = contractName c
     p      = post c
     classI = fst $ methodCN c
     path   = path2it c
     target = snd $ methodCN c
     xs     = splitOnIdentifier "\\old(" p
 in if (length xs == 1)
    then generateOldExpr cs
    else let ys    = tail xs
             zs    = removeDuplicates $ map ((\(x,y) -> trim (tail x)) . (splitAtClosingParen 0)) ys
             xs'   = foldr (\x xs -> (OExpr x ""):xs) [] zs
         in (OldExpr cn classI path target xs'):generateOldExpr cs


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
                                             , classInf      = snd' x 
                                             , path          = fth x
                                             , methoD        = trd' x
                                             , oldExprs      = oexpr
                                             }) : xs
                fst' (x,y,z,t) = x
                snd' (x,y,z,t) = y
                trd' (x,y,z,t) = z
                fth  (x,y,z,t) = t


getContractInfo :: Content i -> (ContractId, ClassInfo, Target,Type)
getContractInfo (CElem (Elem name as _) _) =
  if (getFromQN name == "contract")
  then let cid  = lookForVal "ID" as
           cinf = lookForVal "class" as
           t    = lookForVal "type" as 
           tar  = lookForVal "target" as 
       in (cid, t, tar,cinf)
  else ("","","","")


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
