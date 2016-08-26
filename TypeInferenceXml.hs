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
inferTypesOldExprs ppd jpath output_add = 
 do let ppdate  = getValue ppd
    let env     = getEnvVal ppd
    let toXml   = generateOldExpr (contractsGet ppdate) jpath
    let vars    = varsInFiles env
    let mfiles  = methodsInFiles env
    let types   = removeDuplicates [(classInf c, getTypes (classInf c) vars mfiles) | c <- toXml]
    let toXml'  = map (\oexpr -> addType oexpr types mfiles) toXml
    let xml_add = output_add ++ "tmp.xml"
    generateXmlFile toXml' jpath xml_add
    --Run JavaExprReader
    oldExpsJER <- parse xml_add   
    let oldExpTypes = foldr (\x xs -> Map.insert (contractID x) (map toTuple $ oldExprs x) xs) Map.empty oldExpsJER
    return oldExpTypes
                 where getTypes c vs ms = getListOfTypesAndVars c vs ++ getListOfTypesAndMethods c ms
                       toTuple (OExpr e t) = (e,t) 


--------------------
-- Type inference --
--------------------

addType :: OldExpr -> [(ClassInfo,[(Type,String)])] -> [(String, ClassInfo, [(Type,Id,[String])])] -> OldExpr
addType oexpr ts minfs = 
 let xs = map (\(OExpr e t) -> OExpr e (checkType ts minfs oexpr e)) (oldExprs oexpr)
 in updateOldExprs oexpr xs

checkType :: [(ClassInfo,[(Type,String)])] -> [(String, ClassInfo, [(Type,Id,[String])])] -> OldExpr -> String -> String
checkType xs ys oexpr s = 
 let cinf = classInf oexpr
 in if (checkTypeArgs ys oexpr s == "") 
    then checkTypeClass xs cinf s
    else checkTypeArgs ys oexpr s

--TODO: Fix if classes with the same name in different folders is allowed
checkTypeArgs :: [(String, ClassInfo, [(Type,Id,[String])])] -> OldExpr -> String -> String
checkTypeArgs [] _ s                   = ""
checkTypeArgs ((_,cinf,ys):xs) oexpr s = 
 let cinf' = classInf oexpr
     mn    = methoD oexpr
 in if cinf' == cinf
    then if (null $ getListOfArgs mn ys)
         then ""
         else let zs = [ t | [t,s'] <- map words $ getListOfArgs mn ys, s' == s]
              in if null zs
                 then ""
                 else head zs
    else checkTypeArgs xs oexpr s

checkTypeClass :: [(ClassInfo,[(Type,String)])] -> ClassInfo -> String -> Type
checkTypeClass [] cn s             = ""
checkTypeClass ((cn',ts):xss) cn s = 
 if (cn == cn')
 then let ys = [ t | (t,s') <- ts, s == s' || isPrefixOf (s'++"(") s]
      in if null ys 
         then checkType' ts s
         else head ys
 else checkTypeClass xss cn s

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

generateXmlFile :: [OldExpr] -> FilePath -> FilePath -> IO ()
generateXmlFile xs jpath output_add = 
 do writeFile output_add "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    appendFile output_add "<result>\n"
    appendFile output_add (concat $ map oldExpr2Xml xs)
    appendFile output_add "</result>" 


oldExpr2Xml :: OldExpr -> String
oldExpr2Xml oldExpr = 
 let xs = map oExpr2Xml (oldExprs oldExpr) in
 twoSpaces ++ "<contract Id=" ++ "\"" ++ contractID oldExpr ++ "\"" 
 ++ " class=" ++ "\"" ++ classInf oldExpr ++ "\"\n" 
 ++ " path=" ++ "\"" ++ path oldExpr ++ "\"\n" 
 ++ " target=" ++ "\"" ++ targ oldExpr ++ "\""
 ++ " method=" ++ "\"" ++ methoD oldExpr ++ "\">\n" 
 ++ concat xs
 ++ twoSpaces ++ "</contract>\n\n"
 

oExpr2Xml :: OExpr -> String
oExpr2Xml oexpr = 
 fourSpaces ++"<oldExpr expr=" ++ "\"" ++ expr oexpr ++ "\"" 
 ++ " type=" ++ "\"" ++ inferType oexpr ++ "\">\n" 
 ++ fourSpaces ++ "</oldExpr>\n\n"


generateOldExpr :: Contracts -> FilePath ->  [OldExpr]
generateOldExpr [] _         = []
generateOldExpr (c:cs) jpath = 
 let cn     = contractName c
     p      = post c
     classI = fst $ methodCN c
     path   = jpath
     tar    = path2it c
     mn = snd $ methodCN c
     xs     = splitOnIdentifier "\\old(" p
 in if (length xs == 1)
    then generateOldExpr cs jpath
    else let ys    = tail xs
             zs    = removeDuplicates $ map ((\(x,y) -> trim (tail x)) . (splitAtClosingParen 0)) ys
             xs'   = foldr (\x xs -> (OExpr x ""):xs) [] zs
         in (OldExpr cn classI tar path mn xs'):generateOldExpr cs jpath

twoSpaces :: String
twoSpaces = "  "

fourSpaces :: String
fourSpaces = "    "

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
                                             , classInf      = fth x 
                                             , path          = fifth x
                                             , methoD        = snd' x
                                             , targ          = trd' x
                                             , oldExprs      = oexpr
                                             }) : xs
                fst' (x,y,z,t,u)  = x
                snd' (x,y,z,t,u)  = y
                trd' (x,y,z,t,u)  = z
                fth  (x,y,z,t,u)  = t
                fifth (x,y,z,t,u) = u


getContractInfo :: Content i -> (ContractId, ClassInfo, Target,Type, FilePath)
getContractInfo (CElem (Elem name as _) _) =
  if (getFromQN name == "contract")
  then let cid  = lookForVal "Id" as
           cinf = lookForVal "class" as
           mn   = lookForVal "method" as 
           tar  = lookForVal "target" as
           fp   = lookForVal "path" as  
       in (cid, mn, tar,cinf,fp)
  else ("","","","","")


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
