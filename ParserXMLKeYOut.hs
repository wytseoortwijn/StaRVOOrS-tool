module ParserXMLKeYOut(parse) where 

import Types
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import CommonFunctions
import Data.List 


parse :: XML -> IO [Proof]
parse xml_fn = 
  do r <- readFile xml_fn
     let xml = xmlParse "(No Document)" r
     let (Document _ _ root _) = xml
     let rootElem = CElem root noPos
     let xs = (tag "result" /> tag "proof" $ rootElem)
     let ys = map (tag "proof" /> tag "executionPath") xs
     let proof_info       = map getProofInfo xs
     let epath_conds      = map (map getPathCondition) ys
     let epath_verif      = map (map getVerified) ys
     let epath_newconds   = map (map getNewPrecondition) ys
     let epath_tkind      = map (map getTerminationKind) ys
     let epath_notfpres   = map (map getNotFulfilledPres) ys
     let epath_notchecks  = map (map getNotFulfilledNullChecks) ys
     let epath_initLoop   = map (map getNotInitValidLoopInvs) ys
     let epath_preserLoop = map (map getNotPreservedLoopInvs) ys
     let components       = zip8 epath_conds epath_verif epath_newconds epath_tkind epath_notfpres epath_notchecks epath_initLoop epath_preserLoop
     let epath  = map (foldr (\p xs -> (makeEPath p):xs) [] . (\(x, y, z, t, u, i, o, p) -> zip8 x y z t u i o p)) components
     let epath' = map (map translateEPATH) epath
     let proof  = foldr foo [] $ zip proof_info epath'
     return proof
          where foo (x,ep) xs  = (Proof { contractId    = fst' x
                                        , contractText  = snd' x
                                        , typee         = trd' x
                                        , target        = frth x
                                        , executionPath = ep 
                                        }) : xs
                fst' (x,y,z,t) = x
                snd' (x,y,z,t) = y
                trd' (x,y,z,t) = z
                frth (x,y,z,t) = t

translateEPATH :: EPath -> EPath
translateEPATH epath = epath { pathCondition = translate $ pathCondition epath }

symbolsXML :: [(String,String)]
symbolsXML = [("&amp;","&&"), ("&gt;=",">="), ("&gt;", ">"), (" =","=="), ("&quot;","\""),("&apos;","\\"),("&lt;","<"),("&lt;=","<=")]

translate :: String -> String
translate s = (\ x -> replaceSymbols x symbolsXML) s

replaceSymbols :: String -> [(String, String)] -> String
replaceSymbols [] _            = ""
replaceSymbols s []            = s
replaceSymbols s ((id,def):xs) = let ys = splitOnIdentifier id s
                                 in if (length ys == 1)
                                    then replaceSymbols s xs
                                    else replaceSymbols (head ys ++ (concat (map (def++) $ tail ys))) xs

getFromQN :: QName -> Name
getFromQN (N s) = s 

-----------
-- Proof --
-----------

getProofInfo :: Content i -> (ContractId, ContractText, Type, Target)
getProofInfo (CElem (Elem name as _) _) =
  if (getFromQN name == "proof")
  then let cid = lookForVal "contractId" as
           ct  = lookForVal "contractText" as 
           t   = lookForVal "type" as 
           tar = lookForVal "target" as 
       in (cid, ct, t, tar)
  else ("","","","")

-------------------
-- executionPath --
-------------------

getPathCondition :: Content i -> String
getPathCondition (CElem (Elem name attributes _) _) =
  if (getFromQN name == "executionPath")
  then lookForVal "pathCondition" attributes
  else ""

getVerified :: Content i -> String
getVerified (CElem (Elem name attributes _) _) = 
  if (getFromQN name == "executionPath")
  then lookForVal "verified" attributes
  else ""

getNewPrecondition :: Content i -> String
getNewPrecondition (CElem (Elem name attributes _) _) = 
  if (getFromQN name == "executionPath")
  then lookForVal "newPrecondition" attributes
  else ""

getTerminationKind :: Content i -> String
getTerminationKind (CElem (Elem name attributes _) _) = 
  if (getFromQN name == "executionPath")
  then lookForVal "terminationKind" attributes
  else ""

-------------------------------
-- MethodContractApplication --
------------------------------

getNotFulfilledPres :: Content i -> [MethodContractApplication]
getNotFulfilledPres (CElem (Elem name _ content) _) = 
  if (getFromQN name == "executionPath")
  then getNotFulfilledPresC content
  else []

getNotFulfilledPresC :: [Content i] -> [MethodContractApplication]
getNotFulfilledPresC []                                   = []
getNotFulfilledPresC ((CElem (Elem name _ content) _):xs) = 
  if (getFromQN name == "notFulfilledPreconditions")
  then getMethodContractApplication content ++ getNotFulfilledPresC xs
  else getNotFulfilledPresC xs
getNotFulfilledPresC (_:xs)                               = getNotFulfilledPresC xs


getNotFulfilledNullChecks :: Content i -> [MethodContractApplication]
getNotFulfilledNullChecks (CElem (Elem name _ content) _) = 
  if (getFromQN name == "executionPath")
  then getNotFulfilledNullChecksC content
  else []

getNotFulfilledNullChecksC :: [Content i] -> [MethodContractApplication]
getNotFulfilledNullChecksC []                                   = []
getNotFulfilledNullChecksC ((CElem (Elem name _ content) _):xs) = 
  if (getFromQN name == "notFulfilledNullChecks")
  then getMethodContractApplication content ++ getNotFulfilledNullChecksC xs
  else getNotFulfilledNullChecksC xs
getNotFulfilledNullChecksC (_:xs)                               = getNotFulfilledNullChecksC xs


getNotInitValidLoopInvs :: Content i -> [MethodContractApplication]
getNotInitValidLoopInvs (CElem (Elem name _ content) _) = 
  if (getFromQN name == "executionPath")
  then getNotInitValidLoopInvsC content
  else []

getNotInitValidLoopInvsC :: [Content i] -> [MethodContractApplication]
getNotInitValidLoopInvsC []                                   = []
getNotInitValidLoopInvsC ((CElem (Elem name _ content) _):xs) = 
  if (getFromQN name == "notInitiallyValidLoopInvariants")
  then getMethodContractApplication content ++ getNotInitValidLoopInvsC xs
  else getNotInitValidLoopInvsC xs
getNotInitValidLoopInvsC (_:xs)                               = getNotInitValidLoopInvsC xs


getNotPreservedLoopInvs :: Content i -> [MethodContractApplication]
getNotPreservedLoopInvs (CElem (Elem name _ content) _) = 
  if (getFromQN name == "executionPath")
  then getNotPreservedLoopInvsC content
  else []

getNotPreservedLoopInvsC :: [Content i] -> [MethodContractApplication]
getNotPreservedLoopInvsC []                                   = []
getNotPreservedLoopInvsC ((CElem (Elem name _ content) _):xs) = 
  if (getFromQN name == "notInitiallyValidLoopInvariants")
  then getMethodContractApplication content ++ getNotPreservedLoopInvsC xs
  else getNotPreservedLoopInvsC xs
getNotPreservedLoopInvsC (_:xs)                               = getNotPreservedLoopInvsC xs



getMethodContractApplication :: [Content i] -> [MethodContractApplication]
getMethodContractApplication []                                      = []
getMethodContractApplication ((CElem (Elem name attributes _) _):xs) =
  if (getFromQN name == "methodContractApplication")
  then getMCA attributes : getMethodContractApplication xs
  else getMethodContractApplication xs
getMethodContractApplication (_:xs)                                  = getMethodContractApplication xs


getMCA :: [Attribute] -> MethodContractApplication
getMCA xs = 
 MCA { fileMCA     = lookForVal "file" xs
     , startLine   = lookForVal "startLine" xs
     , startColumn = lookForVal "startColumn" xs
     , endLine     = lookForVal "endLine" xs 
     , endColumn   = lookForVal "endColumn" xs
     , methodMCA   = lookForVal "method" xs
     , contractMCA = lookForVal "contract" xs
     }

------------------------
-- Auxiliar functions --
------------------------

lookForVal :: String -> [Attribute] -> String
lookForVal s []             = ""
lookForVal s ((an, ref):as) = if (getFromQN an == s) 
                              then show ref
                              else lookForVal s as

type MCAL = [MethodContractApplication]

makeEPath :: (String, String, String, String, MCAL, MCAL, MCAL, MCAL) -> EPath
makeEPath (pcond, pverif, newpres, tkind, nfpres, nfchecks, initLoop, preserLoop) = 
 EPath pcond pverif newpres tkind nfpres nfchecks initLoop preserLoop

zip8 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [(a,b,c,d,e,f,g,h)]
zip8 [] [] [] [] [] [] [] []                                 = []
zip8 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) = (a,b,c,d,e,f,g,h):zip8 as bs cs ds es fs gs hs
