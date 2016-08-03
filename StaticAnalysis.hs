module StaticAnalysis(staticAnalysis) where

import Types
import qualified ParserJML
import CommonFunctions
import System.Directory
import System.Environment
import System.Process
import JMLInjection
import qualified ParserXMLKeYOut
import RefinementPPDATE
import ReportGen
import ErrM
import UpgradePPDATE
import OperationalizationPP
import Instrumentation
import PartialInfoFilesGeneration
import Data.Functor ((<$>))
import Data.List ((\\))
import Data.Maybe
import System.FilePath
import qualified Data.Map as Map

-------------------------------
-- Static Analysis using KeY --
-------------------------------

staticAnalysis :: FilePath -> UpgradePPD PPDATE -> FilePath -> IO (UpgradePPD PPDATE)
staticAnalysis jpath ppd output_add =
 let ppdate      = getValue ppd
     consts      = contractsGet ppdate
 in if (null consts)
    then do putStrLn "\nThere are no Hoare triples to analyse."
            return ppd
    else staticAnalysis' jpath ppd output_add

staticAnalysis' :: FilePath -> UpgradePPD PPDATE -> FilePath -> IO (UpgradePPD PPDATE)
staticAnalysis' jpath ppd output_add =
 let output_add' = output_add ++ "/workspace/files2analyse"
     tmp_add     = output_add ++ "/workspace/files/"
     cinv_add    = output_add ++ "/workspace/filescinv"
     nulla_add   = output_add ++ "/workspace/filesnullable"
     ppdate      = getValue ppd
     consts      = contractsGet ppdate
 in do
       createDirectoryIfMissing False tmp_add
       createDirectoryIfMissing False output_add'
       createDirectoryIfMissing False nulla_add
       createDirectoryIfMissing False cinv_add
       createDirectoryIfMissing False (output_add ++ "/workspace/filesnu")
       generateDummyBoolVars ppd tmp_add jpath
       generateTmpFilesCInvs ppd cinv_add tmp_add
       updateTmpFilesCInvs ppd nulla_add cinv_add
       let consts_jml = ParserJML.getContracts' ppd
       copyFiles jpath output_add'
       generateTmpFilesAllConsts ppd consts_jml output_add' (nulla_add ++ "/")
       eapp_add <- getExecutablePath
       let eapp_add' = reverse $ snd $ splitAtIdentifier '/' $ reverse eapp_add
       let api_add   = eapp_add' ++ "key_api"
       rawSystem api_add [output_add', (output_add ++ "/")]
       let xml_add = output_add ++ "/out.xml"
       b <- doesFileExist xml_add
       if b
       then do xml <- ParserXMLKeYOut.parse xml_add
               let cns  = getContractNamesEnv ppd
               let xml' = removeNoneContracts xml cns
               let ppdate'  = ppd >>= (\x -> return $ refinePPDATE x xml')
               info <- report xml'
               writeFile (output_add ++ "/report.txt") info
               putStrLn "\nStatic verification completed."
               putStrLn "Generating Java files to control the (partially proven) Hoare triple(s)."
               methods <- methodsNames ppdate' jpath
               let (ppdate'', tnewvars) = operationalizeOldResultBind ppdate' methods
               let add = output_add ++ "/ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_add ++ "/" ++ annotated_add)
               let ppdate''' = generateNewTriggers ppdate''
               --putStrLn (show $ generateNewTriggers ppdate'')
               contractsJavaFileGen ppdate''' add tnewvars
               idFileGen add
               copyFiles jpath (output_add ++ "/" ++ annotated_add)
               methodsInstrumentation ppdate''' jpath (output_add ++ "/" ++ annotated_add)
               return ppdate'''
       else do putStrLn "\nWarning: KeY execution has failed."
               writeFile (output_add ++ "/report.txt") "Warning: KeY execution has failed.\n"
               putStrLn "Generating Java files to control the Hoare triple(s) at runtime."
               methods <- methodsNames ppd jpath
               let (ppdate'', tnewvars) = operationalizeOldResultBind ppd methods
               let add = output_add ++ "/ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_add ++ "/" ++ annotated_add)
               let ppdate''' = generateNewTriggers ppdate''
               contractsJavaFileGen ppdate''' add tnewvars
               idFileGen add
               methodsInstrumentation ppdate''' jpath (output_add ++ "/" ++ annotated_add)
               return ppdate'''

--------------------------------------------------------------
-- Copy all the files within a Directory to a new directory --
--------------------------------------------------------------

copyFiles source dest =
 do
    createDirectoryIfMissing True dest
    subItems <- getSubitems' source
    mapM_ (copyItem' source dest) subItems


getSubitems' :: FilePath -> IO [(Bool, FilePath)]
getSubitems' path = getSubitemsRec ""
  where
    getChildren path =  (\\ [".", ".."]) <$> getDirectoryContents path

    getSubitemsRec relPath = do
        let absPath = path </> relPath
        isDir <- doesDirectoryExist absPath
        children <- if isDir then getChildren absPath else return []
        let relChildren = [relPath </> p | p <- children]
        ((isDir, relPath) :) . concat <$> mapM getSubitemsRec relChildren

copyItem' baseSourcePath baseTargetPath (isDir, relativePath) =
 do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath
    if isDir
    then createDirectoryIfMissing False targetPath
    else copyFile sourcePath targetPath

-------------------------
-- Auxiliary functions --
-------------------------

getSourceCodeFolderName :: FilePath -> String
getSourceCodeFolderName s = let (xs,ys) = splitAtIdentifier '/' $ (reverse . init) s
                            in reverse xs

--TODO: Fix this method if classes with the same name in different paths are allowed
--Generates triggers whenever a method to be runtime verified is not associated to one
--generateNewTriggers :: UpgradePPD PPDATE -> UpgradePPD PPDATE
generateNewTriggers ppd =
  do let env     = getEnvVal ppd
     let ppdate  = getValue ppd
     let consts  = contractsGet ppdate
     let mfiles  = methodsInFiles env
     let mns     = removeDuplicates [mn | mn <- map methodCN consts]
     let entry   = filterDefinedTriggers (entryEventsInfo env) mns
     let exit    = filterDefinedTriggers (exitEventsInfo env) mns
     let entry'  = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- entry, (_,d,z) <- mfiles,d==x]
     let exit'   = [(x,y,head $ filter (\(a,b,c) -> y == b) z) | (x,y) <- exit, (_,d,z) <- mfiles,d==x]
     let (env',ppdate')   = addNewTriggerEntry env ppdate entry'--Problem when updating the ppdate
     let (env'',ppdate'') = addNewTriggerExit env' ppdate' 0 exit'
     put env''
     return ppdate''
     
filterDefinedTriggers :: Map.Map ClassInfo MapTrigger -> [(ClassInfo,MethodName)] -> [(ClassInfo,MethodName)]
filterDefinedTriggers mci []           = []
filterDefinedTriggers mci ((ci,mn):xs) = 
 case Map.lookup ci mci of
      Nothing -> (ci,mn):filterDefinedTriggers mci xs
      Just m  -> case Map.lookup mn m of
                      Nothing -> (ci,mn):filterDefinedTriggers mci xs
                      Just _  -> filterDefinedTriggers mci xs

--Creates the info to be added in the environment and the ppDATE about the new entry trigger
createTriggerEntry :: (ClassInfo,MethodName,(String,MethodName,[String])) -> ((ClassInfo,MethodName,(Id, String, [Args])),EventDef)
createTriggerEntry (cn,mn,(rt,mn',xs)) = 
 if (mn == mn')
 then let trnm = mn ++ "_en"
          cn'  = cn ++ " cv"
          cpe  = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) EVEntry
          tr   = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
      in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)),tr)
 else error $ "Problem when creating an entry trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerEntry :: Env -> PPDATE -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env,PPDATE)
addNewTriggerEntry env ppdate []     = (env,ppdate)
addNewTriggerEntry env ppdate (x:xs) =
 let (p,tr) = createTriggerEntry x
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
  in case Map.lookup cn (entryEventsInfo env) of
      Nothing -> let mapeinfo' =  Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerEntry (env { entryEventsInfo = Map.insert cn mapeinfo' (entryEventsInfo env) }) ppdate' xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerEntry (env { entryEventsInfo = Map.insert cn mapeinfo' (entryEventsInfo env) }) ppdate' xs

--Creates the info to be added in the environment and the ppDATE about the new exit trigger
createTriggerExit:: (ClassInfo,MethodName,(String,MethodName,[String])) -> Integer -> ((ClassInfo,MethodName,(Id, String, [Args])), EventDef)
createTriggerExit (cn,mn,(rt,mn',xs)) n = 
 let trnm = mn ++ "_ex" 
     cn'  = cn ++ " cv" 
     ret  = "r" ++ (show n) in
 if (mn == mn')
 then if (rt == "void")
      then let cpe  = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [])
               tr   = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
           in ((cn,mn,(trnm, cn', map ((\[x,y] -> Args x y).words) xs)), tr)
      else let cpe = NormalEvent (BindingVar (BindType cn "cv")) mn (map ((\[x,y] -> BindId y).words) xs) (EVExit [BindId ret]) 
               tr  = EventDef trnm (map ((\[x,y] -> BindType x y).words) xs) cpe ""
           in ((cn,mn,(trnm, cn', (map ((\[x,y] -> Args x y).words) xs) ++ [Args rt ret])),tr)
 else error $ "Problem when creating an exit trigger. Mismatch between method names " ++ mn ++ " and " ++ mn' ++ ".\n"

addNewTriggerExit :: Env -> PPDATE -> Integer -> [(ClassInfo,MethodName,(String,MethodName,[String]))] -> (Env, PPDATE)
addNewTriggerExit env ppdate _ []     = (env,ppdate)
addNewTriggerExit env ppdate n (x:xs) =
 let (p,tr) = createTriggerExit x n
     cn     = (\(x,y,z) -> x) p 
     mn     = (\(x,y,z) -> y) p 
     v      = (\(x,y,z) -> z) p 
 in case Map.lookup cn (exitEventsInfo env) of
      Nothing -> let mapeinfo' = Map.insert mn v Map.empty
                     ppdate'   = addTrigger2ppDATE tr ppdate 
                 in addNewTriggerExit (env { exitEventsInfo = Map.insert cn mapeinfo' (exitEventsInfo env) }) ppdate' (n+1) xs
      Just mapeinfo -> 
           let mapeinfo' = Map.insert mn v mapeinfo
               ppdate'   = addTrigger2ppDATE tr ppdate 
           in addNewTriggerExit (env { exitEventsInfo = Map.insert cn mapeinfo' (exitEventsInfo env) }) ppdate' (n+1) xs

addTrigger2ppDATE :: EventDef -> PPDATE -> PPDATE
addTrigger2ppDATE tr (PPDATE imp (Global (Ctxt vars trs p for)) ci consts ms) =
 PPDATE imp (Global (Ctxt vars (tr:trs) p for)) ci consts ms

