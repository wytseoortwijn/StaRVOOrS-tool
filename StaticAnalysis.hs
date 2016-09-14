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
import TypeInferenceXml

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
 let output_addr = if ((last $ trim output_add) == '/') 
                   then output_add
                   else output_add ++ "/"
     output_add' = output_addr ++ "workspace/files2analyse"
     tmp_add     = output_addr ++ "workspace/files/"
     cinv_add    = output_addr ++ "workspace/filescinv"
     nulla_add   = output_addr ++ "workspace/filesnullable"
     ppdate      = getValue ppd
     consts      = contractsGet ppdate
 in do
       createDirectoryIfMissing False tmp_add
       createDirectoryIfMissing False output_add'
       createDirectoryIfMissing False nulla_add
       createDirectoryIfMissing False cinv_add
       createDirectoryIfMissing False (output_addr ++ "workspace/filesnu")
       generateDummyBoolVars ppd tmp_add jpath
       generateTmpFilesCInvs ppd cinv_add tmp_add
       updateTmpFilesCInvs ppd nulla_add cinv_add
       let consts_jml = ParserJML.getContracts' ppd
       copyFiles jpath output_add'
       generateTmpFilesAllConsts ppd consts_jml output_add' (nulla_add ++ "/")
       rawSystem "java" ["-jar","key.starvoors.jar",output_add', output_addr]
       let xml_add = output_addr ++ "out.xml"
       b <- doesFileExist xml_add
       if b
       then do xml_to_parse <- readFile xml_add
               let xml     = ParserXMLKeYOut.parse xml_to_parse
               let cns     = getContractNamesEnv ppd
               let xml'    = removeNoneContracts xml cns
               let ppdate' = refinePPDATE ppd xml'
               generateReport xml' output_addr
               putStrLn "Generating Java files to control the (partially proven) Hoare triple(s)."
               oldExpTypes <- inferTypesOldExprs ppdate' jpath (output_addr ++ "workspace/")
               putStrLn (show oldExpTypes)
               let (ppdate'', tnewvars) = operationalizeOldResultBind ppdate' oldExpTypes
               let add = output_add ++ "/ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_addr ++ annotated_add)
               contractsJavaFileGen ppdate'' add tnewvars
               idFileGen add
               oldExprFileGen add ppdate''
               copyFiles jpath (output_addr ++ annotated_add)
               methodsInstrumentation ppdate'' jpath (output_addr ++ annotated_add)
               return ppdate''
       else do generateReportFailure output_addr
               let ppd' = generateNewTriggers ppd (contractsGet $ getValue ppd)
               putStrLn "Generating Java files to control the Hoare triple(s) at runtime."
               oldExpTypes <- inferTypesOldExprs ppd' jpath (output_addr ++ "workspace/")
               let (ppdate'', tnewvars) = operationalizeOldResultBind ppd' oldExpTypes
               let add = output_addr ++ "ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_addr ++ annotated_add)
               contractsJavaFileGen ppdate'' add tnewvars
               idFileGen add
               oldExprFileGen add ppdate''
               methodsInstrumentation ppdate'' jpath (output_addr ++ annotated_add)
               return ppdate''

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

