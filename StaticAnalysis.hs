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
import UpgradePPDATE
import OperationalizationPP
import Instrumentation
import PartialInfoFilesGeneration
import Data.Functor ((<$>))
import Data.List ((\\))
import System.FilePath

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
               contractsJavaFileGen ppdate'' add tnewvars
               idFileGen add
               copyFiles jpath (output_add ++ "/" ++ annotated_add)
               methodsInstrumentation ppdate'' jpath (output_add ++ "/" ++ annotated_add)
               return ppdate''
       else do putStrLn "\nWarning: KeY execution has failed." 
               writeFile (output_add ++ "/report.txt") "Warning: KeY execution has failed.\n"
               putStrLn "Generating Java files to control the Hoare triple(s) at runtime."    
               methods <- methodsNames ppd jpath
               let (ppdate'', tnewvars) = operationalizeOldResultBind ppd methods
               let add = output_add ++ "/ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_add ++ "/" ++ annotated_add)
               contractsJavaFileGen ppdate'' add tnewvars
               idFileGen add
               methodsInstrumentation ppdate'' jpath (output_add ++ "/" ++ annotated_add)           
               return ppdate''

getSourceCodeFolderName :: FilePath -> String
getSourceCodeFolderName s = let (xs,ys) = splitAtIdentifier '/' $ (reverse . init) s
                            in reverse xs


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



