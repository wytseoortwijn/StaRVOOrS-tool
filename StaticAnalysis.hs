module StaticAnalysis(staticAnalysis) where

import Types
import qualified JMLGenerator
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
import Data.List ((\\),isInfixOf)
import Data.Maybe
import System.FilePath
import qualified Data.Map as Map
import TypeInferenceXml
import DL2JML
import qualified PrintActions as PrintAct
import qualified ParserAct as ParAct
import TranslatorActions
import System.Exit
import qualified System.IO

-------------------------------
-- Static Analysis using KeY --
-------------------------------

staticAnalysis :: FilePath -> UpgradePPD PPDATE -> FilePath -> Filename -> [Flag] -> IO (UpgradePPD PPDATE)
staticAnalysis jpath ppd output_add fn flags =
 let ppdate      = getValue ppd
     consts      = htsGet ppdate
 in if elem OnlyRV flags
    then staticAnalysis' jpath ppd output_add fn flags
    else if (null consts)
         then do putStrLn "\nThere are no Hoare triples to analyse."
                 return $ translateActions $ replacePInit ppd
         else staticAnalysis' jpath ppd output_add fn flags

staticAnalysis' :: FilePath -> UpgradePPD PPDATE -> FilePath -> Filename -> [Flag] -> IO (UpgradePPD PPDATE)
staticAnalysis' jpath ppd output_add fn flags =
 let output_addr = if ((last $ trim output_add) == '/') 
                   then output_add
                   else output_add ++ "/"
     output_add' = output_addr ++ "workspace/files2analyse"
     tmp_add     = output_addr ++ "workspace/files/"
     cinv_add    = output_addr ++ "workspace/filescinv"
     nulla_add   = output_addr ++ "workspace/filesnullable"
     ppdate      = getValue ppd
     consts      = htsGet ppdate
 in do
       createDirectoryIfMissing False tmp_add
       createDirectoryIfMissing False output_add'
       createDirectoryIfMissing False nulla_add
       createDirectoryIfMissing False cinv_add
       createDirectoryIfMissing False (output_addr ++ "workspace/filesnu")
       generateDummyBoolVars ppd tmp_add jpath
       generateTmpFilesCInvs ppd cinv_add tmp_add
       updateTmpFilesCInvs ppd nulla_add cinv_add
       let consts_jml = JMLGenerator.getHTs ppd
       copyFiles jpath output_add'
       generateTmpFilesAllConsts ppd consts_jml output_add' (nulla_add ++ "/")       
       runKeY output_add' output_addr flags
       let xml_add = output_addr ++ "out.xml"
       b <- doesFileExist xml_add
       if b
       then do xml_to_parse <- readFile xml_add
               let xml     = ParserXMLKeYOut.parse xml_to_parse
               let cns     = getHTNamesEnv ppd
               let xml'    = removeNoneHTs xml cns
               let ppdref  = refinePPDATE ppd xml'
               let ppdref' = prepareRefPPD ppdref
               let ppdate' = translateActions $ replacePInit ppdref
               let refFile = output_addr ++ generateRefPPDFileName fn
               if (not (elem XML flags)) then removeFile xml_add else return ()
               writeFile refFile (writePPD ppdref')
               generateReport xml' output_addr
               putStrLn "Generating Java files to control the (partially proven) Hoare triple(s)."
               oldExpTypes <- inferTypesOldExprs ppdate' jpath (output_addr ++ "workspace/")
               let ppdate'' = operationalizeOldResultBind ppdate' oldExpTypes
               let add = output_add ++ "/ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_addr ++ annotated_add)
               htsJavaFileGen ppdate'' add
               idFileGen add
               cloningFileGen add
               oldExprFileGen add ppdate''
               messagesFileGen add (getEnvVal ppdate'')
               copyFiles jpath (output_addr ++ annotated_add)
               methodsInstrumentation ppdate'' jpath (output_addr ++ annotated_add)
               return ppdate''
       else do generateReportFailure output_addr flags
               let ppd'' = generateNewTriggers ppd (htsGet $ getValue ppd)    
               let ppd' = translateActions $ replacePInit ppd''
               putStrLn "Generating Java files to control the Hoare triple(s) at runtime."
               oldExpTypes <- inferTypesOldExprs ppd' jpath (output_addr ++ "workspace/")
               let ppdate'' = operationalizeOldResultBind ppd' oldExpTypes
               let add = output_addr ++ "ppArtifacts/"
               let annotated_add = getSourceCodeFolderName jpath ++ "/"
               createDirectoryIfMissing True add
               createDirectoryIfMissing True (output_addr ++ annotated_add)
               htsJavaFileGen ppdate'' add
               idFileGen add
               cloningFileGen add
               oldExprFileGen add ppdate''
               messagesFileGen add (getEnvVal ppdate'')
               copyFiles jpath (output_addr ++ annotated_add)
               methodsInstrumentation ppdate'' jpath (output_addr ++ annotated_add)
               return ppdate''

-------------
-- Run KeY --
-------------

runKeY :: FilePath -> FilePath -> [Flag] -> IO ExitCode
runKeY output_add' output_addr flags = 
 if elem OnlyRV flags
 then return ExitSuccess
 else rawSystem "java" ["-jar","key.starvoors.jar",output_add', output_addr]

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

generateRefPPDFileName :: Filename -> Filename
generateRefPPDFileName fn = 
 let (ext, _:name) = break ('.' ==) $ reverse fn 
     xs = splitOnIdentifier "/" name     
 in if (length xs == 1)
    then reverse name ++ "_optimised.ppd"
    else reverse (head xs) ++ "_optimised.ppd"

prepareRefPPD :: UpgradePPD PPDATE -> UpgradePPD PPDATE
prepareRefPPD = removeGeneratedTriggers . introduceNewHTriples


removeGeneratedTriggers :: UpgradePPD PPDATE -> UpgradePPD PPDATE
removeGeneratedTriggers ppd = 
 do ppdate <- ppd
    return $ remGeneratedTriggers ppdate

remGeneratedTriggers :: PPDATE -> PPDATE
remGeneratedTriggers ppdate@(PPDATE _ (Global ctxt@(Ctxt [] [] [] PNIL (foreach:fors))) _ _ _ _) = 
 let ctxt''  = removeFromTrsCtxt (getCtxtForeach foreach)
     fors'   = (updCtxtForeach foreach ctxt''):fors
     ctxt''' = updateCtxtFors ctxt fors'
     global' = Global ctxt'''
 in updateGlobalPP ppdate global'
remGeneratedTriggers ppdate@(PPDATE _ (Global ctxt) _ _ _ _) = 
 let ctxt'   = removeFromTrsCtxt ctxt
     global' = Global ctxt'
 in updateGlobalPP ppdate global'

removeFromTrsCtxt :: Context -> Context 
removeFromTrsCtxt ctxt@(Ctxt _ _ trs _ _) = updateCtxtTrs ctxt (removeFromTriggers trs)

removeFromTriggers :: Triggers -> Triggers
removeFromTriggers []       = []
removeFromTriggers (tr:trs) = 
 if isInfixOf "_ppden" (tName tr) || isInfixOf "_ppdex" (tName tr)
 then removeFromTriggers trs
 else tr:removeFromTriggers trs

introduceNewHTriples :: UpgradePPD PPDATE -> UpgradePPD PPDATE
introduceNewHTriples ppd = 
 do ppdate <- ppd
    return (updateHTsPP ppdate (newHTriples (htsGet ppdate)))

newHTriples :: HTriples -> HTriples
newHTriples []      = []
newHTriples (h:hts) = 
 let newpre = (removeSelf.head.optimized) h
     pre'   = "(" ++ pre h ++ ") && " ++ newpre
 in if ((head.optimized) h == "(true)")
    then h:newHTriples hts
    else updatePre h pre':newHTriples hts


translateActions :: UpgradePPD PPDATE -> UpgradePPD PPDATE
translateActions ppd =
 do ppdate <- ppd
    return $ translateActInPPD ppdate

translateActInPPD :: PPDATE -> PPDATE
translateActInPPD (PPDATE imps global temps cinvs hts ms) = 
 PPDATE imps (translateActInGlobal global) temps cinvs hts ms


translateActInGlobal :: Global -> Global
translateActInGlobal (Global ctxt) = Global (translateActInCtxt ctxt)

translateActInCtxt :: Context -> Context
translateActInCtxt ctxt = 
 let prop' = translateActInProps (property ctxt)
     fors' = translateActInFors (foreaches ctxt)
 in updateCtxtProps (updateCtxtFors ctxt fors') prop'
 
translateActInProps :: Property -> Property
translateActInProps PNIL                          = PNIL
translateActInProps (PINIT nm tmp bnds props)     = PINIT nm tmp bnds (translateActInProps props)
translateActInProps (Property nm sts trans props) = Property nm sts (translateActInTrans trans) (translateActInProps props)

translateActInFors :: Foreaches -> Foreaches
translateActInFors = map translateActInFor

translateActInFor :: Foreach -> Foreach
translateActInFor foreach = updCtxtForeach foreach (translateActInCtxt (getCtxtForeach foreach))

translateActInTrans :: Transitions -> Transitions
translateActInTrans = map translateActInTran

translateActInTran :: Transition -> Transition
translateActInTran (Transition q (Arrow tr cond act) q') =
 Transition q (Arrow tr cond (translateAction act)) q'

translateAction :: Action -> Action
translateAction []  = ""
translateAction act = 
 case ParAct.parse act of 
      Ok ac -> PrintAct.printTree (translateAct ac)

translateActInTemps :: Templates -> Templates
translateActInTemps TempNil = TempNil
translateActInTemps (Temp tmps) = Temp $ map translateActInTemp tmps

translateActInTemp :: Template -> Template
translateActInTemp tmp = 
 updateTemplateProp tmp (translateActInProps $ tempProp tmp)

