module JMLInjection(injectJMLannotations) where

import Types
import JMLGenerator
import CommonFunctions
import System.Directory
import Data.Char
import UpgradePPDATE
import ErrM
import JavaLanguage
import System.IO
import Control.Lens hiding(Context,pre)

-------------------------------------------------
-- Injecting JML annotations for Hoare triples --
-------------------------------------------------

injectJMLannotations :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO ()
injectJMLannotations ppd jpath output_addr = 
 let toAnalyse_add = output_addr ++ "workspace/files2analyse"
     tmp_add       = output_addr ++ "workspace/tempFiles/"
 in do createDirectoryIfMissing False tmp_add
       createDirectoryIfMissing False toAnalyse_add
       prepareTmpFiles ppd tmp_add jpath
       copyFiles jpath toAnalyse_add
       generateTmpFilesAllConsts ppd (getHTs ppd) toAnalyse_add tmp_add  


------------------------------------- 
-- Generating annotated temp files --
-------------------------------------

prepareTmpFiles :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO [()]
prepareTmpFiles ppd output_add jpath = 
 do let (ppdate, env) = fromOK $ runStateT ppd emptyEnv
    let imports       = ppdate ^. importsGet
    let cinvs         = ppdate ^. cinvariantsGet    
    let ys            = splitCInvariants cinvs []
    let consts        = ppdate ^. htsGet
    let xs            = splitClassHT consts
    let join_xs       = joinClassHT xs []
    let jinfo         = javaFilesInfo env
    let imports'      = [i | i <- imports,not (elem ((\ (Import s) -> s) i) importsInKeY)]
    sequence $ map (\ i -> annotateTmpFiles i output_add jpath jinfo ys join_xs ) imports'

annotateTmpFiles :: Import -> FilePath -> FilePath -> [(String, ClassInfo, JavaFilesInfo)] 
                    -> [(Class, CInvariants)] -> [(ClassInfo, [HTName])] -> IO ()
annotateTmpFiles i output_add jpath jinfo ys jxs = 
  do (main, cl) <- makeAddFile i
     let jpath'      = jpath ++ "/" ++ main
     let output_add' = output_add ++ "/" ++ main
     createDirectoryIfMissing True output_add'
     let file        = jpath' ++ "/" ++ (cl ++ ".java")  
     let tmp         = output_add' ++ "/" ++ (cl ++ ".java")
     r <- readFile file
     let dummyVars = generateDBMFile cl jxs r
     let cinvs     = generateTmpFileCInv cl ys dummyVars
     let nullable  = updateTmpFileCInv cl jinfo cinvs
     writeFile tmp nullable
     return ()

---------------------------------------------
-- Injecting JML annotations for contracts --
---------------------------------------------

generateTmpFilesAllConsts :: UpgradePPD PPDATE -> HTjml -> FilePath -> FilePath -> IO ()
generateTmpFilesAllConsts ppd consts_jml output_add jpath =
 do let (ppdate, env) = fromOK $ runStateT ppd emptyEnv
    let imports       = ppdate ^. importsGet
    let imports'      = [i | i <- imports,not (elem ((\ (Import s) -> s) i) importsInKeY)]
    let ys = map makeAddFile imports'
    sequence [ do 
                  (main, cl) <- y
                  createDirectoryIfMissing True (output_add ++ "/" ++ main) 
                  r <- readFile (jpath ++ "/" ++ main ++ "/" ++ cl ++ ".java")
                  genTmpFilesConst (main, cl) output_add consts_jml r
             | y <- ys
             ]
    return ()

genTmpFilesConst :: (String, ClassInfo) -> FilePath -> HTjml -> String -> IO ()
genTmpFilesConst (main, cl) output_add [] r                      = writeFile (output_add ++ "/" ++ main ++ "/" ++ (cl ++ ".java")) r
genTmpFilesConst (main, cl) output_add ((mn, cl', ov,jml):xs)  r = 
 do if (cl == cl') 
    then do 
            let output_add'' = output_add ++ "/" ++ main
            createDirectoryIfMissing True output_add''
            let tmp  = output_add'' ++ "/" ++ (cl ++ ".java")
            let (ys, zs) = if (mn == cl)
                           then lookForConstructorDef mn (lines r)
                           else lookForMethodDef mn ov (lines r)
            let r' = ((unlines ys) ++ jml ++ (unlines zs))
            genTmpFilesConst (main, cl') output_add xs r'
    else genTmpFilesConst (main, cl) output_add xs  r


lookForMethodDef :: MethodName -> Overriding -> [String] -> ([String], [String])
lookForMethodDef mn _ []        = error $ "Something went wrong when annotating the method " ++ mn ++ ".\n"
lookForMethodDef mn ov (xs:xss) = 
 let ys = splitOnIdentifier mn xs
 in if (length ys == 1)
    then (xs:a, b) --method name does not appear in the line
    else let zs     = (clean.head.tail) ys 
             beginl = (words.head) ys 
         in if ((head zs) == '(')
            then if (length beginl == 1)
                 then (xs:a, b) --not a method definition (does not have modifier or type)
                 else let ws = (clean.head) beginl
                      in if (elem ws javaModifiers)
                         then let ts = splitOnIdentifier "," $ fst $ splitAtClosingParen 0 $ tail zs in
                              if checkArguments ov ts
                              then ([], xs:xss)
                              else (xs:a, b) --not the intended method (overloading)
                         else (xs:a, b) --not a method definition (does not start with modifier)
            else (xs:a, b) --not referring to a method
                where (a, b) = lookForMethodDef mn ov xss

checkArguments :: Overriding -> [String] -> Bool
checkArguments _ []           = False
checkArguments OverNil _      = True
checkArguments (Over []) [[]] = True
checkArguments (Over []) _    = False
checkArguments (Over ts) [[]] = False
checkArguments (Over ts) ts'  =  
 let types = map (head.words) ts'
 in ts == types

lookForConstructorDef :: MethodName -> [String] -> ([String], [String])
lookForConstructorDef mn []       = error $ "Something went wrong when checking the constructor " ++ mn ++ ".\n"
lookForConstructorDef mn (xs:xss) = 
 let ys = splitOnIdentifier mn xs
 in if (length ys <= 1 || length ys > 2)
    then (xs:a, b)
    else let zs     = (clean.head.tail) ys 
             beginl = (clean.head) ys 
         in if (null beginl || elem beginl javaModifiers)
            then if ((head zs) == '(')
                 then ([], xs:xss)                      
                 else (xs:a, b)
            else (xs:a, b)
                where (a, b) = lookForConstructorDef mn xss

-------------------------------------
-- Add nullable to class variables --
-------------------------------------

updateTmpFileCInv :: String -> [(String, ClassInfo, JavaFilesInfo)] -> String -> String
updateTmpFileCInv cl jinfo r =
 let varsc    = getListOfTypesAndVars cl jinfo
     (ys, zs) = lookForClassBeginning cl (lines r)
 in (unlines ys) ++ (unlines (searchAndAnnotateVars zs varsc))

searchAndAnnotateVars :: [String] -> [(String, String)] -> [String]
searchAndAnnotateVars xss []              = xss
searchAndAnnotateVars xss ((type',v):yss) = if (elem type' primitiveJavaTypes)
                                            then searchAndAnnotateVars xss yss
                                            else let xss' = annotateNullable (type',v) xss
                                                 in searchAndAnnotateVars xss' yss

--TODO: This method may need to be upgraded
annotateNullable :: (String, String) -> [String] -> [String]
annotateNullable (type', v) []       = []
annotateNullable (type', v) (xs:xss) = 
 let ident = type' ++ " " ++ v
     ys    = splitOnIdentifier ident xs
 in if (length ys == 1)
    then xs:annotateNullable (type', v) xss
    else let xs' = (head ys) ++ " /*@ nullable @*/ " ++ ident ++ (head.tail) ys
         in xs':xss


----------------------------------------------------
-- Injecting JML annotations for Class Invariants --
----------------------------------------------------

splitCInvariants :: CInvariants -> [(Class, CInvariants)] -> [(Class, CInvariants)]
splitCInvariants [] acum           = acum
splitCInvariants (cinv:cinvs) acum = splitCInvariants cinvs (updateAcum cinv acum)
 
updateAcum :: CInvariant -> [(Class, CInvariants)] -> [(Class, CInvariants)]
updateAcum cinv@(CI class' body) []               = [(class', [cinv])]
updateAcum cinv@(CI class' body) ((cl, cinvs):as) = 
 if (class' == cl)
 then (cl, cinv:cinvs):as
 else (cl,cinvs):updateAcum cinv as

generateTmpFileCInv :: String -> [(Class, CInvariants)] -> String -> String
generateTmpFileCInv cl xs r =
 let cinvs_jml = getCInvariants $ getCInvs' xs cl
     (ys, zs)  = lookForClassBeginning cl (lines r)
 in (unlines ys) ++ cinvs_jml ++ (unlines zs) 

lookForClassBeginning :: ClassInfo -> [String] -> ([String], [String])
lookForClassBeginning c []       = error $  "Something went wrong when checking a class invariant for the class " ++ c  ++ ".\n"
lookForClassBeginning c (xs:xss) = let ys = splitOnIdentifier ("class " ++ c) xs
                                   in if (length ys == 1)
                                      then (xs:a, b)
                                      else let (ts,zs) = splitAtIdentifier '{' $ (head.tail) ys
                                           in if (null zs)
                                              then splitOpeningBracket (xs:xss)                                                   
                                              else ([xs], xss)
                                           where (a, b) = lookForClassBeginning c xss


splitOpeningBracket :: [String] -> ([String],[String])
splitOpeningBracket []       = ([],[])
splitOpeningBracket (xs:xss) = 
 let ys = splitOnIdentifier "{" xs
 in if (length ys == 1)
    then (xs:a,b)
    else ([xs++"\n"],"\n":xss)
       where (a,b) = splitOpeningBracket xss

getCInvs' :: [(Class, CInvariants)] -> Class -> CInvariants
getCInvs' [] _           = []
getCInvs' ((cl', cinvs):xs) cl = if (cl' == cl)
                                then cinvs
                                else getCInvs' xs cl

---------------------------------------
-- Injecting dummy boolean variables --
---------------------------------------

generateDBMFile :: String -> [(ClassInfo, [HTName])] -> String -> String
generateDBMFile cl xs r =
 let dummy_vars = map genDummyVarJava $ lookForConstsNames cl xs
     (ys, zs)   = lookForClassBeginning cl (lines r)
 in (unlines ys) ++ concat dummy_vars ++ (unlines zs) 


genDummyVarJava :: HTName -> String
genDummyVarJava cn = "  public static final boolean " ++ cn ++ " = true;\n"

lookForConstsNames :: ClassInfo -> [(ClassInfo, [HTName])] -> [HTName]
lookForConstsNames cn []             = []
lookForConstsNames cn ((cn', cs):xs) = if (cn == cn')
                                       then cs
                                       else lookForConstsNames cn xs 

splitClassHT :: HTriples -> [(ClassInfo, HTName)]
splitClassHT []     = []
splitClassHT (c:cs) = (_methodCN c ^. clinf, c ^. htName) : splitClassHT cs

joinClassHT :: [(ClassInfo, HTName)] -> [(ClassInfo, [HTName])] -> [(ClassInfo, [HTName])]
joinClassHT [] jcc     = jcc
joinClassHT (x:xs) jcc = joinClassHT xs (updateJCC x jcc) 

updateJCC :: (ClassInfo, HTName) -> [(ClassInfo, [HTName])] -> [(ClassInfo, [HTName])]
updateJCC (cn, c) []            = [(cn, [c])]
updateJCC (cn, c) ((cn',cs):xs) = if (cn == cn')
                                  then (cn', c:cs) : xs
                                  else (cn', cs):updateJCC (cn, c) xs
