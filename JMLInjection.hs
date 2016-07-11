module JMLInjection(generateTmpFilesAllConsts,generateTmpFilesCInvs,updateTmpFilesCInvs,generateDummyBoolVars) where

import Types
import ParserJML as PJML
import CommonFunctions
import System.Directory
import Data.Char
import UpgradePPDATE
import ErrM
import JavaLanguage

-------------------------------------------------
-- Injecting JML annotations for Hoare triples --
-------------------------------------------------

generateTmpFilesAllConsts :: UpgradePPD PPDATE -> [(MethodName, ClassInfo, String)] -> FilePath -> FilePath -> IO ()
generateTmpFilesAllConsts ppd consts_jml output_add jpath =
 do let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
    let imports       = importsGet ppdate
    let ys = map makeAddFile imports
    sequence [ do 
                  (main, cl) <- y
                  createDirectoryIfMissing True (output_add ++ "/" ++ main) 
                  r <- readFile (jpath ++ "/" ++ main ++ "/" ++ cl ++ ".java")
                  genTmpFilesConst (main, cl) output_add consts_jml r
             | y <- ys
             ]
    return ()

genTmpFilesConst :: (String, ClassInfo) -> FilePath -> [(MethodName, ClassInfo, String)] -> String -> IO ()
genTmpFilesConst (main, cl) output_add [] r                   = writeFile (output_add ++ "/" ++ main ++ "/" ++ (cl ++ ".java")) r
genTmpFilesConst (main, cl) output_add ((mn, cl', jml):xs)  r = 
 do 
    if (cl == cl') 
    then do 
            let output_add'' = output_add ++ "/" ++ main
            createDirectoryIfMissing True output_add''
            let tmp  = output_add'' ++ "/" ++ (cl ++ ".java")
            let (ys, zs) = if (mn == cl)
                           then lookForConstructorDef mn (lines r)
                           else lookForMethodDef mn (lines r)
            let r' = ((unlines ys) ++ jml ++ (unlines zs))
            genTmpFilesConst (main, cl') output_add xs r'
    else genTmpFilesConst (main, cl) output_add xs  r


lookForMethodDef :: MethodName -> [String] -> ([String], [String])
lookForMethodDef mn []       = error $ "Something went wrong when checking the method " ++ mn ++ ".\n"
lookForMethodDef mn (xs:xss) = 
 let ys = splitOnIdentifier mn xs
 in if (length ys == 1)
    then (xs:a, b)
    else let zs     = (clean.head.tail) ys 
             beginl = (words.head) ys 
         in if ((head zs) == '(')
            then if (length beginl == 1)
                 then (xs:a, b)
                 else let ws = (clean.head) beginl
                      in if (elem ws javaModifiers)
                         then ([], xs:xss)
                         else (xs:a, b)
            else (xs:a, b)
                where (a, b) = lookForMethodDef mn xss
  

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

updateTmpFilesCInvs :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO [()]
updateTmpFilesCInvs ppd output_add jpath = 
 do let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
    let imports       = importsGet ppdate
    sequence $ map (\ i -> updateTmpFileCInv i output_add jpath (varsInFiles env)) imports

updateTmpFileCInv :: Import -> FilePath -> FilePath -> [(String, String, [(String,String)])] -> IO ()
updateTmpFileCInv i output_add jpath vars =
  do (main, cl) <- makeAddFile i
     let jpath' = jpath ++ "/" ++ main
     let output_add' = output_add ++ "/" ++ main
     createDirectoryIfMissing True output_add'
     let file        = jpath' ++ "/" ++ (cl ++ ".java")    
     let tmp         = output_add' ++ "/" ++ (cl ++ ".java")
     r <- readFile file
     let varsc = getListOfTypesAndVars cl vars
     let (ys, zs) = lookForClassBeginning cl (lines r)
     writeFile tmp ((unlines ys) ++ (unlines (searchAndAnnotateVars zs varsc)))


searchAndAnnotateVars :: [String] -> [(String, String)] -> [String]
searchAndAnnotateVars xss []              = xss
searchAndAnnotateVars xss ((type',v):yss) = if (elem type' primitiveJavaTypes)
                                            then searchAndAnnotateVars xss yss
                                            else let xss' = annotateNullable (type',v) xss
                                                 in searchAndAnnotateVars xss' yss

--This method may need to be upgraded
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

generateTmpFilesCInvs :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO [()]
generateTmpFilesCInvs ppd output_add jpath = 
 let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
     imports       = importsGet ppdate
     cinvs         = cinvariantsGet ppdate
     xs            = splitCInvariants cinvs []
 in sequence $ map (\ i -> generateTmpFileCInv i output_add jpath xs) imports

splitCInvariants :: CInvariants -> [(Class, CInvariants)] -> [(Class, CInvariants)]
splitCInvariants [] acum           = acum
splitCInvariants (cinv:cinvs) acum = splitCInvariants cinvs (updateAcum cinv acum)
 
updateAcum :: CInvariant -> [(Class, CInvariants)] -> [(Class, CInvariants)]
updateAcum cinv@(CI class' body) []               = [(class', [cinv])]
updateAcum cinv@(CI class' body) ((cl, cinvs):as) = 
 if (class' == cl)
 then (cl, cinv:cinvs):as
 else (cl,cinvs):updateAcum cinv as

generateTmpFileCInv :: Import -> FilePath -> FilePath -> [(Class, CInvariants)] -> IO ()
generateTmpFileCInv i output_add jpath xs =
  do (main, cl) <- makeAddFile i
     let output_add'' = output_add ++ "/" ++ main
     createDirectoryIfMissing True output_add''
     let file = jpath ++ main ++ "/" ++ (cl ++ ".java")
     let tmp  = output_add'' ++ "/" ++ (cl ++ ".java")
     r <- readFile file
     let cinvs_jml = getCInvariants $ getCInvs' xs cl
     let (ys, zs) = lookForClassBeginning cl (lines r)
     writeFile tmp ((unlines ys) ++ cinvs_jml ++ (unlines zs)) 

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

generateDummyBoolVars :: UpgradePPD PPDATE -> FilePath -> FilePath -> IO [()]
generateDummyBoolVars ppd output_add jpath = 
 let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
     imports       = importsGet ppdate
     consts        = contractsGet ppdate
     xs            = splitClassContract consts
     join_xs       = joinClassContract xs []
  in sequence $ map (\ i -> generateDBMFile i output_add jpath join_xs) imports


generateDBMFile :: Import -> FilePath -> FilePath -> [(ClassInfo, [ContractName])] -> IO ()
generateDBMFile i output_add jpath xs =
  do (main, cl) <- makeAddFile i
     createDirectoryIfMissing True output_add
     let output_add'' = output_add ++ "/" ++ main
     createDirectoryIfMissing True output_add''
     let file = jpath ++ main ++ "/" ++ (cl ++ ".java")
     let tmp  = output_add'' ++ "/" ++ (cl ++ ".java")
     r <- readFile file
     let dummy_vars = map genDummyVarJava $ lookForConstsNames cl xs
     let (ys, zs) = lookForClassBeginning cl (lines r)
     writeFile tmp ((unlines ys) ++ concat dummy_vars ++ (unlines zs)) 

genDummyVarJava :: ContractName -> String
genDummyVarJava cn = "  public static final boolean " ++ cn ++ " = true;\n"

lookForConstsNames :: ClassInfo -> [(ClassInfo, [ContractName])] -> [ContractName]
lookForConstsNames cn []             = []
lookForConstsNames cn ((cn', cs):xs) = if (cn == cn')
                                       then cs
                                       else lookForConstsNames cn xs 

splitClassContract :: Contracts -> [(ClassInfo, ContractName)]
splitClassContract []     = []
splitClassContract (c:cs) = (fst $ methodCN c, contractName c) : splitClassContract cs

joinClassContract :: [(ClassInfo, ContractName)] -> [(ClassInfo, [ContractName])] -> [(ClassInfo, [ContractName])]
joinClassContract [] jcc     = jcc
joinClassContract (x:xs) jcc = joinClassContract xs (updateJCC x jcc) 

updateJCC :: (ClassInfo, ContractName) -> [(ClassInfo, [ContractName])] -> [(ClassInfo, [ContractName])]
updateJCC (cn, c) []            = [(cn, [c])]
updateJCC (cn, c) ((cn',cs):xs) = if (cn == cn')
                                  then (cn', c:cs) : xs
                                  else (cn', cs):updateJCC (cn, c) xs

