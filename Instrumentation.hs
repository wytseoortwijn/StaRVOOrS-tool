module Instrumentation (methodsInstrumentation) where

import qualified Types as T
import CommonFunctions
import JavaParser
import System.Directory
import UpgradePPDATE
import ErrM
import JavaLanguage
import Control.Lens hiding(Context,pre)

--------------------------
-- Code Instrumentation --
--------------------------

-- creates new files with methods instrumented
methodsInstrumentation :: UpgradePPD T.PPDATE -> FilePath -> FilePath -> IO ()
methodsInstrumentation ppd jpath output_add =
 do let (ppdate, env) = fromOK $ runStateT ppd emptyEnv
    let consts        = T._htsGet ppdate
    if (null consts)
    then return ()
    else methodsInstrumentation' ppd jpath output_add

methodsInstrumentation' :: UpgradePPD T.PPDATE -> FilePath -> FilePath -> IO ()
methodsInstrumentation' ppd jpath output_add =
  do let (ppdate, env) = fromOK $ runStateT ppd emptyEnv
     let consts        = ppdate ^. T.htsGet
     let imp           = ppdate ^. T.importsGet
     sequence [ instrumentFile i consts jpath output_add
              | i <- imp, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
              ]
     putStrLn "Java files generation completed."


instrumentFile :: T.Import -> T.HTriples -> FilePath -> FilePath -> IO ()
instrumentFile i consts jpath output_add =
 do (main, cl) <- makeAddFile i
    let file_add = jpath ++ main ++ "/" ++ (cl ++ ".java")
    r <- readFile file_add
    let java = (\(Right x) -> x) $ parseJavaFile r
    let mns  = removeDuplicates $ getMethodsNames cl consts
    let java_aux = lookForCTD cl $ map getClassDecls $ getClassTypeDecls java
    let decls  = (getDecls.getClassBody) java_aux
    let decls' = instrumentMethodMemberDecl' decls mns
    let output_add' = output_add ++ main
    let r'  = unlines $ addInstrumentedMethodsInFile (lines r) decls'
    let r'' = addNewImportInfo r' decls' cl
    createDirectoryIfMissing True output_add'
    writeFile (output_add' ++ "/" ++ (cl ++ ".java")) r''


addNewImportInfo :: String -> [(String,String,String)] -> T.ClassInfo -> String
addNewImportInfo s [] _     = s
addNewImportInfo s (_:_) cl = let xs = splitOnIdentifier "package" (clean s)
                              in if (length xs == 1)
                                 then let s'    = lines s
                                      in unlines $ addFidDec s' cl
                                 else let s'    = lines ((head.tail) xs)
                                          begin = (head xs) ++ "package" ++ (head s') ++ "\n"
                                      in begin ++ "\nimport ppArtifacts.IdPPD;\n" ++ unlines (addFidDec (tail s') cl)

addFidDec :: [String] -> T.ClassInfo -> [String]
addFidDec [] _         = []
addFidDec (xs:xss) cl  = if (clean xs == "")
                         then xs:addFidDec xss cl
                         else let ys = splitOnIdentifier ("class " ++ cl) xs
                              in if (length ys == 1)
                                 then xs:addFidDec xss cl
                                 else xs:addFidDec' xss 

addFidDec' :: [String] -> [String]
addFidDec' []       = []
addFidDec' (xs:xss) = if (clean xs == "")
                      then (xs ++ "\n  public IdPPD fid = new IdPPD();\n"):xss 
                      else xs:addFidDec' xss

-- Added due to bug in the java library
addInstrumentedMethodsInFile :: [String] -> [(String, String, String)] -> [String]
addInstrumentedMethodsInFile xss []     = xss
addInstrumentedMethodsInFile xss (y:ys) = addInstrumentedMethodsInFile (addMethodInFile y xss) ys

-- Added due to bug in the java library
addMethodInFile :: (String,String,String) -> [String] -> [String]
addMethodInFile (id,methodaux,method) []       = []
addMethodInFile (id,methodaux,method) (xs:xss) = 
 let ys = splitOnIdentifier id xs
 in if (length ys == 1)
    then xs:addMethodInFile (id,methodaux,method) xss
    else let zs     = (clean.head.tail) ys 
             beginl = (words.head) ys 
         in if ((head zs) == '(')
            then if (length beginl == 1)
                 then xs:addMethodInFile (id,methodaux,method) xss
                 else let ws = (clean.head) beginl in
                      if (elem ws javaModifiers)
                      then if ((clean $ fst $ break (=='{') $ reverse xs) == "")
                           then (methodaux ++ "\n"):method:xss
                           else (methodaux ++ "\n"):(head $ lines method):xss
                      else xs:addMethodInFile (id,methodaux,method) xss
            else xs:addMethodInFile (id,methodaux,method) xss

