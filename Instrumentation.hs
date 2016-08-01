module Instrumentation (programMethods,programVariables, methodsInstrumentation, methodsNames,updateVarsEnv) where

import qualified Types as T
import CommonFunctions
import JavaParser
import System.Directory
import Data.Char
import UpgradePPDATE
import ErrM
import JavaLanguage

--------------------------
-- Code Instrumentation --
--------------------------

-- creates new files with methods instrumented
methodsInstrumentation :: UpgradePPD T.PPDATE -> FilePath -> FilePath -> IO ()
methodsInstrumentation ppd jpath output_add =
 do let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
    let consts        = T.contractsGet ppdate
    if (null consts)
    then return ()
    else methodsInstrumentation' ppd jpath output_add

methodsInstrumentation' :: UpgradePPD T.PPDATE -> FilePath -> FilePath -> IO ()
methodsInstrumentation' ppd jpath output_add =
  do let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     let consts        = T.contractsGet ppdate
     let imp           = T.importsGet ppdate
     sequence [ instrumentFile i consts jpath output_add
              | i <- imp, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
              ]
     putStrLn "Java files generation completed."

-- if same class name in different files, fix this method
instrumentFile :: T.Import -> T.Contracts -> FilePath -> FilePath -> IO ()
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
                                      in begin ++ "\nimport ppArtifacts.Id;\n" ++ unlines (addFidDec (tail s') cl)

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
                      then (xs ++ "\n  public Id fid = new Id();\n"):xss 
                      else xs:addFidDec' xss

-- Added due to bug in the java library
addInstrumentedMethodsInFile :: [String] -> [(String, String, String)] -> [String]
addInstrumentedMethodsInFile xss []                          = xss
addInstrumentedMethodsInFile xss ((id,methodaux, method):ys) = 
 addInstrumentedMethodsInFile (addMethodInFile (id,methodaux, method) xss) ys

javaModifiers' = ["public", "private", "protected", "no modifier"]

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
                      if (elem ws javaModifiers')
                      then if ((clean $ fst $ break (=='{') $ reverse xs) == "")
                           then (methodaux ++ "\n"):method:xss
                           else (methodaux ++ "\n"):(head $ lines method):xss
                      else xs:addMethodInFile (id,methodaux,method) xss
            else xs:addMethodInFile (id,methodaux,method) xss


--Adds to the upgraded ppDATE's env the variables of all the java files involved in the verification process
programVariables :: UpgradePPD T.PPDATE -> FilePath -> IO (UpgradePPD T.PPDATE)
programVariables ppd jpath = 
 do let imports = T.importsGet (fst . (\(Ok x) -> x) $ runStateT ppd emptyEnv)
    vars <- sequence [ getVariables i jpath
                     | i <- imports, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
                     ]
    return $ updateVarsEnv ppd vars

updateVarsEnv :: UpgradePPD T.PPDATE -> [(String, T.ClassInfo, [(String, String)])] -> UpgradePPD T.PPDATE
updateVarsEnv ppd vars = ppd >>= (\x -> do env <- get; put (env { varsInFiles = vars }); return x)                     

getVariables :: T.Import -> FilePath -> IO (String, T.ClassInfo, [(String, String)])
getVariables i jpath =
  do (main, cl) <- makeAddFile i
     let file_add = jpath ++ main ++ "/" ++ (cl ++ ".java")
     r <- readFile file_add
     let java = (\(Right x) -> x) $ parseJavaFile r
     let java_aux = lookForCTD cl $ map getClassDecls $ getClassTypeDecls java
     let vars = map getTypeAndId $ (getVarDecl'.getMemberDecl.getDecls.getClassBody) java_aux
     return (main, cl, (splitVars vars))


--Adds to the upgraded ppDATE's env the method declarations of all the java files involved in the verification process
programMethods :: UpgradePPD T.PPDATE -> FilePath -> IO (UpgradePPD T.PPDATE)
programMethods ppd jpath = 
 do let imports = T.importsGet (fst . (\(Ok x) -> x) $ runStateT ppd emptyEnv)
    ms <- sequence [ getMethods i jpath
                   | i <- imports, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
                   ]
    return $ updateMethodsEnv ppd ms

updateMethodsEnv :: UpgradePPD T.PPDATE -> [(String, T.ClassInfo, [(T.Id,String,[String])])] -> UpgradePPD T.PPDATE
updateMethodsEnv ppd ms = ppd >>= (\x -> do env <- get; put (env { methodsInFiles = ms }); return x)                     

getMethods :: T.Import -> FilePath -> IO (String, T.ClassInfo, [(T.Id,String,[String])])
getMethods i jpath =
  do (main, cl) <- makeAddFile i
     let file_add = jpath ++ main ++ "/" ++ (cl ++ ".java")
     r <- readFile file_add
     let java     = (\(Right x) -> x) $ parseJavaFile r
     let java_aux = lookForCTD cl $ map getClassDecls $ getClassTypeDecls java
     let methods = (getMethodDecl.getDecls.getClassBody) java_aux
     return (main, cl, (map methodsDetails methods))


--TODO: Modify to use info in the enviroment (methodsInFiles) when dealing with private variables
--returns the name of all the public methods in the java files involved in the verification process
methodsNames :: UpgradePPD T.PPDATE -> FilePath -> IO [(String, T.ClassInfo, [String])]
methodsNames ppd jpath = 
 do let (ppdate, env) =  (\(Ok x) -> x) $ runStateT ppd emptyEnv
    let imports       = T.importsGet ppdate
    mnames <- sequence [ getMethodName i jpath
                       | i <- imports, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
                       ]
    return mnames

getMethodName :: T.Import -> FilePath -> IO (String, T.ClassInfo, [String])
getMethodName i jpath =
  do (main, cl) <- makeAddFile i
     let file_add = jpath ++ main ++ "/" ++ (cl ++ ".java")
     r <- readFile file_add
     let java     = (\(Right x) -> x) $ parseJavaFile r
     let java_aux = lookForCTD cl $ map getClassDecls $ getClassTypeDecls java
     let methods  = (getMethodDeclId.getMemberDecl.getDecls.getClassBody) java_aux
     return (main, cl, methods)

