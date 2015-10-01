module PartialInfoFilesGeneration (contractsJavaFileGen, idFileGen) where

import Types
import System.Directory
import System.Environment
import CommonFunctions
import OperationalizationPP
import UpgradePPDATE
import ErrM


--------------------
-- Contracts.java --
--------------------

contractsJavaFileGen :: UpgradePPD PPDATE -> FilePath -> [(Contract, Variables)] -> IO ()
contractsJavaFileGen ppd output_add tnewvars = 
 let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     imp           = importsGet ppdate
     global        = globalGet ppdate
     events        = getAllEvents global
     consts        = contractsGet ppdate
     forallop      = map (\c -> genMethodsForConstForall c env tnewvars) consts
     new_methods   = map snd forallop
     consts'       = map fst forallop
     existsop      = map (\c -> genMethodsForConstExists c env tnewvars) consts'
     new_methods'  = map snd existsop
     consts''      = map fst existsop
     methods       = map (\ c -> (methodForPre c env, methodForPost c env tnewvars)) consts''
     body          = concat $ map joinInfo $ zip3 methods new_methods new_methods'
 in do 
       let address = output_add ++ "Contracts.java"
       writeFile address (genPackageInfo ++ genImports' imp ++ "\n\n")
       appendFile address "public class Contracts {\n\n  Contracts () {}\n\n"
       if (null consts) then return () else appendFile address body
       appendFile address "\n}"

joinInfo :: ((String, String), String, String) -> String
joinInfo ((pre_s, post_s), opf, ope) = 
 pre_s 
 ++ "  " ++ post_s
 ++ "  " ++ opf 
 ++ "  " ++ ope 

-- Package info
genPackageInfo :: String
genPackageInfo = "package ppArtifacts;\n\n"

-- Imports info
getImports' :: Imports -> String
getImports' []            = ""
getImports' (Import s:xs) = "import " ++ s ++ ";\n" ++ getImports' xs

genImports' :: Imports -> String
genImports' xss = getImports' xss

-- Contracts methods
genMethodsForConstForall :: Contract -> Env -> [(Contract, Variables)] -> (Contract, String)
genMethodsForConstForall c env tnewvars =
 let (body_pre, body_post) = operationalizeForall c env tnewvars
     newpre  = flattenBody body_pre
     newpost = flattenBody body_post
     pre_opmethods  = concat $ extracMethodDefinitions body_pre
     post_opmethods = concat $ extracMethodDefinitions body_post
     c'  = updatePre c newpre
     c'' = updatePost c' newpost
 in (c'', pre_opmethods ++ post_opmethods)


genMethodsForConstExists :: Contract -> Env -> [(Contract, Variables)] -> (Contract, String)
genMethodsForConstExists c env tnewvars =
 let (body_pre, body_post) = operationalizeExists c env tnewvars
     newpre  = flattenBody body_pre
     newpost = flattenBody body_post
     pre_opmethods  = concat $ extracMethodDefinitions body_pre
     post_opmethods = concat $ extracMethodDefinitions body_post
     c'  = updatePre c newpre
     c'' = updatePost c' newpost
 in (c'', pre_opmethods ++ post_opmethods)


auxNewVars :: Variables -> [String]
auxNewVars []                          = []
auxNewVars (Var _ t [VarDecl id _]:xs) = (t ++ " " ++ id):auxNewVars xs

methodForPost :: Contract -> Env -> [(Contract, Variables)] -> String
methodForPost c env ctnewvars =
 let (argsPost, argsPostwt) = lookForAllExitEventArgs env (snd $ methodCN c)
     tnvs    = getConstTnv c ctnewvars
     tnvs'   = auxNewVars tnvs
     newargs = init $ foldr (\ x xs -> x ++ "," ++ xs) "" tnvs'
     nargs   = if (null tnvs) then "" else "," ++ newargs
 in 
  "  // " ++ (contractName c) ++ "\n"
  ++ "  public static boolean " ++ (contractName c) ++ "_post(" ++ argsPost ++ nargs ++ ") {\n" 
  ++ "    return " ++ (post c) ++ ";\n" 
  ++ "  }\n\n"

getConstTnv :: Contract ->  [(Contract, Variables)] -> Variables
getConstTnv c []             = []
getConstTnv c ((c',tnvs):cs) = if (c == c')
                               then tnvs
                               else getConstTnv c cs 

--check opt for new predicates for the precondition due to partial proof
methodForPre :: Contract -> Env -> String
methodForPre c env =
 let (argsPre, argsPrewt) = lookForAllEntryEventArgs env (snd $ methodCN c)     
 in 
  "  // " ++ (contractName c) ++ "\n"
  ++ "  public static boolean " ++ (contractName c) ++ "_pre(" ++ argsPre ++ ") {\n" 
  ++ "    return " ++ pre c ++ addNewPre c ++ ";\n"
  ++ "  }\n\n"


addNewPre :: Contract -> String
addNewPre c = if (null (optimized c))
              then ""
              else " && " ++ (head.optimized) c

extracMethodDefinitions :: [Either (String,String) String] -> [String]
extracMethodDefinitions []             = []
extracMethodDefinitions ((Right x):xs) = extracMethodDefinitions xs
extracMethodDefinitions ((Left x):xs)  = (snd x):extracMethodDefinitions xs

flattenBody :: [Either (String,String) String] -> String
flattenBody []             = ""
flattenBody ((Right x):xs) = x ++ flattenBody xs
flattenBody ((Left x):xs)  = (fst x) ++ flattenBody xs

lookforArgs :: [(Event, [String])] -> Event -> [String]
lookforArgs [] _     = []
lookforArgs (x:xs) e = if (fst x==e)
                       then snd x
                       else lookforArgs xs e 


-------------
-- Id.java --
-------------

idFileGen :: FilePath -> IO ()
idFileGen output_add = 
 do 
    let output_add' = output_add ++ "Id.java"
    writeFile output_add' idGen
    
idGen :: String
idGen =
 "package ppArtifacts;\n\n"
  ++ "public class Id {\n\n"
  ++ "  private static int count; \n\n"
  ++ "  public Id () {\n"
  ++ "     count = 0;\n"
  ++ "  }\n\n"
  ++ "public Integer getNewId() {\n"
  ++ "  Integer r = new Integer(count);\n"
  ++ "  count++;\n\n"
  ++ "  if (Integer.MAX_VALUE == count)\n"
  ++ "     count = 0;\n"
  ++ "  return r;\n"
  ++ "}\n\n"
  ++ "}\n"

