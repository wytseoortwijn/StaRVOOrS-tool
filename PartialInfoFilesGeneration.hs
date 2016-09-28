module PartialInfoFilesGeneration (htsJavaFileGen, idFileGen, oldExprFileGen,messagesFileGen,cloningFileGen) where

import Types
import System.Directory
import System.Environment
import CommonFunctions
import OperationalizationPP
import UpgradePPDATE
import ErrM
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-----------------------
-- HoareTriples.java --
-----------------------

htsJavaFileGen :: UpgradePPD PPDATE -> FilePath -> IO ()
htsJavaFileGen ppd output_add = 
 let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     imp           = importsGet ppdate
     global        = globalGet ppdate
     consts        = htsGet ppdate
     oldExpM       = oldExpTypes env
     forallop      = map (\c -> genMethodsForConstForall c env oldExpM) consts
     new_methods   = map snd forallop
     consts'       = map fst forallop
     existsop      = map (\c -> genMethodsForConstExists c env oldExpM) consts'
     new_methods'  = map snd existsop
     consts''      = map fst existsop
     methods       = map (\ c -> (methodForPre c env, methodForPost c env oldExpM)) consts''
     body          = concat $ map joinInfo $ zip3 methods new_methods new_methods'
 in do 
       let address = output_add ++ "HoareTriples.java"
       writeFile address (genPackageInfo ++ genImports' imp ++ "\n\n")
       appendFile address "public class HoareTriples {\n\n  HoareTriples () {}\n\n"
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

-- Hoare triples methods
genMethodsForConstForall :: HT -> Env -> OldExprM -> (HT, String)
genMethodsForConstForall c env oldExpM =
 let (body_pre, body_post) = operationalizeForall c env oldExpM
     newpre  = flattenBody body_pre
     newpost = flattenBody body_post
     pre_opmethods  = concat $ extracMethodDefinitions body_pre
     post_opmethods = concat $ extracMethodDefinitions body_post
     c'  = updatePre c newpre
     c'' = updatePost c' newpost
 in (c'', pre_opmethods ++ post_opmethods)


genMethodsForConstExists :: HT -> Env -> OldExprM -> (HT, String)
genMethodsForConstExists c env oldExpM =
 let (body_pre, body_post) = operationalizeExists c env oldExpM
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


methodForPost :: HT -> Env -> OldExprM -> String
methodForPost c env oldExpM =
 let (argsPost, argsPostwt) = lookForAllExitTriggerArgs env (fst $ methodCN c) (snd $ methodCN c)
     tnvs      = getConstTnv c oldExpM
     tnvs'     = auxNewVars tnvs
     newargs   = addComma tnvs'
     nargs     = if (null tnvs) then "" else "," ++ newargs
     args      = addComma $ map unwords $ map (\s -> if isInfixOf "ret_ppd" (head $ tail s) then (head s):["ret"] else s) $ map words $ splitOnIdentifier "," (argsPost ++ nargs)
 in 
  "  // " ++ (htName c) ++ "\n"
  ++ "  public static boolean " ++ (htName c) ++ "_post(" ++ args ++ ") {\n"
  ++ "    return " ++ (post c) ++ ";\n" 
  ++ "  }\n\n"

--check opt for new predicates for the precondition due to partial proof
methodForPre :: HT -> Env -> String
methodForPre c env =
 let (argsPre, _) = lookForAllEntryTriggerArgs env (fst $ methodCN c) (snd $ methodCN c)     
 in 
  "  // " ++ (htName c) ++ "\n"
  ++ "  public static boolean " ++ (htName c) ++ "_pre(" ++ argsPre ++ ") {\n" 
  ++ "    return " ++ pre c ++ addNewPre c ++ ";\n"
  ++ "  }\n\n"


addNewPre :: HT -> String
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

lookforArgs :: [(Trigger, [String])] -> Trigger -> [String]
lookforArgs [] _     = []
lookforArgs (x:xs) e = if (fst x==e)
                       then snd x
                       else lookforArgs xs e 


-------------
-- Id.java --
-------------

idFileGen :: FilePath -> IO ()
idFileGen output_add = writeFile (output_add ++ "Id.java") idGen
    
idGen :: String
idGen =
 "package ppArtifacts;\n\n"
  ++ "public class Id {\n\n"
  ++ "  private static int count = 0; \n\n"
  ++ "  public Id () { }\n\n"
  ++ "public Integer getNewId() {\n"
  ++ "  Integer r = new Integer(count);\n"
  ++ "  count++;\n\n"
  ++ "  if (Integer.MAX_VALUE == count)\n"
  ++ "     count = 0;\n"
  ++ "  return r;\n"
  ++ "}\n\n"
  ++ "}\n"

-------------------
-- OldExpr Files --
-------------------

oldExprFileGen :: FilePath -> UpgradePPD PPDATE -> IO [()]
oldExprFileGen output_add ppd = 
 let (ppdate, env) = (\(Ok x) -> x) $ runStateT ppd emptyEnv
     consts        = htsGet ppdate      
     oldExpM       = oldExpTypes env 
     consts'       = [c | c <- consts, noOldExprInHT $ Map.lookup (htName c) oldExpM]    
 in if Map.null oldExpM
    then return [()]
    else sequence [writeFile (output_add ++ (snd $ oldExpGen c oldExpM)) (fst $ oldExpGen c oldExpM) | c <- consts']
                     where noOldExprInHT v = v /= Nothing && (not.null.fromJust) v 

oldExpGen :: HT -> OldExprM -> (String,String)
oldExpGen c oldExpM = 
 let cn = htName c
     nameClass = "Old_" ++ cn
     xs        = map (\(x,y,z) -> (y,z)) $ fromJust $ Map.lookup cn oldExpM
 in ("package ppArtifacts;\n\n"
    ++ "public class " ++ nameClass ++ " {\n\n"
    ++ varDeclOldExpr xs
    ++ "  public " ++ nameClass ++ "() { }\n\n"
    ++ "  public " ++ nameClass ++ "(" ++ addComma (map (\(x,y) -> x ++ " " ++ y) xs) ++ ") {\n"
    ++ constructorOldExpr xs 
    ++ "  }\n\n"
    ++ "}\n", nameClass ++ ".java")


varDeclOldExpr :: [(String,String)] -> String
varDeclOldExpr []           = "\n"
varDeclOldExpr ((t,exp):xs) = 
 "  public " ++ t ++ " " ++ exp ++ ";\n" ++ varDeclOldExpr xs
 
constructorOldExpr :: [(String,String)] -> String
constructorOldExpr []           = ""
constructorOldExpr ((t,exp):xs) = "    " ++ "this." ++ exp ++ " = " ++ exp ++ ";\n" ++ constructorOldExpr xs

----------------------------------------
-- Messages to send over the channels --
----------------------------------------

messagesFileGen :: FilePath -> Env -> IO [()]
messagesFileGen output_add env = 
 let oldExpM  = oldExpTypes env
     cns      = map ("Old_"++) [x | (x,y) <- Map.toList oldExpM, (not.null) y]
     files    = (output_add ++ "MessagesPPD.java") : map (\s -> output_add ++ "Messages" ++ s ++ ".java") cns
     xs       = messagesGen : map messageOldExpGen cns
 in sequence $ map (uncurry writeFile) $ zip files xs
    
messagesGen :: String
messagesGen =
 "package ppArtifacts;\n\n"
  ++ "public class MessagesPPD {\n\n"
  ++ "  public Integer id; \n\n"
  ++ "  public MessagesPPD (Integer id) { \n"
  ++ "     this.id = id; \n" 
  ++ "  }\n\n"
  ++ "  public static int getId(MessagesPPD m) {\n"
  ++ "     return m.id;\n"
  ++ "  }\n"
  ++ "}\n"
 
messageOldExpGen :: String -> String
messageOldExpGen t =
 "package ppArtifacts;\n\n"
  ++ "public class Messages" ++ t ++ " extends MessagesPPD  {\n\n"
  ++ "  public " ++ t ++ " oldExpr; \n\n"
  ++ "  public Messages" ++ t ++ " (Integer id, " ++ t ++ " oldExpr) { \n"
  ++ "     super(id); \n" 
  ++ "     this.oldExpr = oldExpr; \n" 
  ++ "  }\n\n"  
  ++ "  public static " ++ t ++ " getOldExpr(Messages" ++ t ++ " m) {\n"
  ++ "     return m.oldExpr;\n"
  ++ "  }\n"
  ++ "}\n"


------------------------------------
-- Cloning reference type objects --
------------------------------------

cloningFileGen :: FilePath -> IO ()
cloningFileGen output_add = writeFile (output_add ++ "CopyUtilsPPD.java") cloningGen
    
cloningGen :: String
cloningGen =
 "package ppArtifacts;\n\n"
  ++ "import java.lang.reflect.Field;\nimport java.lang.reflect.Modifier;\n\n"
  ++ "public class CopyUtilsPPD {\n\n"
  ++ "  private CopyUtilsPPD() {}\n\n"
  ++ "  public static void copy(Object src, Object dest) {\n"
  ++ "     copyFields(src, dest, src.getClass());\n"
  ++ "  }\n\n"
  ++ "  private static void copyFields(Object src, Object dest, Class<?> cl) {\n"
  ++ "     Field[] fields = cl.getDeclaredFields();\n"
  ++ "     for (Field f : fields) {\n"
  ++ "        if (copyableField(f)) {\n"
  ++ "           f.setAccessible(true);\n"
  ++ "           copyFieldValue(src, dest, f);\n"
  ++ "        }\n"
  ++ "     }\n"
  ++ "     cl = cl.getSuperclass();\n"
  ++ "     if (cl != null) {\n"
  ++ "        copyFields(src, dest, cl);\n"
  ++ "     }\n"
  ++ "  }\n\n"
  ++ "  private static void copyFieldValue(Object src, Object dest, Field f) {\n"
  ++ "     try {\n"
  ++ "         Object value = f.get(src);\n"
  ++ "         f.set(dest, value);\n"
  ++ "     } catch (ReflectiveOperationException e) {\n"
  ++ "         throw new RuntimeException(e);\n"
  ++ "     }\n"
  ++ "  }\n\n"
  ++ "  private static boolean copyableField(Field f) {\n"
  ++ "     return !Modifier.isStatic(f.getModifiers());\n"
  ++ "  }\n\n"
  ++ "}\n"

