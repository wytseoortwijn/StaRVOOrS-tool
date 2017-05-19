module JavaFilesAnalysis(programVariables,programMethods,updateVarsEnv) where 

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.Lexer
import JavaParser
import JavaLanguage
import qualified Types as T
import UpgradePPDATE
import ErrM
import CommonFunctions

-----------------------------------
-- Static analysis of java files --
-----------------------------------

--
--Adds to the upgraded ppDATE's env the variables of all the java files involved in the verification process
--
programVariables :: UpgradePPD T.PPDATE -> FilePath -> IO (UpgradePPD T.PPDATE)
programVariables ppd jpath = 
 do let imports = T.importsGet (fst . fromOK $ runStateT ppd emptyEnv)
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

--
--Adds to the upgraded ppDATE's env the method declarations of all the java files involved in the verification process
--
programMethods :: UpgradePPD T.PPDATE -> FilePath -> IO (UpgradePPD T.PPDATE)
programMethods ppd jpath = 
 do let imports = T.importsGet (fst . fromOK $ runStateT ppd emptyEnv)
    ms <- sequence [ getMethods i jpath
                   | i <- imports, not (elem ((\ (T.Import s) -> s) i) importsInKeY)
                   ]
    return $ updateMethodsEnv ppd ms

updateMethodsEnv :: UpgradePPD T.PPDATE -> [(String, T.ClassInfo, [(T.Id,String,[String],T.MethodInvocations)])] -> UpgradePPD T.PPDATE
updateMethodsEnv ppd ms = ppd >>= (\x -> do env <- get; put (env { methodsInFiles = ms }); return x)                     

getMethods :: T.Import -> FilePath -> IO (String, T.ClassInfo, [(T.Id,String,[String],T.MethodInvocations)])
getMethods i jpath =
  do (main, cl) <- makeAddFile i
     let file_add = jpath ++ main ++ "/" ++ (cl ++ ".java")
     r <- readFile file_add
     let java     = (\(Right x) -> x) $ parseJavaFile r
     let java_aux = lookForCTD cl $ map getClassDecls $ getClassTypeDecls java
     let methods = (getMethodDecl.getDecls.getClassBody) java_aux
     return (main, cl, (map methodsDetails methods))

-------------------------
-- Auxiliary Functions --
-------------------------

methodsDetails :: MemberDecl -> (String, String, [String],[Exp])
methodsDetails (MethodDecl _ _ Nothing (Ident mn) args _ body)   = ("void",mn,map prettyPrint args, getMethodsInvocation body)
methodsDetails (MethodDecl _ _ (Just rt) (Ident mn) args _ body) = (prettyPrint rt,mn,map prettyPrint args, getMethodsInvocation body)

--
--Gets all method invocations within a method definition
--
getMethodsInvocation :: MethodBody -> [Exp]
getMethodsInvocation (MethodBody Nothing)           = []
getMethodsInvocation (MethodBody (Just (Block []))) = []
getMethodsInvocation (MethodBody (Just (Block bs))) = concatMap getMIBlockRec bs


getMIBlockRec :: BlockStmt -> [Exp]
getMIBlockRec (BlockStmt stm)          = getMIStmRec stm
getMIBlockRec (LocalClass _)           = []
getMIBlockRec (LocalVars _ _ vdecls)   = concatMap getMIVarDeclRec vdecls

getMIStmRec :: Stmt -> [Exp]
getMIStmRec (StmtBlock (Block block))       = concatMap getMIBlockRec block
getMIStmRec (IfThen exp stm)                = getMIExp exp ++ getMIStmRec stm
getMIStmRec (IfThenElse exp stm1 stm2)      = getMIExp exp ++ getMIStmRec stm1 ++ getMIStmRec stm2
getMIStmRec (While exp stm)                 = getMIExp exp ++ getMIStmRec stm
getMIStmRec (BasicFor _ _ _ stm)            = getMIStmRec stm
getMIStmRec (EnhancedFor _ _ _ exp stm)     = getMIExp exp ++ getMIStmRec stm
getMIStmRec Empty                           = []
getMIStmRec (ExpStmt exp)                   = getMIExp exp
getMIStmRec (Assert exp mexp)               = 
 case mexp of 
      Nothing   -> getMIExp exp
      Just exp' -> getMIExp exp ++ getMIExp exp'
getMIStmRec (Switch _ bstms)                = concatMap getMIBlockRec $ concatMap (\(SwitchBlock e bstm) -> bstm) bstms
getMIStmRec (Do stm _)                      = getMIStmRec stm
getMIStmRec (Break _)                       = []
getMIStmRec (Continue _)                    = []
getMIStmRec (Return mexp)                   =
 case mexp of 
      Nothing  -> []
      Just exp -> getMIExp exp
getMIStmRec (Synchronized _ (Block block))  = concatMap getMIBlockRec block
getMIStmRec (Throw exp)                     = getMIExp exp
getMIStmRec (Try (Block block) _ mblock)    = 
  case mblock of 
      Nothing             -> concatMap getMIBlockRec block
      Just (Block block') -> concatMap getMIBlockRec block ++ concatMap getMIBlockRec block'
getMIStmRec (Labeled _ stm)                 = getMIStmRec stm


getMIVarDeclRec :: VarDecl -> [Exp]
getMIVarDeclRec (VarDecl id Nothing)        = []
getMIVarDeclRec (VarDecl id (Just vinit))   = getMIVarInitRec vinit

getMIVarInitRec :: VarInit -> [Exp]
getMIVarInitRec (InitExp exp)               = getMIExp exp
getMIVarInitRec (InitArray (ArrayInit arr)) = concatMap getMIVarInitRec arr

getMIExp :: Exp -> [Exp]
getMIExp (InstanceCreation _ _ args _)         = concatMap getMIExp args
getMIExp (QualInstanceCreation exp _ _ args _) = getMIExp exp ++ concatMap getMIExp args
getMIExp (ArrayCreateInit _ _ (ArrayInit arr)) = concatMap getMIVarInitRec arr
getMIExp (MethodInv minv)    = [MethodInv minv]
getMIExp (PostIncrement exp) = getMIExp exp
getMIExp (PostDecrement exp) = getMIExp exp
getMIExp (PreIncrement exp)  = getMIExp exp
getMIExp (PreDecrement exp)  = getMIExp exp
getMIExp (PrePlus exp)       = getMIExp exp
getMIExp (PreMinus exp)      = getMIExp exp
getMIExp (PreBitCompl exp)   = getMIExp exp
getMIExp (PreNot exp)        = getMIExp exp
getMIExp (Cast _ exp)        = getMIExp exp
getMIExp (BinOp exp1 _ exp2) = getMIExp exp1 ++ getMIExp exp2 
getMIExp (InstanceOf exp _)  = getMIExp exp
getMIExp (Cond _ exp1 exp2)  = getMIExp exp1 ++ getMIExp exp2
getMIExp (Assign _ _ exp)    = getMIExp exp
getMIExp _                   = []
