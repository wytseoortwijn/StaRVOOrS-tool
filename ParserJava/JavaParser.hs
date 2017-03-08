module JavaParser where 

import qualified Types as T
import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.Lexer

---------------------------------------
-- Manipulation of parsed java files --
---------------------------------------

--receives the content of a Java file as a string
parseJavaFile = parser compilationUnit

updateDecls :: CompilationUnit -> T.ClassInfo -> [Decl] -> CompilationUnit
updateDecls (CompilationUnit a b c) cl decls = 
 let f = ClassTypeDecl . (\x -> updateClassDecl cl x decls) . getClassDecls
 in CompilationUnit a b (map f c)

getClassTypeDecls :: CompilationUnit -> [TypeDecl]
getClassTypeDecls (CompilationUnit _ _ xs) = xs

getClassDecls :: TypeDecl -> ClassDecl
getClassDecls (ClassTypeDecl cd) = cd

updateClassDecl :: T.ClassInfo -> ClassDecl -> [Decl] -> ClassDecl
updateClassDecl cl cd@(ClassDecl _ id _ _ _ _) decls = if (cl == prettyPrint id) 
                                                       then updateClassBody cd (ClassBody decls) 
                                                       else cd
 

lookForCTD :: T.ClassInfo -> [ClassDecl] -> ClassDecl
lookForCTD _  []       = error $ "Missmatch between file name and class name.\n"
lookForCTD cl (cd:cds) = case cd of 
                              ClassDecl _ id _ _ _ _ -> if (cl == prettyPrint id) 
                                                        then cd 
                                                        else lookForCTD cl cds
                              _                      -> lookForCTD cl cds

getClassBody :: ClassDecl -> ClassBody
getClassBody (ClassDecl _ _ _ _ _ cbody) = cbody

updateClassBody :: ClassDecl -> ClassBody -> ClassDecl
updateClassBody (ClassDecl a b c d e cbody) cbody' = ClassDecl a b c d e cbody'

getDecls :: ClassBody -> [Decl]
getDecls (ClassBody decls) = decls

getMemberDecl :: [Decl] -> [Decl]
getMemberDecl []     = []
getMemberDecl (d:ds) = case d of
                         MemberDecl _ -> d:getMemberDecl ds
                         _            -> getMemberDecl ds

getVarDecl' :: [Decl] -> [MemberDecl]
getVarDecl' []     = []
getVarDecl' (d:ds) = case d of
                         MemberDecl md -> case md of 
                                              FieldDecl _ _ _ -> md:getVarDecl' ds
                                              _               -> getVarDecl' ds
                         _             -> getVarDecl' ds

getMethodDecl :: [Decl] -> [MemberDecl]
getMethodDecl []     = []
getMethodDecl (d:ds) = 
 case d of
      MemberDecl md -> case md of 
               MethodDecl _ _ _ _ _ _ _ -> md:getMethodDecl ds
               _                        -> getMethodDecl ds
      _             -> getMethodDecl ds

--TODO: Update when dealing with private values
getMethodDeclId :: [Decl] -> [String]
getMethodDeclId []     = []
getMethodDeclId (d:ds) = 
 case d of
      MemberDecl md -> case md of 
               MethodDecl xs _ _ (Ident id) _ _ _ 
                          -> if (elem Public xs) 
                             then id:getMethodDeclId ds
                             else getMethodDeclId ds
               _          -> getMethodDeclId ds
      _             -> getMethodDeclId ds

getTypeAndId :: MemberDecl -> (String, [String])
getTypeAndId (FieldDecl _ type' vid) = (prettyPrint type', map getIdVar vid)

getIdVar :: VarDecl -> String
getIdVar (VarDecl (VarId (Ident id)) _) = id

splitVars :: [(String, [String])] -> [(String,String)]
splitVars []                 = []
splitVars (tv:tvs) = (uncurry splitV) tv ++ splitVars tvs

splitV :: String -> [String] -> [(String, String)]
splitV _ []         = []
splitV type' (v:vs) = (type',v):splitV type' vs

getMethodsNames :: T.ClassInfo -> T.HTriples -> [T.MethodName]
getMethodsNames _ []      = []
getMethodsNames cl (c:cs) = if ((T.clinf $ T.methodCN c) == cl)  
                            then (T.mname $ T.methodCN c):getMethodsNames cl cs
                            else getMethodsNames cl cs

instrumentMethodMemberDecl :: [Decl] -> [T.MethodName] -> [Decl]
instrumentMethodMemberDecl [] _       = []
instrumentMethodMemberDecl (d:ds) mns = 
 case d of
      MemberDecl md -> case md of 
                            MethodDecl _ _ _ _ _ _ _ -> let (m1,m2,mns') = generateMethods d mns
                                                        in if (m1 == Nothing)
                                                           then m2:instrumentMethodMemberDecl ds mns'
                                                           else [(\(Just x) -> x) m1, m2] ++ instrumentMethodMemberDecl ds mns'
                            _                        -> d:instrumentMethodMemberDecl ds mns
      _             -> d:instrumentMethodMemberDecl ds mns

-- Added due to bug in the java library
-- generates the new methods that have to be added due to instrumentation of code
instrumentMethodMemberDecl' :: [Decl] -> [T.MethodName] -> [(String, String, String)]
instrumentMethodMemberDecl' [] _       = []
instrumentMethodMemberDecl' (d:ds) mns = 
 case d of
      MemberDecl md -> case md of 
                            MethodDecl _ _ _ (Ident id) _ _ _ -> 
                               let (m1,m2,mns') = generateMethods d mns
                               in if (m1 == Nothing)
                                  then instrumentMethodMemberDecl' ds mns'
                                  else (id, prettyPrint $ (\(Just x) -> x) m1, ((head.lines.prettyPrint) m2) ++ "\n {"):instrumentMethodMemberDecl' ds mns'
                            _                                 -> instrumentMethodMemberDecl' ds mns
      _             -> instrumentMethodMemberDecl' ds mns



generateMethods :: Decl -> [T.MethodName] -> (Maybe Decl, Decl, [T.MethodName])
generateMethods md@(MemberDecl (MethodDecl public xs type' (Ident id) param ex mbody)) mns = 
 if (elem id mns)
 then if (type' /= Nothing)
      then let new_body    = makeNewBodyRet (id ++ "Aux") param
               new_method  = MethodDecl public xs type' (Ident id) param ex new_body
               id_arg = FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Integer",[])]))) False (VarId (Ident "id"))
               method_inst = MethodDecl public xs type' (Ident (id ++ "Aux")) (param ++ [id_arg]) ex mbody
           in (Just (MemberDecl new_method), MemberDecl method_inst, filter (/= id) mns)
      else let new_body    = makeNewBodyVoid (id ++ "Aux") param
               new_method  = MethodDecl public xs type' (Ident id) param ex new_body
               id_arg = FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Integer",[])]))) False (VarId (Ident "id"))
               method_inst = MethodDecl public xs type' (Ident (id ++ "Aux")) (param ++ [id_arg]) ex mbody
           in (Just (MemberDecl new_method), MemberDecl method_inst, filter (/= id) mns)
 else (Nothing, md, mns)


makeNewBodyRet :: T.MethodName -> [FormalParam] -> MethodBody
makeNewBodyRet mn fps = 
 let expNames = makeNewArgsInCall fps
 in MethodBody (Just (Block [BlockStmt (Return (Just (MethodInv (MethodCall (Name [Ident mn]) expNames))))]))

makeNewBodyVoid :: T.MethodName -> [FormalParam] -> MethodBody
makeNewBodyVoid mn fps = 
 let expNames = makeNewArgsInCall fps
 in MethodBody (Just (Block [BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident mn]) expNames)))]))

makeNewArgsInCall :: [FormalParam] -> [Exp]
makeNewArgsInCall []       = [MethodInv (MethodCall (Name [Ident "fid",Ident "getNewId"]) [])]
makeNewArgsInCall ((FormalParam _ _ _ (VarId (Ident id))):fps) = 
 (ExpName (Name [Ident id])) : makeNewArgsInCall fps

-------------------------
-- Auxiliary Functions --
-------------------------

methodsDetails :: MemberDecl -> (String, String, [String],[Exp])
methodsDetails (MethodDecl _ _ Nothing (Ident mn) args _ body)   = ("void",mn,map prettyPrint args, getMethodsInvocation body)
methodsDetails (MethodDecl _ _ (Just rt) (Ident mn) args _ body) = (prettyPrint rt,mn,map prettyPrint args, getMethodsInvocation body)

--Gets all method invocations within a method definition
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
