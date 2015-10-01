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

getMethodsNames :: T.ClassInfo -> T.Contracts -> [T.MethodName]
getMethodsNames _ []      = []
getMethodsNames cl (c:cs) = if ((fst $ T.methodCN c) == cl)  
                            then (snd $ T.methodCN c):getMethodsNames cl cs
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


