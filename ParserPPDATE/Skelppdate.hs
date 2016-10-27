module Skelppdate where

-- Haskell module generated by the BNF converter

import Absppdate
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transId :: Id -> Result
transId x = case x of
  Id str  -> failure x


transSymbols :: Symbols -> Result
transSymbols x = case x of
  Symbols str  -> failure x


transAbsPPDATE :: AbsPPDATE -> Result
transAbsPPDATE x = case x of
  AbsPPDATE imports global cinvariants htriples methods  -> failure x


transImports :: Imports -> Result
transImports x = case x of
  Imports imports  -> failure x


transImport :: Import -> Result
transImport x = case x of
  Import javafiless  -> failure x


transJavaFiles :: JavaFiles -> Result
transJavaFiles x = case x of
  JavaFiles id  -> failure x


transGlobal :: Global -> Result
transGlobal x = case x of
  Global context  -> failure x


transContext :: Context -> Result
transContext x = case x of
  Ctxt variables ievents triggers properties foreaches  -> failure x


transVariables :: Variables -> Result
transVariables x = case x of
  VarNil  -> failure x
  VarDef variables  -> failure x


transVariable :: Variable -> Result
transVariable x = case x of
  Var varmodifier type' vardecls  -> failure x


transVarModifier :: VarModifier -> Result
transVarModifier x = case x of
  VarModifierFinal  -> failure x
  VarModifierNil  -> failure x


transIEvents :: IEvents -> Result
transIEvents x = case x of
  IEventsNil  -> failure x
  IEventsDef ievents  -> failure x


transIEvent :: IEvent -> Result
transIEvent x = case x of
  IEvent id  -> failure x


transTriggers :: Triggers -> Result
transTriggers x = case x of
  TriggersNil  -> failure x
  TriggersDef triggers  -> failure x


transTrigger :: Trigger -> Result
transTrigger x = case x of
  Trigger id binds compoundtrigger whereclause  -> failure x


transCompoundTrigger :: CompoundTrigger -> Result
transCompoundTrigger x = case x of
  Collection triggerlist  -> failure x
  NormalEvent binding id varss triggervariation  -> failure x
  ClockEvent id n  -> failure x
  OnlyId id  -> failure x
  OnlyIdPar id  -> failure x


transTriggerList :: TriggerList -> Result
transTriggerList x = case x of
  CECollection compoundtriggers  -> failure x


transTriggerVariation :: TriggerVariation -> Result
transTriggerVariation x = case x of
  EVEntry  -> failure x
  EVExit varss  -> failure x
  EVThrow varss  -> failure x
  EVHadle varss  -> failure x


transBinding :: Binding -> Result
transBinding x = case x of
  BindingVar bind  -> failure x


transBind :: Bind -> Result
transBind x = case x of
  BindStar  -> failure x
  BindType type' id  -> failure x
  BindId id  -> failure x


transWhereClause :: WhereClause -> Result
transWhereClause x = case x of
  WhereClauseNil  -> failure x
  WhereClauseDef whereexps  -> failure x


transWhereExp :: WhereExp -> Result
transWhereExp x = case x of
  WhereExp bind varexp  -> failure x


transVars :: Vars -> Result
transVars x = case x of
  Vars bind  -> failure x


transProperties :: Properties -> Result
transProperties x = case x of
  PropertiesNil  -> failure x
  ProperiesDef id states transitions properties  -> failure x


transStates :: States -> Result
transStates x = case x of
  States accepting bad normal starting  -> failure x


transAccepting :: Accepting -> Result
transAccepting x = case x of
  AcceptingNil  -> failure x
  AcceptingDef states  -> failure x


transBad :: Bad -> Result
transBad x = case x of
  BadNil  -> failure x
  BadDef states  -> failure x


transNormal :: Normal -> Result
transNormal x = case x of
  NormalNil  -> failure x
  NormalDef states  -> failure x


transStarting :: Starting -> Result
transStarting x = case x of
  StartingDef states  -> failure x


transState :: State -> Result
transState x = case x of
  State namestate initialcode htnames  -> failure x


transNameState :: NameState -> Result
transNameState x = case x of
  NameState id  -> failure x


transHTNames :: HTNames -> Result
transHTNames x = case x of
  CNS htnames  -> failure x
  CNSNil  -> failure x


transHTName :: HTName -> Result
transHTName x = case x of
  CN id  -> failure x


transInitialCode :: InitialCode -> Result
transInitialCode x = case x of
  InitNil  -> failure x
  InitProg java  -> failure x


transTransitions :: Transitions -> Result
transTransitions x = case x of
  Transitions transitions  -> failure x


transTransition :: Transition -> Result
transTransition x = case x of
  Transition namestate1 namestate2 arrow3  -> failure x


transArrow :: Arrow -> Result
transArrow x = case x of
  Arrow id condition  -> failure x


transCondition :: Condition -> Result
transCondition x = case x of
  Cond1  -> failure x
  Cond2 cond  -> failure x


transCond :: Cond -> Result
transCond x = case x of
  CondExpDef condexp  -> failure x
  CondAction condexp action  -> failure x


transAction :: Action -> Result
transAction x = case x of
  Action expressions  -> failure x


transForeaches :: Foreaches -> Result
transForeaches x = case x of
  ForeachesNil  -> failure x
  ForeachesDef argss context  -> failure x


transCInvariants :: CInvariants -> Result
transCInvariants x = case x of
  CInvariants cinvariants  -> failure x
  CInvempty  -> failure x


transCInvariant :: CInvariant -> Result
transCInvariant x = case x of
  CI id jml  -> failure x


transHTriples :: HTriples -> Result
transHTriples x = case x of
  HTriples hts  -> failure x
  HTempty  -> failure x


transHT :: HT -> Result
transHT x = case x of
  HT id pre method post assignable  -> failure x


transPre :: Pre -> Result
transPre x = case x of
  Pre jml  -> failure x


transMethod :: Method -> Result
transMethod x = case x of
  Method id1 id2  -> failure x


transPost :: Post -> Result
transPost x = case x of
  Post jml  -> failure x


transAssignable :: Assignable -> Result
transAssignable x = case x of
  Assignable assigs  -> failure x


transAssig :: Assig -> Result
transAssig x = case x of
  AssigJML jml  -> failure x
  AssigE  -> failure x
  AssigN  -> failure x


transMethods :: Methods -> Result
transMethods x = case x of
  Methods bodymethods  -> failure x
  MethodsNil  -> failure x


transBodyMethods :: BodyMethods -> Result
transBodyMethods x = case x of
  BodyMemDecl memberdecls  -> failure x
  BodyImport importfile  -> failure x


transMemberDecl :: MemberDecl -> Result
transMemberDecl x = case x of
  MemberDeclMethod type' id argss java  -> failure x
  MemberDeclField variabledecl  -> failure x


transVariableDecl :: VariableDecl -> Result
transVariableDecl x = case x of
  VariableDecl type' vardecls  -> failure x


transVarDecl :: VarDecl -> Result
transVarDecl x = case x of
  VarDecl id variableinitializer  -> failure x


transVariableInitializer :: VariableInitializer -> Result
transVariableInitializer x = case x of
  VarInit varexp  -> failure x
  VarInitNil  -> failure x


transType :: Type -> Result
transType x = case x of
  Type id  -> failure x


transArgs :: Args -> Result
transArgs x = case x of
  Args type' id  -> failure x


transImportFile :: ImportFile -> Result
transImportFile x = case x of
  ImportFile address  -> failure x


transAddress :: Address -> Result
transAddress x = case x of
  Address id add  -> failure x


transAdd :: Add -> Result
transAdd x = case x of
  AddBar id add  -> failure x
  AddId id  -> failure x


transVarExp :: VarExp -> Result
transVarExp x = case x of
  VarExpId id varexp  -> failure x
  VarExpSymb symbols varexp  -> failure x
  VarExpInt n varexp  -> failure x
  VarExpDouble d varexp  -> failure x
  VarExpTimes varexp  -> failure x
  VarExpParent varexp1 varexp2  -> failure x
  VarExpDot varexp  -> failure x
  VarExpComma varexp  -> failure x
  VarExpCorchete varexp1 varexp2  -> failure x
  VarExpSlash varexp  -> failure x
  VarExpBar varexp  -> failure x
  VarExpNil  -> failure x


transExpressions :: Expressions -> Result
transExpressions x = case x of
  ExpId id expressions  -> failure x
  ExpSymb symbols expressions  -> failure x
  ExpInt n expressions  -> failure x
  ExpDouble d expressions  -> failure x
  ExpTimes expressions  -> failure x
  ExpDot expressions  -> failure x
  ExpBrack expressions1 expressions2  -> failure x
  ExpParent expressions1 expressions2  -> failure x
  ExpCorchete expressions1 expressions2  -> failure x
  ExpEq expressions  -> failure x
  ExpSemiColon expressions  -> failure x
  ExpBSlash expressions  -> failure x
  ExpComma expressions  -> failure x
  ExpSlash expressions  -> failure x
  ExpBar expressions  -> failure x
  ExpNil  -> failure x


transJava :: Java -> Result
transJava x = case x of
  Java expressions  -> failure x


transCondExp :: CondExp -> Result
transCondExp x = case x of
  CondExp expressions  -> failure x


transJML :: JML -> Result
transJML x = case x of
  JML expressions  -> failure x



