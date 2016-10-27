

module Absjml where

-- Haskell module generated by the BNF converter




newtype IdJml = IdJml String deriving (Eq,Ord,Show,Read)
newtype Symbols = Symbols String deriving (Eq,Ord,Show,Read)
data JML =
   JMLAnd JML JML
 | JMLOr JML JML
 | JMLNot JML
 | JMLImp JML JML
 | JMLIff JML JML
 | JMLForallRT Type IdJml BodyF
 | JMLExistsRT Type IdJml BodyF
 | JMLPar JML
 | JMLExp [Expression]
  deriving (Eq,Ord,Show,Read)

data BodyF =
   BodyF RangeTerm
  deriving (Eq,Ord,Show,Read)

data RangeTerm =
   RangeTerm JML JML
 | OnlyRange JML
  deriving (Eq,Ord,Show,Read)

data Type =
   Type IdJml
  deriving (Eq,Ord,Show,Read)

data Expression =
   Exp IdJml
 | ExpS Symbols
 | ExpRes
 | ExpOld JML
 | ExpDls Content
 | ExpMC MethodCall
  deriving (Eq,Ord,Show,Read)

data MethodCall =
   MC IdJml [Args]
  deriving (Eq,Ord,Show,Read)

data Args =
   ArgsId IdJml
 | ArgsS String
  deriving (Eq,Ord,Show,Read)

data Content =
   ContentStr String
 | ContentId IdJml
 | ContentMC MethodCall
  deriving (Eq,Ord,Show,Read)

