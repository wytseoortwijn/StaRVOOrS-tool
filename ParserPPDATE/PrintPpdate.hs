{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintPpdate where

-- pretty-printer generated by the BNF converter

import AbsPpdate
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

-- Modify to remove white space before '[' and '('
-- Modify to remove white space between ==
-- Modify to remove white spaces around '.'
-- Modify to remove white space after '\\' and '!'
-- Modify to remove white space in "> =", "< =" , "! =" "& &" and "| |"
-- Modify to remove white space after openning \" and before closing \"
-- Modify to remove white spaces around and between ::
render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "\\"     :ts -> showChar '\\' . rend i ts
    t:"+":"+":ts -> showString t . showChar '+' . space "+" . rend i ts
    t:"-":"-":ts -> showString t . showChar '-' . space "-" . rend i ts
    t: "%"   :ts -> space t . showChar '%' . rend i ts
    t: "."   :ts -> showString t . showChar '.' . rend i ts 
    t:":":":":ts -> showString t . showChar ':' . showChar ':' . rend i ts
    "=" :"=" :ts -> showChar '=' . space "=" . rend i ts
    ">" :"=" :ts -> showChar '>' . space "=" . rend i ts
    "<" :"=" :ts -> showChar '<' . space "=" . rend i ts
    "!" :"=" :ts -> showChar '!' . space "=" . rend i ts
    "!" :t   :ts -> showChar '!' . showString t . rend i ts
    "&" :"&" :ts -> showChar '&' . space "&" . rend i ts
    "|" :"|" :ts -> showChar '|' . space "|" . rend i ts
    t : "["  :ts -> showString t . showChar '[' . rend i ts
    t : "("  :ts -> showString t . showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showString ") " . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    "\""     :ts -> showChar '\"' . rend i ts
    t  :"\"" :ts -> if (t == " ") then showChar '\"' . rend i ts 
                                  else showString t . showChar '\"' . rend i ts 
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print Id where
  prt _ (Id i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print Symbols where
  prt _ (Symbols i) = doc (showString ( i))



instance Print AbsPPDATE where
  prt i e = case e of
    AbsPPDATE imports global templates cinvariants htriples methods -> prPrec i 0 (concatD [prt 0 imports, prt 0 global, prt 0 templates, prt 0 cinvariants, prt 0 htriples, prt 0 methods])

instance Print Imports where
  prt i e = case e of
    Imports imports -> prPrec i 0 (concatD [doc (showString "IMPORTS"), doc (showString "{"), prt 0 imports, doc (showString "}")])

instance Print Import where
  prt i e = case e of
    Import javafiless -> prPrec i 0 (concatD [doc (showString "import"), prt 0 javafiless])
  prtList _ [x] = (concatD [prt 0 x, doc (showString ";")])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print JavaFiles where
  prt i e = case e of
    JavaFiles id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "."), prt 0 xs])
instance Print Global where
  prt i e = case e of
    Global context -> prPrec i 0 (concatD [doc (showString "GLOBAL"), doc (showString "{"), prt 0 context, doc (showString "}")])

instance Print Context where
  prt i e = case e of
    Ctxt variables actevents triggers properties foreaches -> prPrec i 0 (concatD [prt 0 variables, prt 0 actevents, prt 0 triggers, prt 0 properties, prt 0 foreaches])

instance Print Variables where
  prt i e = case e of
    VarNil -> prPrec i 0 (concatD [])
    VarDef variables -> prPrec i 0 (concatD [doc (showString "VARIABLES"), doc (showString "{"), prt 0 variables, doc (showString "}")])

instance Print Variable where
  prt i e = case e of
    Var varmodifier type_ vardecls -> prPrec i 0 (concatD [prt 0 varmodifier, prt 0 type_, prt 0 vardecls])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print VarModifier where
  prt i e = case e of
    VarModifierFinal -> prPrec i 0 (concatD [doc (showString "final")])
    VarModifierNil -> prPrec i 0 (concatD [])

instance Print ActEvents where
  prt i e = case e of
    ActEventsNil -> prPrec i 0 (concatD [])
    ActEventsDef actevents -> prPrec i 0 (concatD [doc (showString "ACTEVENTS"), doc (showString "{"), prt 0 actevents, doc (showString "}")])

instance Print ActEvent where
  prt i e = case e of
    ActEvent id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Triggers where
  prt i e = case e of
    TriggersNil -> prPrec i 0 (concatD [])
    TriggersDef triggers -> prPrec i 0 (concatD [doc (showString "TRIGGERS"), doc (showString "{"), prt 0 triggers, doc (showString "}")])

instance Print Trigger where
  prt i e = case e of
    Trigger id binds compoundtrigger whereclause -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 binds, doc (showString ")"), doc (showString "="), prt 0 compoundtrigger, prt 0 whereclause])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print CompoundTrigger where
  prt i e = case e of
    Collection triggerlist -> prPrec i 0 (concatD [prt 0 triggerlist])
    NormalEvent binding id varss triggervariation -> prPrec i 0 (concatD [doc (showString "{"), prt 0 binding, prt 0 id, doc (showString "("), prt 0 varss, doc (showString ")"), prt 0 triggervariation, doc (showString "}")])
    ClockEvent id n -> prPrec i 0 (concatD [doc (showString "{"), prt 0 id, doc (showString "@"), prt 0 n, doc (showString "}")])
    OnlyId id -> prPrec i 0 (concatD [doc (showString "{"), prt 0 id, doc (showString "}")])
    OnlyIdPar id -> prPrec i 0 (concatD [doc (showString "{"), prt 0 id, doc (showString "("), doc (showString ")"), doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])
instance Print TriggerList where
  prt i e = case e of
    CECollection compoundtriggers -> prPrec i 0 (concatD [doc (showString "{"), prt 0 compoundtriggers, doc (showString "}")])

instance Print TriggerVariation where
  prt i e = case e of
    EVEntry -> prPrec i 0 (concatD [doc (showString "entry")])
    EVExit varss -> prPrec i 0 (concatD [doc (showString "exit"), doc (showString "("), prt 0 varss, doc (showString ")")])
    EVThrow varss -> prPrec i 0 (concatD [doc (showString "uponThrowing"), doc (showString "("), prt 0 varss, doc (showString ")")])
    EVHadle varss -> prPrec i 0 (concatD [doc (showString "uponHandling"), doc (showString "("), prt 0 varss, doc (showString ")")])

instance Print Binding where
  prt i e = case e of
    BindingVar bind -> prPrec i 0 (concatD [prt 0 bind, doc (showString ".")])

instance Print Bind where
  prt i e = case e of
    BindStar -> prPrec i 0 (concatD [doc (showString "*")])
    BindType type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
    BindId id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print WhereClause where
  prt i e = case e of
    WhereClauseNil -> prPrec i 0 (concatD [])
    WhereClauseDef whereexps -> prPrec i 0 (concatD [doc (showString "where"), doc (showString "{"), prt 0 whereexps, doc (showString "}")])

instance Print WhereExp where
  prt i e = case e of
    WhereExp bind varexp -> prPrec i 0 (concatD [prt 0 bind, doc (showString "="), prt 0 varexp])
  prtList _ [x] = (concatD [prt 0 x, doc (showString ";")])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Vars where
  prt i e = case e of
    Vars bind -> prPrec i 0 (concatD [prt 0 bind])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Properties where
  prt i e = case e of
    PropertiesNil -> prPrec i 0 (concatD [])
    ProperiesDef id propkind properties -> prPrec i 0 (concatD [doc (showString "PROPERTY"), prt 0 id, doc (showString "{"), prt 0 propkind, doc (showString "}"), prt 0 properties])

instance Print PropKind where
  prt i e = case e of
    PropKindNormal states transitions -> prPrec i 0 (concatD [prt 0 states, prt 0 transitions])
    PropKindPinit id ids -> prPrec i 0 (concatD [doc (showString "PINIT"), doc (showString "{"), doc (showString "("), prt 0 id, doc (showString ","), prt 0 ids, doc (showString ")"), doc (showString "}")])

instance Print States where
  prt i e = case e of
    States starting accepting bad normal -> prPrec i 0 (concatD [doc (showString "STATES"), doc (showString "{"), prt 0 starting, prt 0 accepting, prt 0 bad, prt 0 normal, doc (showString "}")])

instance Print Accepting where
  prt i e = case e of
    AcceptingNil -> prPrec i 0 (concatD [])
    AcceptingDef states -> prPrec i 0 (concatD [doc (showString "ACCEPTING"), doc (showString "{"), prt 0 states, doc (showString "}")])

instance Print Bad where
  prt i e = case e of
    BadNil -> prPrec i 0 (concatD [])
    BadDef states -> prPrec i 0 (concatD [doc (showString "BAD"), doc (showString "{"), prt 0 states, doc (showString "}")])

instance Print Normal where
  prt i e = case e of
    NormalNil -> prPrec i 0 (concatD [])
    NormalDef states -> prPrec i 0 (concatD [doc (showString "NORMAL"), doc (showString "{"), prt 0 states, doc (showString "}")])

instance Print Starting where
  prt i e = case e of
    StartingDef states -> prPrec i 0 (concatD [doc (showString "STARTING"), doc (showString "{"), prt 0 states, doc (showString "}")])

instance Print State where
  prt i e = case e of
    State namestate initialcode htnames -> prPrec i 0 (concatD [prt 0 namestate, prt 0 initialcode, prt 0 htnames])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print NameState where
  prt i e = case e of
    NameState id -> prPrec i 0 (concatD [prt 0 id])

instance Print HTNames where
  prt i e = case e of
    CNS htnames -> prPrec i 0 (concatD [doc (showString "("), prt 0 htnames, doc (showString ")")])
    CNSNil -> prPrec i 0 (concatD [])

instance Print HTName where
  prt i e = case e of
    CN id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print InitialCode where
  prt i e = case e of
    InitNil -> prPrec i 0 (concatD [])
    InitProg java -> prPrec i 0 (concatD [doc (showString "{"), prt 0 java, doc (showString "}")])

instance Print Transitions where
  prt i e = case e of
    Transitions transitions -> prPrec i 0 (concatD [doc (showString "TRANSITIONS"), doc (showString "{"), prt 0 transitions, doc (showString "}")])

instance Print Transition where
  prt i e = case e of
    Transition namestate1 namestate2 arrow -> prPrec i 0 (concatD [prt 0 namestate1, doc (showString "->"), prt 0 namestate2, doc (showString "["), prt 0 arrow, doc (showString "]")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Arrow where
  prt i e = case e of
    Arrow id actmark condition -> prPrec i 0 (concatD [prt 0 id, prt 0 actmark, prt 0 condition])

instance Print Actmark where
  prt i e = case e of
    ActMarkNil -> prPrec i 0 (concatD [])
    ActMark -> prPrec i 0 (concatD [doc (showString "?")])

instance Print Condition where
  prt i e = case e of
    Cond1 -> prPrec i 0 (concatD [])
    Cond2 cond -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 cond])

instance Print Cond where
  prt i e = case e of
    CondExpDef condexp -> prPrec i 0 (concatD [prt 0 condexp])
    CondAction condexp action -> prPrec i 0 (concatD [prt 0 condexp, doc (showString "\\"), prt 0 action])

instance Print Action where
  prt i e = case e of
    Action expressions -> prPrec i 0 (concatD [prt 0 expressions])

instance Print Foreaches where
  prt i e = case e of
    ForeachesNil -> prPrec i 0 (concatD [])
    ForeachesDef argss context foreaches -> prPrec i 0 (concatD [doc (showString "FOREACH"), doc (showString "("), prt 0 argss, doc (showString ")"), doc (showString "{"), prt 0 context, doc (showString "}"), prt 0 foreaches])

instance Print Templates where
  prt i e = case e of
    Temps templates -> prPrec i 0 (concatD [doc (showString "TEMPLATES"), doc (showString "{"), prt 0 templates, doc (showString "}")])
    TempsNil -> prPrec i 0 (concatD [])

instance Print Template where
  prt i e = case e of
    Temp id argss bodytemp -> prPrec i 0 (concatD [doc (showString "TEMPLATE"), prt 0 id, doc (showString "("), prt 0 argss, doc (showString ")"), doc (showString "{"), prt 0 bodytemp, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print BodyTemp where
  prt i e = case e of
    Body variables actevents triggers properties -> prPrec i 0 (concatD [prt 0 variables, prt 0 actevents, prt 0 triggers, prt 0 properties])

instance Print CInvariants where
  prt i e = case e of
    CInvariants cinvariants -> prPrec i 0 (concatD [doc (showString "CINVARIANTS"), doc (showString "{"), prt 0 cinvariants, doc (showString "}")])
    CInvempty -> prPrec i 0 (concatD [])

instance Print CInvariant where
  prt i e = case e of
    CI id jml -> prPrec i 0 (concatD [prt 0 id, doc (showString "{"), prt 0 jml, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print HTriples where
  prt i e = case e of
    HTriples hts -> prPrec i 0 (concatD [doc (showString "HTRIPLES"), doc (showString "{"), prt 0 hts, doc (showString "}")])
    HTempty -> prPrec i 0 (concatD [])

instance Print HT where
  prt i e = case e of
    HT id pre method post assignable -> prPrec i 0 (concatD [doc (showString "HT"), prt 0 id, doc (showString "{"), prt 0 pre, prt 0 method, prt 0 post, prt 0 assignable, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Pre where
  prt i e = case e of
    Pre jml -> prPrec i 0 (concatD [doc (showString "PRE"), doc (showString "{"), prt 0 jml, doc (showString "}")])
    PreNil -> prPrec i 0 (concatD [])

instance Print Method where
  prt i e = case e of
    Method id1 id2 overriding -> prPrec i 0 (concatD [doc (showString "METHOD"), doc (showString "{"), prt 0 id1, doc (showString "."), prt 0 id2, prt 0 overriding, doc (showString "}")])

instance Print Post where
  prt i e = case e of
    Post jml -> prPrec i 0 (concatD [doc (showString "POST"), doc (showString "{"), prt 0 jml, doc (showString "}")])
    PostNil -> prPrec i 0 (concatD [])

instance Print Assignable where
  prt i e = case e of
    Assignable assigs -> prPrec i 0 (concatD [doc (showString "ASSIGNABLE"), doc (showString "{"), prt 0 assigs, doc (showString "}")])
    AssigNil -> prPrec i 0 (concatD [])

instance Print Assig where
  prt i e = case e of
    AssigJML jml -> prPrec i 0 (concatD [prt 0 jml])
    AssigE -> prPrec i 0 (concatD [doc (showString "\\everything")])
    AssigN -> prPrec i 0 (concatD [doc (showString "\\nothing")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Overriding where
  prt i e = case e of
    Over types -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")")])
    OverNil -> prPrec i 0 (concatD [])

instance Print Methods where
  prt i e = case e of
    Methods bodymethods -> prPrec i 0 (concatD [doc (showString "METHODS"), doc (showString "{"), prt 0 bodymethods, doc (showString "}")])
    MethodsNil -> prPrec i 0 (concatD [])

instance Print BodyMethods where
  prt i e = case e of
    BodyMemDecl memberdecls -> prPrec i 0 (concatD [prt 0 memberdecls])
    BodyImport importfile -> prPrec i 0 (concatD [prt 0 importfile])

instance Print MemberDecl where
  prt i e = case e of
    MemberDeclMethod type_ id argss java -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 argss, doc (showString ")"), doc (showString "{"), prt 0 java, doc (showString "}")])
    MemberDeclField variabledecl -> prPrec i 0 (concatD [prt 0 variabledecl])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print VariableDecl where
  prt i e = case e of
    VariableDecl type_ vardecls -> prPrec i 0 (concatD [prt 0 type_, prt 0 vardecls, doc (showString ";")])

instance Print VarDecl where
  prt i e = case e of
    VarDecl id variableinitializer -> prPrec i 0 (concatD [prt 0 id, prt 0 variableinitializer])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print VariableInitializer where
  prt i e = case e of
    VarInit varexp -> prPrec i 0 (concatD [doc (showString "="), prt 0 varexp])
    VarInitNil -> prPrec i 0 (concatD [])

instance Print Type where
  prt i e = case e of
    Type typedef -> prPrec i 0 (concatD [prt 0 typedef])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print TypeDef where
  prt i e = case e of
    TypeDef id -> prPrec i 0 (concatD [prt 0 id])
    TypeGen id1 symbols1 id2 symbols2 -> prPrec i 0 (concatD [prt 0 id1, prt 0 symbols1, prt 0 id2, prt 0 symbols2])
    TypeArray id -> prPrec i 0 (concatD [prt 0 id, doc (showString "["), doc (showString "]")])

instance Print Args where
  prt i e = case e of
    Args type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print ImportFile where
  prt i e = case e of
    ImportFile address -> prPrec i 0 (concatD [doc (showString "import"), prt 0 address, doc (showString ";")])

instance Print Address where
  prt i e = case e of
    Address id add -> prPrec i 0 (concatD [prt 0 id, doc (showString "/"), prt 0 add])

instance Print Add where
  prt i e = case e of
    AddBar id add -> prPrec i 0 (concatD [prt 0 id, doc (showString "/"), prt 0 add])
    AddId id -> prPrec i 0 (concatD [prt 0 id])

instance Print CondExp where
  prt i e = case e of
    CondExpId id condexp -> prPrec i 0 (concatD [prt 0 id, prt 0 condexp])
    CondExpSymb symbols condexp -> prPrec i 0 (concatD [prt 0 symbols, prt 0 condexp])
    CondExpInt n condexp -> prPrec i 0 (concatD [prt 0 n, prt 0 condexp])
    CondExpDouble d condexp -> prPrec i 0 (concatD [prt 0 d, prt 0 condexp])
    CondExpTimes condexp -> prPrec i 0 (concatD [doc (showString "*"), prt 0 condexp])
    CondExpParent condexp1 condexp2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 condexp1, doc (showString ")"), prt 0 condexp2])
    CondExpDot condexp -> prPrec i 0 (concatD [doc (showString "."), prt 0 condexp])
    CondExpCorchete condexp1 condexp2 -> prPrec i 0 (concatD [doc (showString "["), prt 0 condexp1, doc (showString "]"), prt 0 condexp2])
    CondExpComma condexp -> prPrec i 0 (concatD [doc (showString ","), prt 0 condexp])
    CondExpSlash condexp -> prPrec i 0 (concatD [doc (showString "/"), prt 0 condexp])
    CondExpEq condexp -> prPrec i 0 (concatD [doc (showString "="), prt 0 condexp])
    CondExpBar condexp -> prPrec i 0 (concatD [doc (showString "|"), prt 0 condexp])
    CondExpNil -> prPrec i 0 (concatD [])

instance Print VarExp where
  prt i e = case e of
    VarExpId id varexp -> prPrec i 0 (concatD [prt 0 id, prt 0 varexp])
    VarExpSymb symbols varexp -> prPrec i 0 (concatD [prt 0 symbols, prt 0 varexp])
    VarExpInt n varexp -> prPrec i 0 (concatD [prt 0 n, prt 0 varexp])
    VarExpDouble d varexp -> prPrec i 0 (concatD [prt 0 d, prt 0 varexp])
    VarExpTimes varexp -> prPrec i 0 (concatD [doc (showString "*"), prt 0 varexp])
    VarExpParent varexp1 varexp2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 varexp1, doc (showString ")"), prt 0 varexp2])
    VarExpBrack varexp1 varexp2 -> prPrec i 0 (concatD [doc (showString "{"), prt 0 varexp1, doc (showString "}"), prt 0 varexp2])
    VarExpDot varexp -> prPrec i 0 (concatD [doc (showString "."), prt 0 varexp])
    VarExpComma varexp -> prPrec i 0 (concatD [doc (showString ","), prt 0 varexp])
    VarExpCorchete varexp1 varexp2 -> prPrec i 0 (concatD [doc (showString "["), prt 0 varexp1, doc (showString "]"), prt 0 varexp2])
    VarExpSlash varexp -> prPrec i 0 (concatD [doc (showString "/"), prt 0 varexp])
    VarExpBar varexp -> prPrec i 0 (concatD [doc (showString "|"), prt 0 varexp])
    VarExpNil -> prPrec i 0 (concatD [])

instance Print Expressions where
  prt i e = case e of
    ExpId id expressions -> prPrec i 0 (concatD [prt 0 id, prt 0 expressions])
    ExpSymb symbols expressions -> prPrec i 0 (concatD [prt 0 symbols, prt 0 expressions])
    ExpInt n expressions -> prPrec i 0 (concatD [prt 0 n, prt 0 expressions])
    ExpDouble d expressions -> prPrec i 0 (concatD [prt 0 d, prt 0 expressions])
    ExpTimes expressions -> prPrec i 0 (concatD [doc (showString "*"), prt 0 expressions])
    ExpDot expressions -> prPrec i 0 (concatD [doc (showString "."), prt 0 expressions])
    ExpBrack expressions1 expressions2 -> prPrec i 0 (concatD [doc (showString "{"), prt 0 expressions1, doc (showString "}"), prt 0 expressions2])
    ExpParent expressions1 expressions2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 expressions1, doc (showString ")"), prt 0 expressions2])
    ExpCorchete expressions1 expressions2 -> prPrec i 0 (concatD [doc (showString "["), prt 0 expressions1, doc (showString "]"), prt 0 expressions2])
    ExpEq expressions -> prPrec i 0 (concatD [doc (showString "="), prt 0 expressions])
    ExpSemiColon expressions -> prPrec i 0 (concatD [doc (showString ";"), prt 0 expressions])
    ExpBSlash expressions -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 expressions])
    ExpComma expressions -> prPrec i 0 (concatD [doc (showString ","), prt 0 expressions])
    ExpSlash expressions -> prPrec i 0 (concatD [doc (showString "/"), prt 0 expressions])
    ExpBar expressions -> prPrec i 0 (concatD [doc (showString "|"), prt 0 expressions])
    ExpNil -> prPrec i 0 (concatD [])

instance Print Java where
  prt i e = case e of
    Java expressions -> prPrec i 0 (concatD [prt 0 expressions])

instance Print JML where
  prt i e = case e of
    JML expressions -> prPrec i 0 (concatD [prt 0 expressions])


