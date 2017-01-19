module TranslatorActions(translateAct) where

import Absactions


translateAct :: Actions -> Actions
translateAct (Actions acts) = Actions $ map translateAction acts



translateAction :: Action -> Action
translateAction (ActCond conds act)  = ActCond conds (translateAction act)
translateAction (ActBlock acts)      = ActBlock (translateAct acts)
translateAction (ActBang (IdAct id)) = ActProg (Prog (IdAct (id++".send")) [])
translateAction (ActLog s parms)     = ActProg (Prog (IdAct "System.out.printf") (ArgsS s:fromParm2Arg parms))
translateAction act                  = act

fromParm2Arg :: Params -> [Args]
fromParm2Arg ParamsNil   = []
fromParm2Arg (Params xs) = map (\(Param id) -> ArgsId id) xs
