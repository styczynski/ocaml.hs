module Macros where

import Environment
import Runtime
import Printer
import AbsSyntax

discardMacro :: BuiltinMacro -> Implementation
discardMacro _ = INothing

removeMacros :: Implementation -> Implementation
removeMacros (IRootComplex a b) =
  IRootComplex (removeMacros a) (removeMacros b)
removeMacros (IRoot phrase) = case phrase of
  IPhrase expr -> IRoot $ IPhrase expr
  IDef def -> IRoot $ IDef def
  IMacro macro -> discardMacro macro
removeMacros impl = impl
