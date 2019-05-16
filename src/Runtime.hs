module Runtime where

import Environment

data ProgramResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment

runtimeValueToStr :: RuntimeValue -> String
runtimeValueToStr RUnit = "()"
runtimeValueToStr (RInt val) = (show val)
runtimeValueToStr (RString val) = (show val)
runtimeValueToStr (RFunc _ _) = " "
runtimeValueToStr val = (show val)

resultToStr :: ProgramResult -> String
resultToStr (FailedParse err) = unlines [ "  ParseError: " ++ err ]
resultToStr (FailedExecution err) = unlines [ "  RuntimeError: " ++ err ]
resultToStr (Executed val state) = unlines [ " -: " ++ (getTypeString val) ++ (runtimeValueToStr val) ]