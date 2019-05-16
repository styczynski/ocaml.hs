module Runtime where

import Environment

data ProgramResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment

runtimeValueToStr :: RuntimeValue -> String
runtimeValueToStr RUnit = "()"
runtimeValueToStr (RInt val) = "Int " ++ (show val)
runtimeValueToStr (RString val) = "String " ++ (show val)
runtimeValueToStr val = (show val)

resultToStr :: ProgramResult -> String
resultToStr (FailedParse err) = unlines [ "  ParseError: " ++ err ]
resultToStr (FailedExecution err) = unlines [ "  RuntimeError: " ++ err ]
resultToStr (Executed val state) = unlines [ " -: " ++ (runtimeValueToStr val) ]