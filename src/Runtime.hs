module Runtime where

data RuntimeValue = RUnit deriving (Show, Eq)
data RuntimeState = EmptyRuntimeState
data ProgramResult = FailedParse String | FailedExecution String | Executed RuntimeValue RuntimeState

runtimeValueToStr :: RuntimeValue -> String
runtimeValueToStr RUnit = "()"
runtimeValueToStr val = (show val)

resultToStr :: ProgramResult -> String
resultToStr (FailedParse err) = unlines [ "  ParseError: " ++ err ]
resultToStr (FailedExecution err) = unlines [ "  RuntimeError: " ++ err ]
resultToStr (Executed val state) = unlines [ " -: " ++ (runtimeValueToStr val) ]