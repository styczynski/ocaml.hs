module Runtime where

import Environment

data ProgramResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment

getProgramResult :: ProgramResult -> RuntimeValue
getProgramResult (Executed v _) = v

getProgramEnvironmentDefault :: ProgramResult -> Environment -> Environment
getProgramEnvironmentDefault (Executed _ env) _ = env
getProgramEnvironmentDefault _ defaultEnv = defaultEnv

getProgramEnvironment :: ProgramResult -> Environment
getProgramEnvironment r = getProgramEnvironmentDefault r emptyEnv

runtimeValueToStr :: RuntimeValue -> String
runtimeValueToStr RUnit = "()"
runtimeValueToStr (RInt val) = (show val)
runtimeValueToStr (RString val) = (show val)
runtimeValueToStr (RBool True) = "true"
runtimeValueToStr (RBool False) = "false"
runtimeValueToStr (RFunc _ _ _) = " "
runtimeValueToStr val = (show val)

resultToStr :: ProgramResult -> String
resultToStr (FailedParse err) = unlines [ "  ParseError: " ++ err ]
resultToStr (FailedExecution err) = unlines [ "  RuntimeError: " ++ err ]
resultToStr (Executed val state) = unlines [ " -: " ++ (getTypeString val) ++ " " ++ (runtimeValueToStr val) ]