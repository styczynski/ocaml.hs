module InterpreterDefinitions where

import Runtime
import Environment

data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment
