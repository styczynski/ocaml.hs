module Inference.Errors where

import Syntax.Base hiding (TV)

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Type

typesListToStr :: [Type] -> String
typesListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (typeToStr [] t)) "" l) ++ "}"

--data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)
generateTypePayloadMessage :: TypeErrorPayload -> String
generateTypePayloadMessage EmptyPayload = "Typechecking error:\nLocation: <unknown>\n\n"
generateTypePayloadMessage (TypeErrorPayload ast) = "Typechecking error:\nLocation: " ++ ast ++ "\n\n"

typeErrorToStr :: TypeError -> String
typeErrorToStr (UnificationFail payl a b) = (generateTypePayloadMessage payl) ++ "Cannot match types, expected: " ++ (typeToStr [] b) ++ ", got: " ++ (typeToStr [] a)
typeErrorToStr (Debug payl mes) = (generateTypePayloadMessage payl) ++ mes
typeErrorToStr (UnificationMismatch payl a b) = (generateTypePayloadMessage payl) ++ "Cannot match types, mismatch when unyfying: " ++ (typesListToStr a) ++ " and " ++ (typesListToStr b)
typeErrorToStr (Ambigious payl a) = (generateTypePayloadMessage payl) ++ "Cannot infer types, expression is ambigious: " ++ (constraintsListToStr a)
typeErrorToStr (UnboundVariable payl (Ident a)) = (generateTypePayloadMessage payl) ++ "Variable not in scope: \"" ++ a ++ "\""
typeErrorToStr (InfiniteType payl (TV v) t) = (generateTypePayloadMessage payl) ++ "Infinite type detected: " ++ v ++ "': " ++ (typeToStr [] t)
typeErrorToStr e = (generateTypePayloadMessage EmptyPayload) ++ "Got unexpected error during type inference phase.\n" ++ (show e)
