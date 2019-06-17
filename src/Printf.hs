{-# LANGUAGE ExistentialQuantification #-}
module Printf where

import Runtime
import Environment
import Text.Printf

data PrintfArgT = forall a. PrintfArg a => P a

printfa :: PrintfType t => String -> [ PrintfArgT ] -> t
printfa format = printfa' format . reverse
  where printfa' :: PrintfType t => String -> [ PrintfArgT ] -> t
        printfa' format [] = printf format
        printfa' format (P a:as) = printfa' format as a

printfUnpack :: RuntimeValue -> PrintfArgT
printfUnpack (RInt v) = P v
printfUnpack (RBool v) = P (show v)
printfUnpack (RString v) = P v
printfUnpack v = P (valueToStr v)

printfVars :: PrintfType t => String -> [ RuntimeValue ] -> t
printfVars format vars =
  printfa format $ map printfUnpack vars