module Main where

import Lexer
import Parser
import Interpreter
import TypeChecker

-- Para rodar no windows:
-- runghc Main.hs < examples\ex1.ml 

main = getContents >>= print . eval . typecheck . parser . lexer 