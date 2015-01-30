\documentclass{article}
%include polycode.fmt
\usepackage{hyperref}
\begin{document}
This program is used from Eclipse 'external tools' to convert the currently opened lhs file to a latex file.
It wraps a call to the lhs2TeX program which must be installed on the system path. This program requires a file-name
transformation from X.lhs to X.tex and that cannot be achieved from the Eclipse-external tools settings.

Input: --input = 'fully qualified path name of the .lhs file. This is expected to be in a /src directory.
Runs command "lhs2TeX -o latex-path/file.tex src-path/file.lhs"
Output: side-effect creates the .tex file in a sibling (to /src) folder called /latex.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
\end{code}
This language extension is needed by CmdArgs library.

\begin{code}
module Main where

import System.Environment
import System.Exit
import System.Process
import System.Console.CmdArgs
import Data.List.Split
import Data.List
import System.FilePath

data Lhs2TeXArgs = Lhs2TeXArgs {input :: FilePath} deriving (Show, Data, Typeable)    
currentArgs = Lhs2TeXArgs {input = def &= typFile &= help "The full path of the .lhs file"}
\end{code}
CmdArgs first requires a record to be declared which names the arguments. In this case, we're declaring an argument
called @--input@ which will be wrapped in a Haskell |System.FilePath|.
We then define an instance of that record type. Here we provide attributes such as default value, how the 'type'
is displayed (e.g. |typFile| shows @--input=FILE@, |typDir| shows @--input=DIR@ ), and the help message.

\begin{code}
main :: IO ()
main = do
    args <- cmdArgs currentArgs
    let lhsPath = input args
    putStrLn ("Path to input file: " ++ lhsPath)
    putStrLn ("Is it an lhs? " ++ show (takeExtension lhsPath == ".lhs"))
    -- endBy returns array [String], this should contain only one entry for the full path. 
    let latexPath = replace "\\src" "\\latex" (head (endBy ".lhs" lhsPath) ++ ".tex")
    putStrLn ("Path to output: " ++ latexPath) 
    let cmd = ("lhs2TeX " ++ "-o" ++ latexPath ++ " " ++ lhsPath)
    putStrLn cmd
    ExitSuccess <- system cmd
    return()


--Helper
replace :: String -> String -> String -> String
replace new old path = (intercalate old . splitOn new) path

\end{code}

\end{document}