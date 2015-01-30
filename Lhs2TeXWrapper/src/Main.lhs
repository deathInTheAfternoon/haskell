\documentclass{article}
%include polycode.fmt
\usepackage{hyperref}
\begin{document}
Lhs2TeXWrapper is used from 'Eclipse External Tools (EET)' to call the lhs2TeX program which converts .lhs files to .tex format. 
lhs2TeX could be called directly from EET, except we want to provide the string "file.tex" (derived from "file.lhs") on the 
lhs2TeX command line. Sadly, EET has no way of referring to a file name without extension. 
Therefore, EET delegates to Lh2TeXWrapper to construct the appropriate command line arguments.

For Lhs2TeXWrapper to work, the following must be true:
\begin{itemize}
 \item lhs2TeX is installed on the system path.
 \item Eclipse External Tools has a 'program configuration':
 \begin{itemize}
   \item 'Location' set to where the .exe is installed 
        e.g. @C:\Users\nthakur\git\haskell\Lhs2TeXWrapper\.cabal-sandbox\bin\Lhs2TeXWrapper.exe@
   \item 'Arguments' set to @-i ${resource_loc}@ 
 \end{itemize}
 \item The .lhs file is directly under the @/src@ directory.
 \item A sibling directory of @/src@ exists called @/latex@.
 \item The .lhs file is selected in Eclipse when Lhs2TeXWrapper is run.
 \item the fully qualified name of the .lhs file is provided to Lhs2TeXWrapper.
\end{itemize}

Lhs2TeX runs the command: @lhs2TeX -o latex-path/file.tex src-path/file.lhs@
This results in the .tex file with the same name as the .lhs file being formed in the @/latex@ directory.

\section{Lhs2TeXWrapper.hs}
We start with a language extension needed by CmdArgs library and the |import| statements.
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (system)
import System.Console.CmdArgs (cmdArgs, (&=), def, typFile, help)
import Data.List.Split (splitOn, endBy)
import Data.List (intercalate)
import System.FilePath (takeExtension)
import Data.Typeable (Typeable)
import Data.Data (Data)
\end{code}
We use the CmdArgs library to handle command-line arguments.
\begin{code}
data Lhs2TeXArgs = Lhs2TeXArgs {input :: FilePath} deriving (Show, Data, Typeable)   
currentArgs :: Lhs2TeXArgs 
currentArgs = Lhs2TeXArgs {input = def &= typFile &= help "The full path of the .lhs file"}
\end{code}
CmdArgs first requires a record to be declared whose field names identify each command-line argument. In this case, we're declaring 
a field |input| that will appear on the command-line as @--input@. Further, any value provided for this argument will be wrapped in 
a Haskell |System.FilePath| by the time it reaches us.
We then define an instance of that record type which is used to define 'attributes' such as: default value, how the 'type'
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
    putStrLn ("Running command: " ++ cmd)
    ExitSuccess <- system cmd
    return()


--Helper
replace :: String -> String -> String -> String
replace new old path = (intercalate old . splitOn new) path

\end{code}

\end{document}