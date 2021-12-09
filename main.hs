
-- Author: Marko Vukasinovic, mvukasinovic2019@my.fit.edu
-- Course: CSE 4250, Fall 2019
-- Project: Proj4, Tautology Checker
-- Language implementation: Glorious Glasgow Haskell Compilation System, version 8.4.3
import System.IO (isEOF)
import Data.Char()

-- Datatype for tautolgy symbol


main :: IO ()
main = do

  scanInput

scanInput :: IO ()
scanInput = do
  done <- isEOF
  if done
    then putStrLn "Bye!"
    else do
      line <- getLine
      char_arr <- mapM disjunctionOp line
      mapM_ print char_arr


      putStrLn "\nEnd of Line"
      scanInput

-- Char passes through, checks if disjunction, if not passes onto implicationOp, else returns
disjunctionOp :: Char -> IO (Char, String)
disjunctionOp inChar = do
  if inChar == 'A'
    then do
      return (inChar, "disjunction")

    else do
      res <- implicationOp inChar
      return res

implicationOp :: Char -> IO (Char, String)
implicationOp inChar = do
  if inChar == 'C'
    then do
      return (inChar, "implication")
    else do
      res <- nandOp inChar
      return res

nandOp :: Char -> IO (Char, String)
nandOp inChar = do
  if inChar == 'D'
    then do
      return (inChar, "nand")
    else do
      res <- equivalenceOp inChar
      return res

equivalenceOp :: Char -> IO (Char, String)
equivalenceOp inChar = do
  if inChar == 'E'
    then do
      return (inChar, "equivalence")
    else do
      res <- xorOp inChar
      return res

xorOp :: Char -> IO (Char, String)
xorOp inChar = do
  if inChar == 'J'
    then do
      return (inChar, "xor")
    else do
      res <- conjunctionOp inChar
      return res

conjunctionOp :: Char -> IO (Char, String)
conjunctionOp inChar = do
  if inChar == 'K'
    then do
      return (inChar, "conjunction")
    else do
      res <- unaryNegationOp inChar
      return res

unaryNegationOp :: Char -> IO (Char, String)
unaryNegationOp inChar = do
  if inChar == 'N'
    then do
      return (inChar, "unary negation")
    else do
      res <- objectOp inChar
      return res

objectOp :: Char -> IO (Char, String)
objectOp inChar  = do
    return (inChar, "object")

-- END OF FUNC GROUP

-- Check if a tautology
