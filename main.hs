
-- Author: Marko Vukasinovic, mvukasinovic2019@my.fit.edu
-- Course: CSE 4250, Fall 2019
-- Project: Proj4, Tautology Checker
-- Language implementation: Glorious Glasgow Haskell Compilation System, version 8.4.3
import System.IO (isEOF)
import Data.Char()
import Text.Read()
import Control.Monad.State

-- Datatype for tautolgy symbol
data Taut = Taut {iniChar :: Char, name :: String} deriving (Show)

-- Datatype for Stack Function and Monad

-- Stack functionality

-- END OF FUNC GROUP


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
      check_t <- checkTaut char_arr
      mapM_ print check_t
      putStrLn "End of Line"
      scanInput

-- Char passes through, checks if disjunction, if not passes onto implicationOp, else returns
disjunctionOp :: Char ->  IO (Taut)
disjunctionOp inChar = do
  if inChar == 'A'
    then do
      let resol = Taut {iniChar=inChar, name="disjunction"}
      return resol
    else do
      resol <- implicationOp inChar
      return resol

implicationOp :: Char -> IO (Taut)
implicationOp inChar = do
  if inChar == 'C'
    then do
      let reso = Taut {iniChar=inChar, name="implication"}
      return reso
    else do
      reso <- nandOp inChar
      return reso

nandOp :: Char -> IO (Taut)
nandOp inChar = do
  if inChar == 'D'
    then do
      let reso = Taut {iniChar=inChar, name="nand"}
      return reso
    else do
      reso <- equivalenceOp inChar
      return reso

equivalenceOp :: Char -> IO (Taut)
equivalenceOp inChar = do
  if inChar == 'E'
    then do
      let reso = Taut {iniChar=inChar, name="equivalence"}
      return reso
    else do
      reso <- xorOp inChar
      return reso

xorOp :: Char -> IO (Taut)
xorOp inChar = do
  if inChar == 'J'
    then do
      let reso = Taut {iniChar=inChar, name="xor"}
      return reso
    else do
      reso <- conjunctionOp inChar
      return reso

conjunctionOp :: Char -> IO (Taut)
conjunctionOp inChar = do
  if inChar == 'K'
    then do
      let reso = Taut {iniChar=inChar, name="conjunction"}
      return reso
    else do
      reso <- unaryNegationOp inChar
      return reso

unaryNegationOp :: Char -> IO (Taut)
unaryNegationOp inChar = do
  if (inChar == 'N')
    then do
      let res = Taut {iniChar=inChar, name="unary negation"}
      return res
    else do
      res <- objectOp inChar
      return res


objectOp :: Char -> IO (Taut)
objectOp inChar  = do
    let reso = Taut {iniChar=inChar, name="object"}
    return reso

-- END OF FUNC GROUP

-- Check if a tautology

-- Input: List of Tautologies, Output: Rearanged List Of Tautologies
-- Take in input, split the objects from everything else into two stacks
-- pop object stack into new list, pop tautolgy stack into new list
-- repeat until stacks empty
-- if both stacks not empty at same time, return false 

checkTaut :: [Taut] -> [Taut]
checkTaut (x:xs) = do
  res <- (x:xs)
  return res

-- END OF FUNC GROUP
