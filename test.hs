import Data.List
import System.IO

-- Notes
-- Concatinating is ++

-- These are the sequences we will manipulate
leftSequence = ("L")
rightSequence = ("R")
highlightSequence = ("H")
bufferSequence = ("B")

-- This is the text editor itself
textEditor = [leftSequence, highlightSequence, rightSequence, bufferSequence]

insertCharacter = do
  leftSequence ++ "Hi"
