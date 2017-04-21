import Data.List
import System.IO

-- Notes
-- Concatinating is ++

-- These are the sequences we will manipulate
leftSequence = ("Left")
rightSequence = ("Right")
highlightSequence = ("Highlight")
bufferSequence = ("Buffer")

-- This is the text editor itself
textEditor = [leftSequence, highlightSequence, rightSequence, bufferSequence]

-- This concatenates a message onto the left sequence. However I need to allow
-- parameters to do this.
characterInsert = do
  leftSequence ++ "Hi"

-- This deletes a character but I need to show the full text editor after
characterDelete = do
  drop 1 rightSequence
