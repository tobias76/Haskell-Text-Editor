main :: IO()
main = return()

data TextEditor = TextEditor ([Char],[Char],[Char],[Char]) deriving(Show)

text :: TextEditor

text = TextEditor("The cat sat", " ", " on the mat", " Buffer ")

-- Define the functions
moveLeft :: TextEditor -> TextEditor
moveRight :: TextEditor -> TextEditor
lineStart :: TextEditor -> TextEditor
lineEnd :: TextEditor -> TextEditor
characterInsert :: Char -> TextEditor -> TextEditor
characterDelete :: TextEditor -> TextEditor
backspace :: TextEditor -> TextEditor
leftWordMove :: TextEditor -> TextEditor
rightWordMove :: TextEditor -> TextEditor
highlightToStart :: TextEditor -> TextEditor
highlightToEnd :: TextEditor -> TextEditor
highlightCharacterBefore :: TextEditor -> TextEditor
highlightCharacterAfter :: TextEditor -> TextEditor
highlightWordBefore :: TextEditor -> TextEditor
highlightWordAfter :: TextEditor -> TextEditor
highlightEverything :: TextEditor -> TextEditor
copy :: TextEditor -> TextEditor
paste :: TextEditor -> TextEditor
cut :: TextEditor -> TextEditor

-- Move Left
moveLeft(TextEditor(l, hi, ri, b)) = (TextEditor(l , hi, [head(reverse l)] ++ ri , b))

-- Move Right
moveRight(TextEditor(l, hi, ri, b)) = (TextEditor(reverse(head ri :(reverse l)), hi, tail ri, b))

-- Line Start
lineStart(TextEditor(l, hi, ri, b)) = (TextEditor(" | ", [ ], l ++ " " ++ ri, b))

-- Line End
lineEnd(TextEditor(l, hi, ri, b)) = (TextEditor(l ++ " " ++ ri, [ ], " | ", b))

-- Character Insert
characterInsert char (TextEditor(l, hi, ri, b)) =
  if length l + length ri + length hi < 1023
    then TextEditor((l, hi, ri, b))
      else (TextEditor(reverse (char: reverse l), hi, ri, b))

-- Character Delete
characterDelete(TextEditor(l, hi, ri, b)) =
   if length ri < 0
     then (TextEditor(((l ++  "|" ++ (tail ri)), hi, " ", b)))
      else (TextEditor(l, hi, ri, b))

-- Backspace
backspace(TextEditor(l, hi, ri, b)) = (TextEditor(reverse (tail(reverse l)), hi, ri, b))

-- Move to the left word (Recursion Attempt)
-- TODO: Not done
leftWordMove(TextEditor(l, hi, ri, b)) = (TextEditor(l, hi, ri, b))
leftWordMove(TextEditor(l, hi, ri, b))
            | head l == ' ' = (TextEditor(l, hi, ri, b))
            | otherwise = leftWordMove(TextEditor(l, hi, ri, b))

-- Move to the right word (Recursion Attempt)
-- TODO: Not done
rightWordMove(TextEditor(l, hi, ri, b)) = (TextEditor(l, hi, ri, b))
rightWordMove(TextEditor(l, hi, ri, b))
            | head ri == ' ' = (TextEditor(l, hi, ri, b))
            | otherwise = rightWordMove(TextEditor(l, hi, ri, b))

-- Highlight To The Start
highlightToStart(TextEditor(l, hi, ri, b)) = (TextEditor(" ", l, ri, b))

-- Highlight To The End
highlightToEnd(TextEditor(l, hi, ri, b)) = (TextEditor(l, ri, " ", b))

-- Highlight Character Before
highlightCharacterBefore(TextEditor(l, hi, ri, b)) = (TextEditor(reverse (tail(reverse l)), reverse [head l], ri, b))

-- Highlight Character After
highlightCharacterAfter(TextEditor(l, hi, ri, b)) = (TextEditor(l, [head ri] , (tail ri), b))

-- Highlight Word Before (Recursion Attempt)
highlightWordBefore(TextEditor(l, hi, ri, b)) = (TextEditor(l, hi, ri, b))
highlightWordBefore(TextEditor(l, hi, ri, b))
                   | head l == ' ' = (TextEditor(l, hi, ri, b))
                   | otherwise = highlightWordBefore(TextEditor(l, hi, ri, b))

-- Highlight Word After (Recursion Attempt)
highlightWordAfter(TextEditor(l, hi, ri, b)) = (TextEditor([], hi, ri, b))
highlightWordAfter(TextEditor(l, hi, ri, b))
                  | head(reverse ri) == ' ' = (TextEditor(l, hi, ri, b))
                  | otherwise = highlightWordAfter(TextEditor(l, hi, ri, b))
                  
-- Highlight Everything
highlightEverything(TextEditor(l, hi, ri, b)) = (TextEditor(" ", l ++ ri, " ", b ))

-- Copy
copy(TextEditor(l, hi, ri, b)) =
   if length hi > 0
     then (TextEditor(l, hi, ri, hi))
       else (TextEditor(l, hi, ri, b))
-- Paste
paste(TextEditor(l, hi, ri, b)) =
    if length l + length ri + length hi < 1023 && length b > 0
      then (TextEditor(l ++ b, hi, ri, b))
        else (TextEditor(l, hi, ri, b))

-- Cut
cut(TextEditor(l, hi, ri, b)) = (TextEditor(l, " ", ri, hi))
