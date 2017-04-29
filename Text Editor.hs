data TextEditor = TextEditor ([Char],[Char],[Char],[Char]) deriving(Show)

text :: TextEditor

text = TextEditor("The cat", " ", "sat on the mat", " ")

-- Define the functions
lineStart :: TextEditor -> TextEditor
lineEnd :: TextEditor -> TextEditor
characterInsert :: Char -> TextEditor -> TextEditor
characterDelete :: TextEditor -> TextEditor
backspace :: TextEditor -> TextEditor
copy :: TextEditor -> TextEditor
paste :: TextEditor -> TextEditor

-- Line Start
lineStart(TextEditor(l, hi, ri, b)) = (TextEditor(" | ", [ ], l ++ " " ++ ri, b))

-- Line End
lineEnd(TextEditor(l, hi, ri, b)) = (TextEditor(l ++ " " ++ ri, [ ], " | ", b))

-- Character Insert
characterInsert char (TextEditor(l, hi, r, b)) = (TextEditor(reverse (char: reverse l), hi, r, b))

-- Character Delete
characterDelete(TextEditor(l, hi, r, b)) = (TextEditor(((l ++  "|" ++ (tail r)), hi, " ", b)))

-- Backspace
backspace(TextEditor(l, hi, ri, b)) = (TextEditor(reverse (tail(reverse l)), hi, ri, b))

-- Copy
copy(TextEditor(l, hi, r, b)) = (TextEditor(l, hi, r, hi))

-- Paste
-- TODO: Add error checking
paste(TextEditor(l, hi, r, b)) = (TextEditor(l ++ b, hi, r, b))
