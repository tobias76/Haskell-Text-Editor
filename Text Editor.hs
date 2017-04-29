data TextEditor = TextEditor ([Char],[Char],[Char],[Char]) deriving(Show)

text :: TextEditor

text = TextEditor("The cat", " ", "sat on the mat", " ")

-- Define the functions
characterInsert :: Char -> TextEditor -> TextEditor
characterDelete :: TextEditor -> TextEditor

-- Character Insert
characterInsert char (TextEditor(l, hi, r, b)) = (TextEditor(reverse (char: reverse l), hi, r, b))

-- Character Delete
characterDelete(TextEditor(l, hi, r, b)) = (TextEditor(((l ++ (tail r)), hi, " ", b)))
