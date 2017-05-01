data TextEditor = TextEditor ([Char],[Char],[Char],[Char]) deriving(Show)

text :: TextEditor

text = TextEditor("The cat sat", " ", " on the mat", " ")

-- Define the functions
moveLeft :: TextEditor -> TextEditor
moveRight :: TextEditor -> TextEditor
lineStart :: TextEditor -> TextEditor
lineEnd :: TextEditor -> TextEditor
characterInsert :: Char -> TextEditor -> TextEditor
characterDelete :: TextEditor -> TextEditor
backspace :: TextEditor -> TextEditor
highlightToStart :: TextEditor -> TextEditor
highlightToEnd :: TextEditor -> TextEditor
highlightCharacterBefore :: TextEditor -> TextEditor
highlightCharacterAfter :: TextEditor -> TextEditor
highlightEverything :: TextEditor -> TextEditor
copy :: TextEditor -> TextEditor
paste :: TextEditor -> TextEditor
cut :: TextEditor -> TextEditor

-- Move Left
-- TODO: Check this is right with Callum
moveLeft(TextEditor(l, hi, ri, b)) = (TextEditor(l , hi, [head(reverse l)] ++ ri , b))

-- Move Right
-- TODO: Find a way to reverse this again
moveRight(TextEditor(l, hi, ri, b)) = (TextEditor(reverse(head r :(reverse l)), hi, tail ri, b))

-- Line Start
lineStart(TextEditor(l, hi, ri, b)) = (TextEditor(" | ", [ ], l ++ " " ++ ri, b))

-- Line End
lineEnd(TextEditor(l, hi, ri, b)) = (TextEditor(l ++ " " ++ ri, [ ], " | ", b))

-- Character Insert
-- TODO: Add error checking / pre-processing checks
characterInsert char (TextEditor(l, hi, r, b)) = (TextEditor(reverse (char: reverse l), hi, r, b))

-- Character Delete
characterDelete(TextEditor(l, hi, r, b)) = (TextEditor(((l ++  "|" ++ (tail r)), hi, " ", b)))

-- Backspace
backspace(TextEditor(l, hi, ri, b)) = (TextEditor(reverse (tail(reverse l)), hi, ri, b))

-- Highlight To The Start
highlightToStart(TextEditor(l, hi, ri, b)) = (TextEditor(" ", l, ri, b))

-- Highlight To The End
highlightToEnd(TextEditor(l, hi, ri, b)) = (TextEditor(l, ri, " ", b))

-- Highlight Character Before
highlightCharacterBefore(TextEditor(l, hi, ri, b)) = (TextEditor(reverse (tail(reverse l)), reverse [head l], ri, b))

-- Highlight Character After
highlightCharacterAfter(TextEditor(l, hi, ri, b)) = (TextEditor(l, [head ri] , (tail ri), b))

-- Highlight Everything
highlightEverything(TextEditor(l, hi, ri, b)) = (TextEditor(" ", l ++ ri, " ", b ))

-- Copy
copy(TextEditor(l, hi, ri, b)) = (TextEditor(l, hi, ri, hi))

-- Paste
-- TODO: Add error checking
paste(TextEditor(l, hi, ri, b)) = (TextEditor(l ++ b, hi, ri, b))

-- Cut
cut(TextEditor(l, hi, ri, b)) = (TextEditor(l, " ", ri, hi))
