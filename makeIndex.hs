-- List operations
import Data.List
import Data.Char

----------------------------------------------------------------
-- CS 352 - Project 3 (starter file)
--
-- Ben Creighton
--
----------------------------------------------------------------

----------------------------------------------------------------
-- function exec - reads the contents of the file "input.txt",
--   and creates an index of the wordLs; it writes the result to
--   standard output
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to standard output, because the 'createIndex' function is
--   dummied up.
--
----------------------------------------------------------------
exec :: IO ()
exec = 
  do input <- readFile "input.txt"
     putStr $ createIndex input

main :: IO ()
main = exec

----------------------------------------------------------------
-- function exec2 - reads the contents of the file "input.txt",
--   and creates an index of the wordLs; it writes the result to
--   the file "output.txt"
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec2
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to "output.txt", because the 'createIndex' function is
--   dummied up.
----------------------------------------------------------------
exec2 :: IO ()
exec2 = 
  do input <- readFile "input.txt"
     writeFile "output.txt" $ createIndex input

----------------------------------------------------------------
-- function createIndex - treats its String argument as a document;
--   produces an index for that document, with one entry per line,
--   where each word is followed by the line number(s) on which it
--   appears
--
-- **** THIS IS THE FUNCTION YOU NEED TO CHANGE ****
--
-- calling sequence:
--   createIndex str
--
-- example:
--   If the contents of the String 'str' is
--     I like the sound of fingernails on the chalkboard.
--     It dawned on me that not everyone, like me, does.
--   Then the result should be:
--     I 1
--     It 2
--     chalkboard 1
--     dawned 2
--     does 2
--     everyone 2
--     fingernails 1
--     like 1,2
--     me 2
--     not 2
--     of 1
--     on 1,2
--     sound 1
--     that 2
--     the 1
--   In other words, each word occuring in the string is listed
--   in alphabetical (OK, really ASCII) order, followed by an ordered
--   list of line number in which it occurs
--
-- status:
--   Incomplete.  Presently, it just echos returns the contents of the
--   'str'.
--
----------------------------------------------------------------
createIndex :: String -> String
createIndex str = showWordLIdx $ sort $ buildWordLList str -- map words $ lines str

-- Awful names, needs to be changed. Originally 'Word' because
-- I'm an idiot and ghc 7.10 throws a bunch of ambiguous defintion
-- errors.
-- WordL type, a pair of the wordL and a list of line locations
newtype WordL = WordL (String, [Int]) deriving (Eq)
type WordLIdx = [WordL]

--Ord instance for WordL, used for sorting the list.
instance Ord WordL where
  (WordL (str1, _)) `compare` (WordL (str2, _)) = str1 `compare` str2

-- Show instance for WordL, makes printing the list super easy.
instance Show WordL where
  show (WordL (wrd, lns)) = wrd ++ " " ++ showLines lns
    where
      showLines [a] = show a
      showLines (x:xs) = show x ++ ", " ++ showLines xs
      showLines [] = ""

-- Does exactly what is says, builds the list of words w/ line
--numbers
buildWordLList :: String -> WordLIdx
buildWordLList str = buildWordLList' [] 1 lineList
  where
    lineList = lines str
  --  fstr = concat $ map wordLs $ lines str

-- Recursive helper for buildWordLList
buildWordLList' :: WordLIdx -> Int -> [String] -> WordLIdx
buildWordLList' idx _ [] = idx
buildWordLList' idx n (l:[]) = processLine idx n l
buildWordLList' idx n (l:ls) = processLine nextLine n l
  where
    nextLine = buildWordLList' idx (n+1) ls

-- Takes the WordLIdx, the line number, and the line.
-- Updates the WordLs in the WordLIdx if they appear in
-- the processed line.
processLine :: WordLIdx -> Int -> String -> WordLIdx
processLine idx ln str = foldl (\i s -> updateWordLLines i s ln) idx cln
  where
    cln = cleanLine $ words str

--wordLHelper :: WordLIdx -> 
--processLine' :: String -> [String]

--countWordLLines :: String -> String -> [Int]
--countWordLLines wordL lns = countWordLLines' wordLs (lines lns) 0 []

--countWordLLines' :: String -> [String] -> Int -> [Int] -> [Int]
--countWordLLines' _    []  _  linelist = linelist
--countWordLLines' wordL lns ln = 

-- Takes a String and a list of line number and makes
-- a WordL
makeWordL :: String -> [Int] -> WordL
makeWordL str lns = WordL (makeLower str, lns)
  -- null fixedStr
  --where
  --  fixedStr = makeLower str

--addWordL :: WordLIdx -> String -> [Int] -> WordLIdx
--addWordL idx str = case wordLLookup idx str of
--  Just _ -> idx

-- Updates a WorldL in a WordLIdx based on the string and
-- adds the new line number.
updateWordLLines :: WordLIdx -> String -> Int -> WordLIdx
updateWordLLines idx str ln = case wordLLookup idx str of
  Just (WordL (_, lns)) -> updateWordL idx (WordL (str, ln:lns))
  Nothing -> addWordL idx str [ln]

-- Adds a WordL with a string and list of line numbers to a
-- WordLIdx
addWordL :: WordLIdx -> String -> [Int] -> WordLIdx
addWordL idx ""  _   = idx
addWordL idx str lns = case wordLLookup idx str of
  Just _ -> idx
  Nothing -> (makeWordL str lns):idx

-- Takes a WordLIdx and a WordL and replaces any WordL
-- with the same String in the Idx w/ the new one.
updateWordL :: WordLIdx -> WordL -> WordLIdx
updateWordL idx w@(WordL (str, _)) = case wordLLookup idx str of
  Just a -> insertNew $ break (== a) idx 
  Nothing -> idx

  where insertNew (x, old:xs) = x ++ (w:xs)

-- Takes an Idx and a string. If there is a WordL in that
-- Idx then it returns Just WordL, if not, then Nothing.
wordLLookup :: WordLIdx -> String -> Maybe WordL
wordLLookup [] _ = Nothing
wordLLookup _ "" = Nothing
wordLLookup (w@(WordL (str, ln)):rest) key
  | str == makeLower key = Just w
  | otherwise = wordLLookup rest key

--addWordLLine :: WordLIdx -> Int -> String -> WordLIdx
--addWordLLine idx line str = case wordLLookup idx str of
--  Just a -> idx
--  Nothing -> idx
  
--  where
--    appLine line
--      | line `elem` l = l
--      | otherwise = insert line l

--addWordLLine :: Int -> WordL -> WordL
--addWordLLine line (WordL (s, l)) = WordL (s, appLine line)
--  where
--    appLine line
--      | line `elem` l = l
--      | otherwise = insert line l

-- Also removes punctuation
makeLower :: String -> String
makeLower = map toLower . filter isLetter

-- Makes all words clean and removes duplicates
cleanLine :: [String] -> [String]
cleanLine = nub . map makeLower

-- Makes a nice string from a WordLIdx
showWordLIdx :: WordLIdx -> String
showWordLIdx idx = unlines $ map show idx