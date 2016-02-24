-- List operations
import Data.List
import Data.Char
import Data.Ord

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
--   In other words, each word occurring in the string is listed
--   in alphabetical (OK, really ASCII) order, followed by an ordered
--   list of line number in which it occurs
--
-- status:
--    Complete.
-- 
-- Enhancements:
--    Words with different capitalization are treated as the same word.
--    Single quotes/apostrophes are handled correctly.
--
----------------------------------------------------------------
createIndex :: String -> String
createIndex = unlines . showWords . mergeWordNums . sortWords . numberWords . numberLines . makeLines

-- Splits a complete file string into a list of lines (which are a list of words)
makeLines :: String -> [[String]]
makeLines = map (cleanLine . words) . lines

-- Returns a list of pairs. Each pair is a line number and a list of the words
-- on the line.
numberLines :: [[String]] -> [(Int, [String])]
numberLines = zip [1..]

-- Converts a list of line numbered pairs to a list of pairs of
-- a singleton list of the line number (for convenience later) and
-- a word. One element per word per line.
numberWords :: [(Int, [String])] -> [([Int], String)]
numberWords [] = []
numberWords ((ln, wrds):xs) = addLn wrds ++ numberWords xs
  where
    addLn = zip repLn
    repLn = repeat [ln]

-- The most complex function in the program. Takes the whole (sorted!) list of
-- of pairs of a singleton list w/ a line number and a word. Returns a reduced
-- list of pairs of a complete list of line numbers and a word, with one element
-- per word. Last step in the processing phase (i.e. not printing/formatting).
--
-- As an aside, I think this is technically a right fold? Maybe?
mergeWordNums :: [([Int], String)] -> [([Int], String)]
mergeWordNums [] = []
mergeWordNums [x] = [x]
mergeWordNums ls@(x:_) = (lns, snd x) : mergeWordNums rest
  where
    -- Splits the list into a pair with the first element
    -- being all the occurrences of head word, and the second
    -- being the rest of the list. Thanks lazy evaluation!
    splitWords = span (\y -> snd x == snd y) ls
    cwords = fst splitWords
    rest = snd splitWords
    lns = concatMap fst cwords

-- Takes the completed list of line numbers and words, and maps
-- the show function over each element. Returns a list of pretty,
-- formatted strings able to be conveniently 'unline'ed.
showWords :: [([Int], String)] -> [String]
showWords = map showWord

-- Builds a nicely formatted string from a pair of a list of line
-- numbers and a word. String is the word, a space, and a comma
-- separated list of line numbers. EX: 'the 2, 6, 19'
showWord :: ([Int], String) -> String
showWord (lns, wrd) = wrd ++ " " ++ showLines lns
  where
    showLines [] = ""
    showLines [a] = show a
    showLines (x:xs) = show x ++ ", " ++ showLines xs

-- Sorts a list of pairs of a (singleton) list of line numbers and a
-- a word alphabetically. The line numbers will already be in the correct
-- order.
sortWords :: [([Int], String)] -> [([Int], String)]
sortWords = sortBy (comparing snd)

-- Lowercases letters and removes non-letters (except apostrophes)
-- from a word. See below about the apostrophes.
cleanWord :: String -> String
cleanWord = map toLower . cleanApos . filterOkChars
  where
    filterOkChars = filter (\x -> isLetter x || (== '\'') x)

-- Deals with the odd situation of single quotes,
-- Empty strings are barfed back, but I think that's
-- actually redundant, since any strings with 0 apostrophes
-- are also returned as-is.
-- 
-- The interesting situations are when words are surrounded in quotes
-- or have them inside the word, or just at the end.
-- Those surrounded in quotes have the quotes removed and this function
-- is called (recursively) on the resulting string.
-- Those with an apostrophe at JUST the beginning have that removed and
-- the function is recursively called.
--
-- Everything else is left alone.
--
-- Fun fact: "nature's" is the only word with an apostrophe in the
-- Declaration of Independence.
cleanApos :: String -> String
cleanApos wrd
  | null wrd = ""
  | null aposIdx = wrd
  | inQuotes = cleanApos . init . tail $ wrd
  | 0 `elem` aposIdx = cleanApos . tail $ wrd
  | otherwise = wrd
  where
    lastChar = length wrd - 1
    aposIdx  = elemIndices '\'' wrd
    inQuotes = (0 `elem` aposIdx) && (lastChar `elem` aposIdx)

-- Makes all words clean and removes duplicates
-- Requires a pre-'words'ed line
cleanLine :: [String] -> [String]
cleanLine = nub . filter (not . null) . map cleanWord