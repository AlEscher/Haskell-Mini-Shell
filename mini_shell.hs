module Mini_Shell where
import Data.List (break)
import System.IO

type Name = String
type Data = String
data FSItem = File Name Data | Directory Name [FSItem]
  deriving (Eq, Show)

-- basic Zipper explanation: http://learnyouahaskell.com/zippers
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
type FSZipper = (FSItem, [FSCrumb])

-- fs = Directory "" [ Directory "Last" [ Directory "Christmas" [ File "heart.wav" "I gave you" ], File "Merry" "Christmas"], File "proof.cprf" "Stop Lemma Time"]
indent :: Int -> String
indent i = [' ' | x <- [1..i*2]]
prettyHelp :: FSItem -> Int -> Bool -> String
prettyHelp (File name dat) depth _ = indent depth ++ "|- " ++ name ++ " \"" ++ dat ++ "\"\n"
prettyHelp (Directory name []) depth printName
  | null name = "/\n"
  | printName = indent depth ++ "|- " ++ name ++ "/\n"
  | otherwise = []
prettyHelp (Directory name (f:fs)) depth printName
  | null name = "/\n" ++ prettyHelp f 0 True ++ prettyHelp (Directory "foo" fs) 0 False
  | printName = indent depth ++ "|- " ++ name ++ "/\n" ++ prettyHelp f (depth+1) True ++ prettyHelp (Directory name fs) (depth+1) False
  | otherwise = prettyHelp f depth True ++ prettyHelp (Directory name fs) depth False
-- show a FSItem as a String
pretty :: FSItem -> String
pretty f = prettyHelp f 0 True

-- moves the Zipper one level down, into a directory / file specified by "name"
moveZipper :: Name -> FSZipper -> FSZipper
moveZipper name (Directory dirName [], cs) = (Directory dirName [], cs)
-- break returns a pair of lists, first pair contains all elements before "name", second list contains the element we are looking for and the rest of the item-list
moveZipper name (Directory dirName fsitems, cs)
 | null name && null dirName = (Directory dirName fsitems, cs)
 | otherwise = let (ls, itemToFocus:rs) = break (namesMatch name) fsitems in (itemToFocus, FSCrumb dirName ls rs :cs)

-- works through a path (relative to the current item of the zipper) given by a list of directory names and moves the zipper accordingly
workThroughPath :: [String] -> FSZipper -> FSZipper
workThroughPath [] currentFocus = currentFocus
workThroughPath (s:ss) currentFocus = workThroughPath ss (moveZipper s currentFocus)

-- check if a Name matches with the name of a FSItem
namesMatch :: Name -> FSItem -> Bool
namesMatch name (File fileName _) = name == fileName
namesMatch name (Directory dirName _) = name == dirName

-- like Data.List.Split
splitOn :: String -> Char -> String -> [String] -> [String]
splitOn [] spliterator currentS sList = sList ++ [currentS]
splitOn (c:cs) spliterator currentS sList 
  | c == spliterator = splitOn cs spliterator [] (sList ++ [currentS])
  | otherwise = splitOn cs spliterator (currentS++[c]) sList

-- given a Zipper, this method moves the zipper up until we reach the root directory
zipperToItem :: FSZipper -> FSItem
zipperToItem (item, []) = item
zipperToItem (item, FSCrumb name ls rs:cs) = zipperToItem (Directory name (ls++[item]++rs), cs)

-- removes quotation marks from the data input for a file
removeQuotation :: String -> String
removeQuotation s = init (tail s)

-- since we split the whole userInput on ' ', if the new data for a file contains spaces, it will be split into its words
handleEditData :: [String] -> String
handleEditData [] = []
handleEditData [s] = s
handleEditData (s:ss) = s ++ " " ++ handleEditData ss

-- check if a Name (for a File or Directory) is already contained in the FSItem-list of a Directory
itemAlreadyContained :: Name -> [FSItem] -> Bool
itemAlreadyContained _ [] = False
itemAlreadyContained name (f:fs) = namesMatch name f || itemAlreadyContained name fs

-- insert an FSItem in alphabetical order: Directories always come first, then all Files
insertAlphabetically :: FSItem -> [FSItem] -> [FSItem] -> [FSItem]
insertAlphabetically (File name text) [] ls = ls ++ [File name text]
insertAlphabetically (Directory name fs2) [] ls = ls ++ [Directory name fs2]
-- if we are inserting a Directory and we have reached a file, insert the Directory
insertAlphabetically (Directory name fs2) ((File fileName text):fs) ls = ls ++ [Directory name fs2] ++ ((File fileName text):fs)
-- if we are inserting a file, skip all Directories
insertAlphabetically (File name text) (Directory dirName fs2 :fs) ls = insertAlphabetically (File name text) fs (ls ++ [Directory dirName fs2])
-- check if we should insert our item or continue moving through the FSItems
insertAlphabetically (File name text) (File name2 text2 :fs) ls
    | name > name2 = insertAlphabetically (File name text) fs (ls ++ [File name2 text2])
    | otherwise = ls ++ [File name text] ++ [File name2 text2] ++ fs
insertAlphabetically (Directory name fs2) (Directory dirName fs3 :fs) ls
    | name > dirName = insertAlphabetically (Directory name fs2) fs (ls ++ [Directory dirName fs3])
    | otherwise = ls ++ [Directory name fs2] ++ [Directory dirName fs3] ++ fs

-- The Test 2 apparently writes "mkdir /new/" instead of "mkdir /new" like on the sheet
-- converts "mkdir /foo/ to mkdir /foo, needed because we split the path on the character '/'"
fixWrongTestInput :: String -> String
fixWrongTestInput [] = []
-- length > 1 so that we dont accidentally delete the path "/" (root)
fixWrongTestInput s = if length s > 1 && (last s == '/') then init s else s

-- given an absolute path, moves the zipper to the last directory and inserts the new directory into it (returns the new root directory)
mkdir :: String -> FSZipper -> FSItem
mkdir absPath zipper = let dirList = splitOn absPath '/' [] []; dirToInsert = last dirList; path = init dirList; (Directory dirName fs, cs) = workThroughPath path zipper
  in if itemAlreadyContained dirToInsert fs then zipperToItem (Directory dirName fs, cs)
      else zipperToItem (Directory dirName (insertAlphabetically (Directory dirToInsert []) fs []), cs)

-- creates a new File in a Folder specified by an absolute path (returns the new root directory)
touch :: String -> FSZipper -> FSItem
touch absPath zipper = let dirList = splitOn absPath '/' [] []; newFile = last dirList; path = init dirList; (Directory dirName fs, cs) = workThroughPath path zipper
  in if itemAlreadyContained newFile fs then zipperToItem (Directory dirName fs, cs) else zipperToItem (Directory dirName (insertAlphabetically (File newFile []) fs []), cs)

-- removes a file or directory (returns the new root directory)
remove :: String -> FSZipper -> FSItem
remove absPath zipper = let dirList = splitOn absPath '/' [] []; (_, (FSCrumb name ls rs) : cs) = workThroughPath dirList zipper
  in zipperToItem (Directory name (ls++rs), cs)

-- edit the contents of a file (returns the new root directory)
edit :: String -> String -> FSZipper -> FSItem
edit absPath newData zipper = let dirList = splitOn absPath '/' [] []; (File fileName _, FSCrumb dirName ls rs : cs) = workThroughPath dirList zipper
  in zipperToItem (Directory dirName (ls ++ [File fileName newData] ++ rs), cs)

cd :: String -> FSZipper -> FSZipper
cd path (File fileName content, FSCrumb name ls rs : cs)
  | path == ".." = (Directory name (ls ++ [File fileName content] ++ rs), cs)
  | path == "." = (File fileName content, FSCrumb name ls rs : cs)
  | otherwise = (File fileName content, FSCrumb name ls rs : cs)
-- if our FSCrumb list is empty, it means that we are in the root directory
cd path (Directory dirName fs,  [])
  -- and "cd .." will return the unchanged Zipper
  | path == ".." || path == "." = (Directory dirName fs, [])
  -- the path will be an absolute path
  | otherwise = let dirList = splitOn path '/' [] []
    in workThroughPath dirList (Directory dirName fs, [])
cd path (Directory dirName fs, FSCrumb name ls rs : cs)
  -- move up one directory
  | path == ".." = (Directory name (ls ++ [Directory dirName fs] ++ rs), cs)
  | path == "." = (Directory dirName fs, FSCrumb name ls rs : cs)
  | otherwise = let dirList = splitOn path '/' [] []
    in workThroughPath dirList (Directory dirName fs, FSCrumb name ls rs : cs)

-- take inputs from the user and handle them
mainHelp :: FSZipper -> IO ()
mainHelp zipper = do
    putStr "> "
    userInput <- getLine
    let { parsedInput = splitOn userInput ' ' [] [] }
    case (head parsedInput) of {
      "quit" ->  (do 
        putStrLn "Bye!"
        return ());
      "mkdir" -> (do
        let { newRoot = mkdir (fixWrongTestInput (last parsedInput)) zipper }
        putStrLn (pretty newRoot)
        mainHelp (newRoot, []));
      "rm" -> (do
        let { newRoot = remove (fixWrongTestInput (last parsedInput)) zipper }
        putStrLn (pretty newRoot)
        mainHelp (newRoot, []));
      "touch" -> (do
        let { newRoot = touch (last parsedInput) zipper }
        putStrLn (pretty newRoot)
        mainHelp (newRoot, []));
      "edit" -> (do
        let { path = head (tail parsedInput);
              -- length > 3 means that the new data contained spaces, which means it was also split
              newData = if (length parsedInput) > 3 then removeQuotation (handleEditData (tail (tail parsedInput)))
                        else removeQuotation (last parsedInput);
              newRoot = edit path newData zipper}
        putStrLn (pretty newRoot)
        mainHelp (newRoot, []));
      "cd" -> (do
        let { (newItem, cs) = cd (fixWrongTestInput (last parsedInput)) zipper }
        putStrLn (pretty (newItem))
        mainHelp (newItem, cs));
      other -> mainHelp zipper
    }

-- this function should be called by the user to start the shell
main :: IO ()
main = mainHelp (Directory "" [], [])
