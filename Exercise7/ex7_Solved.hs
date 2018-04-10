import System.Environment (getArgs)
import Data.Char          (isAlpha, ord)
import Data.List          (group, sort)
import System.IO          (isEOF)
import Data.Monoid        ((<>))
import qualified Data.Foldable as F    

{-
Problem 1. 
Write a Haskell program that reads a file from standard input and 
prints the mod256 check sum of the file. Suppose the executable
file is called check1. Then running it on Unix might look like the
following.
unxPrompt% ./check1
This is a line of text
and this is another one
^D
The check sum is 69
unxPrompt%
-}

--main = checksum1

checksum1 :: IO()
checksum1 = do 
            checksum <- getChecksum1 0
            putStrLn ("The check sum is "++ show checksum)

getCharacter :: IO(Maybe Char)
getCharacter = do {
               eof <- isEOF;
               if eof 
                then return Nothing
                else do 
                  ch <- getChar
                  return (Just ch)
}

getChecksum1 :: Int -> IO(Int)
getChecksum1 n = do {
                ch <- getCharacter;
                case ch of
                  Nothing -> return n
                  Just ch -> getChecksum1 ((n + ord (ch)) `mod` 256)
}

{-
Problem 2.
Same as above except that the program takes the name of the file to
check from the command line.2 Suppose the executable file is called
check2. Then running it on Unix might look like the following. this exercise.
unxPrompt% ./check2 file.txt
The check sum of file.txt is 6022.
unxPrompt%
-}
-- Use map ord fileData then sum it and then do mod

--main = checksum2 

checksum2 :: IO()
checksum2 = do 
            checksum <- getChecksum2 0
            putStrLn ("The check sum is "++ show checksum)

getChecksum2 n = do {
                (a:_) <- getArgs;
                fileData <- readFile a;
                putStrLn ("The check sum of "++ a ++ " is "++ show (length(fileData) `mod` 256))
}

main = checksum2'

checksum2' = getChecksum2' 0

getChecksum2' n = do {
                (a:_) <- getArgs;
                fileData <- readFile a;
                putStrLn ("The check sum of "++ a ++ " is "++ show ((total fileData) `mod` 256))
              }
          where 
              total fd = sum $ map (sum) (map (\x -> map ord x) (words fd))

{-
Problem 3.
Write a program that repeatly reads Floats, one per line, until 0.0 is
read; then the program prints the average of the numbers
-}

--main = floatSum

floatSum = do
           sum1 <- avg1 0
           putStrLn ("sUM : "++(show sum1))

avg1 n = do {
        input <- getLine
        ; let floatVal = (read input::Float)
        ; if floatVal==0.0 then return n else avg1 (floatVal+n)
      }


{-
Problem 4.
Write a Haskell program that takes the name of a file from the command
line and then produces a sorted word count from the input
file.4 For example, for a file sample.txt:
This is line one readFile, sort, unlines, and words.
and this is line one plus one

Then running the program on sample.txt might look like:
unxPrompt% ./wrdcount sample.txt
and 1
is 2
line 2
one 3
plus 1
this 2
unxPrompt%
-}

--main = wordCount

wordCount = do{
            (fileName:_) <- getArgs
            ; fileData <- readFile fileName
            ; putStrLn ("Here is the desired output \n" ++ wcount fileData)
          }
      where 
            wcount fd = unlines ( map (\x -> head(x) ++ " " ++ show (length(x))) 
              (group (sort (map (filter isAlpha) (words fd)))))

{-
Problem 5.
Suppose we represent binary trees via
data BinTree a = Empty | Branch a (BinTree a) (BinTree a)
deriving (Show,Eq)

Definition: A binary search tree (abbreviated: BST) is a binary
tree with the property that, for each node (Branch k tl tr):
• the labels in the left subtree (tl) are < k and
• the labels in the right subtree (tr) are > k.
Our BST’s will have no repeated values.
-}

data BinTree a = Empty | Branch a (BinTree a) (BinTree a) deriving (Eq,Show)

instance Functor BinTree where
    fmap f Empty            = Empty
    fmap f (Branch x tl tr) = Branch (f x) (fmap f tl) (fmap f tr)

instance Foldable BinTree where
    foldMap f Empty            = mempty
    foldMap f (Branch x tl tr) = foldMap f tl <> (f x) <> foldMap f tr

-- A sample BST
t = foldr insrt Empty [3,5,6,32,2,7,9]  -- insrt defined below
t' = fmap (\n-> ((97*n) `mod` 256)) t   -- a jumbled version of t


{- Below, do not assume a (BinTree a) is a BST unless we state it is.
(a) Write functions
preord, inord, postord :: BinTree a -> [a]
that given a (BinTree a), returns the list of labels in, respectively,
preorder, inorder, and postorder. -}

preord :: BinTree a -> [a]
preord Empty = []
preord (Branch a bTreeLeft bTreeRight) = a : preord (bTreeLeft) ++ preord (bTreeRight)


inord :: BinTree a -> [a]
inord Empty = []
inord (Branch a bTreeLeft bTreeRight) = inord (bTreeLeft) ++ [a] ++inord (bTreeRight)


postord :: BinTree a -> [a]
postord Empty = []
postord (Branch a bTreeLeft bTreeRight) = postord (bTreeLeft) ++ postord (bTreeRight) ++ [a]


{-(b) Write a function
isBST :: (Ord a) => (BinTree a) -> Bool
that tests if a (BinTree a) is a BST.
-}

isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing [a] = True
isIncreasing (x:y:xs) = (x<y) && isIncreasing (y:xs)

isBST :: (Ord a) => (BinTree a) -> Bool
isBST Empty = True
isBST tree@(Branch a tl tr) = isIncreasing (inord (tree))

{-(c) Write a function
insrt :: (Ord a) => (BinTree a) -> a -> (BinTree a)
such that, given that t is a BST, (insrt t v) returns an updated
version of t with the value v inserted inorder. When t already
contains v, then the function simply returns t.
-}

insrt :: Int -> BinTree Int -> BinTree Int
insrt n Empty = Branch n Empty Empty
insrt n t@(Branch m tl tr)
    | n==m      = t
    | n<m       = Branch m (insrt n tl) tr
    | otherwise = Branch m tl (insrt n tr)

{-(d) Write a function
postRebuild :: (Ord a) => [a] -> (BinTree a)
such that, given postorder listing of the labels of a BST, rebuilds
the tree.6 I.e., given a BST t, you should have:
(postRebuild (postord t)) == t
-}

postRebuild ::  [Int] -> BinTree Int
postRebuild xs = foldr insrt Empty xs

{-(e) Write a function
preRebuild :: (Ord a) => [a] -> (BinTree a)
such that, given preorder listing of the labels of a BST, rebuilds
the tree.7 I.e., given a BST t, you should have:
(preRebuild (preord t)) == t
-}

preRebuild ::  [Int] -> BinTree Int
preRebuild xs = foldl (flip insrt) Empty xs

{-(f) Write a function
rebuild :: [a] -> [a] -> (BinTree a)
such that, given preorder and inorder listings of the labels of a
(BinTree a) t, reconstructs t, where t is a (BinTree a) with no
repeated labels.8 I.e., given a such a t, you should have:
(rebuild (preord t) (inord t)) == t
-}

rebuild :: (Eq a) => [a] -> [a] -> (BinTree a)
rebuild [] [] = Empty
rebuild (c:cs) ds = Branch c (rebuild prelow inlow) (rebuild prehigh inhigh)
    where
      (inlow, (x:inhigh)) = break (==c) ds
      (prelow,prehigh)    = splitAt (length inlow) cs

tpre = preord t
tin  = inord  t

-- (tst t) tests rebuild on t, we assume t has no repeated values
tst t = (t == rebuild (preord t) (inord t))


