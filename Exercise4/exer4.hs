
module Exercise4 where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio

-- • Problem 1.

--  Haskell functions are values, which may be processed in the
--  same way as other data such as numbers, tuples or lists. In this
--  tutorial we'll use a number of higher-order functions, which
--  take other functions as arguments, to write succinct definitions for
--  the sort of list-processing tasks that you've previously coded
--  explicitly using recursion or comprehensions.

--  The first part of the tutorial deals with three higher-order functions,
--  map, filter, and fold. For each of these you will be asked to write
--  three functions. The second part deals with fold in some more detail,
--  and will ask you to write functions using both map and filter at the
--  same time.
--
--  Transforming every list element by a particular function is a common
--  need when processing lists---for example, we may want to
--
--  - add one to each element of a list of numbers,
--  - extract the first element of every pair in a list,
--  - convert every character in a string to uppercase, or
--  - add a grey background to every picture in a list of pictures.
--
--
--  The map function captures this pattern, allowing us to avoid
--  the repetitious code that results from writing a recursive function
--  for each case.
--
--  Consider a function g defined in terms of an imaginary function
--  f as follows:
--
--  g [] = []
--  g (x:xs) = f x : g xs
--
--  The function g can be written with recursion (as above),
--  or with a comprehension, or with map: all three definitions are
--  equivalent.
--
--  g xs = [ f x | x <- xs ]
--  g xs = map f xs
--
--  Below is the definition of map. Note the similarity to
--  the recursive definition of g (just below map). As compared with g,
--  map takes one additional argument: the function f that
--  we want to apply to each element.
--
--  map :: (a -> b) -> [a] -> [b]
--  map f [] = []
--  map f (x:xs) = f x : map f xs
--
--  g [] = []
--  g (x:xs) = f x : g xs
--
--  Given map and a function that operates on a single element, we
--  can easily write a function that operates on a list. For instance,
--  the function that extracts the first element of every pair can be
--  defined as follows (using fst :: (a,b) -> a):
--
--  fsts :: [(a,b)] -> [a]
--  fsts pairs = map fst pairs
--
--  Use map and other suitable library functions, for this exercise and the next ones.
--
--  Write a function uppers :: String -> String that converts a string to uppercase.

uppers :: String -> String
uppers xs = map toUpper xs

--
--  • Problem 2.
--
--  Write a list-comprehension version of uppers and use it to check your answer to the first exercise.
--

uppers' :: String -> String
uppers' xs = [toUpper x | x <-xs]

prop_upper :: String -> Bool
prop_upper str = uppers str == uppers' str

--  • Problem 3.
--
--  Write a function doubles :: [Int] -> [Int] that doubles every item in a list.

doubles :: [Int] -> [Int]
doubles xs = map double xs
    where double x = x*2

--  • Problem 4.
--
--  Write a function penceToPounds :: [Int] -> [Float] that turns prices given in pence into the same price in pounds.

penceToPounds :: [Int] -> [Float]
penceToPounds xs = map penceToPound xs
    where penceToPound x = fromIntegral x / 100

--  • Problem 5.
--
--  Removing elements from a list is another common need. For example, we
--  might want to remove non-alphabetic characters from a string, or
--  negative integers from a list. This pattern is captured by the
--  filter function.
--
--  Consider a function g defined in terms of an imaginary
--  predicate p as follows (a predicate is just a function into a
--  Bool value):
--
--  g [] = []
--  g (x:xs) | p x = x : g xs
--  | otherwise = g xs
--
--  The function g can be written with recursion (as above), or with
--  a comprehension, or with filter: all three definitions are equivalent.
--
--  g xs = [ x | x <- xs, p x ]
--  g xs = filter p xs
--
--  For instance, we can write a function evens :: [Int] -> [Int],
--  which removes all odd numbers from a list using filter and the
--  standard function even :: Int -> Int:
--
--  evens list = filter even list

--  This is equivalent to:
--
--  evens list = [x | x <- list, even x]
--
--  Below is the definition of filter. Note the similarity to
--  the way g is defined (just below filter). As compared with g,
--  filter takes one additional argument: the predicate that we use
--  to test each element.
--
--  filter :: (a -> Bool) -> [a] -> [a]
--  filter p [] = []
--  filter p (x:xs) | p x = x : filter p xs
--  | otherwise = filter p xs
--
--  g [] = []
--  g (x:xs) | p x = x : g xs
--  | otherwise = g xs
--
--  Use filter and other standard library functions for this exercise and the next ones.
--
--  Write a function alphas :: String -> String that removes all non-alphabetic characters from a string.

alphas :: String -> String
alphas xs = filter isAlpha xs

--
--  • Problem 6.
--
--  Define a function rmChar :: Char -> String -> String that removes all occurrences of a character from a string.

rmChar :: Char -> String -> String
rmChar ch str = filter remove str
    where remove xs = xs /= ch


--  • Problem 7.
--
--  Define a function above :: Int -> [Int] -> [Int] that removes
--  all numbers less than or equal to a given number.

above :: Int -> [Int] -> [Int]
above threshold list = filter element list
    where element ele = ele > threshold

--  • Problem 8.
--
--  Define a function unequals :: [(Int,Int)] -> [(Int,Int)] that
--  removes all pairs (x,y) where x == y.
--
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter pair xs
    where pair xs | fst xs == snd xs = False
                  | otherwise = True

--  • Problem 9.
--
--  As we have seen, list comprehensions process a list using transformations similar to
--  map and filter. In general, [f x | x <- xs, p x] is equivalent to map f (filter p xs).
--
--  Write an expression that is equivalent to the given expression using map and filter.
--
--  [toUpper c | c <- s, isAlpha c]
--
toUpperChar :: String -> String
toUpperChar xs = map toUpper (filter isAlpha xs)
--
--  • Problem 10.
--
--  Write an expression that is equivalent to the given expression using
--  map and filter.
--
--  [2 * x | x <- xs, x > 3]
--

doubles1 :: [Int] ->  [Int]
doubles1 xs = map (*2) $ filter (>3) xs


doubles' :: [Int] -> [Int]
doubles' xs = map p (filter (>3) xs)
    where
        p x = x * 2

--  • Problem 11.
--
--  Write an expression that is equivalent to the given expression using map and filter.
--
--  [reverse s | s <- strs, even (length s)]
--
--

reverse' xs = map reverse (filter p xs)
    where p x = even (length x)

reverseListComprehension strs = [reverse s | s <- strs, even (length s)]

--  • Problem 12.
--
--
--  The map and filter functions act on elements individually;
--  they never combine one element with another.
--
--  Sometimes we want to combine elements using some operation. For
--  example, the sum function can be written like this:
--
--  sum [] = 0
--  sum (x:xs) = x + sum xs

--  Here we're essentially just combining the elements of the list using
--  the + operation. Another example is reverse, which
--  reverses a list:
--
--  reverse [] = []
--  reverse (x:xs) = reverse xs ++ [x]
--
--  This function is just combining the elements of the list, one by one,
--  by appending them onto the end of the reversed list. This time the
--  ``combining'' function is a little harder to see. It might be easier
--  if we wrote it this way:
--
--  reverse [] = []
--  reverse (x:xs) = x `snoc` reverse xs
--
--  snoc x xs = xs ++ [x]
--
--  Now you can see that `snoc` plays the same role as +
--  played in the example of sum.
--
--  These examples (and many more) follow a pattern: we break down a list
--  into its head x and tail xs, recurse on xs, and then apply some function
--  to x and the modified xs. The only things we need to specify are the
--  function such as + or snoc and the initial value such as
--  0 in the case of sum and [] in the case of reverse.
--
--  This pattern is called ``a fold'' and is implemented in Haskell via
--  the function foldr.
--
--  foldr :: (a -> b -> b) -> b -> [a] -> b
--  foldr f u [] = u
--  foldr f u (x:xs) = x `f` foldr f u xs
--
--  g [] = u
--  g (x:xs) = x `f` g xs
--
--  The function g can be written with recursion (as above) or by
--  using a fold: both definitions are equivalent.
--
--  g xs = foldr f u xs
--
--  For example, we can define sum :: [Int] -> Int as follows, using (+) as the function and 0 as the initial value (unit):
--
--  sum :: [Int] -> Int
--  sum ns = foldr (+) 0 ns
--
--  Note: to treat an infix operator like + as a
--  function name, we need to wrap it in parentheses.
--
--  We will practice the use of foldr by writing several functions first with recursion,
--  and then using foldr. You can use other standard library functions as well.
--
--  Look at the recursive function productRec :: [Int] -> Int that computes the product of
--  the numbers in a list, and write an equivalent function productFold using foldr.

productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold xs = foldr (*) 1 xs

--  • Problem 13.
--
--  Write a recursive function andRec :: [Bool] -> Bool that checks whether every item in a list is True.
--  Then, write the same function using foldr, this time called andFold.
--
--
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs 

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

--  • Problem 14.
--
--  Write a recursive function concatRec :: [[a]] -> [a] that concatenates a list of lists into a single list. Then, write a similar function concatFold using foldr.
--

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold xs = foldr (++) [] xs

--
--  • Problem 15.
--
--  Write a recursive function rmCharsRec :: String -> String -> String that removes all characters in the
--  first string from the second string, using your function rmChar from exercise 6.
--  Then write the same function rmCharsFold using foldr.
--

rmCharsRec :: String -> String -> String 
rmCharsRec [] str2 = str2
rmCharsRec (x:str1) str2 =  rmCharsRec str1 $ rmChar x str2


rmCharsFold str1 str2 = foldr (rmChar) str2 str1

--  • Problem 16.

--  Next, we will look at matrix addition and multiplication. As matrices we will use lists of lists of Ints.
--
--  The declaration below, which you can find in your tutorial3.hs, makes the type Matrix a shorthand for the type [[Int]].
--

type Matrix = [[Int]]
--
--  Our first task is to write a test to show whether a list of lists of Int 
--  is a matrix. This test should verify two things: 
--  1) that the lists of Int are all of equal length, and 
--  2) that there is at least one row and one column in the list of lists.
--  Write a function uniform :: [Int] -> Bool that tests whether the integers in a list 
--  are all equal. You can use the library function all, 
--  which tests whether all the elements of a list satisfy a predicate; 
--  check the type to see how it is used. If you want, you can try to define all in terms of foldr and map.

uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all  (== head xs) $ tail xs


--  • Problem 17.
--
--  Using your function uniform write a function valid :: [[Int]] -> Bool that tests whether a list of lists of Int is a matrix (it should test the properties 1) and 2) specified above).
--

valid :: [[Int]] -> Bool
valid mat = not (null (head mat)) && uniform (map length mat)

--
--  • Problem 18.
--
--  A useful higher-order function is zipWith. It is a lot like the function zip that you have seen, which takes two lists and combines the elements in a list of pairs. The difference is that instead of combining elements as a pair, you can give zipWith a specific function to combine each two elements. The definition is as follows:
--
--  zipWith f [] _ = []
--  zipWith f _ [] = []
--  zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
--
--  Another useful function for working with pairs is uncurry, which turns a function that takes two arguments into a function that operates on a pair.
--
--  Look up the definition of uncurry. What is returned by the following expression?
--
--  Main> uncurry (+) (10,8)

--  Show how to define zipWith using zip and a list comprehension.
--
zipWith' f xs ys = [ f x y | (x, y) <- zip xs ys ]

--  • Problem 19.
--
--
--  Show how to define zipWith using zip and the higher-order functions map and uncurry, instead of the list comprehension.
--
zipWithNew f xs ys = map (uncurry f) (zip xs ys)

--  • Problem 20.


--  Adding two matrices of equal size is done by pairwise adding the elements that are in the same position, i.e. in the same column and row, to form the new element at that position.
--
--  We will use zipWith to implement matrix addition.
--
--  Write a function plusM that adds two matrices. Return an error if the input is not suitable. It might be helpful to define a helper function plusRow that adds two rows of a matrix.
--

matrixRow :: Matrix -> Int
matrixRow mat = length mat
 
matrixColumn :: Matrix -> Int
matrixColumn mat = length (head mat)

matrixAddition :: Matrix -> Matrix -> Matrix
matrixAddition mat1 mat2 | validateMatrix = zipWith (zipWith (+)) mat1 mat2
                         | otherwise = error "Invalid Matrix"
    where validateMatrix = matrixRow mat1 == matrixRow mat2 
                        && matrixColumn mat1 == matrixColumn mat2 

--  • Problem 21.
--
--
--  For matrix multiplication we need what is called the dot product or inner product of two vectors.
--
--  Matrix multiplication is then defined as follows: two matrices with dimensions (n,m) and (m,p) are multiplied to form a matrix of dimension (n,p) in which the element in row i, column j is the dot product of row i in the first matrix and column j in the second.
--
--  Define a function timesM to perform matrix multiplication. Return an error if the input is not suitable. It might be helpful to define a helper function dot for the dot product of two vectors (lists). The function should then take the dot product of the single row with every column of the matrix, and return the values as a list. To make the columns of a matrix readily available you can use the function transpose.

matrixMultiplication :: Matrix -> Matrix -> Matrix
matrixMultiplication mat1 mat2 | validateMatrix = [ [ dot row col | col <- transpose mat2 ] | row <- mat1 ]
                               | otherwise = error "Invalid Matrix"
    where dot xs ys = sum (zipWith (*) xs ys)
          validateMatrix = matrixColumn mat1 == matrixRow mat2
