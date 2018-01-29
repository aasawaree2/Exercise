-- Name - Aasawaree Deshmukh

-- CIS 623, Exercises 2

import Data.Char
import Data.List

-- • Problem 1. ----------------------------------------------------------

-- Write a function halveEvens :: [Int] -> [Int], which takes as argument a list of integers and returns half of each even integer in that list.
-- For example halveEvens [0,2,1,7,8] == [0,1,4]

-- Your definition should use list comprehension, not recursion.
-- You may use the functions div, mod :: Int -> Int -> Int.

halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs , even x]

-- • Problem 2. ----------------------------------------------------------

-- Again, write a function which takes as argument a list of integers and returns half of each even integer in that list.
-- For example halveEvensRec [0,2,1,7,8] == [0,1,4]

-- This time use recursion, not a list comprehension. You may use div and mod again.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) 
    | even x = x `div` 2 : halveEvensRec xs
    | otherwise = halveEvensRec xs

-- • Problem 3. ----------------------------------------------------------

-- Write a function inRange :: Int -> Int -> [Int] -> [Int] to return all numbers in the input list within the range given 
-- by the first two arguments (inclusive). For example, inRange 5 10 [1..15] == [5,6,7,8,9,10]

-- Your definition should use list comprehension, not recursion.

inRange :: Int -> Int -> [Int] -> [Int]
inRange low high xs = [ x | x <- xs , low <= x && high >= x ]

-- • Problem 4. ----------------------------------------------------------

-- Once again, write a function to return all numbers in the input list within the range given by the first two arguments (inclusive). For example,

-- inRangeRec 5 10 [1..15] == [5,6,7,8,9,10]

-- This time your definition should use recursion, not list comprehension.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec low high [] = []
inRangeRec low high (x:xs)
    | low <= x && high >= x = x : inRangeRec low high xs
    | otherwise = inRangeRec low high xs

-- • Problem 5. ----------------------------------------------------------

-- Write a function countPositives to count the positive numbers (strictly greater than 0) in a list. For example,
-- countPositives [0,1,-3,-2,8,-1,6] == 3
-- Your solution should use a list comprehension. You may not use recursion, but you will need a specific library function.

countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs , x > 0 ] 

-- • Problem 6. ----------------------------------------------------------

-- Again, write a function countPositivesRec to count the positive numbers (strictly greater than 0) in a list. For example,
-- countPositivesRec [0,1,-3,-2,8,-1,6] == 3
-- Your function countPositivesRec should use recursion and no library functions.
-- Why do you think it's not possible to write countPositives using only list comprehension, without library functions?

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
    | x > 0 = 1 + countPositivesRec xs
    | otherwise = countPositivesRec xs

-- • Problem 7. ----------------------------------------------------------

-- Professor Pennypincher will not buy anything if he has to pay more than 199.00 Pounds. But, as a member of the Generous Teachers Society, 
-- he gets a 10% discount on anything he buys. Write a function pennypincher :: [Int] -> Int that takes a list of prices and returns the total 
-- amount that Professor Pennypincher would have to pay, if he bought everything that was cheap enough for him.

-- Prices should be represented in Pence, not Pounds, by integers. To deduct 10% off them, you will need to convert them into float first, 
-- using the function fromIntegral. To convert back to Int, you can use the function round, which rounds to the nearest integer. 
--You can write a helper function discount :: Int -> Int to do this. Note that all your function definitions should come with a type signature.

-- For example,
-- pennypincher [4500, 19900, 22000, 39900] == 41760

-- Your solution should use a list comprehension, and you may use a library function to do the additions for you.

discount :: Int -> Int
discount x = round ((fromIntegral x) * 0.9)

pennypincher :: [Int] -> Int
pennypincher xs = sum [ discount x | x <- xs , discount x <= 19900]

-- • Problem 8. ----------------------------------------------------------

-- Again, Professor Pennypincher will not buy anything if he has to pay more than 199.00 Pounds. But, as a member of the Generous Teachers Society,
-- he gets a 10% discount on anything he buys. Write a function pennypincherRec :: [Int] -> Int that takes a list of prices and returns the 
-- total amount that Professor Pennypincher would have to pay, if he bought everything that was cheap enough for him.

-- Prices should be represented in Pence, not Pounds, by integers. To deduct 10% off them, you will need to convert them into float first, 
--using the function fromIntegral. To convert back to Int, you can use the function round, which rounds to the nearest integer. 
--You can write a helper function discount :: Int -> Int to do this. Note that all your function definitions should come with a type signature.

-- For example, pennypincherRec [4500, 19900, 22000, 39900] == 41760

-- This time your solution should use a recursion and no library functions.

pennypincherRec :: [Int] -> Int 
pennypincherRec [] = 0
pennypincherRec (x:xs)
    | discount x <= 19900 = discount x + pennypincherRec xs
    | otherwise = pennypincherRec xs


-- • Problem 9. ----------------------------------------------------------

-- Write a function multDigits :: String -> Int that returns the product of all the digits in the input string. If there are no digits, 
-- your function should return 1. For example,

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

-- Your definition should use list comprehension. You'll need a library function to determine if a character is a digit, one to convert a
-- digit to an integer, and one to do the multiplication.

multDigits :: String -> Int
multDigits s = product [ digitToInt x | x <- s , isDigit x ]

-- • Problem 10. ----------------------------------------------------------

-- Now write an equivalent function multDigitsRec that also returns the product of all the digits in the input string. 
-- If there are no digits, your function should return 1. For example,

-- multDigitsRec "The time is 4:25" == 40
-- multDigitsRec "No digits here!" == 1

-- This time use recursion. You may use library functions that act on single characters or integers, but you may not use library 
-- functions that act on a list.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
    | isDigit x = digitToInt x * multDigitsRec xs
    | otherwise = multDigitsRec xs

-- • Problem 11. ----------------------------------------------------------

-- Write a function capitalise :: String -> String which, given a word, capitalises it. That means that the first character should be made uppercase and any other letters should be made lowercase. For example,

-- capitalise "edINBurgH" == "Edinburgh"

-- Your definition should use a list comprehension and library functions toUpper and toLower that change the case of a character.


-- • Problem 12. ----------------------------------------------------------

-- Write another function, this time using recursion, capitaliseRec which, given a word, capitalises it. That means that the first character should be made uppercase and any other letters should be made lowercase. For example,

-- capitaliseRec "edINBurgH" == "Edinburgh"

-- You may need to write a helper function; of the helper function and the main function only one needs to be recursive. You must write a type signature for each function you write.


-- • Problem 13. ----------------------------------------------------------

-- Using the function capitalise from the previous problem (which you should explicitly copy into the workspace for this problem), write a function

-- title :: [String] -> [String]

-- which, given a list of words, capitalises them as a title should be capitalised. The proper capitalisation of a title (for our purposes) is as follows: The first word should be capitalised. Any other word should be capitalised if it is at least four letters long. For example,

-- title ["tHe", "sOunD", "ANd", "thE", "FuRY"] == ["The", "Sound", "and", "the", "Fury"]

-- Your function should use a list comprehension, and not recursion. Besides the capitalise function, you will probably need some other auxiliary functions. You must specify their type signature. You may use library functions that change the case of a character and the function length.

-- • Problem 14. ----------------------------------------------------------

-- Now write the equivalent function using a recursion.
-- Using the function capitalise from the previous problem (which you should explicitly copy into the workspace for this problem), write a function

-- titleRec :: [String] -> [String]

-- which, given a list of words, capitalises them as a title should be capitalised. The proper capitalisation of a title (for our purposes) is as follows: The first word should be capitalised. Any other word should be capitalised if it is at least four letters long. For example,

-- titleRec ["tHe", "sOunD", "ANd", "thE", "FuRY"] == ["The", "Sound", "and", "the", "Fury"]

-- You may use capitaliseRec and any of its auxiliary functions (but need to explicitly copy them into the workspace for this problem). All functions should have a type signature.

-- • Problem 15. ----------------------------------------------------------

-- Dame Curious is a crossword enthusiast. She has a long list of words that might appear in a crossword puzzle, but she has trouble finding the ones that fit a slot. Write a function

-- crosswordFind :: Char -> Int -> Int -> [String] -> [String]

-- to help her. The expression

-- crosswordFind letter inPosition len words

-- should return all the items from words which (a) are of the given length and (b) have letter in the position inPosition. For example, if Curious is looking for seven-letter words that have 'k' in position 1, she can evaluate the expression:

-- crosswordFind 'k' 1 7 ["funky", "fabulous", "kite", "icky", "ukelele"] 

-- which returns ["ukelele"]. (Remember that we start counting with 0, so position 1 is the second position of a string.)

-- Your definition should use a list comprehension. You may also use a library function which returns the nth element of a list, for argument n, and the function length.

-- • Problem 16. ----------------------------------------------------------

-- Write a recursive function crosswordFindRec to the same specification (you can use the same library functions).

-- Dame Curious is a crossword enthusiast. She has a long list of words that might appear in a crossword puzzle, but she has trouble finding the ones that fit a slot. Write a function

-- crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]

-- to help her. The expression

-- crosswordFindRec letter inPosition len words

-- should return all the items from words which (a) are of the given length and (b) have letter in the position inPosition. For example, if Curious is looking for seven-letter words that have 'k' in position 1, she can evaluate the expression:

-- crosswordFindRec 'k' 1 7 ["funky", "fabulous", "kite", "icky", "ukelele"] 

-- which returns ["ukelele"]. (Remember that we start counting with 0, so position 1 is the second position of a string.)

-- Use recursion. You may also use a library function which returns the nth element of a list, for argument n, and the function length.

-- • Problem 17. ----------------------------------------------------------

-- Write a function search :: String -> Char -> [Int] that returns the positions of all occurrences of the second argument in the first. For example

-- search "Bookshop" 'o' == [1,2,6]
-- search "senselessness's" 's' == [0,3,7,8,11,12,14]

-- You should use a list comprehension, not recursion. You may use the function zip :: [a] -> [b] -> [(a,b)], the function length :: [a] -> Int, and the term forms [m..n] and [m..].

-- • Problem 18. ----------------------------------------------------------

-- Write the recursive function searchRec. You may like to use an auxiliary function in your definition, but you shouldn't use any library functions.
-- Your function should return the positions of all occurrences of the second argument in the first. For example

-- searchRec "Bookshop" 'o' == [1,2,6]
-- searchRec "senselessness's" 's' == [0,3,7,8,11,12,14]


-- • Problem 19. ----------------------------------------------------------

-- Write a function contains that takes two strings and returns True if the first contains the second as a substring. You can use the library function isPrefixOf, which returns True if the second string begins with the first string, and/or any other library function. For example,

-- contains "United Kingdom" "King" == True
-- contains "Appleton" "peon" == False
-- contains "" "" == True

-- Your definition should use a list comprehension, not recursion. A hint: you can use the library function drop to create a list of all possible suffixes (``last parts'') of a string. 


-- • Problem 20. ----------------------------------------------------------

-- Now write a recursive function containsRec that takes two strings and returns True if the first contains the second as a substring. You can use the library function isPrefixOf, which returns True if the second string begins with the first string, and/or any other library function. For example,

-- containsRec "United Kingdom" "King" == True
-- containsRec "Appleton" "peon" == False
-- containsRec "" "" == True

-- Pay attention to the last case of the above three.

