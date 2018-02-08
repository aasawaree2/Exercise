-- Based on Inf 1A Tutorial 2: http://camlback.cs.ucla.edu/phpredux.php
--module Exercise3 where

import Data.Char
import Data.List
import Test.QuickCheck

-- * Problem 1.

-- When we talk about cryptography these days, we usually refer to
-- the encryption of digital messages, but encryption actually
-- predates the computer by quite a long period. One of the best
-- examples of early cryptography is the Caesar cipher, named after
-- Julius Caesar because he is believed to have used it, even if he
-- didn't actually invent it. The idea is simple: take the message
-- you want to encrypt and shift all letters by a certain amount
-- between 0 and 26 (called offset). For example: encrypting the
-- sentence ``THIS IS A BIG SECRET'' with shifts of 5, would result
-- in ``YMNX NX F GNL XJHWJY''.

-- In this exercise you will be implementing a variant of the Caesar
-- cipher. You can use the list of library functions linked from the
-- course webpage, as well as those in the Appendix of the tutorial
-- sheet.


-- A character-by-character cipher such as a Caesar cipher can be
-- represented by a key, a list of pairs. Each pair in the list
-- indicates how one letter should be encoded. For example, a cipher
-- for the letters A--E could be given by the list

-- [('A', 'C'), ('B', 'D'), ('C', 'E'), ('D', 'A'), ('E', 'B')] .

-- Although it's possible to choose any letter as the ciphertext for
-- any other letter, this tutorial deals mainly with the type of
-- cipher where we encipher each letter by shifting it the same
-- number of spots around a circle, for the whole English alphabet.

-- We can rotate a list by taking some items off the front of it and
-- putting them on the end. For example:


-- Main> rotate 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- "DEFGHIJKLMNOPQRSTUVWXYZABC"

-- Your first task is to write a function:
--    rotate :: Int -> [Char] -> [Char]
-- When given a number n greater than 0 and smaller than the length
-- of the input list, your function should rotate the list by n
-- items. Your function should return an error if the number n is
-- negative or too large.
rotate :: Int -> [Char] -> [Char]
rotate n str 
    | n >=0 && n <=length str = drop n str ++ take n str 
    | otherwise = "Error - number n is either negative or too large"
                                       
--  TESTING
--  Use: quickCheck prop_rotate
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate m' (rotate m str) == str
                        where l = length str
                              m  = if l == 0 then 0 else k `mod` l
                              m' = if m == 0 then 0 else l-m
--  to avoid errors with 'rotate', m should be between 0 and l; to
--  get m from a random number k we use k `mod` l (but then l can't
--  be 0, since you can't divide by 0)

-- * Problem 2.

-- Use your function rotate from the previous question to write a function
--     makeKey :: Int -> [(Char, Char)]
-- that returns the cipher key with the given offset.

-- Example:
-- Main> makeKey 5
-- [('A','F'),('B','G'),('C','H'),('D','I'),('E','J'),('F','K'),
-- ('G','L'),('H','M'),('I','N'),('J','O'),('K','P'),('L','Q'),
-- ('M','R'),('N','S'),('O','T'),('P','U'),('Q','V'),('R','W'),
-- ('S','X'),('T','Y'),('U','Z'),('V','A'),('W','B'),('X','C'),
-- ('Y','D'),('Z','E')]

-- The cipher key should show how to encrypt all of the uppercase
-- English letters, and there should be no duplicates: each letter
-- should appear just once amongst the pairs' first components (and
-- just once amongst the second components).

alphabet = ['A'..'Z']

makeKey k = zip alphabet (rotate k alphabet)
--makeKey k = zip alphabet $ rotate k alphabet

--  TESTING
--  Use: quickCheck prop_makeKey

prop_makeKey :: Int -> Bool
prop_makeKey k = (row1==alphabet) && (row2 == rotate m alphabet)
    where
      m = k `mod` 26
      key = makeKey m
      (row1,row2) = unzip key

-- * Problem 3.
-- Write a function
--    lookUpRec :: Char -> [(Char, Char)] -> Char
-- that finds a pair by its first component and returns that pair's
-- second component. When you try to look up a character that does
-- not occur in the cipher key, your function should leave it
-- unchanged. Examples:

-- Main> lookUpRec 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')]
-- 'G'
-- Main> lookUpRec '9' [('A', 'X'), ('B', 'Y'), ('C', 'Z')]
-- '9'

-- Your definition should use recursion, not list comprehension.

lookUpRec c [] = c
lookUpRec c ((key,val):input)
    | key == c = val
    | otherwise = lookUpRec c input

-- * Problem 4.

-- This time use a list comprehension to write a function

--   lookUp :: Char -> [(Char, Char)] -> Char

-- that finds a pair by its first component and returns that pair's
-- second component. When you try to look up a character that does
-- not occur in the cipher key, your function should leave it
-- unchanged. Examples:

-- Main> lookUp 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')]
-- 'G'
-- Main> lookUp '9' [('A', 'X'), ('B', 'Y'), ('C', 'Z')]
-- '9'

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c input 
    | null condition = c
    | otherwise = head condition
      where condition = [ val | (key,val) <- input , key == c]


--  TESTING
-- Use: quickCheck prop_lookUp
prop_lookUp c k = (lookUpRec c key == lookUp c key)
    where key = makeKey (k `mod` 26)
                         

-- * Problem 5.

-- Write a function
--   encipher :: Int -> Char -> Char
-- that encrypts the given single character using the key with the
-- given offset. For example:

-- Main> encipher 5 'C'
-- 'H'
-- Main> encipher 7 'Q'
-- 'X'

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)

--  TESTING
-- Use: quickCheck prop_encipher
prop_encipher k c
    | not (isUpper c) = encipher m c == c
    | otherwise       = encipher m c == c'
    where
      m  = k `mod` 26
      c' = chr ( ((ord c - ord 'A' + m) `mod` 26) + ord 'A')



-- * Problem 6.

-- Text encrypted by a cipher is conventionally written in uppercase
-- and without punctuation. Write a function
--    normalize :: String -> String
-- that converts a string to uppercase, removing all characters other
-- than letters and digits (remove spaces too). Example:

-- Main> normalize "July 4th!"
-- "JULY4TH"

normalize :: String -> String
normalize [] = []
normalize (ch:txt)
    | isAlpha ch = toUpper ch : normalize txt
    | isDigit ch = ch : normalize txt
    | otherwise = normalize txt


-- * Problem 7.

-- Write a function 

-- encipherStr :: Int -> String -> String

-- that normalizes a string and encrypts it, using your functions
-- normalize and encipher. Example:

-- Main> encipherStr 5 "July 4th!"
-- "OZQD4YM"

-- You will need to copy your solutions to the previous questions.

encipherStr :: Int -> String -> String
encipherStr k txt = [ encipher k x | x <- normalize txt ]


-- * Problem 8.

-- The Caesar cipher is one of the easiest forms of encryption to
-- break. Unlike most encryption schemes commonly in use today, it
-- is susceptible to a simple brute-force attack of trying all the
-- possible keys in succession. The Caesar cipher is a symmetric key
-- cipher: the key has enough information within it to use it for
-- encryption as well as decryption.


-- Decrypting an encoded message is easiest if we transform the key
-- first. Write a function

-- reverseKey :: [(Char, Char)] -> [(Char, Char)]

-- to reverse a key. This function should swap each pair in the given
-- list. For example:

-- Main> reverseKey [('A', 'G'), ('B', 'H') , ('C', 'I')]
-- [('G', 'A'), ('H', 'B') , ('I', 'C')]

-- You should use list comprehension, not recursion.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [ (y,x) | (x,y) <- key ]


-- * Problem 9.

-- Using recursion instead of a list comprehension, write a function

--    reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]

-- to reverse a key. This function should swap each pair in the
-- given list. For example:

-- Main> reverseKeyRec [('A', 'G'), ('B', 'H') , ('C', 'I')]
-- [('G', 'A'), ('H', 'B') , ('I', 'C')]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((k,v):key) = (v,k):reverseKeyRec key


--  TESTING
-- Use: quickCheck prop_reverseKey
prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = (reverseKey key == reverseKeyRec key)


-- * Problem 10.

-- Write the functions

-- decipher :: Int -> Char -> Char
-- decipherStr :: Int -> String -> String

-- that decipher a character and a string, respectively, by using
-- the key with the given offset. Your decipherStr function should
-- leave digits and spaces unchanged, but remove lowercase letters
-- and other characters. For example:

-- Main> decipherStr 5 "OZQD4YM"
-- "JULY4TH"


decipher :: Int -> Char -> Char
decipher k c = lookUp c ( reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k txt
    = map (decipher k) [c | c <- txt, isUpper c || isDigit c || isSpace c]

-- * Problem 11.


-- One kind of brute-force attack on an encrypted string is to
-- decrypt it using each possible key and then search for common
-- English letter sequences in the resulting text. If such sequences
-- are discovered then the key is a candidate for the actual key
-- used to encrypt the plaintext. For example, the words ``the'' and
-- ``and'' occur very frequently in English text: in the Adventures
-- of Sherlock Holmes, ``the'' and ``and'' account for about one in
-- every 12 words, and there is no sequence of more than 150 words
-- without either ``the'' or ``and''.

-- The conclusion to draw is that if we try a key on a sufficiently
-- long sequence of text and the result does not contain any
-- occurrences of ``the'' or ``and'' then the key can be discarded
-- as a candidate.

-- Write a function
--    contains :: String -> String -> Bool
-- that returns True if the first string contains the second as a
-- substring.

-- Main> contains "Example" "amp"
-- True
-- Main> contains "Example" "xml"
-- False

contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains txt str = prefix str txt || contains (tail txt) str

-- (prefix str txt) returns True if and only if str is a prefix of txt
-- This may be useful in defining contains.
-- Examples: (prefix "un" "unlikely") ~> True
--           (prefix "ke" "unlikely") ~> False
prefix :: String -> String -> Bool    
prefix [] _ = True    
prefix _ [] = False            
prefix (s:str) (t:txt)
    | s == t = prefix str txt
    | otherwise = False

--  TESTING
-- Use: quickCheck prop_contains
-- No fair using isInfixOf in your solution.
prop_contains :: String -> String -> Bool
prop_contains txt str = (contains txt str == isInfixOf str txt)

-- * Problem 12.

-- Write a function 

-- candidates :: String -> [(Int, String)]

-- that decrypts the input string with each of the 26 possible keys
-- and, when the decrypted text contains ``THE'' or ``AND'',
-- includes the decryption key and the text in the output list.

-- Main> candidates "DGGADBCOOCZYMJHZYVMTOJOCZHVS"
-- [(5,"YBBVYWXJJXUTHECUTQHOJEJXUCQN"),
-- (14,"PSSMPNOAAOLKYVTLKHYFAVAOLTHE"),
-- (21,"ILLFIGHTTHEDROMEDARYTOTHEMAX")]

candidates :: String -> [(Int, String)]
candidates txt = [ (i,decipherStr i txt) | i <- [1..26], candidate(decipherStr i txt)]
    where candidate str = str `contains` "AND" || str `contains` "THE"