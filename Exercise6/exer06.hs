-- My answers to Field's http://wp.doc.ic.ac.uk/ajf/suffix-trees/
-- You may have better ones.

import qualified Data.List as L
-- I want to use Data.List.partition and Data.List.SortOn in the following.

type Edge = (String, SuffixTree)
data SuffixTree = Leaf Int | Node [Edge]
                  deriving (Eq, Show, Ord)
------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix ""     _      = True
isPrefix _      ""     = False
isPrefix (c:cs) (d:ds) = (c==d) && isPrefix cs ds

removePrefix :: String -> String -> String
--Precondition: s is a prefix of s'
removePrefix s s' = drop (length s) s'

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes cs = cs:suffixes (tail cs)

isSubstring :: String -> String -> Bool
isSubstring s s' = or (map (isPrefix s) (suffixes s'))

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
    = [n-(length w) | w <- suffixes s', isPrefix s w]
    where n = length s'

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node es) = concatMap (getIndices . snd) es


partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition []     s'     = ([],[],s')
partition s      []     = ([],s,[])
partition (c:cs) (e:es)
    | c==e      = (c:cs',ds',es')
    | otherwise = ([],c:cs,e:es)
    where (cs',ds',es') = partition cs es


findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf n) = [n]
findSubstrings' _  (Leaf _) = []
findSubstrings' s  (Node es) = concatMap (explore s) es

explore s (a,t)
    = case (partition s a) of
        (_,[],_) ->  getIndices t
        (_,s',[]) -> findSubstrings' s' t
        (_,_,_)   -> []
                     
------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s,n) (Node es)
    = case (L.partition startsSame es) of
        ([] ,es') -> Node ((s,Leaf n):es')
        ([e],es') -> Node ((revise s n e):es')
      where startsSame (a,_) = take 1 s == take 1 a

revise :: String -> Int -> Edge -> Edge
-- Note s_p is Field's ``s - p'' and a_p is Field's ``a - p''
revise s n (a,t)
    = case (partition s a) of
        ([],_,_)    -> error "should not be possible"
        (_,s_p,[])  -> (a,insert (s_p,n) t)
        (p,s_p,a_p) -> (p,Node [(s_p,Leaf n),(a_p,t)])
                     
-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

-- See: https://www.cise.ufl.edu/~sahni/dsaaj/enrich/c16/suffix.htm

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring t = last (L.sortOn length (repeats t))
           
-- (repeats t) = a list of all substrings that repeat in t's string
repeats :: SuffixTree -> [String]
repeats (Leaf _) = []
repeats (Node [e]) = [""]
repeats (Node es)  = concatMap repeatsE es

repeatsE :: Edge -> [String]
repeatsE (s,t)     = case (lst) of
                       (_:_:_) -> map (s++) lst
                       _       -> [""]
    where lst = repeats t
    
------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


--- Testing tools
--
-- (normalize t) = t put in to a standard order
normalize :: SuffixTree -> SuffixTree
normalize t@(Leaf _) = t
normalize (Node es)
    = Node (L.sort (map (\(s,t)->(s,normalize t)) es))

-- Test if t and t' are the same tree when normalized.
eqTree t t' = (normalize t)==(normalize t')



