data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix pre str
  = pre == (take $ length pre) str

removePrefix :: String -> String -> String
removePrefix pre str
--Pre: s is a prefix of s'
  = (drop $ length pre) str

suffixes :: [a] -> [[a]]
suffixes []           = []
suffixes str@(s : ss) = str : suffixes ss 

isSubstring :: String -> String -> Bool
isSubstring sub str
  = (or . map (isPrefix sub)) (suffixes str)    --any = or . map

findSubstrings :: String -> String -> [Int]
findSubstrings sub str
  |isSubstring sub str   = [index | (bVal, index) <- indexZip, bVal]
  |otherwise             = []
  where
    findSubs = map (isPrefix sub) (suffixes str)
    indexZip = zip findSubs [0..]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Node [])       = []
getIndices (Leaf n)        = [n]
getIndices (Node (n : ns)) = getIndices (snd n) ++ getIndices (Node ns)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition str str'
  = (comPre, drop comLen str, drop comLen str')
  where
    comPre = partition' str str'
    comLen = length comPre
    partition' :: Eq a => [a] -> [a] -> [a]
    partition' [] str'  = str'
    partition' str []   = []  
    partition' (s : ss) (s' : ss')
      |s == s'          = s : partition' ss ss'
      |otherwise        = []



findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' sub (Node [])   = []
findSubstrings' sub (Node ((str, Leaf i) : ns))
  |isPrefix sub str || (null str && null sub) = [i] ++ findSubstrings' sub (Node ns)
  |otherwise                    = findSubstrings' sub (Node ns) 
findSubstrings' sub (Node (n : ns))
  |not $ null comPre            = findSubstrings' remSub (snd n) ++ findSubstrings' sub (Node ns)
  |otherwise                    = findSubstrings' sub (Node ns)
  where
    (comPre,remSub,_)  = partition sub (fst n)

------------------------------------------------------

-- insert :: (String, Int) -> SuffixTree -> SuffixTree
-- insert p@(suf, i) (Node ((str, subTree) : ns))
--   |null comPre        = insert p (Node ns)
--   |not $ null comPre  = case comPre of  
--                              str -> (remSuf, Leaf i) : subTree

--   where
--     (comPre,remSuf,remStr)  = partition sub str
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (tree @ (Node xs))
  | or (map commonPrefix xs) = Node (map inspect xs)
  | otherwise                = Node ((s, Leaf n) : xs)
  where
    inspect :: (String, SuffixTree) -> (String, SuffixTree)
    inspect (a, t)
      | p == "" = (a, t)
      | p == a  = (a, insert (sp, n) t)
      | p /= a  = (p, Node [(sp, Leaf n), (ap, t)])
      where
        (p, sp, ap) = partition s a
    
    commonPrefix :: (String, SuffixTree) -> Bool
    commonPrefix (xs, _)
      =  x /= ""
      where
        (x, _, _) = partition s xs


insert' :: (String, Int) -> SuffixTree -> SuffixTree
insert' (s, n) (Node []) = Node [(s, Leaf n)]
insert' (s, n) (Node ((a, t) : ats))
  | null pre  = merge (Node [(a, t)]) (insert' (s, n) (Node ats))
  | pre == a  = Node ((a, insert' (remS, n) t) : ats)
  | otherwise = merge (Node [(pre, Node [(remS, Leaf n), (remA, t)])]) (Node ats)
  where
    (pre, remS, remA) = partition s a

    merge :: SuffixTree -> SuffixTree ->SuffixTree
    merge (Node xs) (Node ys)
      = Node (xs ++ ys)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

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


