import Data.List



data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)


-------------PART I

isPrefix :: String -> String -> Bool
isPrefix pre str
  = pre == take (length pre) str

removePrefix :: String -> String -> String
removePrefix pre
  = drop (length pre)

suffixes :: [a] -> [[a]]
suffixes ss
  = [drop d ss | d <- [0..length ss - 1]]

isSubstring :: String -> String -> Bool
isSubstring sub str
  = any (isPrefix sub) (suffixes str)

findSubstrings :: String -> String -> [Int]
findSubstrings sub str
  | isSubstring sub str = [index | (bVal, index) <- indexZip, bVal]
  | otherwise           = []
  where
    findSubs            = map (isPrefix sub) (suffixes str)
    indexZip            = zip findSubs [0..]


-------------------PART II
getIndices :: SuffixTree -> [Int]
getIndices (Leaf i) 
  = [i]
getIndices (Node ts)
  = sort $ concatMap (getIndices . snd) ts

























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