module SuffixTrees where 

import Data.List (findIndices, any, drop, maximumBy)
import Data.Ord (comparing)

data SuffixTree = Leaf Int 
                | Node [(String, SuffixTree)]
                deriving (Eq, Ord, Show)

isLeaf :: SuffixTree -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

isPrefix :: String -> String -> Bool
isPrefix xs ys = length xs <= length ys && all (uncurry (==)) zs
    where zs = zip xs ys

removePrefix :: String -> String -> String
removePrefix x = drop (length x)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xs = xs : suffixes (tail xs)

isSubstring :: String -> String -> Bool
isSubstring x y = any (isPrefix x) (suffixes y)

findSubstrings :: String -> String -> [Int]
findSubstrings x y = findIndices (isPrefix x) (suffixes y)

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node xs) = concatMap (getIndices . snd) xs

-- Only one scan over the string
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition xs ys = partition' xs ys []

partition' [] ys zs = (reverse zs, [], ys)
partition' xs [] zs = (zs, xs, [])
partition' l@(x:xs) r@(y:ys) zs
    | x == y    = partition' xs ys (x:zs)
    | otherwise = (reverse zs, l, r)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf i) = [i]
findSubstrings' s (Node xs) = do
    (x, t) <- xs
    case partition x s of
         ("", _, _)  -> []
         (_, _, "")  -> getIndices t
         (_, _, s')  -> findSubstrings' s' t

buildTree :: String -> SuffixTree
buildTree s = foldl (flip insert) (Node []) xs
    where xs = zip (suffixes s) [0..length s-1]

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert _ (Leaf _) = error "Malformed suffix tree, only Leaf encountered"
insert ("", _) t = error "Empty search string"
insert x t = Node $ insert' x t

insert' :: (String, Int) -> SuffixTree -> [(String, SuffixTree)]
insert' (x,i) (Node []) = [(x, Leaf i)]
insert' (x,i) (Node (n@(y,t):ys))
    | y == p && b         = (y,Node $ insert' (x',i) t):ys
    | y /= p && b         = split t i:ys
    | otherwise           = n:insert' (x,i) (Node ys)
    where pxy@(p, x', y') = partition x y
          split :: SuffixTree -> Int -> (String, SuffixTree)
          split t i = (p, Node[(y',t), (x',Leaf i)])
          b = p /= ""

longestRepeatedSubstring :: String -> String
longestRepeatedSubstring s 
    | null xs   = ""
    | otherwise = maximumBy (comparing length) xs
    where t = buildTree s
          xs = [x | (x,i) <- repeated t "", i > 1]

repeated :: SuffixTree -> String -> [(String, Int)]
repeated (Leaf i) s = []
repeated (Node xs) s =
    let collect (x,t) ys
            | isLeaf t && not (null s) = updateLast
            | otherwise                = ys ++ recurse
            where recurse = repeated t (s ++ x)
                  updateLast | null ys   = [(s,1)]
                             | otherwise = (s, snd (head ys) + 1):tail ys
    in foldr collect [] xs

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
