module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp item 
  = (fromJust . (lookup item))
    --head [val | (id, val) <- dict, id == item]
    --filter (\(id, val) -> id == item) dict

-- 3 marks
vars :: Formula -> [Id]
vars 
  = nub . sort . vars'
  where
    vars' :: Formula -> [Id]
    vars' (Var id)
      = [id]
    vars' (Not form) 
      = vars' form
    vars' (And form form')
      = (vars' form) ++ (vars' form')
    vars' (Or form form')
      = (vars' form) ++ (vars' form')  
    

-- 1 mark
idMap :: Formula -> IdMap
idMap form
  = zip (vars form) [1..] 

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Var id)
  = Var id
toNNF (Not (Var id))
  = Not (Var id)
toNNF (Not (Not form))
  = toNNF form
toNNF (Not (Or form form'))
  = And (toNNF (Not form)) (toNNF (Not form'))
toNNF (Not (And form form'))
  = Or (toNNF (Not form)) (toNNF (Not form'))
toNNF (Or form form')
  = Or (toNNF form) (toNNF form')
toNNF (And form form')
  = And (toNNF form) (toNNF form')

-- 3 marks
toCNF :: Formula -> CNF
toCNF 
  = toCNF' . toNNF 
  where
    toCNF' :: Formula -> CNF
    toCNF' (Var id)
      = Var id
    toCNF' (Not (Var id))
      = Not (Var id)
    toCNF' (And form form')
      = And (toCNF' form) (toCNF' form')
    toCNF' (Or form form')
      = distribute (toCNF' form) (toCNF' form')
  

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    dict = idMap f

    flatten' :: CNF -> CNFRep
    flatten' (And form form')
      = (flattenOr form) : (flatten' form')
    flatten' form
      = [flattenOr form]

    flattenOr :: CNF -> [Int]
    flattenOr (Var id)
      = [(lookUp id dict)]
    flattenOr (Not (Var id))
      = [-(lookUp id dict)]
    flattenOr (Or form form')
      = (flattenOr form) ++ (flattenOr form')



--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cs
  = propUnits' cs (uFinder cs) []
  where
    uFinder :: CNFRep -> [Int]
    uFinder cnfr
      = concat $ filter ((1 ==) . length) cnfr

    propUnits' :: CNFRep -> [Int] -> [Int]-> (CNFRep, [Int])
    propUnits' cs [] fps
      = (cs, fps)
    propUnits' cs (p : ps) fps
      = propUnits' csR2 (uFinder csR2) (p : fps)
      where
        csR1 = filter (not . elem p) cs
        csR2 = map (filter (-p /=)) csR1 


-- 4 marks
dp :: CNFRep -> [[Int]]
dp cs
  | cs' == []   = [ts]
  | elem [] cs' = []
  | otherwise   = map (ts ++ ) ((dp ([tryLit] : cs')) ++ (dp ([-tryLit] : cs')))

  where
   (cs', ts) = propUnits cs 
   tryLit    = (head . head) cs'

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined
--compute everytinhng
-- find missing varaibles, (missing ones dont affect outcome)
-- add missing variables to [Int]
-- lookup the variable id and assign Bool value
-- sort the list of lists
-- Return

