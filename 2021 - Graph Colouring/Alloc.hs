module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count item 
  = length . filter (== item)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees graph
  = [(x, (count x dests + count x starts)) | x <- fst graph]
  where
    edges = snd graph
    dests  = map snd edges 
    starts = map fst edges

neighbours :: Eq a => a -> Graph a -> [a]
neighbours i g
  = nLookUp i edges ++ nLookUp i edges'
  where
    edges  = snd g
    edges' = switch edges

    nLookUp :: Eq a => a -> [(a, a)] -> [a]
    nLookUp i dict
      = [y | (x, y) <- dict, x == i]


removeNode :: Eq a => a -> Graph a -> Graph a
removeNode i g
  = (delete i nodes, filter (\(n, n') -> n /= i && n' /= i) edges)
  where
    nodes = fst g
    edges = snd g
------------------------------------------------------
--
-- Part II
--
switch :: [(a, b)] -> [(b, a)]
switch
  = map (\(x, y) -> (y, x))





colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph maxC g          -- Colours range form 1 to maxC
  | null colours' = (n, 0) : cMap
  | otherwise     = (n, head colours' ) : cMap
  
  where
    ((n, _) : ds) = (switch . sort . switch . degrees) g   --sorts in ascending degree 
                                                           -- -> (n, d)
    colours       = [1..maxC]
    cMap          = colourGraph maxC g'
    usedColours   = map snd $ filter (\(x, y) -> flip elem ns x) cMap
                  -- same as =>[lookUp neighbour cMap | neighbour <- neighbours n g]
    colours'      = sort $ colours \\ usedColours
    ns            = neighbours n g
    g'            = removeNode n g



------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap [] 
  = [("return", "return")]
buildIdMap ((v, col) : cs)
  | col == 0  = (v, v) : buildIdMap cs
  | otherwise = (v, "R" ++ show col) : buildIdMap cs

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments [] _ 
  = []
buildArgAssignments (v : vs) im
  | isRegNeeded v = Assign sId (Var v) : buildArgAssignments vs im
  | otherwise     = buildArgAssignments vs im
  where
    sId = lookUp v im
    isRegNeeded :: Id -> Bool
    isRegNeeded var
      = head sId == 'R'

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const n) _
  = Const n
renameExp (Var v) im
  = Var (lookUp v im)
renameExp (Apply op e e') im
  = Apply op (renameExp e im) (renameExp e' im)

renameStat :: Statement -> IdMap -> Statement
renameStat (Assign id e) im
  = Assign sArg renameE
  where
    renameE = renameExp e im
    sArg    =lookUp id im
renameStat (If e b b') im
  = If (renameExp e im) (renameBlock b im) (renameBlock b' im)
renameStat (While e b) im
  = While (renameExp e im) (renameBlock b im) 

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _
  = []
renameBlock (s@(Assign id e) : ss) im
  | Var sArg == renameE = renameBlock ss im
  | otherwise           = renameStat s im : renameBlock ss im
  where
    renameE = renameExp e im
    sArg    = lookUp id im
renameBlock (s : ss) im
  = renameStat s im : renameBlock ss im

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG lv
  =   (nubcat lv, nub $ sort (map order es) )
  where
    nubcat        = (nub . concat)
    lv'           = filter ((>= 2) . length) lv
    es            = concatMap edges (concatMap permutations lv') --finds all edges
    order (n, n') = (min n n', max n n') -- uses the sorting technique from sortGraph

edges :: [Id] -> [Edge Id]
edges []
  = []
edges [n]
  = []
edges (n : n' : ns) 
  = (n, n') : edges (n : ns)    
-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined