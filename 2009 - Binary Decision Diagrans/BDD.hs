import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
-- because of the precondition no need to check for value not appearing
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp val dict
  = head [y | (x, y) <- dict, x == val]

checkSat :: BDD -> Env -> Bool
checkSat (start, tree) env
  = checkSatHelper start 
  where 
    checkSatHelper 0      = False
    checkSatHelper 1      = True
    checkSatHelper i
      |currentVal == True = checkSatHelper tBranch 
      |otherwise          = checkSatHelper fBranch
        where
        (currentVar, fBranch, tBranch) = lookUp i tree
        currentVal = lookUp currentVar env

sat :: BDD -> [[(Index, Bool)]]
sat (start, tree)

  = satHelper start
  where
    satHelper :: Index -> [[(Index, Bool)]]
    satHelper 0      = []
    satHelper 1      = [[]]
    satHelper i      = map satHelper [fBranch, rBranch]
      where
        (currentVar, fBranch, tBranch) = lookUp i tree
        
varFinder :: BDD -> [Index]
varFinder (start, tree)
  = varHelper start []
  where
    varHelper :: NodeId -> [Index] -> [Index]
    varHelper i is
      |fBranch == 0 || fBranch == 1 = currentVar : is
      |otherwise                    = currentVar : varHelper fBranch is
        where
          (currentVar, fBranch, _) = lookUp i tree
          
  
  
  
layerCounter :: BDD -> Int 
layerCounter (start, tree) 
  = layerCounterHelper start 0
  where
    layerCounterHelper i n
      |fBranch == 0 || fBranch == 1 = n + 1
      |otherwise                    = layerCounterHelper fBranch (n + 1)
        where
          (currentVar, fBranch, tBranch) = lookUp i tree
------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify exp@(Not bexp)
  |bexp == Prim False      = Prim True
  |bexp == Prim True       = Prim False
  |otherwise               = exp
simplify exp@(Or bexp bexp')
  |bexp == Prim True ||  bexp' == Prim True    = Prim True
  |bexp == Prim False && bexp' == Prim False   = Prim False
  |otherwise                                   = exp
simplify exp@(And bexp bexp')
  |bexp == Prim False && bexp' == Prim False                    = Prim False
  |bexp == Prim True && bexp' == Prim True                      = Prim True
  |otherwise                                                    = exp



restrict :: BExp -> Index -> Bool -> BExp
restrict exp n bval
  = restrictHelper exp
  where
    restrictHelper (Not bexp) 
      = simplify $ Not (restrictHelper bexp)
    restrictHelper (Or bexp bexp')
      = simplify $ Or (restrictHelper bexp) (restrictHelper bexp')
    restrictHelper (And bexp bexp')
      = simplify $ And (restrictHelper bexp) (restrictHelper bexp')
    restrictHelper (IdRef i) 
      |n == i    = Prim bval
      |otherwise = IdRef i
    restrictHelper (Prim bool)
      = Prim bool

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
-- _ []   = (0, [])
--buildBDD _   = (1, [])
buildBDD exp is = (2, buildBDD' exp 2 is)

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> [BDDNode]
--buildBDD' (Prim False) id is' = (id, ())
--buildBDD' (Prim True) id is  = 
buildBDD' exp id is
  = (id, (i,li, ri)) : ((buildBDD' lexp li is') ++ (buildBDD' rexp ri is'))
  where
    i   = head is
    is' = tail is
    li  = id * 2
    ri  = id * 2 + 1
    lexp = restrict exp i False
    rexp = restrict exp i True

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])