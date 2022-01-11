import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame
    = ((<=1).length.nub)

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x xys
    = filter ((/= x).fst ) xys

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attName hdr row
    = head(filter (flip elem (lookUp attName hdr)) row)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt attName hdr row
    = filter (/=(lookUpAtt attName hdr row)) row

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (key, value) [] = [(key, [value])]
addToMapping (key, value) ((k, v) : t) 
    | key == k = ((k, (value:v)) : t)
    | otherwise = (k, v) : (addToMapping (key, value) t)

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (x, []) _ = []
buildFrequencyTable (x, (h : t)) (hdr, rows)
    = (h, getFreq h rows) : buildFrequencyTable (x, t) (hdr, rows)
    where
        getFreq h [] = 0
        getFreq h (row : t)
            | elem h row = 1 + getFreq h t
            | otherwise = getFreq h t


--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null = 0
nodes (Leaf x) = 1
nodes (Node name decTrees) = 1 + (sum (map nodes (map snd decTrees)))

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _ = ""
evalTree (Leaf x) _ _ = x
evalTree (Node name trees) header row
    = evalTree (lookUp (lookUpAtt name header row) trees) header row

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (hdr, rows) (attName, attVals)
  = zip attVals datasets
  where
    newRows = [[removeAtt attName hdr row' | row' <- rows, lookUpAtt attName hdr row' == attVal] | attVal <- attVals]
    datasets = zip (repeat (remove attName hdr)) newRows

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_, []) _ _
  = Null
buildTree (hdr, rows) clasifAtt attSelector
  | allSame (map (lookUpAtt (fst clasifAtt) hdr) rows)
    = (Leaf (lookUpAtt (fst clasifAtt) hdr (head rows)))
  | otherwise
    = (Node rootAttName (zip (rootAttVals) trees))
  where
    (rootAttName, rootAttVals) = attSelector (hdr, rows) clasifAtt
    trees = [buildTree dataSet clasifAtt attSelector | 
             (_, dataSet) <- partitionData (hdr, rows) (rootAttName, rootAttVals)]

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

prob :: DataSet -> Attribute -> AttValue -> Double
prob dataSet att attVal
  | not (elem att (fst dataSet)) || length (snd dataSet) == 0
    = 0.0
  | otherwise 
    = (fromIntegral (lookUp attVal (buildFrequencyTable att dataSet))) / 
      (fromIntegral (length (snd dataSet)))

entropy :: DataSet -> Attribute -> Double
entropy dataSet att
  = - sum (map (xlogx) (map (prob dataSet att) (snd att)))

gain :: DataSet -> Attribute -> Attribute -> Double
gain dataSet partAtt clasifAtt
  = (entropy dataSet clasifAtt) - longSum
  where
    pairs= partitionData dataSet partAtt
    longSum =
      sum [(prob dataSet partAtt attVal) * (entropy dataSet' clasifAtt) |
           (attVal, dataSet') <- pairs]

bestGainAtt :: AttSelector
bestGainAtt dataSet@(hdr, rows) clasiffAtt 
  = lookUp maxGain (zip gains hdr')
  where
    hdr' = filter (/= clasiffAtt) hdr
    gains = map (flip (gain dataSet) clasiffAtt) hdr'
    maxGain = maximum gains

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]