  import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp src dict
  = head [y | (x, y) <- dict, x == src]

states :: LTS -> [State]
states lts
  = nub $ concatMap (\(x, y) -> [x, y]) (map fst lts)

transitions :: State -> LTS -> [Transition]
transitions stat lts
  = [(x, y) | (x, y) <- lts, fst x == stat]

alphabet :: LTS -> Alphabet
alphabet lts
  = nub $ map snd lts

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions p
  = nub $ actionsHelper p
  where
    actionsHelper :: Process -> [Id]
    actionsHelper (Prefix id p)      = id : actionsHelper p
    actionsHelper (Choice (p : ps))  = actionsHelper p ++ actionsHelper (Choice ps)
    actionsHelper _                  = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids pds@(p : _)
  = acceptsHelper ids pds (snd p) -- snd p is the Process associated to the definiton
  where
    acceptsHelper :: [Id] -> [ProcessDef] -> Process -> Bool
    acceptsHelper [] _ _            
      = True
    acceptsHelper ids pds (Ref x)
      = acceptsHelper ids pds (lookUp x pds)  
    acceptsHelper (i : is) pds (Prefix x p)
      = i == x && acceptsHelper is pds p
    acceptsHelper ids@(i : _) pds (Choice (p : ps))
      | acceptsHelper [i] pds p = acceptsHelper ids pds p
      | otherwise               = acceptsHelper ids pds (Choice ps)
    acceptsHelper _ _ _        
      = False
------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition
                   -> Alphabet -> Alphabet -> StateMap -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') al al' sMap
  | a == a'                   = [((ss', tt'), a)]
  | elem a al' && elem a' al  = []
  | elem a' al                = [((ss', ts'), a)]
  | elem a al'                = [((ss', st'), a')]
  | otherwise                 = [((ss', ts'), a), ((ss', st'), a')]
  where
    ss' = lookUp (s, s') sMap
    tt' = lookUp (t, t') sMap
    ts' = lookUp (t, s') sMap
    st' = lookUp (s, t') sMap

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s visited
      | elem s visited = []
      | otherwise      = trans ++ concatMap (flip visit (s : visited)) newDest
      where 
        trans   = transitions s ts
        newDest = map (snd . fst) trans

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = nub $ (pruneTransitions . concat) [composeTransitions t1 t2 a1 a2 (getStateMap s1 s2) 
                                | t1 <- lts1 ++ sentinels1 s1, t2 <- lts2 ++ sentinels2 s2]
  where
    a1         = "$2" : alphabet lts1 -- explained in 5.2.2 The Catch
    a2         = "$1" : alphabet lts2
    s1         = states lts1
    s2         = states lts2
    sentinels1 = map (\s -> ((s, 0), "$1"))
    sentinels2 = map (\s -> ((s, 0), "$2"))

    getStateMap :: [State] -> [State] -> StateMap          -- cartesian product of the 2 LTSs
    getStateMap ss1 ss2 = zip [(s1, s2) | s1 <- ss1, s2 <- ss2] 
                            [0..length ss1 * length ss2 - 1]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS :: LTS
pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

