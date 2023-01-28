
-- A Cell represents a position on the grid
type Cell = (Int,Int)

-- The MyState structure represents the state of the robot at any given time. It is either Null or the
-- data constructor S followed by a cell representing the robot’s position, a list of cells
-- representing the positions of the mines to be collected, a string representing the last
-- action performed to reach this state, and the parent state.
-- The parent state is the last
-- state the robot was in before doing the last performed action.
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

-- The function takes as input a state and returns the state resulting from moving UP
-- from the input state. If up will result in going out of the boundaries of the grid OR it is on a mine 
-- OR it will go far from the nearest mine, Null should be returned.
up:: MyState -> MyState
up ( S (x,y) mines string myState ) = if x<=0 || elem (x,y) mines || (checkGoFar "up" (x,y) mines) then Null else 
													(S (x-1,y) mines "up" (S (x,y) mines string myState))
-- Same goes here but DOWN
down:: MyState -> MyState
down ( S (x,y) mines string myState ) = if x>=10 || elem (x,y) mines || (checkGoFar "down" (x,y) mines) then Null else 
													(S (x+1,y) mines "down" (S (x,y) mines string myState))
-- Same goes here but LEFT
left:: MyState -> MyState
left ( S (x,y) mines string myState ) = if y<=0 || elem (x,y) mines || (checkGoFar "left" (x,y) mines) then Null else 
													(S (x,y-1) mines "left" (S (x,y) mines string myState))
-- Same goes here but RIGHT
right:: MyState -> MyState
right ( S (x,y) mines string myState ) = if y>=10 || elem (x,y) mines || (checkGoFar "right" (x,y) mines) then Null else 
													(S (x,y+1) mines "right" (S (x,y) mines string myState))

-- The function takes as input a state and returns the state resulting from collecting a mine which we stand on
-- from the input state.
collect:: MyState -> MyState
collect (S cell mines string myState) = if elem cell mines then (S cell (filter (/=cell) mines) "collect" (S cell mines string myState)) else Null

-- The function takes as input a state and returns the set of states resulting from
-- applying up, down, left, right, and collect from the input state. The output set
-- of states should not contain any Null states.
nextMyStates:: MyState->[MyState]
nextMyStates myState = filter (/=Null) [(collect myState),(up myState),(down myState),(left myState),(right myState)]

-- The function takes as input a state, returns true if the input state has no more
-- mines to collect (the list of mines is empty), and false otherwise.
isGoal:: MyState->Bool
isGoal (S cell mines string myState) = if mines ==[] then True else False

-- The function takes as input a list of states. It checks if the head of the input list
-- is a goal state, if it is a goal, it returns the head. Otherwise, it gets the next states
-- from the state at head of the input list, and calls itself recursively.
search:: [MyState]->MyState
search [] = Null
search (h:t) = if (isGoal h) then h else search (t ++ (nextMyStates h) )

-- The function takes as input a state and returns a set of strings representing actions
-- that the robot can follow to reach the input state from the initial state.
constructSolution:: MyState ->[String]
constructSolution (S cell mines string myState) = if myState==Null then [] else (constructSolution myState)++[string]

-- The function takes as input a cell representing the starting position of the robot,
-- a set of cells representing the positions of the mines, and returns a set of strings
-- representing actions that the robot can follow to reach a goal state from the initial state.
solve :: Cell->[Cell]->[String]
solve cell mines = constructSolution (search [(S cell mines "" Null)])


-- 					Helpers

-- It checks if the action will get the sweeper more far from the nearest Mine or not.
-- If False, The condition of Null will not occur, so the move will pass (in the up,down,left,right functions).
checkGoFar:: String -> Cell -> [Cell] -> Bool
checkGoFar func (x1,y1) mines	= 	case func of	
										"up" -> if x1>x2 then False
												else True

										"down" -> if x1<x2 then False 
												  else True
										
										"left" -> if y1>y2 then False 
												  else True
										
										"right" -> if y1<y2 then False 
												   else True
									where (x2,y2) = nearestMine (x1,y1) mines

-- returns the cell of the nearest Mine
nearestMine:: Cell -> [Cell] -> Cell
nearestMine cell [h] = h
nearestMine cell (h1:h2:t) = nearestMine cell ( (mahnattanDistanceMin cell h1 h2) : t )

-- Calculates the distance from the sweeper to a mine
mahnattanDistanceMin:: Cell -> Cell -> Cell -> Cell
mahnattanDistanceMin (x1,y1) (x2,y2) (x3,y3) = if (abs (x1-x2) + abs (y1-y2)) < (abs (x1-x3) + abs (y1-y3)) then (x2,y2) else (x3,y3)
