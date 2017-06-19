import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Text.Printf
import qualified Data.List
import qualified Control.Arrow

--------------------------------------------------------------------------------
--------------- BASIC DATA STRUCTURES AND INSTANCES FOR THEM -------------------
--------------------------------------------------------------------------------

data Tree a = Branch [Tree a] | Leaf a

data Pawn = X | O

pawnToChar :: Pawn -> Char
pawnToChar X = 'X'
pawnToChar O = 'O'

pawnToString :: Pawn -> String
pawnToString pawn = [pawnToChar pawn]

instance Eq Pawn where
  a == b = pawnToChar a == pawnToChar b

instance Show Pawn where
  show = pawnToString

instance Read Pawn where
  readsPrec _ input
    | head input == 'X' = [(X, tail input)]
    | head input == 'O' = [(O, tail input)]
    | otherwise = []

data Field = Field Int Int deriving Show

instance Eq Field where
  (Field x1 y1) == (Field x2 y2) = (x1 == x2)  && (y1 == y2)

instance Ord Field where
  compare (Field x1 y1) (Field x2 y2)
    | y1 > y2 = GT
    | y1 < y2 = LT
    | x1 > x2 = GT
    | x1 < x2 = LT
    | otherwise = EQ

data Board = Board (Map.Map Field Pawn) Int Int

data Score = Score Double | Occupied

instance Eq Score where
  (Score x) == (Score y) = x == y
  _ == _ = False

instance Ord Score where
  compare (Score x) (Score y) = compare y x
  compare Occupied (Score _) = GT
  compare (Score _) Occupied = LT
  compare Occupied Occupied = EQ

instance Show Score where
  show = scoreToString

data ScoreBoard = ScoreBoard [[Score]] Int Int

--------------------------------------------------------------------------------
-------------------------- CONVERTING DATA TO STRING ---------------------------
--------------------------------------------------------------------------------

boardfieldToChar :: Maybe Pawn -> String
boardfieldToChar (Just pawn) = [' ', pawnToChar pawn, ' ']
boardfieldToChar Nothing = " - "

getBoardLine :: Board -> Int -> String
getBoardLine (Board boardMap x _) line = concat ([boardfieldToChar(Map.lookup (Field wdth line) boardMap)
                                                  | wdth <- [1..x-1]] ++ [boardfieldToChar(Map.lookup (Field x line) boardMap)])

addNewLineAndNumber :: Int -> String -> String
addNewLineAndNumber i str = Text.Printf.printf "%2d " i ++ str ++ ['\n']

boardToString :: Board -> String
boardToString (Board boardMap x y) = "    1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19\n" ++
                                     concat [addNewLineAndNumber line (getBoardLine (Board boardMap x y) line) | line <- [1..y]]

scoreToString :: Score -> String
scoreToString (Score score) = Text.Printf.printf "%2.2f" score
scoreToString Occupied = "XXX"

--------------------------------------------------------------------------------
------------------------------ MINMAX ALGORITHM --------------------------------
--------------------------------------------------------------------------------

getMoveTree :: Int -> Int -> Board -> Pawn -> Pawn -> Tree Double
getMoveTree 0 _ board _ targetPlayer = Leaf (getBoardScore board targetPlayer)
getMoveTree depth branching board player targetPlayer = Branch [getMoveTree (depth-1) branching branchingBoard (negatePawn player) targetPlayer | branchingBoard <- createBranches branching board player]

createBranches :: Int -> Board -> Pawn -> [Board]
createBranches branching board player = map (\(_, (x, y)) -> setPawn board (Field x y) player) (take branching (listBestMoves (getScoreBoard board player)))

getMaxScoreField :: Board -> Pawn -> [(Double, Field)] -> Field
getMaxScoreField board player list = (\[(_, x)] -> x) $ if (\[(x,_)] -> x /= 0) $ take 1 $ Data.List.sortBy (\(x, _) (y, _) -> y `compare` x) list then
                                           take 1 $ Data.List.sortBy (\(x, _) (y, _) -> y `compare` x) list else
                                           (\(Score val, (xpos, ypos)) -> [(val, Field xpos ypos)]) $ head $ listBestMoves $ getScoreBoard board player

zipMaxWithFields :: Int -> Int -> Board -> Pawn -> [(Double, Field)]
zipMaxWithFields depth branching board player = zipWith (\x y -> (x, y))
                                                (leafBranchToList $ getMaxWin $ getMoveTree depth branching board player player)
                                                (movesToFieldsList $ take branching $ listBestMoves $ getScoreBoard board player)

leafBranchToList :: Tree Double -> [Double]
leafBranchToList (Branch list@(Leaf _:_)) = map (\(Leaf val) -> val) list
leafBranchToList _ = []

movesToFieldsList :: [(t, (Int,Int))] -> [Field]
movesToFieldsList = map (\(_, (x, y)) -> Field x y)

getMaxWin :: Tree Double -> Tree Double
getMaxWin tree@(Branch (Branch _:_)) = getMaxWin (minToMax tree)
getMaxWin tree = tree

maxToMin :: Tree Double -> Tree Double
maxToMin (Branch list@(Branch _:_)) = Branch [minToMax elem | elem <- list]
maxToMin (Branch list) = Leaf (maximum (map (\(Leaf x) -> x) list))
maxToMin (Leaf x) = Leaf x

minToMax :: Tree Double -> Tree Double
minToMax (Branch list@(Branch _:_)) = Branch [maxToMin elem | elem <- list]
minToMax (Branch list) = Leaf (minimum (map (\(Leaf x) -> x) list))
minToMax (Leaf x) = Leaf x

--------------------------------------------------------------------------------
-------------------------- BACISC GAME OPERATIONS ------------------------------
--------------------------------------------------------------------------------

isFieldOccupied :: Maybe Pawn -> Bool
isFieldOccupied (Just _) = True
isFieldOccupied Nothing = False

setPawn :: Board -> Field -> Pawn -> Board
setPawn (Board boardMap x y) field pawn = Board (Map.insert field pawn boardMap) x y

negatePawn :: Pawn -> Pawn
negatePawn X = O
negatePawn O = X

--------------------------------------------------------------------------------
-------------------------- HEURISTIC FUNCTIONS ---------------------------------
--------------------------------------------------------------------------------

calculateFieldScore :: Field -> Board -> Pawn -> Double
calculateFieldScore field board pawn = sum [horizontalMatch field board pawn,
                                            verticalMatch field board pawn,
                                            closeToCenterPoints field board,
                                            closeToOtherPawnPoints field board,
                                            searchWin field board (negatePawn pawn),
                                            threatSearch field board pawn,
                                            searchWin field board pawn,
                                            ascSlopeMatch field board pawn,
                                            descSlopeMatch field board pawn]

closeToCenterPoints :: Field -> Board -> Double
closeToCenterPoints (Field xf yf) (Board boardMap x y) = 0.5 * (sqrt((fromIntegral (x+1) / 2) ^ 2 + (fromIntegral (y+1) / 2) ^ 2) -
                                                         sqrt(((fromIntegral (x+1) / 2) - fromIntegral xf) ^ 2 +
                                                              ((fromIntegral (y+1) / 2) - fromIntegral yf) ^ 2))

closeToOtherPawnPoints :: Field -> Board -> Double
closeToOtherPawnPoints (Field xf yf) (Board boardMap x y) = sum (map (\x -> if x then 5 else 0) [isFieldOccupied (Map.lookup (Field closex closey) boardMap) |
                                                                        closex <- [xf-1..xf+1], closey <- [yf-1..yf+1]])

getFieldOccupationScore :: Pawn -> Field -> Board -> Double
getFieldOccupationScore player (Field xf yf) (Board boardMap x y)
  | Data.Maybe.isNothing (Map.lookup (Field xf yf) boardMap) = 0
  | Map.lookup (Field xf yf) boardMap == Just player = 15
  | otherwise = -1

horizontalLeft :: Field -> Board -> Pawn -> Int -> Double
horizontalLeft (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | xf-pos > 4 = 0
  | otherwise = horizontalLeft (Field xf yf) (Board boardMap x y) player (pos-1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

horizontalRight :: Field -> Board -> Pawn -> Int -> Double
horizontalRight (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | pos-xf > 4 = 0
  | otherwise = horizontalRight (Field xf yf) (Board boardMap x y) player (pos+1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

horizontalMatch :: Field -> Board -> Pawn -> Double
horizontalMatch (Field xf yf) (Board boardMap x y) pawn = horizontalLeft (Field xf yf) (Board boardMap x y) pawn (xf-1) +
                                                     horizontalRight (Field xf yf) (Board boardMap x y) pawn (xf+1)

verticalDown :: Field -> Board -> Pawn -> Int -> Double
verticalDown (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | yf-pos > 4 = 0
 | otherwise = verticalDown (Field xf yf) (Board boardMap x y) player (pos-1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

verticalUp :: Field -> Board -> Pawn -> Int -> Double
verticalUp (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | pos-yf > 4 = 0
 | otherwise = verticalUp (Field xf yf) (Board boardMap x y) player (pos+1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

verticalMatch :: Field -> Board -> Pawn -> Double
verticalMatch (Field xf yf) (Board boardMap x y) pawn = verticalDown (Field xf yf) (Board boardMap x y) pawn (yf-1) +
                                                    verticalUp (Field xf yf) (Board boardMap x y) pawn (yf+1)

slopeUpRight :: Field -> Board -> Pawn -> Int -> Int -> Double
slopeUpRight (Field xf yf) (Board boardMap x y) player posx posy
 | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
 | posy-yf > 4 = 0
 | otherwise = slopeUpRight (Field xf yf) (Board boardMap x y) player (posx+1) (posy+1) +
                 getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

slopeDownLeft :: Field -> Board -> Pawn -> Int -> Int -> Double
slopeDownLeft (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | yf-posy > 4 = 0
  | otherwise = slopeDownLeft (Field xf yf) (Board boardMap x y) player (posx-1) (posy-1) +
                  getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

ascSlopeMatch :: Field -> Board -> Pawn -> Double
ascSlopeMatch (Field xf yf) (Board boardMap x y) pawn = slopeUpRight (Field xf yf) (Board boardMap x y) pawn (xf+1) (yf+1) +
                                                    slopeDownLeft (Field xf yf) (Board boardMap x y) pawn (xf-1) (yf-1)

slopeDownRight :: Field -> Board -> Pawn -> Int -> Int -> Double
slopeDownRight (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | yf-posy > 4 = 0
  | otherwise = slopeDownRight (Field xf yf) (Board boardMap x y) player (posx+1) (posy-1) +
                 getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

slopeUpLeft :: Field -> Board -> Pawn -> Int -> Int -> Double
slopeUpLeft (Field xf yf) (Board boardMap x y) player posx posy
  | getFieldOccupationScore player (Field posx posy) (Board boardMap x y) == -1 = 0
  | posy-yf > 4 = 0
  | otherwise = slopeUpLeft (Field xf yf) (Board boardMap x y) player (posx-1) (posy+1) +
                  getFieldOccupationScore player (Field posx posy) (Board boardMap x y)

descSlopeMatch :: Field -> Board -> Pawn -> Double
descSlopeMatch (Field xf yf) (Board boardMap x y) pawn = slopeDownRight (Field xf yf) (Board boardMap x y) pawn (xf+1) (yf-1) +
                                                    slopeUpLeft (Field xf yf) (Board boardMap x y) pawn (xf-1) (yf+1)

getFieldScore :: Field -> Board -> Pawn -> Score
getFieldScore (Field xf yf) (Board boardMap x y) pawn
  | isFieldOccupied (Map.lookup (Field xf yf) boardMap) = Occupied
  | otherwise = Score (calculateFieldScore (Field xf yf) (Board boardMap x y) pawn)

getScoreBoard :: Board -> Pawn -> ScoreBoard
getScoreBoard (Board boardMap x y) pawn = ScoreBoard [[getFieldScore (Field xf yf) (Board boardMap x y) pawn | xf <- [1..x]] | yf <-[1..y]] x y

listBestMoves :: ScoreBoard -> [(Score, (Int, Int))]
listBestMoves (ScoreBoard scoreList _ _) = Data.List.sortBy (\(x, _) (y, _) -> x `compare` y)
                                  (zipWith (\x y -> (x, y)) (concat scoreList) [(x, y) | y <- [1..19], x <- [1..19]])

searchWinUp :: Field -> Board -> Pawn -> Double
searchWinUp (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field xf (yf+1)) boardMap == Just player = 1 + searchWinUp (Field xf (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field xf (yf+1)) boardMap) = 0.6
  | otherwise = 0

searchWinDown :: Field -> Board -> Pawn -> Double
searchWinDown (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field xf (yf-1)) boardMap == Just player = 1 + searchWinDown (Field xf (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field xf (yf-1)) boardMap) = 0.6
  | otherwise = 0

searchWinLeft :: Field -> Board -> Pawn -> Double
searchWinLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) yf) boardMap == Just player = 1 + searchWinLeft (Field (xf-1) yf) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) yf) boardMap) = 0.6
  | otherwise = 0

searchWinRight :: Field -> Board -> Pawn -> Double
searchWinRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) yf) boardMap == Just player = 1 + searchWinRight (Field (xf+1) yf) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) yf) boardMap) = 0.6
  | otherwise = 0

searchWinUpLeft :: Field -> Board -> Pawn -> Double
searchWinUpLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) (yf+1)) boardMap == Just player = 1 + searchWinUpLeft (Field (xf-1) (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) (yf+1)) boardMap) = 0.6
  | otherwise = 0

searchWinDownRight :: Field -> Board -> Pawn -> Double
searchWinDownRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) (yf-1)) boardMap == Just player = 1 + searchWinDownRight (Field (xf+1) (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) (yf-1)) boardMap) = 0.6
  | otherwise = 0

searchWinUpRight :: Field -> Board -> Pawn -> Double
searchWinUpRight (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf+1) (yf+1)) boardMap == Just player = 1 + searchWinUpRight (Field (xf+1) (yf+1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf+1) (yf+1)) boardMap) = 0.6
  | otherwise = 0

searchWinDownLeft :: Field -> Board -> Pawn -> Double
searchWinDownLeft (Field xf yf) (Board boardMap x y) player
  | Map.lookup (Field (xf-1) (yf-1)) boardMap == Just player = 1 + searchWinDownLeft (Field (xf-1) (yf-1)) (Board boardMap x y) player
  | Data.Maybe.isNothing (Map.lookup (Field (xf-1) (yf-1)) boardMap) = 0.6
  | otherwise = 0

searchWin :: Field -> Board -> Pawn -> Double
searchWin field board player
  | closeToOtherPawnPoints field board == 0 = 0
  | searchWinUp field board player + searchWinDown field board player > 4 = 1000
  | searchWinLeft field board player + searchWinRight field board player > 4 = 1000
  | searchWinUpLeft field board player + searchWinDownRight field board player > 4 = 1000
  | searchWinUpRight field board player + searchWinDownLeft field board player > 4 = 1000
  | otherwise = 0

threatSearch :: Field -> Board -> Pawn -> Double
threatSearch field board player
  | closeToOtherPawnPoints field board == 0 = 0
  | searchWinUp field board (negatePawn player) + searchWinDown field board (negatePawn player) > 3 = 200
  | searchWinLeft field board (negatePawn player) + searchWinRight field board (negatePawn player) > 3 = 200
  | searchWinUpLeft field board (negatePawn player) + searchWinDownRight field board (negatePawn player) > 3 = 200
  | searchWinUpRight field board (negatePawn player) + searchWinDownLeft field board (negatePawn player) > 3 = 200
  | otherwise = 0

winCheck :: Board -> Pawn -> Bool
winCheck (Board boardMap _ _) player = any (fieldWinCheck boardMap) (filter (\(x, y) -> y == player) (Map.toList boardMap))

fieldWinCheck :: Map.Map Field Pawn -> (Field, Pawn) -> Bool
fieldWinCheck boardMap (field, pawn) = verticalWinCheck boardMap (field, pawn) > 4 ||
                                       horizontalWinCheck boardMap (field, pawn) > 4 ||
                                       downwardsWinCheck boardMap (field, pawn) > 4 ||
                                       upwardsWinCheck boardMap (field, pawn) > 4

verticalWinCheck :: Map.Map Field Pawn -> (Field, Pawn) -> Int
verticalWinCheck boardMap (Field xf yf, pawn)
  | Map.lookup (Field xf (yf+1)) boardMap == Just pawn = 1 + verticalWinCheck boardMap (Field xf (yf+1), pawn)
  | otherwise = 1

horizontalWinCheck :: Map.Map Field Pawn -> (Field, Pawn) -> Int
horizontalWinCheck boardMap (Field xf yf, pawn)
  | Map.lookup (Field (xf+1) yf) boardMap == Just pawn = 1 + horizontalWinCheck boardMap (Field (xf+1) yf, pawn)
  | otherwise = 1

downwardsWinCheck :: Map.Map Field Pawn -> (Field, Pawn) -> Int
downwardsWinCheck boardMap (Field xf yf, pawn)
  | Map.lookup (Field (xf+1) (yf-1)) boardMap == Just pawn = 1 + downwardsWinCheck boardMap (Field (xf+1) (yf-1), pawn)
  | otherwise = 1

upwardsWinCheck :: Map.Map Field Pawn -> (Field, Pawn) -> Int
upwardsWinCheck boardMap (Field xf yf, pawn)
  | Map.lookup (Field (xf+1) (yf+1)) boardMap == Just pawn = 1 + upwardsWinCheck boardMap (Field (xf+1) (yf+1), pawn)
  | otherwise = 1

getBoardScore :: Board -> Pawn -> Double
getBoardScore board player = if winCheck board (negatePawn player) then (-10000) else 0 +
                             if winCheck board player then 10000 else 0 +
                             sum [costFunc (Field x y) board player | y <- [1..19], x <- [1..19]]

costFunc :: Field -> Board -> Pawn -> Double
costFunc field board@(Board boardMap _ _) player
    | isFieldOccupied (Map.lookup field boardMap) = 0
    | otherwise = threatSearch field board (negatePawn player) +
                  searchWin field board player -
                  (searchWin field board (negatePawn player) * 10)

--------------------------------------------------------------------------------
----------------------------- GAME FUNCTIONS -----------------------------------
--------------------------------------------------------------------------------

makeMove :: Board -> Pawn -> Board
makeMove board pawn = setPawn board (getMaxScoreField board pawn $ zipMaxWithFields 4 3 board pawn) pawn

playerLoop :: Board -> IO()
playerLoop board
  | winCheck board X = do
    putStrLn "COMPUTER WINS. :"
    putStrLn (boardToString board)
    putStrLn "TRY AGAIN!"
  | otherwise = do
    putStrLn "\n\n----- YOU PLAY AS 'O' -----\n"
    putStrLn (boardToString board)
    putStrLn "\n\n----- ENTER ROW:\n"
    row <- getLine
    putStrLn "\n\n----- ENTER COLUMN:\n"
    col <- getLine
    pcLoop (setPawn board (Field (read col) (read row)) O)

pcLoop :: Board -> IO()
pcLoop board
  | winCheck board O = do
    putStrLn "PLAYER WINS!!! :"
    putStrLn (boardToString board)
    putStrLn "CONGRATULATIONS!"
  | otherwise = do
    putStrLn "\n\n----- PC (X) MAKES MOVE (CAN TAKE COUPLE OF SECONDS) -----\n"
    putStrLn (boardToString board)
    playerLoop (makeMove board X)

cpuVsCpuLoop :: Board -> Pawn -> IO()
cpuVsCpuLoop board pawn
  | winCheck board pawn = do
    putStrLn (pawnToString pawn ++ " WINS")
    putStrLn (boardToString board)
  | otherwise = do
    putStrLn $ "\n\nPLAYER " ++ pawnToString pawn ++ "\n"
    putStrLn (boardToString board)
    cpuVsCpuLoop (makeMove board pawn) (negatePawn pawn)

emptyBoard = Board Map.empty 19 19

-- main = playerLoop emptyBoard
main = cpuVsCpuLoop emptyBoard X

--------------------------------------------------------------------------------
---------------- DEBUG FUNCTIONS AND INSTANCES (CAN BE DELETED) ----------------
--------------------------------------------------------------------------------

instance (Show a) => Show (Tree a) where
  show (Leaf val) = show val ++ " "
  show (Branch list) = concat [show elem | elem <- list] ++ "\n"

getMaxScoreFields :: Board -> Pawn -> [(Double, Field)] -> String
getMaxScoreFields board player list = concatMap (\(Score val, (xpos, ypos)) -> ("(" ++ show val ++ " " ++ show xpos ++ " " ++ show ypos ++ ") ")) $ take 5 $ listBestMoves $ getScoreBoard board player

addTabsToScores :: [Score] -> String
addTabsToScores = foldr (\x y -> scoreToString x ++ "\t" ++ y) ""

scrBrdToStr :: ScoreBoard -> String
scrBrdToStr (ScoreBoard scoreList x y) = foldr ((\x y -> x ++ "\n" ++ y) . addTabsToScores) "" scoreList

scoreToDouble :: Score -> Double
scoreToDouble (Score x) = x
scoreToDouble Occupied = 0

zipMaxWithFieldsStr :: Int -> Int -> Board -> Pawn -> String
zipMaxWithFieldsStr depth branching board player = concatMap (\(val, Field xpos ypos) -> ("(" ++ show val ++ " " ++ show xpos ++ " " ++ show ypos ++ ") ")) $ zipMaxWithFields depth branching board player
