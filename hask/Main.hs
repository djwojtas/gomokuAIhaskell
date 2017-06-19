import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Text.Printf
import qualified Data.List
import qualified Control.Arrow

data Tree a = Branch [Tree a] | Leaf a

instance (Show a) => Show (Tree a) where
  show (Leaf val) = show val ++ " "
  show (Branch list) = concat [show elem | elem <- list] ++ "\n"

getMaxScoreField :: Board -> Pawn -> [(Double, Field)] -> Field
getMaxScoreField board player list = (\[(_, x)] -> x) $ if (\[(x,_)] -> x /= 0) $ take 1 $ Data.List.sortBy (\(x, _) (y, _) -> y `compare` x) list then
                                           take 1 $ Data.List.sortBy (\(x, _) (y, _) -> y `compare` x) list else
                                           (\(Score val, (xpos, ypos)) -> [(val, Field xpos ypos)]) $ head $ listBestMoves $ getScoreBoard board player

zipMaxWithFields :: Int -> Int -> Board -> Pawn -> [(Double, Field)]
zipMaxWithFields depth branching board player = zipWith (\x y -> (x, y))
                                                (leafBranchToList $ getMaxWin $ getMoveTree depth branching board player)
                                                (movesToFieldsList $ take branching $ listBestMoves $ getScoreBoard board player)

leafBranchToList :: Tree Double -> [Double]
leafBranchToList (Branch list@(Leaf _:_)) = map (\(Leaf val) -> val) list
leafBranchToList _ = []

movesToFieldsList :: [(t, (Int,Int))] -> [Field]
movesToFieldsList = map (\(_, (x, y)) -> Field x y)

getMaxWin :: Tree Double -> Tree Double
getMaxWin tree@(Branch (Branch _:_)) = getMaxWin (maxToMin tree)
getMaxWin tree = tree

getMoveTree :: Int -> Int -> Board -> Pawn -> Tree Double
getMoveTree 0 _ board player = Leaf (getBoardScore board player)
getMoveTree depth branching board player = Branch [getMoveTree (depth-1) branching branchingBoard (negatePawn player) | branchingBoard <- createBranches branching board player]

createBranches :: Int -> Board -> Pawn -> [Board]
createBranches branching board player = map (\(_, (x, y)) -> setPawn board (Field x y) player) (take branching (listBestMoves (getScoreBoard board player)))

maxToMin :: Tree Double -> Tree Double
maxToMin (Branch list@(Branch _:_)) = Branch [minToMax elem | elem <- list]
maxToMin (Branch list) = Leaf (maximum (map (\(Leaf x) -> x) list))
maxToMin (Leaf x) = Leaf x

minToMax :: Tree Double -> Tree Double
minToMax (Branch list@(Branch _:_)) = Branch [maxToMin elem | elem <- list]
minToMax (Branch list) = Leaf (minimum (map (\(Leaf x) -> x) list))
minToMax (Leaf x) = Leaf x

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

setPawn :: Board -> Field -> Pawn -> Board
setPawn (Board boardMap x y) field pawn = Board (Map.insert field pawn boardMap) x y

boardfieldToChar :: Maybe Pawn -> String
boardfieldToChar (Just pawn) = [pawnToChar pawn, ' ']
boardfieldToChar Nothing = "- "

getBoardLine :: Board -> Int -> String
getBoardLine (Board boardMap x _) line = concat ([boardfieldToChar(Map.lookup (Field wdth line) boardMap)
                                                  | wdth <- [1..x-1]] ++ [boardfieldToChar(Map.lookup (Field x line) boardMap)])

addNewLine :: String -> String
addNewLine str = str ++ ['\n']

boardToString :: Board -> String
boardToString (Board boardMap x y) = concat [addNewLine (getBoardLine (Board boardMap x y) line)
                                        | line <- [1..y-1]] ++ getBoardLine (Board boardMap x y) y

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

scoreToString :: Score -> String
scoreToString (Score score) = Text.Printf.printf "%2.2f" score
scoreToString Occupied = "XXX"

isFieldOccupied :: Maybe Pawn -> Bool
isFieldOccupied (Just _) = True
isFieldOccupied Nothing = False

calculateFieldScore :: Field -> Board -> Pawn -> Double
calculateFieldScore field board pawn = sum [horizontalMatch field board pawn,
                                            verticalMatch field board pawn,
                                            closeToCenterPoints field board,
                                            closeToOtherPawnPoints field board,
                                            searchWin field board (negatePawn pawn),
                                            threatSearch field board pawn,
                                            searchWin field board pawn]

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

--starting filed -> board to serach -> player -> current offset -> score for recursion
horizontalLeft :: Field -> Board -> Pawn -> Int -> Double
horizontalLeft (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | xf-pos > 4 = 0
  | otherwise = horizontalLeft (Field xf yf) (Board boardMap x y) player (pos-1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

--starting filed -> board to serach -> player -> current offset -> score for recursion
horizontalRight :: Field -> Board -> Pawn -> Int -> Double
horizontalRight (Field xf yf) (Board boardMap x y) player pos
  | getFieldOccupationScore player (Field pos yf) (Board boardMap x y) == -1 = 0
  | pos-xf > 4 = 0
  | otherwise = horizontalRight (Field xf yf) (Board boardMap x y) player (pos+1) +
                  getFieldOccupationScore player (Field pos yf) (Board boardMap x y)

horizontalMatch :: Field -> Board -> Pawn -> Double
horizontalMatch (Field xf yf) (Board boardMap x y) pawn = horizontalLeft (Field xf yf) (Board boardMap x y) pawn (xf-1) +
                                                     horizontalRight (Field xf yf) (Board boardMap x y) pawn (xf+1)

--starting filed -> board to serach -> player -> current offset -> score for recursion
verticalDown :: Field -> Board -> Pawn -> Int -> Double
verticalDown (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | yf-pos > 4 = 0
 | otherwise = verticalDown (Field xf yf) (Board boardMap x y) player (pos-1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

--starting filed -> board to serach -> player -> current offset -> score for recursion
verticalUp :: Field -> Board -> Pawn -> Int -> Double
verticalUp (Field xf yf) (Board boardMap x y) player pos
 | getFieldOccupationScore player (Field xf pos) (Board boardMap x y) == -1 = 0
 | pos-yf > 4 = 0
 | otherwise = verticalUp (Field xf yf) (Board boardMap x y) player (pos+1) +
                 getFieldOccupationScore player (Field xf pos) (Board boardMap x y)

verticalMatch :: Field -> Board -> Pawn -> Double
verticalMatch (Field xf yf) (Board boardMap x y) pawn = verticalDown (Field xf yf) (Board boardMap x y) pawn (yf-1) +
                                                    verticalUp (Field xf yf) (Board boardMap x y) pawn (yf+1)

getFieldScore :: Field -> Board -> Pawn -> Score
getFieldScore (Field xf yf) (Board boardMap x y) pawn
  | isFieldOccupied (Map.lookup (Field xf yf) boardMap) = Occupied
  | otherwise = Score (calculateFieldScore (Field xf yf) (Board boardMap x y) pawn)

data ScoreBoard = ScoreBoard [[Score]] Int Int

getScoreBoard :: Board -> Pawn -> ScoreBoard
getScoreBoard (Board boardMap x y) pawn = ScoreBoard [[getFieldScore (Field xf yf) (Board boardMap x y) pawn | xf <- [1..x]] | yf <-[1..y]] x y

addTabsToScores :: [Score] -> String
addTabsToScores = foldr (\x y -> scoreToString x ++ "\t" ++ y) ""

scrBrdToStr :: ScoreBoard -> String
scrBrdToStr (ScoreBoard scoreList x y) = foldr ((\x y -> x ++ "\n" ++ y) . addTabsToScores) "" scoreList

listBestMoves :: ScoreBoard -> [(Score, (Int, Int))]
listBestMoves (ScoreBoard scoreList _ _) = Data.List.sortBy (\(x, _) (y, _) -> x `compare` y)
                                  (zipWith (\x y -> (x, y)) (concat scoreList) [(x, y) | y <- [1..19], x <- [1..19]])

negatePawn :: Pawn -> Pawn
negatePawn X = O
negatePawn O = X

-- threatUp :: Int -> Field -> Board -> Pawn -> Double
-- threatUp count (Field xf yf) (Board boardMap x y) player
--   | yf+1 > 19 = 0
--   | count > 0 && Map.lookup (Field xf (yf+1)) boardMap == Just (negatePawn player) = threatUp (count-1) (Field xf (yf+1)) (Board boardMap x y) player
--   | count == 0 && Data.Maybe.isNothing (Map.lookup (Field xf (yf+1)) boardMap) = 200
--   | count == 0 && Map.lookup (Field xf (yf+1)) boardMap == Just (negatePawn player) = 200
--   | otherwise = 0
--
-- threatDown :: Int -> Field -> Board -> Pawn -> Double
-- threatDown count (Field xf yf) (Board boardMap x y) player
--   | yf-1 < 1 = 0
--   | count > 0 && Map.lookup (Field xf (yf-1)) boardMap == Just (negatePawn player) = threatDown (count-1) (Field xf (yf-1)) (Board boardMap x y) player
--   | count == 0 && Data.Maybe.isNothing (Map.lookup (Field xf (yf-1)) boardMap) = 200
--   | count == 0 && Map.lookup (Field xf (yf-1)) boardMap == Just (negatePawn player) = 200
--   | otherwise = 0
--
-- threatRight :: Int -> Field -> Board -> Pawn -> Double
-- threatRight count (Field xf yf) (Board boardMap x y) player
--   | xf+1 > 19 = 0
--   | count > 0 && Map.lookup (Field (xf+1) yf) boardMap == Just (negatePawn player) = threatRight (count-1) (Field (xf+1) yf) (Board boardMap x y) player
--   | count == 0 && Data.Maybe.isNothing (Map.lookup (Field (xf+1) yf) boardMap) = 200
--   | count == 0 && Map.lookup (Field (xf+1) yf) boardMap == Just (negatePawn player) = 200
--   | otherwise = 0
--
-- threatLeft :: Int -> Field -> Board -> Pawn -> Double
-- threatLeft count (Field xf yf) (Board boardMap x y) player
--   | xf-1 < 1 = 0
--   | count > 0 && Map.lookup (Field (xf-1) yf) boardMap == Just (negatePawn player) = threatLeft (count-1) (Field (xf-1) yf) (Board boardMap x y) player
--   | count == 0 && Data.Maybe.isNothing (Map.lookup (Field (xf-1) yf) boardMap) = 200
--   | count == 0 && Map.lookup (Field (xf-1) yf) boardMap == Just (negatePawn player) = 200
--   | otherwise = 0

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
  -- | closeToOtherPawnPoints field board > 0 = threatUp 3 field board player +
  --                                            threatDown 3 field board player +
  --                                            threatRight 3 field board player +
  --                                            threatLeft 3 field board player
  -- | otherwise = 0


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

scoreToDouble :: Score -> Double
scoreToDouble (Score x) = x
scoreToDouble Occupied = 0

getBoardScore :: Board -> Pawn -> Double
getBoardScore board player = if winCheck board player then (-20000) else 0 +
                             if winCheck board player then 10000 else 0 +
                             sum [costFunc (Field x y) board player | y <- [1..19], x <- [1..19]]

costFunc :: Field -> Board -> Pawn -> Double
costFunc field board player = threatSearch field board (negatePawn player) -
                              searchWin field board (negatePawn player) +
                              searchWin field board player

makeMove :: Board -> Pawn -> Board
makeMove board pawn = setPawn board (getMaxScoreField board pawn $ zipMaxWithFields 4 3 board pawn) pawn

playerLoop :: Board -> IO()
playerLoop board = if winCheck board X then putStrLn "COMPUTER WINS" else do
  putStrLn "\n\n----- YOU PLAY AS 'O' -----\n"
  putStrLn (boardToString board)
  putStrLn "\n\n----- ENTER ROW:\n"
  row <- getLine
  putStrLn "\n\n----- ENTER COLUMN:\n"
  col <- getLine
  pcLoop (setPawn board (Field (read col) (read row)) O)

pcLoop :: Board -> IO()
pcLoop board = if winCheck board O then putStrLn "PLAYER WINS" else do
  putStrLn "\n\n----- PC (X) MAKES MOVE -----\n"
  putStrLn (boardToString board)
  playerLoop (makeMove board X)

testLoop :: Int -> Board -> Pawn -> IO()
testLoop 0 _ _ = putStrLn "END"
testLoop cntdwn board pawn =
  if winCheck board pawn then putStrLn (pawnToString pawn ++ " WINS") else do
    putStrLn $ "\n\n------------------------------------------- SCORE FOR PLAYER " ++ pawnToString pawn ++ " " ++ show (getBoardScore board pawn) ++ "\n"
    putStrLn (boardToString board)
    -- putStrLn (scrBrdToStr (getScoreBoard board pawn))
    -- print (map (Control.Arrow.first scoreToString) $ take 10 (listBestMoves (getScoreBoard board pawn)))
    testLoop (cntdwn-1) (makeMove board pawn) (negatePawn pawn)

testBoard = setPawn (Board Map.empty 19 19) (Field 5 1) X
testBoard2 = setPawn testBoard (Field 4 2) X
testBoard3 = setPawn testBoard2 (Field 3 3) X
testBoard4 = setPawn testBoard3 (Field 2 4) X
testBoard5 = setPawn testBoard4 (Field 1 5) X

emptyBoard = Board Map.empty 19 19

main = playerLoop emptyBoard

-- main = do
--   putStrLn (scrBrdToStr (getScoreBoard testBoard5 X))
--   print (map (Control.Arrow.first scoreToString) (listBestMoves (getScoreBoard testBoard5 X)))
