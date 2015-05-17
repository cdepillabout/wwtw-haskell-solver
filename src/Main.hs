{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust, isJust)
import Data.String.Utils (endswith, startswith)
import System.IO (BufferMode(NoBuffering), Handle, hFlush, hGetChar, hPutStr, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc)

-- | Read character-by-character from a 'Handle' until one of multiple
-- strings is found.  Return the entire string that has been read in.
-- If the 'Bool' is True, then print out the string read so far to stdout.
-- This should really only need to be used for exploratory programming.
--
-- This blocks if a character is not available to read.
--
-- Before calling this function, you may want to set the input stream to be
-- non-buffering:
--
-- @
--  'hSetBuffering' 'stdin' 'NoBuffering'
--  string <- 'readUntil' 'stdin' "hello"
-- @
--
-- Since we are using 'hGetChar', we also throw 'isEOFError' if the EOF is
-- reached.
readUntilMultiDebug :: Bool -> Handle -> [String] -> IO String
readUntilMultiDebug isDebug handle stringsToLookFor = reverse <$> loop []
  where
    searchingStrings :: [String]
    searchingStrings = map reverse stringsToLookFor

    loop :: String -> IO String
    loop accum = do
        newChar <- hGetChar handle
        let newAccum = newChar : accum
        when (isDebug) . print $ reverse newAccum
        if any id $ map (flip startswith newAccum) searchingStrings
            then return newAccum
            else loop newAccum


-- | 'readUntilMultiDebug' with the debug flag set to 'False'
readUntilMulti :: Handle -> [String] -> IO String
readUntilMulti = readUntilMultiDebug False

-- | Just like 'readUntilMutli' but for only one string.
readUntil :: Handle -> String -> IO String
readUntil handle stringToLookFor = readUntilMulti handle [stringToLookFor]

data MazeElement = Player -- ^ V or ^ or < or >
                 | Enemy  -- ^ A
                 | Exit   -- ^ E
                 | Tardis -- ^ T
                 | Empty  -- ^ (blank)
                 deriving (Eq, Ord, Show)

instance Monoid MazeElement where
    mempty = Empty

    mappend _ Empty = Empty
    mappend Empty x = x
    mappend x _ = x

type MazeRow = [MazeElement]
type MazeState = [MazeRow]

charToMazeElement :: Char -> MazeElement
charToMazeElement 'V' = Player
charToMazeElement '^' = Player
charToMazeElement '<' = Player
charToMazeElement '>' = Player
charToMazeElement 'A' = Enemy
charToMazeElement 'E' = Exit
charToMazeElement 'T' = Tardis
charToMazeElement ' ' = Empty
charToMazeElement x = error $ "in charToMazeElement, got a character that wasn't expected: " ++ [x]

parseMazeLine :: String -> [MazeElement]
parseMazeLine mazeStringLine =
    -- get rid of the intial 3 characters, and then convert each character
    -- to it's MazeElement type.
    let mazeStringLine' = map charToMazeElement $ drop 3 mazeStringLine
    -- there might not be 20 characters in the line, so we need to put on
    -- extra 'Empty's.
    in take 20 $ mazeStringLine' ++ repeat Empty

parseMaze :: String -> MazeState
parseMaze mazeString = map parseMazeLine $ init $ lines mazeString

readMazeState :: Handle -> IO MazeState
readMazeState handle = do
    mazeString <- readUntil handle "Your move (w,a,s,d,q): "
    putStrLn mazeString
    return $ parseMaze mazeString

data Direction = UP
               | LEFT
               | RIGHT
               | DOWN
               deriving (Eq, Show)

directionToString :: Direction -> String
directionToString UP = "w"
directionToString LEFT = "d"
directionToString DOWN = "s"
directionToString RIGHT = "a"

data Row = Row Int
data Column = Column Int
type Coordinates = (Row, Column)

-- | This is a partial function.  It's pretty much just a hack.  The whole
-- thing is pretty awful.  It returns the coordinates of a specific (well,
-- a list of potential) maze elements from the MazeState.
--
-- >>> coordinatesOfMazeElement [Player] [[Empty, Empty, Empty], [Empty, Player, Empty], [Empty, Empty, Enemy]]
-- (1,1)
-- >>> coordinatesOfMazeElement [Exit, Tardis] [[Empty, Empty, Enemy], [Empty, Player, Empty], [Empty, Empty, Tardis]]
-- (2,2)
-- >>> coordinatesOfMazeElement [Exit, Tardis] [[Empty, Empty, Enemy], [Empty, Player, Empty], [Tardis, Empty, Empty]]
-- (2,0)
-- >>> coordinatesOfMazeElement [] [[Empty, Empty, Empty], [Empty, Player, Empty], [Empty, Empty, Enemy]]
-- *** Exception: coordinatesOfMazeElement is a bad function and recieved elements it was not expecting! Bad! Bad!
coordinatesOfMazeElement :: [MazeElement] -> MazeState -> Coordinates
coordinatesOfMazeElement mazeElements mazeState =
    case mazeElements of
        [Player] -> fromJust $ coordOfMazeElemMaybe Player
        [Exit, Tardis] -> case coordOfMazeElemMaybe Exit of
                              Nothing -> fromJust $ coordOfMazeElemMaybe Tardis
                              Just coord -> coord
        _ -> error "coordinatesOfMazeElement is a bad function and recieved elements it was not expecting! Bad! Bad!"
  where
    coordOfMazeElemMaybe :: MazeElement -> Maybe Coordinates
    coordOfMazeElemMaybe mazeElement = do
        let listOfMaybeInts = map f mazeState
        indexOfJust <- findIndex (isJust) listOfMaybeInts
        let row = Row indexOfJust
        colum <- Column <$> listOfMaybeInts !! indexOfJust
        return (row, colum)
      where
        f :: MazeRow -> Maybe Int
        f mazeRow = elemIndex mazeElement mazeRow

-- | Calculate the list of directions needed to get from one coordinate to
-- another coordinate.
--
-- >>> directionsToFrom (Row 0, Column 0) (Row 0, Column 10)
-- [LEFT]
-- >>> directionsToFrom (Row 0, Column 0) (Row 10, Column 10)
-- [LEFT, DOWN]
-- >>> directionsToFrom (Row 2, Column 2) (Row 2, Column 2)
-- []
directionsFromTo :: Coordinates -> Coordinates -> [Direction]
directionsFromTo (Row ax, Column ay) (Row bx, Column by) = isUp ++ isLeft ++ isRight ++ isDown
 where
    isUp, isLeft, isRight, isDown :: [Direction]
    isUp    = if ax - bx > 0 then [UP]    else []
    isLeft  = if by - ay > 0 then [LEFT]  else []
    isRight = if ay - by > 0 then [RIGHT] else []
    isDown  = if bx - ax > 0 then [DOWN]  else []


-- | Calculate the direction the exit is in.  This is returning a list
-- because the exit could potentially be in two directions (for instance if
-- we are at the lower left hand corner of the map, and the exit is at the
-- upper right hand corner of the map, then this will return @[UP, RIGHT]@.
exitDirections :: MazeState -> [Direction]
exitDirections mazeState =
    let playerCoord = coordinatesOfMazeElement [Player] mazeState
        exitCoord = coordinatesOfMazeElement [Exit, Tardis] mazeState
    in directionsFromTo playerCoord exitCoord


-- | Check if the 'Coordinates' in a 'MazeState' are an 'Enemy'.
--
-- >>> isEnemy (Row 0, Column 0) [[Empty, Empty, Empty], [Enemy, Player, Tardis]]
-- False
-- >>> isEnemy (Row 0, Column 1) [[Empty, Enemy, Empty], [Enemy, Player, Tardis]]
-- True
-- >>> isEnemy (Row 100, Column 0) [[Empty, Enemy, Empty], [Enemy, Player, Tardis]]
-- *** Exception: Prelude.!!: index too large
--
-- __WARNING__ This is a partial function because it's using the '(!!)'
-- operator.
isEnemy :: Coordinates -> MazeState -> Bool
isEnemy (Row x, Column y) mazeState = mazeState !! x !! y == Enemy

-- | Give the 'MazeState', figure out which directions the player can
-- preceed in without dieing.  The player can preceed in a direction if
-- there are no enemies and the player won't run off the end of the map.
--
-- >>> directionsCanGo [[Empty, Empty, Empty], [Empty, Player, Empty], [Empty, Empty, Empty]]
-- [UP,LEFT,RIGHT,DOWN]
-- >>> directionsCanGo [[Empty, Enemy, Empty], [Empty, Player, Empty], [Empty, Enemy, Empty]]
-- [LEFT,RIGHT]
-- >>> directionsCanGo [[Player, Enemy, Empty], [Empty, Empty, Enemy], [Empty, Enemy, Empty]]
-- [DOWN]
-- >>> directionsCanGo [[Empty, Enemy, Empty], [Empty, Empty, Enemy], [Empty, Enemy, Player]]
-- []
--
-- __WARNING__ This function assumes that a player will be in the maze, so
-- if the player does not exist, this function will throw an error.
directionsCanGo :: MazeState -> [Direction]
directionsCanGo mazeState =
    let playerCoord = coordinatesOfMazeElement [Player] mazeState
    in canGoUp playerCoord ++
        canGoLeft playerCoord ++
        canGoRight playerCoord ++
        canGoDown playerCoord
  where
    -- how many columns and rows are in the maze
    mazeMaxRow, mazeMaxColumn :: Int
    mazeMaxRow    = length mazeState - 1
    mazeMaxColumn = (length $ head mazeState) - 1

    canGoUp, canGoLeft, canGoRight, canGoDown :: Coordinates -> [Direction]
    canGoUp    (Row x, Column y) = if x - 1 >= 0             && not (isEnemy (Row $ x - 1, Column y      ) mazeState) then [UP]    else []
    canGoLeft  (Row x, Column y) = if y + 1 <= mazeMaxColumn && not (isEnemy (Row x      , Column $ y + 1) mazeState) then [LEFT]  else []
    canGoRight (Row x, Column y) = if y - 1 >= 0             && not (isEnemy (Row x      , Column $ y - 1) mazeState) then [RIGHT] else []
    canGoDown  (Row x, Column y) = if x + 1 <= mazeMaxRow    && not (isEnemy (Row $ x + 1, Column y      ) mazeState) then [DOWN]  else []

-- | Given the directions the exit is in, and directions that we can
-- possibly move, this picks the best direction to move in.
--
-- >>> selectBestDirection [UP] []
-- *** Exception: There are no directions the player is able to move in without dieing.
-- >>> selectBestDirection [UP, DOWN] [LEFT]
-- LEFT
-- >>> selectBestDirection [UP, DOWN] [RIGHT, UP, LEFT]
-- UP
--
selectBestDirection :: [Direction] -> [Direction] -> Direction
selectBestDirection _ [] = error "There are no directions the player is able to move in without dieing."
selectBestDirection _ [d] = d
selectBestDirection [] (dirWithNoEnemies:_) = dirWithNoEnemies
selectBestDirection (exitDir:exitDirs) dirsWithNoEnemies =
    if exitDir `elem` dirsWithNoEnemies
        then exitDir
        else selectBestDirection exitDirs dirsWithNoEnemies

-- | Return a 'String' representing the direction the 'Player' can move in
-- order to not die and get close to the exit.
--
-- >>> calcMove [[Empty, Empty, Empty], [Empty, Empty, Player], [Tardis, Empty, Empty]]
-- "d"
-- >>> calcMove [[Tardis, Empty, Empty], [Empty, Empty, Player], [Enemy, Empty, Empty]]
-- "w"
-- >>> calcMove [[Empty, Empty, Enemy], [Tardis, Enemy, Enemy], [Empty, Player, Empty]]
-- "a"
--
calcMove :: MazeState -> String
calcMove mazeState =
    let exitDirs = exitDirections mazeState
        dirsWithNoEnemies = directionsCanGo mazeState
        selectedDirection = selectBestDirection exitDirs dirsWithNoEnemies
    in directionToString selectedDirection

-- | Calculate the best move to be made given a 'MazeState', and write that
-- move out to the 'Handle'.
makeMove :: Handle -> MazeState -> IO ()
makeMove handle mazeState = do
    let move = calcMove mazeState
    hPutStrLn handle move

-- | Solve one step of the maze.
solveMazeStep :: Handle -> Handle -> IO ()
solveMazeStep childStdinHandle childStdoutHandle = do
    mazeState <- readMazeState childStdoutHandle
    makeMove childStdinHandle mazeState

-- | Solve the whole maze.  Returns the portion of the string that has been
-- read so far.  This should probably be a string that ends with "TARDIS
-- KEY: "
solveMaze :: String      -- ^ string that has been read so far from child's stdout (this isn't used)
          -> Handle      -- ^ 'Handle' of the child's stdin
          -> Handle      -- ^ 'Handle' of the child's stdout
          -> IO (String) -- ^ String that has been read so far but nothing done with it.
solveMaze _ childStdinHandle childStdoutHandle = do
    print "hello"
    let askTardisKeyString = "TARDIS KEY: "
        graphNumbersString = "   012345678901234567890\n"
    stringReadIn <- readUntilMultiDebug True
                                 childStdoutHandle
                                 [askTardisKeyString , graphNumbersString]
    print stringReadIn
    if endswith graphNumbersString stringReadIn
        then do
            solveMazeStep childStdinHandle childStdoutHandle
            threadDelay 100000
            solveMaze stringReadIn childStdinHandle childStdoutHandle
        else return stringReadIn

writeTardisKey :: String      -- ^ string that has been read so far from child's stdout (this isn't used)
               -> Handle      -- ^ 'Handle' of the child's stdin
               -> Handle      -- ^ 'Handle' of the child's stdout
               -> IO (String) -- ^ String that has been read so far but nothing done with it.
writeTardisKey _ childStdinHandle childStdoutHandle = do
    hPutStrLn childStdinHandle "UeSlhCAGEp"
    readUntilMultiDebug True childStdoutHandle ["Selection: "]

writeFirstSelectionOverflow :: String -> Handle -> Handle -> IO (String)
writeFirstSelectionOverflow _ childStdinHandle childStdoutHandle = do
    hPutStr childStdinHandle "11111111\00"
    hFlush childStdinHandle
    readUntilMultiDebug True childStdoutHandle ["Selection: "]

writeSecondSelectionOverflow :: String -> Handle -> Handle -> IO (String)
writeSecondSelectionOverflow _ childStdinHandle childStdoutHandle = do
    hPutStrLn childStdinHandle "m+YU"
    readUntilMultiDebug True childStdoutHandle ["Selection: "]

writeThirdSelectionOverflow :: String -> Handle -> Handle -> IO (String)
writeThirdSelectionOverflow _ childStdinHandle childStdoutHandle = do
    hPutStr childStdinHandle "33333333\00"
    hFlush childStdinHandle
    readUntilMultiDebug True childStdoutHandle ["Coordinates: "]

mainLoop :: Handle -> Handle -> IO ()
mainLoop childStdinHandle childStdoutHandle = do
    tardisKeyString <- solveMaze "" childStdinHandle childStdoutHandle
    selectionString <- writeTardisKey tardisKeyString childStdinHandle childStdoutHandle
    selectionStringAfterOne <- writeFirstSelectionOverflow selectionString
                                                           childStdinHandle
                                                           childStdoutHandle
    selectionStringAfterTwo <- writeSecondSelectionOverflow selectionStringAfterOne
                                                            childStdinHandle
                                                            childStdoutHandle
    selectionStringAfterThree <- writeFirstSelectionOverflow selectionStringAfterTwo
                                                             childStdinHandle
                                                             childStdoutHandle
    coordinatesString <- writeThirdSelectionOverflow selectionStringAfterThree
                                                     childStdinHandle
                                                     childStdoutHandle
    return ()

main :: IO ()
main = do
    let processToRun = (proc "./wwtw_c3722e23150e1d5abbc1c248d99d718d" [])
                            { std_out = CreatePipe
                            , std_in = CreatePipe }
    -- this stdin and stdout are the for the CHILD, so WE can READ from
    -- wwtwStdout and WRITE to wwtwStdin.
    (Just wwtwStdin, Just wwtwStdout, _, _) <- createProcess processToRun
    hSetBuffering wwtwStdin NoBuffering
    mainLoop wwtwStdin wwtwStdout
