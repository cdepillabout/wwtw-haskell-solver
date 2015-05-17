{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.String.Utils (startswith)
import System.IO (BufferMode(NoBuffering), Handle, hSetBuffering, hGetChar, stdin, stdout)

-- | Read character-by-character from a 'Handle' until a string is found.
-- Return the entire string that has been read in.
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
readUntil :: Handle -> String -> IO String
readUntil handle stringToLookFor = reverse <$> loop []
  where
    searchingString :: String
    searchingString = reverse stringToLookFor

    loop :: String -> IO String
    loop accum = do
        newChar <- hGetChar handle
        let newAccum = newChar : accum
        if startswith searchingString newAccum
            then return newAccum
            else loop newAccum

data MazeElement = Player -- ^ V or ^ or < or >
                 | Enemy  -- ^ A
                 | Exit   -- ^ E
                 | Tardis -- ^ T
                 | Empty  -- ^ (blank)
                 deriving Show

instance Monoid MazeElement where
    mempty = Empty

    mappend x Empty = Empty
    mappend Empty x = x
    mappend x y = x

type MazeState = [[MazeElement]]

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

makeMove :: Handle -> MazeState -> IO ()
makeMove handle mazeState = undefined

solveMaze :: IO ()
solveMaze = do
    readUntil stdin "   012345678901234567890\n"
    mazeState <- readMazeState stdin
    print mazeState
    makeMove stdout mazeState

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    solveMaze
    print =<< readUntil stdin "hello"
