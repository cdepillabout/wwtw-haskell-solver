{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.String.Utils (startswith)
import System.IO (BufferMode(NoBuffering), Handle, hSetBuffering, hGetChar, stdin)

-- | Read character-by-character from a 'Handle' until a string is found.
-- Return the entire string that has been read in.
--
-- This blocks if a character is not available to read.
--
-- Before calling this function, you may want to set the input stream to be
-- non-buffering:
--
-- @'hSetBuffering' 'stdin' 'NoBuffering'@
-- @string <- 'readUntil' 'stdin' "hello"@
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

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    print =<< readUntil stdin "hello"
