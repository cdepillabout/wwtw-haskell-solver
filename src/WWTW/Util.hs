{-# LANGUAGE CPP #-}

module WWTW.Util where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad (when)
import Data.String.Utils (startswith)
import System.IO (Handle, hGetChar)

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


