{-# LANGUAGE ViewPatterns #-}
module Args where

import           Data.Maybe
import           Data.String
import           Stack.Types.Version (Version)
import qualified Stack.Types.Version as Stack

data Command = Exec String [String]
             | Source

parseArgs :: [(String, String)] -> [String] -> (Version, Command)
parseArgs env args = case args of
  (parseVersion -> Just version) : xs -> (version, parseCommand xs)
  xs -> (defaultVersion, parseCommand xs)
  where
    Just defaultVersion = parseVersion "7.10.3"
    parseCommand xs = case xs of
        ["--env"] -> Source
        prog : progArgs -> Exec prog progArgs
        [] -> Exec shell []

    shell = fromMaybe "/bin/sh" $ lookup "SHELL" env

parseVersion :: String -> Maybe Version
parseVersion = Stack.parseVersion . fromString
