{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Conditional (unlessM)

import Turtle
import Turtle.Format

data Options = Options { dest :: FilePath
                       , src :: FilePath
                       , name :: Text
                       } deriving Show

optionsParser :: Parser Options
optionsParser = Options <$> optPath "dest" 'd' "Destination (has to equal mount point in my case)"
                        <*> optPath "src" 's' "Source directory"
                        <*> optText "name" 'n' "Base identifier name"

main :: IO ()
main = do
  opts <- options "Attic Schedule" optionsParser

  unlessM (isPathMounted $ dest opts) $ do
    echo $ format ("Doesn't seem like "%s%" is mounted. Let me do that for you …") (tshow $ dest opts)
    void $ mount $ dest opts

mount :: FilePath -> IO ExitCode
mount path = let Right tpath = Path.toText path in proc "mount" [tpath] empty

-- | Not really reliable way to check if something is possibly mounted.
--   If the given path is a sub-path of a mounted item, it will return a
--   false positive.
isPathMounted :: FilePath -> IO Bool
isPathMounted path = do
  let Right tpath = Path.toText path
  mounts <- TIO.readFile "/proc/mounts"
  return $ tpath `T.isInfixOf` mounts

tshow :: Show s => s -> Text
tshow = fromString . show
