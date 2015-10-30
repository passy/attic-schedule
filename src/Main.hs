{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import Turtle

data Options = Options { dest :: FilePath
                       , src :: FilePath
                       , name :: Text
                       } deriving Show

optionsParser :: Parser Options
optionsParser = Options <$> optPath "dest" 'd' "Destiny directory"
                        <*> optPath "src" 's' "Source directory"
                        <*> optText "name" 'n' "Base identifier name"

main :: IO ()
main = do
  opts <- options "Attic Schedule" optionsParser
  echo $ fromString $ show opts
