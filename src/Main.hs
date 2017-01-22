{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import qualified Control.Foldl             as Fold
import qualified Data.Attoparsec.Text      as PT
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Protolude          hiding ((%), FilePath, (<>), fold)
import           Turtle

import           Data.String               (String)
import           Data.Char                 (isSpace)
import           Data.Foldable             (maximumBy)
import           Data.Function             (on)
import           Data.Time                 (TimeOfDay (..), UTCTime (..),
                                            addUTCTime, defaultTimeLocale,
                                            diffUTCTime, getCurrentTime,
                                            parseTimeM, timeToTimeOfDay)

type ParsedBackups = Either String [BackupList]

data Options = Options { dest :: FilePath
                       , src  :: FilePath
                       , name :: Text
                       } deriving Show

data BackupList = BackupList { backupTag  :: Text
                             , backupTime :: UTCTime
                             } deriving (Show, Eq)

backupListParser :: PT.Parser BackupList
backupListParser = do
  name' <- PT.takeTill isSpace
  _ <- PT.skipSpace
  _weekDay <- PT.skipWhile (not . isSpace)
  dateStr <- PT.takeTill PT.isEndOfLine

  -- "Oct  5 12:23:45 2015"
  date' <- case parseTimeM True defaultTimeLocale "%b %e %X %Y" (T.unpack dateStr) of
    Just pd -> return pd
    Nothing -> error $ "Invalid date: " <> dateStr

  return BackupList { backupTag = name'
                    , backupTime = date'
                    }

getAtticRepo :: Options -> FilePath
getAtticRepo opts = dest opts </> fromText (name opts <> ".attic")

optionsParser :: Parser Options
optionsParser = Options <$> optPath "dest" 'd' "Destination (has to equal mount point in my case)"
                        <*> optPath "src" 's' "Source directory"
                        <*> optText "name" 'n' "Base identifier name"

getYesterday :: IO UTCTime
getYesterday = do
  let minimumDiff = -1 * 24 * 60 * 60 :: NominalDiffTime
  addUTCTime minimumDiff <$> getCurrentTime

-- | Based on the list of backups, decide whether or not to schedule a backup.
--
-- Don't barf on empty results.
-- >>> shouldBackup $ Right []
-- True
shouldBackup :: ParsedBackups -> IO Bool
shouldBackup backupList = do
  yesterday <- getYesterday
  let lastBackup = findLastBackup <$> backupList
  return $ all (maybe True $ backupOlderThan yesterday) lastBackup

doBackup :: FilePath -> FilePath -> Shell ExitCode
doBackup src' repo = do
  now <- liftIO getCurrentTime
  let dayStr = tshow . utctDay $ now
  let hourStr = tshow . todHour . timeToTimeOfDay . utctDayTime $ now
  let target = format (fp%"::"%s%":"%s) repo dayStr hourStr

  echo $ "Creating new backup with target " <> target
  proc "attic" ["create", target, format fp src', "--stats"] empty

backupOlderThan :: UTCTime -> BackupList -> Bool
backupOlderThan time' backup = diffUTCTime time' (backupTime backup) > 0

mount :: FilePath -> IO ExitCode
mount path = proc "sudo" ["mount", format fp path] empty

obtainBackupList :: FilePath -> IO (Either String [BackupList])
obtainBackupList repo = do
  output' <- fold (inproc "attic" ["list", format fp repo] empty) Fold.list
  return $ sequence $ PT.parseOnly backupListParser <$> output'

-- | O(n) finds the last backup
-- >>> findLastBackup []
-- Nothing
findLastBackup :: [BackupList] -> Maybe BackupList
findLastBackup [] = Nothing
findLastBackup l  = pure . maximumBy (compare `on` backupTime) $ l

-- | Not really reliable way to check if something is possibly mounted.
--   If the given path is a sub-path of a mounted item, it will return a
--   false positive.
isPathMounted :: FilePath -> IO Bool
isPathMounted path = do
  mounts <- TIO.readFile "/proc/mounts"
  return $ format fp path `T.isInfixOf` mounts

tshow :: Show s => s -> Text
tshow = fromString . show

main :: IO ()
main = do
  opts <- options "Attic Schedule" optionsParser
  let repo = getAtticRepo opts

  unlessM (isPathMounted $ dest opts) $ do
    echo $ format ("Doesn't seem like "%s%" is mounted. Let me do that for you …") (tshow $ dest opts)
    ExitSuccess <- mount $ dest opts
    return ()

  backupList <- obtainBackupList repo
  shouldBackup' <- shouldBackup backupList

  if shouldBackup' then do
    echo "Last backup is older than 24h, let's do it!"
    view $ doBackup (src opts) repo
  else
    echo "Old backup isn't even a day old. I'll skip it for now."
