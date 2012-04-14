import System.Environment
import System.Exit

import Itdb

data Conf = Conf { confMountpoint :: FilePath
                 , confPlaylist :: Maybe String
                 }
            deriving (Show)

makeConf :: [String] -> Maybe Conf
makeConf [mp] = Just $ Conf mp Nothing
makeConf [mp, pl] = Just $ Conf mp (Just pl)
makeConf _ = Nothing

exitWithMsg :: String -> IO a
exitWithMsg msg = do
  putStrLn msg
  exitFailure

usage :: IO a
usage = do
  progName <- getProgName
  exitWithMsg $ "Usage: " ++ progName ++" <mountpoint> [<playlistname>]\n\n"
             ++ "<playlistname> - name of the playlist to list (optional)"

main :: IO ()
main = do
  args <- getArgs
  conf <- maybe usage return $ makeConf args
  print conf
  itdbE <- itdbParse $ confMountpoint conf
  case itdbE of
    Left message -> exitWithMsg message
    Right db -> print db
