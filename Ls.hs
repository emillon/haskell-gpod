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
    Right db -> displayAll db

displayAll :: Itdb -> IO ()
displayAll db = do
  putStrLn $ show (length ps) ++ " playlists"
  mapM_ displayPlaylist ps
    where
      ps = itdbPlaylists db

playlistSuffix :: Playlist -> String
playlistSuffix pl | itdbPlaylistIsMpl pl = " (Master Playlist)"
playlistSuffix pl | itdbPlaylistIsPodcasts pl = " (Podcasts Playlist)"
playlistSuffix _ = ""

displayPlaylist :: Playlist -> IO ()
displayPlaylist pl = do
  putStrLn $ playlistName pl ++ playlistSuffix pl
  let tracks = playlistMembers pl
  putStrLn $ "tracks: " ++ show (length tracks)
  mapM_ print tracks
