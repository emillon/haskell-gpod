import Control.Monad
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
displayAll =
  mapM_ displayPlaylist . itdbPlaylists

data PlaylistType = Master | Podcasts | Other

playlistSuffix :: PlaylistType -> String
playlistSuffix Master = " (Master Playlist)"
playlistSuffix Podcasts = " (Podcasts Playlist)"
playlistSuffix Other = ""

getTypeOfPlaylist :: Playlist -> IO PlaylistType
getTypeOfPlaylist pl = do
  isM <- itdbPlaylistIsMpl pl
  isPc <- itdbPlaylistIsPodcasts pl
  return $ case (isM, isPc) of
    (True, _) -> Master
    (_, True) -> Podcasts
    _ -> Other

displayPlaylist :: Playlist -> IO ()
displayPlaylist pl = do
  pt <- getTypeOfPlaylist pl
  putStrLn $ playlistName pl ++ playlistSuffix pt
  let tracks = playlistMembers pl
  putStrLn $ "tracks: " ++ show (length tracks)
  mapM_ displayTrack  tracks

displayTrack :: Track -> IO ()
displayTrack t =
  putStrLn $ s (trackArtist t)
          ++ " - "
          ++ s (trackAlbum t)
          ++ " - "
          ++ s (trackTitle t)
    where
      s Nothing = "(null"
      s (Just x) = x
