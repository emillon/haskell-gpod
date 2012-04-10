import System.Environment
import System.Exit

data Conf = Conf { confMountpoint :: FilePath
                 , confPlaylist :: Maybe String
                 }
            deriving (Show)

makeConf :: [String] -> Maybe Conf
makeConf [mp] = Just $ Conf mp Nothing
makeConf [mp, pl] = Just $ Conf mp (Just pl)
makeConf _ = Nothing

usage :: IO a
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++" <mountpoint> [<playlistname>]\n\n"
	  ++  "<playlistname> - name of the playlist to list (optional)\n"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  conf <- maybe usage return $ makeConf args
  print conf
