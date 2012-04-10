module Itdb ( Itdb
            , Playlist(..)
            , Track(..)
            , itdbPlaylists
            , itdbPlaylistIsMpl
            , itdbPlaylistIsPodcasts
            , itdbParse
            )
    where

data Itdb

data Track = Track { trackArtist :: Maybe String
                   , trackAlbum :: Maybe String
                   , trackTitle :: Maybe String
                   }

data Playlist = Playlist { playlistName :: String
                         , playlistMembers :: [Track]
                         }

itdbParse :: FilePath -> IO (Either String a)
itdbParse _ = return $ Left "itdbParse : not implemented"

itdbPlaylists :: Itdb -> IO [Playlist]
itdbPlaylists db = fail "itdbPlaylists : not implemented"

itdbPlaylistIsMpl :: Playlist -> IO Bool
itdbPlaylistIsMpl pl =
  fail "itdbPlaylistIsMpl : not implemented"

itdbPlaylistIsPodcasts :: Playlist -> IO Bool
itdbPlaylistIsPodcasts pl =
  fail "itdbPlaylistIsPodcasts : not implemented"
