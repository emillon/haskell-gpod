module Itdb ( Itdb
            , Playlist(..)
            , Track(..)
            , itdbPlaylists
            , itdbPlaylistIsMpl
            , itdbPlaylistIsPodcasts
            , itdbParse
            )
    where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr

data ItdbStruct

data GError

gErrorFail :: Ptr (Ptr GError) -> IO (Either String a)
gErrorFail ge = fail "gErrorMessage"

foreign import ccall "itdb.h itdb_parse" c_itdb_parse :: CString
                                                      -> Ptr (Ptr GError)
                                                      -> IO (Ptr ItdbStruct)

type Itdb = Ptr ItdbStruct

data Track = Track { trackArtist :: Maybe String
                   , trackAlbum :: Maybe String
                   , trackTitle :: Maybe String
                   }

data Playlist = Playlist { playlistName :: String
                         , playlistMembers :: [Track]
                         }

itdbParse :: FilePath -> IO (Either String Itdb)
itdbParse fp =
  alloca $ \ perr ->
  withCString fp $ \ cs -> do
    p <- c_itdb_parse cs perr
    if p == nullPtr
      then gErrorFail perr
      else return $ Right p

itdbPlaylists :: Itdb -> IO [Playlist]
itdbPlaylists db = fail "itdbPlaylists : not implemented"

itdbPlaylistIsMpl :: Playlist -> IO Bool
itdbPlaylistIsMpl pl =
  fail "itdbPlaylistIsMpl : not implemented"

itdbPlaylistIsPodcasts :: Playlist -> IO Bool
itdbPlaylistIsPodcasts pl =
  fail "itdbPlaylistIsPodcasts : not implemented"
