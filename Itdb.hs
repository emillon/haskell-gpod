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
import Foreign.Storable

data ItdbStruct

-- struct GError {
--   GQuark       domain;
--   gint         code;
--   gchar       *message;
-- };
data GError

gErrorFail :: Ptr (Ptr GError) -> IO (Either String a)
gErrorFail ge = do
  err <- peek ge
  cMsg <- peekByteOff err 8
  msg <- peekCAString cMsg
  return $ Left msg

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

itdbPlaylists :: Itdb -> [Playlist]
itdbPlaylists db = error "itdbPlaylists : not implemented"

itdbPlaylistIsMpl :: Playlist -> IO Bool
itdbPlaylistIsMpl pl =
  error "itdbPlaylistIsMpl : not implemented"

itdbPlaylistIsPodcasts :: Playlist -> IO Bool
itdbPlaylistIsPodcasts pl =
  error "itdbPlaylistIsPodcasts : not implemented"
