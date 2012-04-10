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
import System.IO.Unsafe

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

instance Storable Track where
  sizeOf _ = 456
  alignment _ = 4

  peek p = do
    ar <- peekByteOff p 16
    al <- peekByteOff p 12
    t  <- peekByteOff p 4
    par <- fromN ar
    pal <- fromN al
    pt <- fromN t
    return $ Track par pal pt

  poke _ _ = error "poking track"

fromN :: CString -> IO (Maybe String)
fromN p | p == nullPtr = return Nothing
fromN p = do
  s <- peekCAString p
  return $ Just s

data Playlist = Playlist { playlistName :: String
                         , playlistMembers :: [Track]
                         }

instance Storable Playlist where
  sizeOf _ = 152
  alignment _ = 4

  peek p = do
    cn <- peekByteOff p 4
    lm <- peekByteOff p 16
    n <- peekCAString cn
    m <- fromGList lm
    return $ Playlist n m

  poke _ _ = error "poking playlist"

itdbParse :: FilePath -> IO (Either String Itdb)
itdbParse fp =
  alloca $ \ perr ->
  withCString fp $ \ cs -> do
    p <- c_itdb_parse cs perr
    if p == nullPtr
      then gErrorFail perr
      else return $ Right p

itdbPlaylists :: Itdb -> [Playlist]
itdbPlaylists =
  unsafePerformIO . safePlaylist

safePlaylist :: Itdb -> IO [Playlist]
safePlaylist db = do
  gl <- peekByteOff db 4
  fromGList gl

-- struct GList {
--   gpointer data;
--   GList *next;
--   GList *prev;
-- };
data GList a = GList (Maybe (GLNode a))

data GLNode a = GLNode { glData :: a
                       , glNext :: GList a
                       , glPrev :: GList a
                       }

instance Storable a => Storable (GList a) where
  sizeOf _ = 12
  alignment _ = 4

  peek p | p == nullPtr = return $ GList Nothing
  peek p = do
    d <- peekByteOff p 0
    n <- peekByteOff p 4
    p <- peekByteOff p 8
    return $ GList $ Just $ GLNode d n p

  poke _ _ = error "poking glist"

fromGList :: GList a -> IO [a]
fromGList (GList Nothing) = return []
fromGList (GList (Just gn)) = do
  let h = glData gn
  t <- fromGList (glNext gn)
  return $ h:t

itdbPlaylistIsMpl :: Playlist -> IO Bool
itdbPlaylistIsMpl pl =
  error "itdbPlaylistIsMpl : not implemented"

itdbPlaylistIsPodcasts :: Playlist -> IO Bool
itdbPlaylistIsPodcasts pl =
  error "itdbPlaylistIsPodcasts : not implemented"
