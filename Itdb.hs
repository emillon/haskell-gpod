module Itdb ( Itdb
            , Playlist(..)
            , Track(..)
            , itdbPlaylists
            , itdbParse
            )
    where

import Control.Applicative
import Data.List
import Data.Maybe
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Glib.GError
import System.Glib.GList
import System.IO.Unsafe

data ItdbStruct

foreign import ccall "itdb.h itdb_parse"
  c_itdb_parse :: CString -> Ptr (Ptr GError) -> IO (Ptr ItdbStruct)

newtype Itdb = Itdb (ForeignPtr ItdbStruct)

instance Show Itdb where
  show db =
    show (length ps) ++ " playlists" ++ "\n"
    ++ intercalate "\n" (map show ps)
      where
        ps = itdbPlaylists db

data Track = Track { trackArtist :: Maybe String
                   , trackAlbum :: Maybe String
                   , trackTitle :: Maybe String
                   }

instance Show Track where
  show t = intercalate " - " $ catMaybes [ trackArtist t
                                         , trackAlbum t
                                         , trackTitle t
                                         ]

instance Storable Track where
  sizeOf _ = 456
  alignment _ = 4

  peek p =
    Track <$> (peekByteOff p 16 >>= fromN)
          <*> (peekByteOff p 12 >>= fromN)
          <*> (peekByteOff p 4  >>= fromN)

  poke _ _ = error "poking track"

fromN :: CString -> IO (Maybe String)
fromN p | p == nullPtr = return Nothing
fromN p = Just <$> peekCAString p

data Playlist = Playlist { playlistName :: String
                         , playlistMembers :: [Track]
                         , playlistType :: Int
                         , playlistPodcastFlag :: Bool
                         }

instance Show Playlist where
  show pl =
    concat [ playlistName pl
           , playlistSuffix pl
           , "\n"
           , "tracks: "
           , show (length tracks)
           , "\n"
           , concatMap (\ t -> "  - " ++ show t ++ "\n") tracks
           ]
    where
      tracks = playlistMembers pl
      playlistSuffix pl | playlistType pl == 1 = " (Master Playlist)"
      playlistSuffix pl | playlistPodcastFlag pl = " (Podcasts Playlist)"
      playlistSuffix _ = ""

instance Storable Playlist where
  sizeOf _ = 152
  alignment _ = 4

  peek p =
    Playlist <$> (peekByteOff p 4 >>= peekCAString)
             <*> (peekByteOff p 16 >>= fromGList >>= mapM peek)
             <*> peekByteOff p 20
             <*> peekByteOff p 40

  poke _ _ = error "poking playlist"

itdbParse :: FilePath -> IO (Either String Itdb)
itdbParse fp =
  alloca $ \ perr -> do
  poke perr nullPtr
  withCString fp $ \ cs -> do
    p <- c_itdb_parse cs perr
    if p == nullPtr
      then Left . getError <$> (peek perr >>= peek)
      else Right <$> makeItdb p
    where
      getError (GError _ _ err) = err

foreign import ccall "itdb.h &itdb_free"
  p_itdb_free :: FunPtr (Ptr ItdbStruct -> IO ())

makeItdb :: Ptr ItdbStruct -> IO Itdb
makeItdb p = do
  fp <- newForeignPtr_ p -- itdb_free should be added to finalizers
  {-addForeignPtrFinalizer p_itdb_free fp-}
  return $ Itdb fp

itdbPlaylists :: Itdb -> [Playlist]
itdbPlaylists =
  unsafePerformIO . safePlaylist

safePlaylist :: Itdb -> IO [Playlist]
safePlaylist (Itdb p) =
  withForeignPtr p $ \ db -> peekByteOff db 4
                         >>= fromGList
                         >>= mapM peek
