module Xbox.Library where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Append
import Data.Serialize
import Data.Serialize.Text ()
import GHC.Generics
import Lens.Simple
import Sound.HTagLib (getTags)
import qualified Sound.HTagLib as Tag
import System.Directory
import System.FilePath.Posix

import Xbox.FileSystem

deriving instance Functor (AppendMap k)
deriving instance Generic (AppendMap k v)

instance (Ord k, Serialize k, Serialize v) => Serialize (AppendMap k v)

instance Serialize Tag.Artist where
  put = put . Tag.unArtist
  get = Tag.mkArtist <$> get

instance Serialize Tag.Album where
  put = put . Tag.unAlbum
  get = Tag.mkAlbum <$> get

instance Serialize Tag.TrackNumber where
  put = put . Tag.unTrackNumber
  get = maybe mzero return . Tag.mkTrackNumber =<< get

instance Serialize Tag.Title where
  put = put . Tag.unTitle
  get = Tag.mkTitle <$> get

instance Serialize Tag.Duration where
  put = put . Tag.unDuration
  get = maybe mzero return . Tag.mkDuration =<< get

instance Serialize Tag.Year where
  put = put . Tag.unYear
  get = maybe mzero return . Tag.mkYear =<< get

data Song = Song
  { _path :: FilePath
  , _title :: Tag.Title
  , _duration :: Tag.Duration
  , _year :: Maybe Tag.Year
  } deriving (Eq, Ord, Show, Generic, Serialize)

makeLenses ''Song

type Album = AppendMap (Maybe Tag.TrackNumber) [Song]
type Artist = AppendMap Tag.Album Album
type Library = AppendMap Tag.Artist Artist

scanFile :: FilePath -> FilePath -> IO Library
scanFile log file
  | takeExtension file `elem` [".flac", ".wav", ".mp3", ".ogg", ".aiff"] = do
      appendFile log $ "scanning " ++ show file ++ "\n"
      catch
        do
          appendFile log $ "reading  " ++ show file ++ "\n"
          (track, title, artist, album, duration, year) <-
            getTags file $ (,,,,,)
              <$> Tag.trackNumberGetter
              <*> Tag.titleGetter
              <*> Tag.artistGetter
              <*> Tag.albumGetter
              <*> Tag.durationGetter
              <*> Tag.yearGetter
          return $
            AppendMap $ Map.singleton artist $
            AppendMap $ Map.singleton album $
            AppendMap $ Map.singleton track [Song file title duration year]
        \(e :: Tag.HTagLibException) -> do
          appendFile log $ show e ++ "\n"
          return $ AppendMap Map.empty
  | otherwise = return $ AppendMap Map.empty

scanDirTree :: FilePath -> DirTree -> IO Library
scanDirTree log (Leaf (Meta path _)) = scanFile log path
scanDirTree log (Node _ dts) = foldMap (scanDirTree log) dts

scanPath :: FilePath -> FilePath -> IO Library
scanPath log = scanDirTree log <=< getDirTree

saveLibrary :: FilePath -> Library -> IO ()
saveLibrary file lib = Bytes.writeFile file $ encode lib

loadLibrary :: FilePath -> FilePath -> IO (Maybe Library)
loadLibrary log file = do
  ok <- doesFileExist file
  if ok then do
    dat <- Bytes.readFile file
    case runGet get dat of
      Left err -> appendFile log err >> return Nothing
      Right l -> return $ Just l
  else
    return Nothing
