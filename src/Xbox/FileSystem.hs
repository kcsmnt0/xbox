module Xbox.FileSystem where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import GHC.Generics (Generic)
import Lens.Simple
import System.Directory
import System.FilePath.Posix

data Rose f n l = Leaf l | Node n (f (Rose f n l))
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance
  (forall a. Show a => Show (f a), Show n, Show l) =>
  Show (Rose f n l)

deriving instance
  (forall a. Eq a => Eq (f a), Eq n, Eq l) =>
  Eq (Rose f n l)

data Meta = Meta
  { _path :: FilePath
  , _modified :: UTCTime
  }
  deriving (Eq, Ord, Show)

makeLenses ''Meta

type FileName = FilePath

type DirTree = Rose (Map FileName) Meta Meta

getDirTree :: FilePath -> IO DirTree
getDirTree fp = do
  fp <- makeAbsolute fp
  isFile <- doesFileExist fp
  if isFile then
    Leaf . Meta fp <$> getModificationTime fp
  else do
    t <- getModificationTime fp
    fps <- mapM (\x -> (x,) <$> getDirTree (fp </> x)) =<< listDirectory fp
    return $ Node (Meta fp t) $ Map.fromList fps
