module Xbox.Config where

import Lens.Simple

data Config = Config
  { _libraryPath :: FilePath
  , _logPath :: FilePath
  , _cachePath :: FilePath
  , _tempPath :: FilePath
  , _ffmpegPath :: FilePath
  }

makeLenses ''Config
