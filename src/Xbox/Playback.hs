module Xbox.Playback where

import Control.Monad
import Data.List
import Lens.Simple
import Sound.ALUT hiding (get)
import System.FilePath.Posix
import System.IO
import System.Process

import Xbox.Config
import Xbox.FileSystem

data ProgressEvent
  = PlayingAt ALfloat
  | PausedAt ALfloat
  | Stopped

data PlaybackEvent
  = Load FileName
  | Play
  | Pause
  | Stop

sanitize :: FilePath -> FilePath
sanitize = concat . intersperse "_" . filter ('/' `notElem`) . splitDirectories

ffmpeg :: Config -> FilePath -> FilePath -> IO ()
ffmpeg cfg src dst = do
  log <- openFile (cfg^.logPath) AppendMode
  void $
    readCreateProcess
      (proc (cfg^.ffmpegPath) ["-y", "-i", src, dst])
        { std_out = UseHandle log
        , std_err = UseHandle log
        }
      ""

handlePlaybackEvent :: Config -> Source -> PlaybackEvent -> IO ()
handlePlaybackEvent cfg src (Load file) = do
  st <- sourceState src
  when (st `elem` [Playing, Paused]) $ stop [src]
  when do takeExtension file `notElem` [".flac",".ogg",".mp3",".wav"]
    do error $ "sorry i can't play " ++ file
  let wavFile = cfg^.tempPath </> sanitize file <.> "wav"
  ffmpeg cfg file wavFile
  (buffer src $=) . Just =<< do createBuffer $ File wavFile
handlePlaybackEvent cfg src Play = play [src]
handlePlaybackEvent cfg src Pause = pause [src]
handlePlaybackEvent cfg src Stop = stop [src]
