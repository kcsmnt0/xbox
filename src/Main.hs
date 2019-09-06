module Main where

import Brick
import Brick.BChan
import Brick.Widgets.Border as Widget
import Brick.Widgets.List as Widget hiding (reverse)
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Map.Append
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Lens.Simple
import Graphics.Vty as Vty hiding (Config)
import Sound.ALUT hiding (get)
import Sound.HTagLib as Tag

import Xbox.Config
import Xbox.Library
import Xbox.Playback
import Xbox.Util

data Name = ArtistList | AlbumList | SongList
  deriving (Eq, Ord, Show, Enum, Bounded)

data PaneChangeEvent = ArtistChanged | AlbumChanged

data AppEvent
  = PaneChanged PaneChangeEvent

data AppState = AppState
  { _config :: Config
  , _library :: Library
  , _artistList :: List Name Tag.Artist
  , _albumList :: List Name Tag.Album
  , _songList :: List Name Song
  , _activeCursor :: Name
  }

makeLenses ''AppState

draw :: AppState -> [Widget Name]
draw s =
  [ joinBorders $ foldr1 (<+>)
    [ pane unArtist ArtistList artistList
    , foldr1 (<=>)
      [ pane unAlbum AlbumList albumList
      , pane (^. title . to unTitle) SongList songList
      ]
    ]
  ]
  where
    pane :: (a -> Text) -> Name -> Getter' AppState (List Name a) -> Widget Name
    pane f c l =
      borderWithLabel
        do txt $ mconcat [headerDecoration, header, headerDecoration]
        do
          renderList
            do \sel x -> txt $ mconcat [if focus then "* " else "- " | sel] <> f x
            do focus
            do s^.l
      where
        focus = s^.activeCursor == c
        headerDecoration = mconcat [" ***** " | focus]
        header = case c of
          ArtistList -> "Artists"
          AlbumList -> "Albums"
          SongList -> "Tracks"

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = find \c -> c^.cursorLocationNameL == Just (s^.activeCursor)

handleAppEvent :: AppState -> AppEvent -> StateT AppState (EventM Name) ()

handleAppEvent s (PaneChanged ArtistChanged) = do
  s <- get
  forM_ (listSelectedElement (s^.artistList)) \(_, artist) -> do
    case s ^. library . to unAppendMap . at artist of
      Nothing -> error $ "bad state, undefined: " ++ show artist
      Just albums -> do
        albumList.listSelectedL .= Nothing
        albumList.listElementsL .= do Vec.fromList $ Map.keys $ unAppendMap albums

handleAppEvent s (PaneChanged AlbumChanged) = do
  s <- get
  forM_ (listSelectedElement (s^.artistList)) \(_, artist) -> do
    case s ^. library . to unAppendMap . at artist of
      Nothing -> error $ "bad state, undefined: " ++ show artist
      Just albums -> do
        forM_ (listSelectedElement (s^.albumList)) \(_, album) -> do
          songList.listSelectedL .= Nothing
          songList.listElementsL .=
            case albums ^. to unAppendMap . at album of
              Nothing -> Vec.empty
              Just songs ->
                Vec.fromList $ concatMap snd $
                    sortOn fst $ Map.toList $ unAppendMap songs

handleEvent ::
  ([AppEvent] -> EventM Name ()) ->
  ([PlaybackEvent] -> EventM Name ()) ->
  AppState ->
  BrickEvent Name AppEvent ->
  EventM Name (Next AppState)
handleEvent uiSend playSend s e =

  flip evalStateT s do
    end <- case e of

      VtyEvent (EvKey KEnter []) ->
        case s^.activeCursor of
          SongList ->
            case listSelectedElement (s^.songList) of
              Nothing -> return False
              Just (_, song) -> do
                lift $ playSend [Load $ song^.path, Play]
                return False
          _ -> return False

      VtyEvent (EvKey KEsc []) -> return True

      VtyEvent (EvKey KRight [MCtrl]) -> do
        activeCursor %= next
        return False

      VtyEvent (EvKey KLeft [MCtrl]) -> do
        activeCursor %= prev
        return False

      VtyEvent e -> do
          let handleList = handleListEventVi handleListEvent
          modifyT \s -> case s^.activeCursor of
            ArtistList ->
              handleEventLensed s artistList handleList e <*
                uiSend [PaneChanged AlbumChanged, PaneChanged ArtistChanged]
            AlbumList ->
              handleEventLensed s albumList handleList e <*
                uiSend [PaneChanged AlbumChanged]
            SongList -> handleEventLensed s songList handleList e
          return False

      AppEvent e -> handleAppEvent s e >> return False

      MouseDown n b ms l -> return False

      MouseUp n b l -> return False

    get >>= lift . if end then halt else continue

startEvent :: BChan PlaybackEvent -> Source -> AppState -> IO AppState
startEvent playEvents src s = do
  forkIO $ forever $ handlePlaybackEvent (s^.config) src =<< readBChan playEvents
  writeFile (s^.config.logPath) ""
  l <- loadLibrary (s^.config.logPath) (s^.config.cachePath) >>= \case
    Just l -> do
      appendFile (s^.config.logPath) $ show l
      return l
    Nothing -> do
      l <- scanPath (s^.config.logPath) (s^.config.libraryPath)
      saveLibrary (s^.config.cachePath) l
      return l
  return $
    s
      & library .~ l
      & artistList.listElementsL .~ do Vec.fromList $ Map.keys $ unAppendMap l

main :: IO ()
main = withProgNameAndArgs runALUT \progName args -> do
  dev <- openDevice Nothing >>= maybe (error "no audio device found") return
  ctx <- createContext dev [] >>= maybe (error "couldn't init ALUT") return
  currentContext $= Just ctx
  src <- genObjectName
  playEvents <- newBChan 10
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  uiEvents <- newBChan $ 2^10
  uiSend <- debouncer_ 50 $ mapM_ $ writeBChan uiEvents
  playSend <- debouncer_ 50 $ mapM_ $ writeBChan playEvents
  void $ customMain initialVty buildVty (Just uiEvents)
    App
      { appDraw = draw
      , appChooseCursor = chooseCursor
      , appHandleEvent = handleEvent (liftIO . uiSend) (liftIO . playSend)
      , appStartEvent = liftIO . startEvent playEvents src
      , appAttrMap = const $ attrMap defAttr []
      }
    AppState
      { _library = AppendMap Map.empty
      , _artistList = list ArtistList Vec.empty 1
      , _albumList = list AlbumList Vec.empty 1
      , _songList = list SongList Vec.empty 1
      , _activeCursor = ArtistList
      , _config = Config 
        { _libraryPath = "/home/katie/difference-engine/mnt/Music"
        , _logPath = "/home/katie/xbox/.log"
        , _cachePath = "/home/katie/xbox/.cache"
        , _tempPath = "/home/katie/xbox/.temp"
        , _ffmpegPath = "/usr/bin/ffmpeg"
        }
      }
