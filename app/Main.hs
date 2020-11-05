{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Minmaxer
import SDL
import Linear (V4(..))
import SDL.Image
import Control.Monad.IO.Class
import SDL.Vect
import qualified Data.Vector as V
import Control.Concurrent as C
import Data.Int
import Foreign.C.Types
import Control.Monad
import qualified Data.ByteString
import Data.FileEmbed

data MousePosition = MousePosition (Point V2 Int32) deriving (Show)

textureFile :: Data.ByteString.ByteString
textureFile = $(embedFile "assets/textures.png")


loadTexturePool :: MonadIO m => Renderer -> m Texture
loadTexturePool r = decodeTexture r textureFile

mousePosition :: (Event -> Bool) -> [Event] -> MousePosition
mousePosition p es = let
        evsContainingMouseButEvs = filter p es
        firstEv = eventPayload (head evsContainingMouseButEvs)
        MouseMotionEvent firstMouseMotEv = firstEv
        mousePos = mouseMotionEventPos firstMouseMotEv
    in MousePosition mousePos

--leftVolRect :: Mixer -> Maybe (Rectangle (Point V2 Int) (V2 Int))
volToPos :: Int -> Int
volToPos v = let minPos = 550
                 maxPos = 120
                 coef = (toRational (minPos - maxPos))/100
             in minPos - round((fromIntegral v)*fromRational(coef))
                 --in minPos - (round (coef*v))

posToVol :: Int -> Int
posToVol v = let minPos = 550
                 maxPos = 120
                 coef = (toRational (minPos - maxPos))/100
                 adjusted_v = 550 - (clamp maxPos minPos v)
             in round((fromIntegral adjusted_v)/fromRational(coef))
                 --in minPos - (round (coef*v))


data GuiVolumeParams = GuiVolumeParams {
                        x_offset :: Int 
                        , device :: String
                        , side :: VolumeSide
                        } deriving (Eq, Show)

volRect :: Mixer -> Int -> String -> VolumeSide -> Maybe (Rectangle CInt)
volRect mixer x_offset device side = do
    vols <- findVolume device mixer
    let vol = if side == LeftV then (left vols) else (right vols)
    let volPos = volToPos vol
    let rectPos = P (V2 (CInt (fromIntegral (x_offset - 22))) (CInt (fromIntegral (volPos - 22))))
    let rectSize = V2 40 44
    return (Rectangle rectPos rectSize)

drawMixer :: Renderer -> Texture -> Mixer -> IO ()
drawMixer ren txt mixer = do
  let button_rect = Rectangle (P (V2 0 600)) (V2 40 45)
  let x_offsets = [64, 128, 266, 330]
  let devices = ["vol", "vol", "mic", "mic"]
  let sides = [LeftV, RightV, LeftV, RightV]
  let dst_rects = zipWith3 (volRect mixer) x_offsets devices sides
  mapM_ (copy ren txt (Just button_rect)) dst_rects

eventIsLeftClickPress event =
        case eventPayload event of
          MouseButtonEvent mouseEvent ->
            mouseButtonEventMotion mouseEvent == Pressed &&
            mouseButtonEventButton mouseEvent == ButtonLeft
          _ -> False

eventIsLeftClickMotion event =
        case eventPayload event of
          MouseMotionEvent mouseEvent ->
            any (\x -> x == ButtonLeft) $ mouseMotionEventState (mouseEvent)
          _ -> False

leftClickMotionPos event =
        case eventPayload event of
          MouseMotionEvent mouseEvent ->
            any (\x -> x == ButtonLeft) $ mouseMotionEventState (mouseEvent)
          _ -> False


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minmaxer" defaultWindow {windowInitialSize = V2 394 600}
  renderer <- createRenderer window (-1) defaultRenderer
  texturePool <- loadTexturePool renderer
  mixerOrError <- readCurrentMixer
  case mixerOrError of
    (Left error) -> putStrLn "Error reading/parsing mixer"
    (Right mixer) -> do
      putStrLn (show mixer)
      appLoop renderer texturePool mixer

data VolumeSetting = VolSet {volDev :: String, volSide :: VolumeSide, volValue :: Int} deriving (Show, Eq)

mkVolSet :: (Integral a) => a -> a -> VolumeSetting
mkVolSet xpos ypos
    | xpos <= 84 = VolSet "vol" LeftV vol
    | xpos <= 108 = VolSet "vol" BothV vol
    | xpos <= 180 = VolSet "vol" RightV vol
    | xpos <= 286 = VolSet "mic" LeftV vol
    | xpos <= 310 = VolSet "mic" BothV vol
    | xpos <= 394 = VolSet "mic" RightV vol
    | otherwise = VolSet "dummy" LeftV vol
    where vol = posToVol (fromIntegral ypos)

appLoop :: Renderer -> Texture -> Mixer -> IO ()
appLoop renderer texturePool mixer = do
  events <- pollEvents
  let eventIsEscapePress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
          _ -> False
      escPressed = any eventIsEscapePress events
      leftClick = any eventIsLeftClickMotion events
      mouse_click_pos = mousePosition eventIsLeftClickMotion events
  let new_mixer = if leftClick then
                        let MousePosition (P (V2 x y)) = mouse_click_pos
                            volSetting = mkVolSet x y
                            dev = volDev volSetting
                            volside = volSide volSetting
                            volvalue = volValue volSetting
                        in setVolume mixer dev volside volvalue
                  else mixer
  if leftClick then applyMixer new_mixer else return ()
  rendererDrawColor renderer $= V4 36 28 28 255
  clear renderer
  let src_rect = Rectangle (P (V2 0 0)) (V2 394 600)
  copy renderer texturePool (Just src_rect) Nothing
  drawMixer renderer texturePool new_mixer
  present renderer
  C.threadDelay 64000
  unless escPressed (appLoop renderer texturePool new_mixer)
