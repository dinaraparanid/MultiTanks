{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Client
import           Control.Monad   hiding (mapM_)
import           Data.Foldable   ()
import           Data.Maybe
import           Foreign.C.Types
import           Prelude         hiding (any, mapM_)
import qualified SDL
import           SDL             (($=))
import           SDL.Vect

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture =
  Texture SDL.Texture (V2 CInt)

createBlank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
createBlank r sz access =
  Texture <$> SDL.createTexture r SDL.RGBA8888 access sz <*> pure sz

renderTexture ::
     SDL.Renderer
  -> Texture
  -> Point V2 CInt
  -> Maybe (SDL.Rectangle CInt)
  -> Maybe CDouble
  -> Maybe (Point V2 CInt)
  -> Maybe (V2 Bool)
  -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
  let dstSize = maybe size (\(SDL.Rectangle _ size') -> size') clip
   in SDL.copyEx
        r
        t
        clip
        (Just (SDL.Rectangle xy dstSize))
        (fromMaybe 0 theta)
        center
        (fromMaybe (pure False) flips)

setAsRenderTarget :: SDL.Renderer -> Maybe Texture -> IO ()
setAsRenderTarget r Nothing              = SDL.rendererRenderTarget r $= Nothing
setAsRenderTarget r (Just (Texture t _)) = SDL.rendererRenderTarget r $= Just t

drawSample :: IO ()
drawSample = do
  SDL.initializeAll
  window <-
    SDL.createWindow
      "MultiTanks"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }
  targetTexture <-
    createBlank
      renderer
      (SDL.V2 screenWidth screenHeight)
      SDL.TextureAccessTarget
  let screenCenter = SDL.P (SDL.V2 (screenWidth `div` 2) (screenHeight `div` 2))
      rectPic = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        setAsRenderTarget renderer (Just targetTexture)
        SDL.rendererDrawColor renderer $=
          SDL.V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= SDL.V4 maxBound 0 0 maxBound
        SDL.fillRect renderer $
          Just $
          SDL.Rectangle
            (SDL.P $ SDL.V2 (screenWidth `div` 4) (screenHeight `div` 4))
            (SDL.V2 (screenWidth `div` 2) (screenHeight `div` 2))
        setAsRenderTarget renderer Nothing
        renderTexture
          renderer
          targetTexture
          0
          Nothing
          (Just 0)
          (Just screenCenter)
          Nothing
        SDL.present renderer
        unless quit rectPic
  rectPic
  SDL.destroyWindow window
  SDL.quit

main :: IO ()
main = Client.sendShoot (2, 3) (4, 5)
