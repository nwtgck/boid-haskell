{-# LANGUAGE Arrows #-}

import           FRP.Yampa
import           Graphics.UI.GLUT

width = 600
height = 400
boidSize = 5

mainSF :: SF a (IO ())
mainSF = constant (draw)

draw :: IO ()
draw = do
  clear [ColorBuffer]
  loadIdentity
  ortho 0 width 0 height 0 400

  preservingMatrix $ do
    translate (Vector3 (20) (40) 0 :: Vector3 Double)
    renderObject Solid $ Cube boidSize


  preservingMatrix $ do
    translate (Vector3 (80) (100) 0 :: Vector3 Double)
    renderObject Solid $ Cube boidSize

  renderObject Solid $ Cube boidSize

  flush
  return ()

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

idle :: ReactHandle () (IO ()) -> IO ()
idle rh = do
  react rh (0.06, Just ())
  return ()

initGL :: IO ()
initGL = do
  let scale = 1
  initialWindowSize $= Size (scale * truncate width) (scale * truncate height)
  getArgsAndInitialize
  createWindow "Boid"
  initialDisplayMode $= [RGBAMode]
  clearColor $= Color4 0 0 0 0

main :: IO ()
main = do
  rh <- reactInit (initGL) (\_ _ b -> b >> return False) mainSF
  displayCallback $= return ()
  reshapeCallback $= Just reshape
  idleCallback    $= Just (idle rh)
  mainLoop
