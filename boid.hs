{-# LANGUAGE Arrows #-}

import           Control.Monad
import           FRP.Yampa
import           Graphics.UI.GLUT
import           System.Random

data Boid = Boid{pos :: (Double, Double), vel :: (Double, Double)}

width = 600
height = 400
boidSize = 5
numBoids = 100

mainSF :: [Boid] -> SF () (IO ())
mainSF boids = proc _ -> do
  out <- arr draw -< boids
  returnA -< out

draw :: [Boid] -> IO ()
draw boids = do
  clear [ColorBuffer]
  loadIdentity
  ortho 0 width 0 height 0 400

  forM_ boids $ \Boid{pos=(x, y)} -> do
    preservingMatrix $ do
      translate (Vector3 x y 0 :: Vector3 Double)
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

-- initBoids :: [Boid]
-- initBoids = replicate numBoids (Boid (100, 100) (0, 0))

randomBoids :: Int -> IO [Boid]
randomBoids 0 = return []
randomBoids n = do
  g1 <- newStdGen
  let (x,g2) = randomR (0, width) g1
  let (y,_) = randomR (0, height) g2
  rest <- randomBoids (n-1)
  return $ (Boid (x, y) (0, 0)) : rest

main :: IO ()
main = do
  initBoids <- randomBoids numBoids
  rh <- reactInit (initGL) (\_ _ b -> b >> return False) (mainSF initBoids)
  displayCallback $= return ()
  reshapeCallback $= Just reshape
  idleCallback    $= Just (idle rh)
  mainLoop
