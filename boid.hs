{-# LANGUAGE Arrows #-}

import           Control.Monad
import           FRP.Yampa
import           Graphics.UI.GLUT
import           System.Random

newtype Id = Id Int deriving Eq
data Boid = Boid{ident :: Id, pos :: (Double, Double), vel :: (Double, Double)}

width = 700
height = 500
boidSize = 3
numBoids = 1000
maxSpeed = 10

mainSF :: [Boid] -> SF () (IO ())
mainSF boids = loopPre boids coreSF
  where
    coreSF :: SF ((), [Boid]) (IO(), [Boid])
    coreSF = proc (_, boids) -> do
      nextBoids <- move -< boids
      drawed   <- arr draw -< boids
      returnA -< (drawed, nextBoids)

move :: SF [Boid] [Boid]
move  = proc boids -> do
  let rule1Boids = map (rule1 boids) boids
  let nextBoids = forwardBoids rule1Boids
  returnA -< nextBoids

forwardBoids :: [Boid] -> [Boid]
forwardBoids [] = []
forwardBoids (b@Boid{pos=(x,y), vel=(vx,vy)}:bs) =
  let speed = sqrt (vx * vx + vy * vy)
      (newVx, newVy) = if (speed >= maxSpeed)
        then let r = maxSpeed / speed in (vx * r, vy * r)
        else (vx, vy)
      newVx2 = if (x < 0 && newVx < 0 || x > width && newVx > 0) then -newVx else newVx
      newVy2 = if (y < 0 && newVy < 0 || y > height && newVy > 0) then -newVy else newVy

  in b{pos=(x+newVx2, y+newVy2)}: forwardBoids bs


rule1 :: [Boid] -> Boid -> Boid
rule1 boids boid@(Boid{ident=ident,pos=(x, y), vel=(vx,vy)}) =
  let (sumX, sumY) = sumBoids boids
      (cx, cy) = (sumX / fromIntegral (length boids - 1), sumY / fromIntegral(length boids - 1))
  in boid{vel=(vx+ (cx-x)/50, vy+ (cy-y)/50)}
  where
    sumBoids :: [Boid] -> (Double, Double) -- TODO 自分以外のsumにしないといけない
    sumBoids [] = (0, 0)
    sumBoids (Boid{ident=ident2, pos=(x, y)}:bs) =
      let (sx, sy) = sumBoids bs
      in if ident==ident2 then (sx, sy) else (x+sx, y+sy)

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
  react rh (0.1, Just ())
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
  return $ (Boid (Id n) (x, y) (0, 0)) : rest

main :: IO ()
main = do
  initBoids <- randomBoids numBoids
  rh <- reactInit (initGL) (\_ _ b -> b >> return False) (mainSF initBoids)
  displayCallback $= return ()
  reshapeCallback $= Just reshape
  idleCallback    $= Just (idle rh)
  mainLoop
