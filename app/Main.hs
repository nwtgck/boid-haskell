{-# LANGUAGE Arrows #-}

module Main where


import           Control.Concurrent
import           Control.Monad
import           FRP.Yampa
import           Graphics.UI.GLUT
import           System.Random

newtype Id = Id Int deriving Eq
data Boid = Boid{ident :: Id, pos :: (Double, Double), vel :: (Double, Double)}

width = 700
height = 700
boidSize = 3
numBoids = 100
maxSpeed = 7

mainSF :: [Boid] -> SF () (IO ())
mainSF boids = loopPre boids coreSF
  where
    coreSF :: SF ((), [Boid]) (IO(), [Boid])
    coreSF = proc (_, boids) -> do
      nextBoids <- arr move -< boids
      drawed   <- arr draw -< boids
      returnA -< (drawed, nextBoids)

move :: [Boid] -> [Boid]
-- move boids = map (forwardBoid . rule3 boids . rule2 boids . rule1 boids) boids
move [] = []
move (b:bs) = _move [] b bs
  where
    transfer :: [Boid] -> Boid -> Boid
    transfer boids = (forwardBoid .  rule3 boids . rule2 boids . rule1 boids)

    _move :: [Boid] -> Boid -> [Boid] -> [Boid]
    _move moved b [] = (transfer moved b): moved
    _move moved b (next:nonMoved) =
      let boids = moved ++ (next:nonMoved)
          newB =  transfer boids b
      in _move (newB:moved) next nonMoved

forwardBoid :: Boid -> Boid
forwardBoid b@Boid{pos=(x,y), vel=(vx,vy)} =
  let speed = sqrt (vx ** 2 + vy ** 2)
      (newVx, newVy) = if (speed >= maxSpeed)
        then let r = maxSpeed / speed in (vx * r, vy * r)
        else (vx, vy)
      -- (newVx, newVy) = (vx, vy)
      newVx2 = if (x < 0 && newVx < 0 || x > width && newVx > 0) then -newVx else newVx
      newVy2 = if (y < 0 && newVy < 0 || y > height && newVy > 0) then -newVy else newVy

  in b{pos=(x+newVx2, y+newVy2), vel=(newVx2, newVy2)}

-- forwardBoids :: [Boid] -> [Boid]
-- forwardBoids [] = []
-- forwardBoids (b@Boid{pos=(x,y), vel=(vx,vy)}:bs) =
--   let speed = sqrt (vx ** 2 + vy ** 2)
--       (newVx, newVy) = if (speed >= maxSpeed)
--         then let r = maxSpeed / speed in (vx * r, vy * r)
--         else (vx, vy)
--       -- (newVx, newVy) = (vx, vy)
--       newVx2 = if (x < 0 && newVx < 0 || x > width && newVx > 0) then -newVx else newVx
--       newVy2 = if (y < 0 && newVy < 0 || y > height && newVy > 0) then -newVy else newVy
--
--   in b{pos=(x+newVx2, y+newVy2)}: forwardBoids bs

-- 群れの中心にあつまる
rule1 :: [Boid] -> Boid -> Boid
rule1 boids boid@(Boid{ident=ident,pos=(x, y), vel=(vx,vy)}) =
  let (sumX, sumY) = sumBoids boids
      size = fromIntegral (length boids)
      (cx, cy) = (sumX / size, sumY / size)
  in boid{vel=(vx+ (cx-x)/100, vy+ (cy-y)/100)}
  where
    sumBoids :: [Boid] -> (Double, Double)
    sumBoids [] = (0, 0)
    sumBoids (Boid{ident=ident2, pos=(x, y)}:bs) =
      let (sx, sy) = sumBoids bs
      in if ident==ident2 then (sx, sy) else (x+sx, y+sy)

-- 近いと離れる
rule2 :: [Boid] -> Boid -> Boid
rule2 boids boid@(Boid{ident=ident, pos=(x,y), vel=(vx,vy)}) =
  let (newVx, newVy) = foldl updateVel (vx, vy) boids
  in boid{vel=(newVx, newVy)}
  where
    updateVel :: (Double, Double) -> Boid -> (Double, Double)
    updateVel (nowVx, nowVy) b2@Boid{ident=ident2, pos=(x2, y2)}  =
      let d = distance boid b2
      in if ident /= ident2 && d < 5 then (nowVx-(x2-x), nowVy-(y2-y)) else (nowVx, nowVy)

    distance :: Boid -> Boid -> Double
    distance Boid{pos=(x1, y1)} Boid{pos=(x2, y2)} =
      sqrt ((x1-x2)**2 + (y1-y2)**2)

-- 同じ向きに合わせる（？）
rule3 :: [Boid] -> Boid -> Boid
rule3 boids boid@(Boid{ident=ident,pos=(x,y),vel=(vx,vy)}) =
  let (sumVx, sumVy) = sumBoidVels boids
      size = fromIntegral $ length boids
      (avgVx, avgVy) = (sumVx / size, sumVy / size)
  in boid{vel=(vx+(avgVx-vx)/8, vy+(avgVy-vy)/8)}
  where
    sumBoidVels :: [Boid] -> (Double, Double)
    sumBoidVels [] = (0, 0)
    sumBoidVels (Boid{ident=ident2, vel=(vx, vy)}:bs) =
      let (svx, svy) = sumBoidVels bs
      in if ident==ident2 then (svx, svy) else (vx+svx, vy+svy)

    -- sumBoidVels = foldl (\(svx, svy) Boid{vel=(vx, vy)} -> (vx+svx, vy+svy)) (0, 0)

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
  -- threadDelay 10000
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
  return $ (Boid (Id n) (x, y) (1, 1)) : rest

main :: IO ()
main = do
  initBoids <- randomBoids numBoids
  rh <- reactInit (initGL) (\_ _ b -> b >> return False) (mainSF initBoids)
  displayCallback $= return ()
  reshapeCallback $= Just reshape
  idleCallback    $= Just (idle rh)
  mainLoop

