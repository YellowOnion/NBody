{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, BangPatterns #-}

import Control.Lens
import Linear

import qualified Data.Vector.Unboxed as V
import qualified Data.Array.Repa as R
import           Data.Array.Repa (Z, (:.))
import qualified Data.Array.Repa.Eval as Eval

import Control.Monad
import Control.Monad.State

import Graphics.GPipe
import Graphics.GPipe.Context
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.UI.GLFW (swapInterval)

import System.Random
import Control.Concurrent
import Control.Concurrent.STM

import System.Mem (performGC)
import System.Clock
import Formatting
import Formatting.Clock

type PointT = Double

type Point a = V2 a
type Body a = V3 (Point a)

-- TODO: Figure out why I need this instance
instance Eval.Elt a => Eval.Elt (V2 a) where
  {-# INLINE touch #-}
  touch (V2 a b)
   = do Eval.touch a
        Eval.touch b
  {-# INLINE zero #-}
  zero = V2 Eval.zero Eval.zero
  {-# INLINE one #-}
  one  = V2 Eval.one Eval.one

origin :: Floating a => V2 a
origin = V2 0 0

type System  a = R.Array R.U R.DIM1 (Body a)
type DSystem a = R.Array R.D R.DIM1 (Body a)

-- helper for the initial seed
rotate90 :: Num a => V2 a -> V2 a
rotate90 vec = vec *! V2 (V2 0 (-1))
                         (V2 1   0)


-- Gravity Fall off function
gravity :: (Floating a, Epsilon a) => Point a -> Point a -> Point a
gravity !p1 !p2 = pd ^* s
  where
    !pd = p2 - p1
    !s = 100 / sqrt (r^3)
    !r = 0.1 + distance p2 p1
{-# INLINE gravity #-}


-- Integrate over all bodies
moveBody :: (V.Unbox a, Eval.Elt a, Floating a, Epsilon a) => a -> System a -> Body a -> Body a
moveBody !dt !bodies (V3 loc vel accel) = V3 nLoc nVel nAccel
  where
    !nLoc = loc + (vel ^* dt * 0.5) + (0.5 *^ accel ^* dt**2)
    !nAccel = R.sumAllS $ R.map (gravity nLoc . view _x) bodies
    !nVel = vel + (0.5 *^ (accel + nAccel) ^* dt)



-- run integration step for each body.
updateSystem :: (Eval.Elt a, Floating a, V.Unbox a, Epsilon a) => a -> System a -> DSystem a
updateSystem dt bodies = R.map (moveBody dt bodies) bodies

toBody :: (Epsilon a, Floating a) => Point a -> a -> a -> a -> Body a
toBody ori theta r v = V3 (loc + ori) vel (V2 0 0)
  where loc = V2 (r * 1200 * sin theta) (r * 1200 * cos theta)
        vel =  v * 1000 *^ normalize (rotate90 loc)

-- Initial seed of system.
initSystem :: (Random a, V.Unbox a, Epsilon a, Floating a, RandomGen s, MonadState s m) => a -> Int -> Point a -> m (V.Vector (Body a))
initSystem rad n ori = V.replicateM n $ do
                                       a <- getR 0  (pi*2)       -- Angle
                                       r <- getR (rad*0.7)  (rad*1.3)  -- Radius
                                       let r' = 1.5 * rad - r
                                       v <- getR (r'*3) (r'*6) -- Velocity
                                       return $ toBody ori a r v
                        where getR a b = state (randomR (a, b))
-- Render loop
renderloop win hexB posB shader systemv = lift (getTime Realtime) >>= loop
  where
    loop t = do
      system <- lift . atomically $ readTBQueue systemv
      writeBuffer posB 0 (sysToList system)
      render $ do
        clearWindowColor win (V3 0 0 0)
        hexArray <- newVertexArray hexB
        posArray <- newVertexArray posB
        let -- pArray :: PrimitiveArray p (V2 Float)
          pArray = toPrimitiveArrayInstanced TriangleFan (,) hexArray posArray
        shader pArray

      swapWindowBuffers win

      t' <- lift $ getTime Realtime
      lift $ fprint ( timeSpecs % "\n" ) t t'
      lift $ performGC

      closeRequested <- GLFW.windowShouldClose win
      case closeRequested of
        Just False -> loop t'
        _ -> return ()

systemloop systemv system = do
          system' <- runUniverseTick system
          atomically $ writeTBQueue systemv system'
          systemloop systemv system'

-- Literally in the name, converts system coords to screen coords.
toScreenCoords :: Floating a => a -> (V2 a, V2 a) -> (V4 a, V3 a)
toScreenCoords aspect (V2 xx yy, V2 a b) =  (V4 x' y' 0 1, V3 1 1 1)
              where
                x = xx+a
                y = yy+b
                x' = x * (0.5/xxx)
                y' = y * (0.5/1200)
                xxx = 1200 * aspect
{-# INLINE toScreenCoords #-}

-- Convert the Vector Body to [Point] ready for GPipe
sysToList :: (Real a, Eval.Elt a, Floating a, V.Unbox a) => System a -> [Point Float]
sysToList s = R.toList $ R.map (fmap realToFrac . view _x) s
{-# INLINE sysToList #-}

-- this removes the net velocity of the system, so it doesn't move off screen.
initUniverse :: (Eval.Elt a, Num a, V.Unbox a, Fractional a) => Int -> System a -> IO (System a)
initUniverse size sys = R.computeP univers'
  where
    uniSum = negate $ (R.sumAllS (R.map (view _y) sys)) ^/ (fromIntegral size)
    univers' = R.map (over _y $ (+ uniSum)) sys

runUniverseTick :: (Eval.Elt a, Floating a, V.Unbox a, Epsilon a) => System a -> IO (System a)
runUniverseTick sys = step sys >>= step >>= step >>= step
  where
    step = R.computeUnboxedP . updateSystem dt
    !dt = 1 / 60 / 32
{-# INLINE runUniverseTick #-}



main :: IO ()
main = do
  let wsizex = 1280
      wsizey = 720
      aspect = fromIntegral wsizex / fromIntegral wsizey
      winConf :: GLFW.WindowConfig
      winConf = GLFW.WindowConfig wsizex wsizey "NBody Sim" Nothing [] (Just 2)
      msize = 350 :: Int
      asize = 350 :: Int
      seed  = mkStdGen 45693762934762945
      size  = msize+asize
      sys = flip evalState seed $ (R.++)
                               <$> (R.fromUnboxed (R.Z R.:. msize) <$> initSystem 1.3 msize (V2   500  0))
                               <*> (R.fromUnboxed (R.Z R.:. asize) <$> initSystem 0.7 asize (V2 (-500) 0))

  univers :: System PointT <- R.computeP (sys)
  system  <- initUniverse size univers
  systemv <- atomically $ newTBQueue 3
  atomically $ writeTBQueue systemv system
  forkIO (systemloop systemv system)
  
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) winConf

    -- Create initial system state

    -- Plug the system in to a vertex array ready for rendering
    posBuffer :: Buffer os (B2 Float) <- newBuffer size
    writeBuffer posBuffer 0 (sysToList system)

    -- Lets create a bunch of "circles" to represent each body.
    let r = 15
        hex = map (\i -> let angle = 2 * pi * (fromIntegral i) / 6 in V2 (r * cos angle) ( r * sin angle) ) [0..6]
    hexBuffer :: Buffer os (B2 Float) <- newBuffer 8
    writeBuffer hexBuffer 0 $ V2 0 0 : hex

  -- Constant for aspect ratio of display
    uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1
    writeBuffer uniformBuffer 0 [aspect]

    let
      preShader :: Shader
                       os
                       (PrimitiveArray Triangles (B2 Float, B2 Float))
                       ()
      preShader = do
          primitiveStream <- toPrimitiveStream id
          aspectU <- getUniform (const (uniformBuffer, 0))
          let --pStream2 :: PrimitiveStream Triangles (V4 VFloat, V3 VFloat)
            pStream2 = toScreenCoords aspectU <$> primitiveStream
          fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 wsizex wsizey), DepthRange 0 1))
                                      pStream2
          drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream
    shader <- compileShader preShader
    renderloop win hexBuffer posBuffer shader systemv
    
