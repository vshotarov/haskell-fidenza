module Main (main) where

import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen, split, randomR)
import Codec.Picture( PixelRGBA8( .. ), writePng, Pixel8)
import Graphics.Rasterific hiding (Vector)
import Graphics.Rasterific.Linear (normalize, dot, distance, (^*))
import Graphics.Rasterific.Texture (uniformTexture)
import Debug.Trace

import ParseArgs (Args( .. ), VectorFieldGenerator( .. ), Vector,
                  helpString, parseArgs, readVectorFieldFromFile)
import qualified PerlinNoise as PNoise (noise2d)

main :: IO ()
main = do
  args <- getArgs
  case args of
    _ | any (`elem` ["-h","--help"]) args -> putStrLn helpString
    _                                     -> do
      putStrLn $ show $ parseArgs args
      fidenza $ parseArgs args

-- NOTE: Make 2 functions for fidenza. 1 to build and dump the curves
-- and another to colour and draw them, so we can experiment with different 
-- colour options without having to rerun the whole algorithm
fidenza :: Args -> IO ()
fidenza args@(Args seed
                   width
                   height
                   collisionMargin
                   padding
                   vectorFieldGenerator
                   numGenerationAttempts
                   maxSteps
                   maxCurves
                   minLength
                   chunkSizes
                   squareBlocks
                   avgBlockSize
                   -- curve generation options
                   stepLength
                   curveWidths
                   fertilities
                   skewAngles
                   -- colours
                   bgColour
                   colourScheme
                   customColours
             ) = do
  let randomGen = mkStdGen seed
  fieldFunc <- case vectorFieldGenerator of
    FromFile fp -> readVectorFieldFromFile (width,height) fp
    PerlinNoise freq ofs -> return (\(x,y) ->
        let p = ((x-fromIntegral ofs)*freq,(y-fromIntegral ofs)*freq)
            randomGen' = snd $ split randomGen
            twoPi = 2 * (22/7)
            angle = twoPi * PNoise.noise2d p width randomGen'
         in (cos angle, sin angle))
  world <- simWorld args fieldFunc randomGen (aMaxSteps args) 0 ([],[])
  ---- Visualising the vector field
  --writePng "test.png" $ renderDrawing width height (PixelRGBA8 125 125 125 255) $
  --    mapM_ (\(x,y) -> withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $ stroke 2 JoinRound (CapRound,CapRound) $
  --                        Line (V2 x y) (V2 (x + 20*(fst $ vf (x,y))) (y + 20*(snd $ vf (x,y))))) [(x,y) | x <- [5,25..(fromIntegral width-20)], y <- [5,25..(fromIntegral height-20)]]
  writePng "test.png" $ renderDrawing width height (PixelRGBA8 125 125 125 255) $
      mapM_ (\crv -> withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $
                       stroke (cWidth crv) (JoinMiter 2) (CapStraight 0, CapStraight 0) $
                           Path (head $ cPoints crv) False (map PathLineTo $ tail $ cPoints crv)) (fst world ++ snd world)

-- Point comes from Rasterific and it's a V2 Float Float
data Curve = Curve { cPoints :: [Point]
                   , cGeneration :: Int
                   , cWidth :: Float
                   , cFertility :: Int
                   , cSkewAngle :: Float }
                   deriving (Show)
type World = ( [Curve]   -- fertile
             , [Curve] ) -- infertile

simWorld :: Args               -- fidenza parameters
         -> (Vector -> Vector) -- vector field function
         -> StdGen             -- random number generator
         -> Int                -- keeping track of number of steps
         -> Int                -- curve generation a.k.a current number of curves
         -> World              -- all fertile and infertile curves
         -> IO World           -- final world
simWorld _ _ _ 0 _ world = return world <$> putStrLn "ending due to reaching max steps"
simWorld args vectorFunc randomGen steps numCurves ([],infertile)
  | numCurves < (aMaxCurves args) =
      case genCurve
            args
            vectorFunc
            randomGen
            infertile
            numCurves
            (aNumGenerationAttempts args) of
        (_, Nothing) -> return ([],infertile) <$> putStrLn "ending due to failure to spawn"
        (randomGen', Just newCurve) -> simWorld
                                         args
                                         vectorFunc
                                         randomGen'
                                         (max 0 $ steps - aMinLength args)
                                         (numCurves + 1)
                                         ([newCurve],infertile)
  | otherwise = return ([],infertile) <$> putStrLn "ending due to hitting max curves limit"
simWorld args vectorFunc randomGen steps numCurves world =
    simWorld args vectorFunc randomGen (steps-1) numCurves $
        stepWorld args vectorFunc world

stepWorld :: Args               -- fidenza parameters
          -> (Vector -> Vector) -- vector field function
          -> World              -- all fertile and infertile curves
          -> World              -- updated world
stepWorld _ _ ([],infertile) = ([],infertile)
stepWorld args vectorFunc ((firstFertile:fertile),infertile) = newWorld
    where dies crv@(Curve { cPoints=(p1:p2:_) }) = isOutOfBounds args (p2,p1,cWidth crv)
                                                || doesCollide
                                                    args
                                                    (p2,p1,cWidth crv)
                                                    (fertile ++ infertile)
          newWorld = case stepCurve args vectorFunc firstFertile of
                       (old, Nothing)  -> (fertile, old:infertile)
                       (old, Just new) -> if dies new
                                             then (fertile, old:infertile)
                                             else (new:fertile, infertile)

type LineSeg = (Point, Point, Float) -- the Float is for width
isOutOfBounds :: Args    -- fidenza parameters
              -> LineSeg -- line segment to check
              -> Bool
isOutOfBounds args (p1, p2, w) = isPtOutOfBounds p1 || isPtOutOfBounds p2
    where -- take the dot products of the normal with the axis
          dotx = abs $ dot (normalize $ p2 - p1) (V2 0 1)
          doty = abs $ dot (normalize $ p2 - p1) (V2 1 0)
          (width,height,padding) = (aWidth args, aHeight args, aPadding args)
          paddingX = fromIntegral padding + dotx * (w / 2)
          paddingY = fromIntegral padding + doty * (w / 2)
          paddedX = fromIntegral width - paddingX
          paddedY = fromIntegral height - paddingY
          isPtOutOfBounds (V2 x y) = x < paddingX || x > paddedX
                                  || y < paddingY || y > paddedY

doesCollide :: Args    -- fidenza parameters
            -> LineSeg -- line segment to check
            -> [Curve] -- list of colliders
            -> Bool
doesCollide _ _ [] = False
doesCollide args lineSeg (crv:crvs) =
    (any (doesLineSegCollide args lineSeg) $ curveToLineSegs crv)
 || doesCollide args lineSeg crvs

curveToLineSegs :: Curve -> [LineSeg]
curveToLineSegs crv = go (cPoints crv) (cWidth crv)
  where go points _ | length points < 2 = []
        go (p1:p2:points) w = (p1,p2,w):(go (p2:points) w)

doesLineSegCollide :: Args    -- fidenza parameters
                   -> LineSeg -- line segment p
                   -> LineSeg -- line segment s
                   -> Bool
doesLineSegCollide args p@(p1, p2, pw) s@(s1, s2, sw) = dist < minDist
  where p2OnS = closestPtOnLineSeg p2 s
        dist = distance p2 p2OnS
        dv = normalize (p2 - p2OnS)
        normal (V2 ax ay) (V2 bx by) = normalize $ (V2 (-(by - ay)) (bx - ax))
        pn = normal p1 p2
        sn = normal s1 s2
        margin = fromIntegral $ aCollisionMargin args
        minDist = margin + (abs $ dot pn dv) * (pw / 2)
                         + (abs $ dot sn dv) * (sw / 2)

closestPtOnLineSeg :: Point   -- point to find closest one on line seg to
                   -> LineSeg -- line seg
                   -> Point
closestPtOnLineSeg p (p1, p2, _) = p1 + v ^* t'
    where v@(V2 vx vy) = p2 - p1
          l2 = vx^2 + vy^2
          t = (dot (p - p1) v) / l2
          t' = min (max 0 t) 1

stepCurve :: Args                 -- fidenza parameters
          -> (Vector -> Vector)   -- vector field function
          -> Curve                -- curve to grow
          -> (Curve, Maybe Curve) -- old curve and maybe a new one
stepCurve _ _ curve@(Curve { cFertility = 0 }) = (curve,Nothing)
stepCurve _ _ (Curve { cPoints = [] }) = error "Can't step empty curve"
stepCurve args vectorFunc curve@(Curve { cPoints = ((V2 x y):ps)}) = (curve, Just newCurve)
    where (x',y') = vectorFunc (x,y)
          len = aStepLength args
          newPoint = V2 (x + len * x') (y + len * y')
          newCurve = curve { cPoints = newPoint:(V2 x y):ps
                           , cFertility = cFertility curve - 1 }

genCurve :: Args               -- fidenza parameters
         -> (Vector -> Vector) -- vector field function
         -> StdGen             -- random number generator
         -> [Curve]            -- collision curves
         -> Int                -- number of curves
         -> Int                -- number of generation attempts before giving up
         -> (StdGen, Maybe Curve)
genCurve _ _ randomGen _ _ 0 = (randomGen, Nothing)
genCurve args vectorFunc randomGen others generation numAttempts = output
    where (width,height,padding) = (aWidth args, aHeight args, aPadding args)
          paddingF = fromIntegral padding
          (paddedX,paddedY) = (fromIntegral width - paddingF
                              ,fromIntegral height - paddingF)
          -- generate curve properties
          (x,randomGen1) = randomR (paddingF,paddedX) randomGen :: (Float,StdGen)
          (y,randomGen2) = randomR (paddingF,paddedY) randomGen1 :: (Float,StdGen)
          (w,randomGen3) = getRandomElem randomGen2 $ aCurveWidths args :: (Float,StdGen)
          (f,randomGen4) = getRandomElem randomGen3 $ aFertilities args :: (Int,StdGen)
          (sw,randomGen') = getRandomElem randomGen4 $ aSkewAngles args :: (Float,StdGen)
          -- generate curve
          curve = Curve { cPoints = [V2 x y]
                        , cGeneration = generation
                        , cWidth = w
                        , cFertility = f
                        , cSkewAngle = sw }
          -- step it minLength number of times, to see if it survives
          curveAfterMinLength = take (aMinLength args)
                              . takeWhile ((==1) . length . fst)
                              $ iterate (stepWorld args vectorFunc) ([curve],others)
          survives = (length curveAfterMinLength == (aMinLength args))
                  && ((==1) . length . fst $ last curveAfterMinLength)
          output = if survives then (randomGen', Just $ head . fst $ last curveAfterMinLength)
                               else genCurve
                                     args
                                     vectorFunc
                                     randomGen'
                                     others
                                     generation
                                     (numAttempts - 1)

getRandomElem :: StdGen          -- random number generator
              -> [a]             -- list of elements
              -> (a,StdGen)      -- random element
getRandomElem randomGen elements = (elements !! randomIndex, randomGen')
    where (randomIndex,randomGen') = randomR (0, length elements - 1) randomGen
