module Main (main) where

import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen, split, randomR, randomRs)
import Codec.Picture( PixelRGBA8( .. ), writePng, Pixel8)
import Graphics.Rasterific hiding (Vector)
import Graphics.Rasterific.Linear (normalize, dot, distance, (^*))
import Graphics.Rasterific.Texture (uniformTexture)
import Debug.Trace

import qualified Data.Map as Map (Map, fromList, insert)

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
                   -- drawing
                   chunksOverlap
                   bgColour
                   aColours
             ) = do
  let randomGen = mkStdGen seed
  let (noiseRandomGen,worldRandomGen) = split randomGen
  fieldFunc <- case vectorFieldGenerator of
    FromFile fp -> readVectorFieldFromFile (width,height) fp
    PerlinNoise freq ofs -> return (\(x,y) ->
        let p = ((x-fromIntegral ofs)*freq,(y-fromIntegral ofs)*freq)
            twoPi = 2 * (22/7)
            angle = twoPi * PNoise.noise2d p width noiseRandomGen
         in (cos angle, sin angle))
  world <- simWorld args fieldFunc worldRandomGen (aMaxSteps args) 0 ([],[])
  let (chunkingRandomGen,colourRandomGen) = split randomGen
  let curves = toCurves args chunkingRandomGen . toFamilies $ fst world ++ snd world
  let colouredCurves = colourCurves args colourRandomGen curves
  ---- Visualising the vector field
  --writePng "test.png" $ renderDrawing width height (PixelRGBA8 125 125 125 255) $
  --    mapM_ (\(x,y) -> withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $ stroke 2 JoinRound (CapRound,CapRound) $
  --                        Line (V2 x y) (V2 (x + 20*(fst $ vf (x,y))) (y + 20*(snd $ vf (x,y))))) [(x,y) | x <- [5,25..(fromIntegral width-20)], y <- [5,25..(fromIntegral height-20)]]
  writePng "test.png" $ renderDrawing width height (aBgColour args) $
      mapM_ (\(curve,colour) ->  withTexture (uniformTexture colour) $
                           --stroke 1 (JoinRound) (CapStraight 0, CapStraight 0) $
                             fill $ 
                               sweepRectOnCurve curve) colouredCurves

data LineSeg = LineSeg { lsP1 :: Point
                       , lsP2 :: Point
                       , lsFamily :: Int
                       , lsWidth :: Float
                       , lsFertility :: Int
                       , lsSkewAngle :: Float}
                         deriving stock (Show)
type Curve = [LineSeg]
type ColouredCurve = (Curve, PixelRGBA8)
sweepRectOnCurve :: Curve
                 -> Path
sweepRectOnCurve lineSegs =
  Path firstTop True . map PathLineTo $ topPoints ++ (reverse $ firstBot:botPoints)
  where chamfer (LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = w, lsSkewAngle = angle }) =
            ((p1+perp,p1-perp),(p2+perp,p2-perp))
            where (V2 x y) = p2 - p1
                  (V2 px py) = (normalize $ (V2 (-y) x)) ^* (w/2)
                  perp = V2 (cos angle * px - sin angle * py)
                            (sin angle * px + cos angle * py)
        (firstTop,firstBot) = fst . chamfer $ head lineSegs
        (topPoints,botPoints) = foldr
                                  (\lineSeg (topAcc,botAcc) ->
                                    let (top,bot) = snd $ chamfer lineSeg
                                     in (top:topAcc,bot:botAcc)) ([],[]) lineSegs

colourCurves :: Args
             -> StdGen
             -> [Curve]
             -> [ColouredCurve]
colourCurves _ _ [] = []
colourCurves args randomGen curves = zip curves colours
  where colours = map (aColours args !!)
                $ randomRs (0, length (aColours args) - 1) $ snd $ split randomGen

toCurves :: Args
         -> StdGen
         -> [[LineSeg]]
         -> [Curve]
toCurves _ _ [] = []
toCurves args randomGen families = concat chunks
  where toChunks _ [] = []
        toChunks randomGen' lineSegs = if length lineSegs < chunkSize'
                                       then [lineSegs]
                                       else chunk:(toChunks randomGen'' remainder)
          where squareChunkSize = round . lsWidth $ head lineSegs
                avgChunkSize = sum (aChunkSizes args) `div` length (aChunkSizes args)
                (chunkSize,randomGen'') = getRandomElem randomGen' (aChunkSizes args)
                distToSquareChunkSize = fromIntegral $ squareChunkSize - chunkSize
                distToAvgChunkSize = fromIntegral $ avgChunkSize - chunkSize
                chunkSize' = chunkSize
                           + (round $ distToSquareChunkSize * aSquareBlocks args)
                           + (round $ distToAvgChunkSize * aAvgBlockSize args)
                chunk = take chunkSize' lineSegs
                remainder = drop (chunkSize' - aChunksOverlap args) lineSegs
        chunks = map (uncurry toChunks)
               $ zip (iterate (fst . split) randomGen) families

toFamilies ::[LineSeg] -- list of line segs to group by family
          -> [[LineSeg]]
toFamilies [] = []
toFamilies (a:[]) = [[a]]
toFamilies allLineSegs = if length firstFamily > 1 then firstFamily:families
                                                   else families
  where familyFold [] lineSeg = [[lineSeg]]
        familyFold ((first:siblings):others) lineSeg
          | lsFamily first == lsFamily lineSeg = (lineSeg:first:siblings):others
          | otherwise                          = [lineSeg]:(first:siblings):others
        (firstFamily:families) = foldl familyFold [] allLineSegs

type World = ( [LineSeg]   -- fertile
             , [LineSeg] ) -- infertile
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
      case genLineSeg
            args
            vectorFunc
            randomGen
            infertile
            numCurves
            (aNumGenerationAttempts args) of
        (_, Nothing) -> return ([],infertile) <$> putStrLn "ending due to failure to spawn"
        (randomGen', Just newLineSeg) -> simWorld
                                           args
                                           vectorFunc
                                           randomGen'
                                           (steps - 1)
                                           (numCurves + 1)
                                           ([newLineSeg],infertile)
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
    where dies lineSeg = isOutOfBounds args lineSeg || doesCollide
                                                         args
                                                         lineSeg
                                                         (fertile ++ infertile)
          newWorld = case stepLineSeg args vectorFunc firstFertile of
                       (old, Nothing)  -> (fertile, old:infertile)
                       (old, Just new) -> if dies new
                                             then (fertile, old:infertile)
                                             else (new:fertile, old:infertile)

isOutOfBounds :: Args    -- fidenza parameters
              -> LineSeg -- line segment to check
              -> Bool
isOutOfBounds args (LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = w }) =
  isPtOutOfBounds p1 || isPtOutOfBounds p2
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

doesCollide :: Args      -- fidenza parameters
            -> LineSeg   -- line segment to check
            -> [LineSeg] -- list of colliders
            -> Bool
doesCollide _ _ [] = False
doesCollide args a (b:others) | lsFamily a == lsFamily b = doesCollide args a others
doesCollide args lineSeg others = any (doesLineSegCollide args lineSeg) others

doesLineSegCollide :: Args    -- fidenza parameters
                   -> LineSeg -- line segment p
                   -> LineSeg -- line segment s
                   -> Bool
doesLineSegCollide args p@(LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = pw })
                        s@(LineSeg { lsP1 = s1, lsP2 = s2, lsWidth = sw }) = dist < minDist
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
closestPtOnLineSeg p (LineSeg { lsP1 = p1, lsP2 = p2 }) = p1 + v ^* t'
    where v@(V2 vx vy) = p2 - p1
          l2 = vx^2 + vy^2
          t = (dot (p - p1) v) / l2
          t' = min (max 0 t) 1

stepLineSeg :: Args                     -- fidenza parameters
            -> (Vector -> Vector)       -- vector field function
            -> LineSeg                  -- line seg to grow
            -> (LineSeg, Maybe LineSeg) -- old line seg and potentially a new one
stepLineSeg _ _ ls@(LineSeg { lsFertility = 0 }) = (ls,Nothing)
stepLineSeg args vectorFunc ls@(LineSeg { lsP2 = V2 x y }) = (ls, Just newLineSeg)
    where (x',y') = vectorFunc (x,y)
          len = aStepLength args
          newPoint = V2 (x + len * x') (y + len * y')
          newLineSeg = ls { lsP1 = V2 x y, lsP2 = newPoint,
                            lsFertility = lsFertility ls - 1 }

genLineSeg :: Args               -- fidenza parameters
           -> (Vector -> Vector) -- vector field function
           -> StdGen             -- random number generator
           -> [LineSeg]          -- collision line segments
           -> Int                -- number of curves
           -> Int                -- number of generation attempts before giving up
           -> (StdGen, Maybe LineSeg)
genLineSeg _ _ randomGen _ _ 0 = (randomGen, Nothing)
genLineSeg args vectorFunc randomGen others generation numAttempts = output
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
          -- generate line segment
          (vx, vy) = vectorFunc (x,y)
          lineSeg = LineSeg { lsP1 = V2 x y
                            , lsP2 = V2 (x+aStepLength args * vx) (y+aStepLength args * vy)
                            , lsFamily = generation
                            , lsWidth = w
                            , lsFertility = f
                            , lsSkewAngle = sw }
          -- step it minLength number of times, to see if it survives
          curveAfterMinLength = take (aMinLength args)
                              . takeWhile ((==1) . length . fst)
                              $ iterate (stepWorld args vectorFunc) ([lineSeg],others)
          survives = (length curveAfterMinLength == (aMinLength args))
                  && ((==1) . length . fst $ last curveAfterMinLength)
          output = if survives then (randomGen', Just lineSeg)
                               else genLineSeg
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
