module Main (main) where

import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen, split, randomR, randomRs, randoms, random)
import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Linear (normalize, dot, distance, (^*), (^/))
import Graphics.Rasterific.Texture (uniformTexture)
import Data.List (sort)
import Data.Fixed (mod')
import Debug.Trace()

import ParseArgs (Args( .. ), helpString, parseArgs)
import VectorFieldGenerator (VectorFieldGenerator(..), VectorFieldFunc,
                             readVectorFieldFromFile)
import Distribution (sampleDistribution)

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
fidenza args@(Args { aSeed = seed
                   , aWidth = width
                   , aHeight = height
                   , aVectorFieldGenerator = vectorFieldGenerator
                   , aVectorFieldStepPiDivisor = stepPiDivisor
                   , aRotationOffset = rotationOffset
                   , aMaxCurves = maxCurves
                   , aWidths = widths
                   , aStrokeOrFill = strokeOrFill
                   , aOutlineColour = outlineColour
                   , aOutlineSize = outlineSize
                   , aDrawSoftly = drawSoftly
                   , aSoftSeed = softSeed
                   , aSoftPerlinFreq = softFreq
                   , aSoftPerlinOfs = softOfs }) = do
  let randomGen = mkStdGen seed
  let (noiseRandomGen,randomGen') = split randomGen
  let (worldRandomGen,widthRandomGen) = split randomGen'
  let (_,chunkSizesRandomGen) = split worldRandomGen
  fieldFunc <- case vectorFieldGenerator of
    FromFile fp -> readVectorFieldFromFile (width,height) fp
    PerlinNoise freq ofs -> return (\(x,y) ->
        let p = ((x-fromIntegral ofs)*freq,(y-fromIntegral ofs)*freq)
            twoPi = 2 * pi
            angle = rotationOffset + twoPi * PNoise.noise2d p width noiseRandomGen
            angle' = if stepPiDivisor == 0
                     then angle
                     else angle - (angle `mod'` (pi / (fromIntegral stepPiDivisor)))
         in (cos angle', sin angle'))
    Horizontal -> return (\_ -> (cos rotationOffset,sin rotationOffset))
    Straight ->
        let angle = rotationOffset + 2 * pi * (fst $ random noiseRandomGen)
         in return (\_ -> (cos angle, sin angle))
  let softRandomGen = mkStdGen softSeed
  let softFieldFunc (x,y) =
        let p = ((x-softOfs)*softFreq,(y-softOfs)*softFreq)
            twoPi = 2 * (22/7)
            angle = twoPi * PNoise.noise2d p width softRandomGen
         in (cos angle, sin angle)
  let widths' = reverse
              . sort
              . take maxCurves
              . map (sampleDistribution widths)
              $ randoms widthRandomGen
  let interweave [] _ = []
      interweave _ [] = []
      interweave (a:as) bs = a:(interweave bs as)
  let widths'' = interweave widths' (reverse widths')
  let chunkSizes = take maxCurves
                 $ map (sampleDistribution $ aChunkSizes args)
                 $ randoms chunkSizesRandomGen
  let args' = args { _aCurveWidths = widths'', _aChunkSizes = chunkSizes }
  world <- simWorld args' fieldFunc worldRandomGen (aMaxSteps args') 0 ([],[])
  let (chunkingRandomGen,colourRandomGen) = split randomGen
  let curves = toCurves args' chunkingRandomGen . toFamilies $ fst world ++ snd world
  let colouredCurves = colourCurves args' colourRandomGen curves
  let customStroke = stroke outlineSize (JoinMiter 0) (CapStraight 0, CapStraight 0)
  let drawSweep :: Path -> Drawing PixelRGBA8 ()
      drawSweep path | strokeOrFill == 0 = customStroke path
                     | strokeOrFill == 1 || strokeOrFill == 3 = fill path
                     | otherwise = do
                         fill path
                         withTexture (uniformTexture outlineColour)
                           $ customStroke path
  let drawFunc gen curve | drawSoftly = drawSoftCurve args' softFieldFunc gen curve
                         | otherwise  = drawSweep $ sweepRectOnCurve args' curve
  writePng "test.png" $ renderDrawing width height (aBgColour args') $ do
      mapM_ (\((curve,colour),gen) -> withTexture (uniformTexture colour) $
                                        drawFunc gen curve) $
            zip colouredCurves $ iterate (fst . split) randomGen
      mapM_ (\curve -> withTexture (uniformTexture outlineColour) $
             customStroke $ sweepRectOnCurve args' curve)
           $ if strokeOrFill == 3
                then toFamilies $ fst world ++ snd world
                else []

data LineSeg = LineSeg { lsP1 :: Point
                       , lsP2 :: Point
                       , lsFamily :: Int
                       , lsWidth :: Float
                       , lsFertility :: Int
                       , lsSkewAngle :: Float}
                         deriving stock (Show)
type Curve = [LineSeg]
type ColouredCurve = (Curve, PixelRGBA8)
sweepRectOnCurve :: Args
                 -> Curve
                 -> Path
sweepRectOnCurve args lineSegs =
  Path firstTop True . map PathLineTo $ topPoints ++ (reverse $ firstBot:botPoints)
  where (topGen,botGen) = split . mkStdGen $ aRandomBoilSeed args
        boils = zip (randomRs (-aRandomBoil args,aRandomBoil args) topGen)
                    (randomRs (-aRandomBoil args,aRandomBoil args) botGen)
        chamfer (boilTop, boilBot)
                (LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = w, lsSkewAngle = angle }) =
            ((p1+perp^*(w/2+boilTop),p1-perp^*(w/2+boilBot)),
             (p2+perp^*(w/2+boilTop),p2-perp^*(w/2+boilBot)))
            where (V2 x y) = p2 - p1
                  (V2 px py) = (normalize $ (V2 (-y) x))
                  perp = V2 (cos angle * px - sin angle * py)
                            (sin angle * px + cos angle * py)
        (firstTop,firstBot) = fst . chamfer (head boils) $ head lineSegs
        (topPoints,botPoints) = foldr
                                  (\(ptBoils,lineSeg) (topAcc,botAcc) ->
                                    let (top,bot) = snd $ chamfer ptBoils lineSeg
                                     in (top:topAcc,bot:botAcc))
                                    ([],[]) $ zip boils lineSegs

drawSoftCurve :: Args
              -> VectorFieldFunc
              -> StdGen
              -> Curve
              -> Drawing PixelRGBA8 ()
drawSoftCurve args vfunc gen lineSegs = mapM_ (strokeDraw . toPath) $ toStrokes gen
  where getPerp lineSeg = normalize $ V2 (-y) x
          where (V2 x y) = lsP2 lineSeg - lsP1 lineSeg
        -- an interim is a combination of
        -- - a point on the negative side of the perp of a line seg
        -- - a point on the positive side of the perp of a line seg
        -- - the vector between the 2 points of a line seg
        toInterim ls@(LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = w }) =
          (p2 + perp ^* (w/2), p2 - perp ^* (w/2), normalize (p2 - p1))
          where perp = getPerp ls
        -- everyNth specifies how many steps to treat as one, so we don't
        -- have small jittery noise but rather broader effect
        everyNth _ [] = []
        everyNth n (x:xs) = x:(everyNth n $ drop (n-1) xs)
        interims = map toInterim $ everyNth (aSoftStepLength args) lineSegs
        nPaths = min (aSoftMaxStrokes args)
              $ round $ (lsWidth $ head lineSegs) * (aSoftNumStrokesWidthRatio args)
        -- toPoint takes in a path number n and an interim and
        -- generates a point between p1 and p2
        (perpOfsSize,vOfsSize) = (aSoftRandomOfsAlongPerp args
                                , aSoftRandomOfsAlongV args)
        toPoint :: StdGen -> Int -> (Point,Point,Point) -> (Point, StdGen)
        toPoint inGen n (p1,p2,v) =
          (p1 + (step ^* nf) + ((normalize step) ^* perpOfs) + v ^* vOfs, gen')
          where step = (p2 - p1) ^/ (fromIntegral nPaths)
                nf = fromIntegral n
                (V2 x y) = p1 + step ^* nf
                perpOfs = (fst $ vfunc (x,y)) * perpOfsSize
                (vOfs,gen') = randomR (-vOfsSize,vOfsSize) inGen
        -- toStroke takes in a path number and goes through all interims
        -- getting a point for each and collecting them into a stroke
        -- which represents the effect of one hair of the brush on the paper
        toStroke n inGen = foldr foldFunc ([],inGen) interims
          where foldFunc interim (points,gen') = (point:points,gen'')
                  where (point,gen'') = toPoint gen' n interim
        toStrokes inGen = fst
                      $ foldr foldFunc ([],inGen) [1..nPaths]
          where foldFunc n (strokes,gen') = (stroke':strokes,gen'')
                  where (stroke',gen'') = toStroke n gen'
        -- a couple of convenience function to have a simpler output
        toPath ps = Path (head ps) False $ map PathLineTo $ tail ps
        strokeDraw = stroke 1 (JoinMiter 0) (CapStraight 1, CapStraight 1)

colourCurves :: Args
             -> StdGen
             -> [Curve]
             -> [ColouredCurve]
colourCurves _ _ [] = []
colourCurves args randomGen curves = zip curves colours
  where colours = map (sampleDistribution (aColours args))
                $ randomRs (0.0, 1.0) $ snd $ split randomGen

toCurves :: Args
         -> StdGen
         -> [[LineSeg]]
         -> [Curve]
toCurves _ _ [] = []
toCurves args randomGen families = filter ((>0) . length) $ concat chunks
  where toChunks _ _ [] = []
        toChunks lenSoFar randomGen' lineSegs = if length lineSegs < chunkSize'
                                                then [lineSegs]
                                                else chunk:remainder'
          where squareChunkSize = round . lsWidth $ head lineSegs
                avgChunkSize = (_aChunkSizes args) !! (lsFamily $ head lineSegs)
                (randomF,randomGen'') = random randomGen'
                (boundaryStart,randomGen''') = randomR (aStopChunkingAt args) randomGen''
                (boundaryEnd,randomGen'''') = randomR (aStopChunkingAt args) randomGen'''
                chunkSize = sampleDistribution (aChunkSizes args) randomF
                distToSquareChunkSize = fromIntegral $ squareChunkSize - chunkSize
                distToAvgChunkSize = fromIntegral $ avgChunkSize - chunkSize
                chunkSize' = chunkSize
                           + (round $ distToSquareChunkSize * aSquareBlocks args)
                           + (round $ distToAvgChunkSize * aAvgBlockSize args)
                chunk = take chunkSize' lineSegs
                remainder = drop (chunkSize' - aChunksOverlap args) lineSegs
                remainderMid = take (length remainder - (boundaryStart + boundaryEnd))
                               remainder
                remainderEnd = drop (length remainderMid - 1) remainder
                remainder' =
                    if lenSoFar > boundaryStart
                    then remainderMid:(toChunks 0 randomGen''' remainderEnd)
                    else toChunks (lenSoFar + length chunk) randomGen'''' remainder
        chunks = map (\(gen,family) -> toChunks 0 gen family)
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
        familyFold _ _ = error "Can't fold families. An empty list maybe?"
        foldedFamilies = foldl familyFold [] allLineSegs
        (firstFamily,families) = (head foldedFamilies, tail foldedFamilies)

type World = ( [LineSeg]   -- fertile
             , [LineSeg] ) -- infertile
simWorld :: Args               -- fidenza parameters
         -> VectorFieldFunc    -- vector field function
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
          -> VectorFieldFunc    -- vector field function
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
doesLineSegCollide args (LineSeg { lsP1 = p1, lsP2 = p2, lsWidth = pw })
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
          l2 = (vx*vx) + (vy*vy)
          t = (dot (p - p1) v) / l2
          t' = min (max 0 t) 1

stepLineSeg :: Args                     -- fidenza parameters
            -> VectorFieldFunc          -- vector field function
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
           -> VectorFieldFunc    -- vector field function
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
          sampleHelper dist gen = (sampleDistribution dist x', gen')
            where (x',gen') = randomR (0,1) gen
          (x,randomGen1) = randomR (paddingF,paddedX) randomGen :: (Float,StdGen)
          (y,randomGen2) = randomR (paddingF,paddedY) randomGen1 :: (Float,StdGen)
          w = _aCurveWidths args !! generation
          (f,randomGen3) = sampleHelper (aFertilities args) randomGen2
          (sw,randomGen') = sampleHelper (aSkewAngles args) randomGen3
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
