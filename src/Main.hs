module Main (main) where

import System.Environment (getArgs)
import System.Random (mkStdGen, split)
import Codec.Picture( PixelRGBA8( .. ), writePng, Pixel8)
import Graphics.Rasterific hiding (Vector)
import Graphics.Rasterific.Texture (uniformTexture)
import Debug.Trace

import ParseArgs (Args( .. ), VectorFieldGenerator( .. ), helpString,
                  parseArgs, readVectorFieldFromFile)
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
fidenza (Args seed
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
    noise <- case vectorFieldGenerator of
      FromFile fp -> readVectorFieldFromFile (width,height) fp
      PerlinNoise freq -> return (\(x,y) -> let p = (x*freq,y*freq)
                                                randomGen' = snd $ split randomGen
                                                twoPi = 2 * (22/7)
                                                angle = twoPi * PNoise.noise2d p width randomGen'
                                             in (cos angle, sin angle))
    ---- Visualising the vector field
    --writePng "test.png" $ renderDrawing width height (PixelRGBA8 125 125 125 255) $
    --    mapM_ (\(x,y) -> withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $ stroke 2 JoinRound (CapRound,CapRound) $
    --                        Line (V2 x y) (V2 (x + 20*(fst $ vf (x,y))) (y + 20*(snd $ vf (x,y))))) [(x,y) | x <- [5,25..(fromIntegral width-20)], y <- [5,25..(fromIntegral height-20)]]
    putStrLn $ show $ noise (10,10)
