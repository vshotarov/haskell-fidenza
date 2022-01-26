module ParseArgs ( Args(..)
                 , helpString
                 , parseArgs
                 ) where

import Codec.Picture (PixelRGBA8( .. ))

import qualified Data.Map as Map (Map, fromList, findWithDefault, map, (!), keys)

import VectorFieldGenerator (VectorFieldGenerator)
import Distribution (Distribution(..), readDistribution, nullDistribution)
import ColourSchemes (colourSchemes, readColour, readColourProbs)

data Args = Args
    { -- General options
      aSeed :: Int
    , aWidth :: Int
    , aHeight :: Int
    , aCollisionMargin :: Int
    , aPadding :: Int
    , aVectorFieldGenerator :: VectorFieldGenerator
    , aNumGenerationAttempts :: Int
    , aRotationOffset :: Float
    , aMaxSteps :: Int
    , aMaxCurves :: Int
    , aMinLength :: Int
    , aChunkSizes :: Distribution Int
    , aStopChunkingAt :: (Int,Int)
    , aSquareBlocks :: Float
    , aAvgBlockSize :: Float
      -- Curve generation options
    , aStepLength :: Float
    , _aCurveWidths :: [Float]
    , aWidths :: Distribution Float
    , aFertilities :: Distribution Int
    , aSkewAngles :: Distribution Float
      -- Drawing
    , aChunksOverlap :: Int
    , aBgColour :: PixelRGBA8
    , aColours :: Distribution PixelRGBA8
    , aRandomBoil :: Float
    , aRandomBoilSeed :: Int
    , aStrokeOrFill :: Int
    , aOutlineColour :: PixelRGBA8
    , aOutlineSize :: Float
    -- Softness
    , aDrawSoftly :: Bool
    , aSoftSeed :: Int
    , aSoftStepLength :: Int
    , aSoftMaxStrokes :: Int
    , aSoftNumStrokesWidthRatio :: Float
    , aSoftRandomOfsAlongPerp :: Float
    , aSoftRandomOfsAlongV :: Float
    , aSoftPerlinFreq :: Float
    , aSoftPerlinOfs :: Float
    } deriving stock (Show)

data ArgDesc = ArgDesc (String,String,String)
instance Show ArgDesc where
    show (ArgDesc (name,_default,desc)) =
        name ++ " - " ++ desc ++ ". Default " ++ _default

recognizedArgs :: Map.Map String ArgDesc
recognizedArgs = Map.fromList $ map (\(x,y,v) -> (x, ArgDesc (x,y,v))) [
                   ("seed","0","seed for the random generator")
                 , ("width","500","image width")
                 , ("height","600","image height")
                 , ("collisionMargin","2","")
                 , ("padding","20","")
                 , ("vectorFieldGenerator","PerlinNoise 0.001 -1000","")
                 , ("rotationOffset","0","")
                 , ("numGenerationAttempts","1000","")
                 , ("maxSteps","10000","")
                 , ("maxCurves","100","")
                 , ("minLength","15","")
                 , ("chunkSizes","[10,15]","")
                 , ("stopChunkingAt","(10000,10000)","")
                 , ("squareBlocks","0.0","")
                 , ("avgBlockSize","0.0","")
                   -- generation options
                 , ("stepLength","1","")
                 , ("widths","(2,60,4)","")
                 , ("fertilities","(25,1500,25)","")
                 , ("skewAngles","(-0.1,0.1,0.01)","")
                   -- drawing
                 , ("chunksOverlap","1","")
                 , ("colourScheme","0","")
                 , ("customBgColour","","")
                 , ("customColours","[]","")
                 , ("randomBoil","0","")
                 , ("randomBoilSeed","0","")
                 , ("strokeOrFill","1","")
                 , ("outlineColour","(10,10,10,255)","")
                 , ("outlineSize","1","")
                   -- softness
                 , ("drawSoftly","0","")
                 , ("softSeed","0","")
                 , ("softStepLength","11","")
                 , ("softMaxStrokes","220","")
                 , ("softNumStrokesWidthRatio","1.8","")
                 , ("softRandomOfsAlongPerp","2","")
                 , ("softRandomOfsAlongV","6","")
                 , ("softPerlinFreq","0.016","")
                 , ("softPerlinOfs","-300","-300")
                 ]
recognizedArgNames :: [String]
recognizedArgNames = map ("--"++) $ Map.keys recognizedArgs

helpString :: String
helpString = "Usage: (./Fidenza|cabal run Fidenza -- ) [ARGS] \n\
\ Ex:\n\
\   ./Fidenza --seed 1\n\
\   cabal run Fidenza -- --seed 1\n\
\ Args:\n" ++ (concat $ Map.map (("    "++) . (++"\n") . show) recognizedArgs)

parseArgs :: [String] -> Args
parseArgs args = parsedArgs
    where toTuples [] = []
          toTuples (x:[]) = error $ "Fidenza.parseArgs: unrecognized term " ++ x ++ ". \
                                    \Do you maybe have an unmatched flag or value?"
          toTuples (x:y:xys) = (x,y):toTuples xys
          argPairs = toTuples args
          unrecognizedArgs = filter
                               (not . (`elem` recognizedArgNames) . fst)
                               argPairs
          unrecognizedArgs' = drop 2 . concat $
                              map (\(k,v) -> ", " ++ k ++ " " ++ v) unrecognizedArgs
          argsMap = Map.fromList $ toTuples args
          getArg name = Map.findWithDefault _default ("--"++name) argsMap
              where (ArgDesc (_,_default,_)) = recognizedArgs Map.! name
          parsedArgs =
              case unrecognizedArgs of
                [] -> Args {..}
                    where aSeed = read $ getArg "seed"
                          aWidth = read $ getArg "width"
                          aHeight = read $ getArg "height"
                          aCollisionMargin = read $ getArg "collisionMargin"
                          aPadding = read $ getArg "padding"
                          aVectorFieldGenerator = read $ getArg "vectorFieldGenerator"
                          aRotationOffset = read $ getArg "rotationOffset"
                          aNumGenerationAttempts = read $ getArg "numGenerationAttempts"
                          aMaxSteps = read $ getArg "maxSteps"
                          aMaxCurves = read $ getArg "maxCurves"
                          aMinLength = read $ getArg "minLength"
                          aChunkSizes = readDistribution $ getArg "chunkSizes"
                          aStopChunkingAt = read $ getArg "stopChunkingAt"
                          aSquareBlocks = read $ getArg "squareBlocks"
                          aAvgBlockSize = read $ getArg "avgBlockSize"
                          aStepLength = read $ getArg "stepLength"
                          _aCurveWidths = []
                          aWidths = readDistribution $ getArg "widths"
                          aFertilities = readDistribution $ getArg "fertilities"
                          aSkewAngles = readDistribution $ getArg "skewAngles"
                          aChunksOverlap = read $ getArg "chunksOverlap"
                          colourScheme = read $ getArg "colourScheme"
                          customColours = readColourProbs $ getArg "customColours"
                          aColours = if not $ nullDistribution customColours
                                     then customColours
                                     else snd $ colourSchemes !! colourScheme
                          aBgColour = if not $ null $ getArg "customBgColour"
                                      then readColour $ getArg "customBgColour"
                                      else fst $ colourSchemes !! colourScheme
                          aRandomBoil = read $ getArg "randomBoil"
                          aRandomBoilSeed = read $ getArg "randomBoilSeed"
                          aStrokeOrFill = read $ getArg "strokeOrFill"
                          aOutlineColour = readColour $ getArg "outlineColour"
                          aOutlineSize = read $ getArg "outlineSize"
                          aDrawSoftly = 1 == (read $ getArg "drawSoftly" :: Int)
                          aSoftSeed = read $ getArg "softSeed"
                          aSoftStepLength = read $ getArg "softStepLength"
                          aSoftMaxStrokes = read $ getArg "softMaxStrokes"
                          aSoftNumStrokesWidthRatio = read $ getArg "softNumStrokesWidthRatio"
                          aSoftRandomOfsAlongPerp = read $ getArg "softRandomOfsAlongPerp"
                          aSoftRandomOfsAlongV = read $ getArg "softRandomOfsAlongV"
                          aSoftPerlinFreq = read $ getArg "softPerlinFreq"
                          aSoftPerlinOfs = read $ getArg "softPerlinOfs"
                _  -> error $ "Fidenza.parseArgs: Unrecognized args: " ++ unrecognizedArgs'
