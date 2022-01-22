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
                 , ("bgColour","(235,228,216,255)","")
                 , ("colourScheme","0","")
                 , ("customColours","[]","")
                 , ("randomBoil","0","")
                 , ("randomBoilSeed","0","")
                 , ("strokeOrFill","1","")
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
                          aBgColour = readColour $ getArg "bgColour"
                          colourScheme = read $ getArg "colourScheme"
                          customColours = readColourProbs $ getArg "customColours"
                          aColours = if not $ nullDistribution customColours
                                       then customColours
                                       else colourSchemes !! colourScheme
                          aRandomBoil = read $ getArg "randomBoil"
                          aRandomBoilSeed = read $ getArg "randomBoilSeed"
                          aStrokeOrFill = read $ getArg "strokeOrFill"
                _  -> error $ "Fidenza.parseArgs: Unrecognized args: " ++ unrecognizedArgs'
