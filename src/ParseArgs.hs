module ParseArgs ( Args(..)
                 , VectorFieldGenerator(..)
                 , helpString
                 , parseArgs
                 , readVectorFieldFromFile
                 ) where

import Codec.Picture( PixelRGBA8( .. ), Pixel8)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

import qualified Data.Map as Map (Map, fromList, findWithDefault, map, (!), keys)

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
    , aChunkSizes :: [Int]
    , aSquareBlocks :: Float
    , aAvgBlockSize :: Float
      -- Curve generation options
    , aStepLength :: Float
    , aCurveWidths :: [Float]
    , aFertilities :: [Int]
    , aSkewAngles :: [Float]
      -- Colours
    , aBgColour :: PixelRGBA8
    , aColourScheme :: Int
    , aCustomColours :: [PixelRGBA8]
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
                 , ("vectorFieldGenerator","PerlinNoise 0.001","")
                 , ("numGenerationAttempts","1000","")
                 , ("maxSteps","10000","")
                 , ("maxCurves","10","")
                 , ("minLength","15","")
                 , ("chunkSizes","[10,15]","")
                 , ("squareBlocks","0.0","")
                 , ("avgBlockSize","0.0","")
                   -- generation options
                 , ("stepLength","1","")
                 , ("curveWidths","[2,10]","")
                 , ("fertilities","(25,1000,25)","")
                 , ("skewAngles","(-0.1,0.1,0.02)","")
                   -- colours
                 , ("bgColour","(0,0,0,255)","")
                 , ("colourScheme","0","")
                 , ("customColours","[]","")
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
                          aChunkSizes = readList' $ getArg "chunkSizes"
                          aSquareBlocks = read $ getArg "squareBlocks"
                          aAvgBlockSize = read $ getArg "avgBlockSize"
                          aStepLength = read $ getArg "stepLength"
                          aCurveWidths = readList' $ getArg "curveWidths"
                          aFertilities = readList' $ getArg "fertilities"
                          aSkewAngles = readList' $ getArg "skewAngles"
                          aBgColour = readColour $ getArg "bgColour"
                          aColourScheme = read $ getArg "colourScheme"
                          aCustomColours = readColours $ getArg "customColours"
                _  -> error $ "Fidenza.parseArgs: Unrecognized args: " ++ unrecognizedArgs'

readList' :: (Num a, Enum a, Read a) => String -> [a]
readList' str@('(':_) = [bottom,(bottom+step)..top]
  where (bottom,top,step) = read str
readList' str = read str

tupleToColour :: (Pixel8,Pixel8,Pixel8,Pixel8) -> PixelRGBA8
tupleToColour (r,g,b,a) = PixelRGBA8 r g b a

readColour :: String -> PixelRGBA8
readColour str = tupleToColour $ read str

readColours :: String -> [PixelRGBA8]
readColours [] = []
readColours "[]" = []
readColours ('[':str) = map (readOne . (++")")) $ init $ splitOn ")" $ init str
    where readOne (',':str') = readColour str'
          readOne str' = readColour str'
readColours str = error $ "Failed interpreting colour list " ++ str

data VectorFieldGenerator = PerlinNoise Float
                          | FromFile String
                            deriving stock (Show)

instance Read VectorFieldGenerator where
    readsPrec _ str
      | isPrefixOf "PerlinNoise " str =
          [(PerlinNoise $ read $ drop (length "PerlinNoise ") $ str, "")]
      | isPrefixOf "FromFile " str =
          [(FromFile $ drop (length "FromFile ") $ str, "")]
    readsPrec _ str = error $ "VectorFieldGenerator.read: Can't read " ++ str

type Vector = (Float,Float)
type VectorField = [[Vector]]
type NoiseFunc = (Vector -> Vector)

readVectorFieldFromFile :: (Int,Int) -> FilePath -> IO NoiseFunc
readVectorFieldFromFile (width,height) fp = do
    contents <- readFile fp
    let toTuples [] = []
        toTuples (_:[]) = error $ "Fidenza.readVectorFieldFromFile: malformatted file"
        toTuples (x:y:xys) = (x,y):toTuples xys
    let vectors = map (toTuples . (map read . words)) $ lines contents :: VectorField
    let (lenX,lenY) = (length $ head vectors, length vectors)
    return $ vectorFileNoiseFunc vectors (width,height) (lenX,lenY)

vectorFileNoiseFunc :: VectorField -> (Int,Int) -> (Int,Int) -> Vector -> Vector
vectorFileNoiseFunc vectors (width,height) (lenX,lenY) (x,y) = vec
  where (px,py) = (x/fromIntegral width, y/fromIntegral height)
        (px',py') = (px * (fromIntegral lenX-1), py * (fromIntegral lenY-1))
        (binX,binY) = (floor px', floor py')
        (vXY,vXY1,vX1Y,vX1Y1) = ( (!! binX) $ vectors !! binY 
                                , (!! binX) $ vectors !! (binY+1)
                                , (!! (binX+1)) $ vectors !! binY 
                                , (!! (binX+1)) $ vectors !! (binY+1)) 
        (u,v) = (px' - fromIntegral binX, py' - fromIntegral binY)
        lerpV (x1,y1) (x2,y2) w = (x1 + (x2 - x1) * w, y1 + (y2 - y1) * w)
        vec = lerpV (lerpV vXY vX1Y u)
                    (lerpV vXY1 vX1Y1 u) v
