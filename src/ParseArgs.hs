module ParseArgs ( Args(..)
                 , VectorFieldGenerator(..)
                 , Vector
                 , helpString
                 , parseArgs
                 , readVectorFieldFromFile
                 , sampleDistribution
                 , mapDistribution
                 ) where

import Codec.Picture( PixelRGBA8( .. ), Pixel8)
import Data.List (isPrefixOf, sortBy)
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
    , aChunkSizes :: Distribution Int
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
                          aColours = if not $ distNull customColours
                                       then customColours
                                       else colourSchemes !! colourScheme
                          aRandomBoil = read $ getArg "randomBoil"
                          aRandomBoilSeed = read $ getArg "randomBoilSeed"
                          aStrokeOrFill = read $ getArg "strokeOrFill"
                _  -> error $ "Fidenza.parseArgs: Unrecognized args: " ++ unrecognizedArgs'

-- Distributions
data Distribution a = Distribution [(a,Float)] 

instance (Show a) => Show (Distribution a) where
    show (Distribution as) | length as > 5 =
      "Distribution [" ++ show (head as) ++ ".." ++ show (last as) ++ "]"
                           | otherwise     = "Distribution " ++ show as

mapDistribution :: ((a,Float) -> b) -> Distribution a -> [b]
mapDistribution f (Distribution as) = map f as

readDistribution :: (Enum a, Read a, Num a) => String -> Distribution a
readDistribution str@('[':'(':_) = toWeightedDistribution $ read str
readDistribution str@('[':_)     = toUniformDistribution $ read str
readDistribution str@('(':_)     = toUniformDistribution $ asList
  where (bottom,top,step) = read str
        asList = [bottom,(bottom+step)..top]
readDistribution _ = error "Can't parse distribution"

toWeightedDistribution :: [(a,Float)] -> Distribution a
toWeightedDistribution = Distribution . sort' . normalize'
  where sort' = sortBy (\a b -> compare (snd a) (snd b))
        normalize' xs = map (\(v,prob) -> (v,prob/sum')) xs
            where sum' = sum $ map snd xs

toUniformDistribution :: [a] -> Distribution a
toUniformDistribution as = Distribution $ zip as $ repeat prob
  where prob = 1.0 / (fromIntegral $ length as)

sampleDistribution :: Distribution a -> Float -> a
sampleDistribution (Distribution distAs) x =
  go x distAs
  where go _ [] = error "Can't sample empty distribution"
        go v ((a,prob):_)  | v <= prob  = a
        go v ((_,prob):as) | otherwise = go (v-prob) as

distNull :: Distribution a -> Bool
distNull (Distribution []) = True
distNull _ = False

-- Colour schemes
luxe,blackAndWhite :: Distribution PixelRGBA8
colourSchemes :: [Distribution PixelRGBA8]
luxe = toWeightedDistribution
       [(PixelRGBA8 41 166 145 255 , (1/13))
       ,(PixelRGBA8 84 62 46 255   , (1/13))
       ,(PixelRGBA8 49 95 140 255  , (1/13))
       ,(PixelRGBA8 252 188 25 255 , (1/13))
       ,(PixelRGBA8 59 43 32 255   , (1/13))
       ,(PixelRGBA8 31 51 89 255   , (1/13))
       ,(PixelRGBA8 252 210 101 255, (1/13))
       ,(PixelRGBA8 219 79 84 255  , (1/13))
       ,(PixelRGBA8 252 188 25 255 , (1/13))
       ,(PixelRGBA8 59 43 32 255   , (1/13))
       ,(PixelRGBA8 224 215 197 255, (1/13))
       ,(PixelRGBA8 184 217 206 255, (1/13))
       ,(PixelRGBA8 84 62 46 255   , (1/13))]
blackAndWhite = toWeightedDistribution
                [(PixelRGBA8 0 0 0 255, 0.5), (PixelRGBA8 255 255 255 255, 0.5)]
colourSchemes = [luxe, blackAndWhite]

-- Parsers
readColour :: String -> PixelRGBA8
readColour str = tupleToColour $ read str

readColourProbs :: String -> Distribution PixelRGBA8
readColourProbs [] = Distribution []
readColourProbs "[]" = Distribution []
readColourProbs ('[':str) =
    toWeightedDistribution $ map (readOne . (++")")) $ init $ splitOn ")" $ init str
    where readOne (',':str') = tupleToColourProb $ read str'
          readOne str' = tupleToColourProb $ read str'
readColourProbs str = error $ "Failed interpreting colour list " ++ str

tupleToColourProb :: (Pixel8,Pixel8,Pixel8,Pixel8,Float) -> (PixelRGBA8, Float)
tupleToColourProb (r,g,b,a,prob) = (PixelRGBA8 r g b a, prob)

tupleToColour :: (Pixel8,Pixel8,Pixel8,Pixel8) -> PixelRGBA8
tupleToColour (r,g,b,a) = PixelRGBA8 r g b a

-- Vector field generation options
data VectorFieldGenerator = PerlinNoise Float Int
                          | FromFile String
                            deriving stock (Show)

instance Read VectorFieldGenerator where
    readsPrec _ str
      | isPrefixOf "PerlinNoise " str =
          let params = words $ drop (length "PerlinNoise ") str
           in [(PerlinNoise (read $ params !! 0) (read $ params !! 1), "")]
      | isPrefixOf "FromFile " str =
          [(FromFile $ drop (length "FromFile ") $ str, "")]
    readsPrec _ str = error $ "VectorFieldGenerator.read: Can't read " ++ str

type Vector = (Float,Float)
type VectorField = [[Vector]]

readVectorFieldFromFile :: (Int,Int) -> FilePath -> IO (Vector -> Vector)
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
