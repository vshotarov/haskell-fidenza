module ParseArgs ( Args(..)
                 , helpString
                 , parseArgs
                 ) where

import Codec.Picture (PixelRGBA8( .. ), Pixel8)
import Data.List (elemIndex, sortBy, isPrefixOf)
import System.Random (setStdGen, getStdRandom, randomR, mkStdGen, random)

import qualified Data.Map as Map (Map, fromList, findWithDefault, map, (!), keys)

import VectorFieldGenerator (VectorFieldGenerator)
import Distribution (Distribution(..), readDistribution, nullDistribution, distToList)
import ColourSchemes (colourSchemes, readColour, readColourProbs)

data Args = Args
    { -- General options
      aSeed :: Int
    , aWidth :: Int
    , aHeight :: Int
    , aCollisionMargin :: Int
    , aPadding :: Int
    , aVectorFieldGenerator :: VectorFieldGenerator
    , aVectorFieldStepPiDivisor :: Int
    , aNumGenerationAttempts :: Int
    , aRotationOffset :: Float
    , aMaxSteps :: Int
    , aMaxCurves :: Int
    , aMinLength :: Int
    , _aChunkSizes :: [Int]
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

simpleArgs :: Map.Map String ArgDesc
simpleArgs = Map.fromList $ map (\(x,y,v) -> (x, ArgDesc (x,y,v))) [
                   ("collisionCheck","","")
                 , ("colours","","")
                 , ("haveMargin","","")
                 , ("outlined","","")
                 , ("scale","","")
                 , ("shapeAngles","","")
                 , ("softShapes","","")
                 , ("superBlocks","","")
                 , ("turbulence","","")
                 ]
simpleArgsNames :: [String]
simpleArgsNames = map ("--"++) $ Map.keys simpleArgs

recognizedArgs :: Map.Map String ArgDesc
recognizedArgs = Map.fromList $ map (\(x,y,v) -> (x, ArgDesc (x,y,v))) [
                   ("help","","shows this help message")
                 , ("seed","0","seed for the random generator")
                 , ("width","1000","image width")
                 , ("height","1200","image height")
                 , ("collisionMargin","2","")
                 , ("padding","20","")
                 , ("vectorFieldGenerator","PerlinNoise 0.001 -400","")
                 , ("vectorFieldStepPiDivisor","0","")
                 , ("rotationOffset","0","")
                 , ("numGenerationAttempts","1000","")
                 , ("maxSteps","100000","")
                 , ("maxCurves","200","")
                 , ("minLength","15","")
                 , ("chunkSizes","(2,12,1)","")
                 , ("stopChunkingAt","(5,15)","")
                 , ("squareBlocks","0.0","")
                 , ("avgBlockSize","0.0","")
                   -- generation options
                 , ("stepLength","1","")
                 , ("widths","(2,60,4)","")
                 , ("fertilities","(50,400,25)","")
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
allRecognizedArgNames,recognizedArgNames :: [String]
recognizedArgNames = map ("--"++) $ Map.keys recognizedArgs
allRecognizedArgNames = recognizedArgNames ++ simpleArgsNames

helpString :: String
helpString = "Usage: (./Fidenza|cabal run Fidenza -- ) [ARGS] \n\
\ Ex:\n\
\   ./Fidenza --seed 1\n\
\   cabal run Fidenza -- --seed 1\n\
\ Args:\n" ++ (concat $ Map.map (("    "++) . (++"\n") . show) recognizedArgs) ++ "\n\
\ Simple Args:\n\
\   If the simple flags are used, then the arguments accepted are significantly\n\
\   simplified and act as switches on defined presets of the arguments above.\n\
\\n\
\   These presets are designed to resemble Tyler Hobbs' parameters as much as possible.\n\
\\n\
\   It's important to note that the presets are evaluated first, and then any of the main\n\
\   arguments above, which means preset values can be overwritten by the main arguments.\n\
\\n\
\   For example, the following command will NOT use the 'small' scale as specified by the\n\
\   preset, but instead use the widths provided afterwards.\n\
\\n\
\   ./Fidenza --scale small --widths  [80,90]\n\
\\n" ++ (concat $ Map.map (("   "++) . (++"\n") . show) simpleArgs)


parseArgs :: [String] -> IO Args
parseArgs args = do
    let toTuples [] = []
        toTuples (x:[]) = error $ "Fidenza.parseArgs: unrecognized term " ++ x ++ ". \
                                  \Do you maybe have an unmatched flag or value?"
        toTuples (x:y:xys) = (x,y):toTuples xys
        argPairs = toTuples args
        unrecognizedArgs = filter
                             (not . (`elem` allRecognizedArgNames) . fst)
                             argPairs
        unrecognizedArgs' = drop 2 . concat $
                            map (\(k,v) -> ", " ++ k ++ " " ++ v) unrecognizedArgs
        seed = if "--seed" `elem` args
               then read . snd . head $ filter ((=="--seed") . fst) argPairs
               else 0
    setStdGen $ mkStdGen seed
    let (simpleArgPairs,complexArgPairs) = foldr 
                                             (\(n,v) (simple,complex) ->
                                               if n `elem` simpleArgsNames
                                               then ((drop 2 n,v):simple,complex)
                                               else (simple,(n,v):complex))
                                             ([],[]) argPairs
    expandedSimpleArgs <- mapM handleSimpleArg simpleArgPairs
    let argsMap = Map.fromList $ (concat expandedSimpleArgs) ++ complexArgPairs
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
                        aVectorFieldStepPiDivisor = read $ getArg "vectorFieldStepPiDivisor"
                        aRotationOffset = read $ getArg "rotationOffset"
                        aNumGenerationAttempts = read $ getArg "numGenerationAttempts"
                        aMaxSteps = read $ getArg "maxSteps"
                        aMaxCurves = read $ getArg "maxCurves"
                        aMinLength = read $ getArg "minLength"
                        aChunkSizes = readDistribution $ getArg "chunkSizes"
                        _aChunkSizes = []
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
    return parsedArgs

handleSimpleArg :: (String,String) -> IO [(String,String)]
-- Collision checking
handleSimpleArg ("collisionCheck","noOverlap") = return [("--collisionMargin","2")]
handleSimpleArg ("collisionCheck","relaxed") = return [("--collisionMargin","-10")]
handleSimpleArg ("collisionCheck","anythingGoes") = return [("--collisionMargin","-1000")]
-- Colours
-- ** Derived versions
handleSimpleArg ("colours",colourScheme) | isPrefixOf "derived" colourScheme = do
  let colourScheme' = drop (length "derived") colourScheme
      colourSchemeId = case elemIndex colourScheme' ["Luxe","Rad","GolfSocks","Baked",
                                                     "Politique","Cool","Am","WhiteOnCream",
                                                     "BlackAndWhite"] of
                           Nothing -> error $ "Couldn't find colour scheme " ++ colourScheme'
                           Just x -> x
      (bg,colours) = colourSchemes !! colourSchemeId
      allColours = bg:(distToList colours)
      maxColourId = length allColours - 1
  bgId <- getStdRandom (randomR (0, maxColourId))
  randomCompares <- mapM (\_ -> getStdRandom (randomR (0,2))) [0..maxColourId]
  subsetLength <- getStdRandom (randomR (1,maxColourId+1))
  let remaining = map (allColours !!) [i | i <- [0..maxColourId],
                                           i /= bgId]
      remainingShuffled = map fst
                        $ sortBy
                            (\a _ -> [EQ,GT,LT] !! (snd a))
                            $ zip remaining randomCompares
      randomSubset = take subsetLength remainingShuffled
  probabilities <- mapM (\_ -> getStdRandom (random)) [0..maxColourId]
  let zipper (PixelRGBA8 r g b a) p = (r,g,b,a,p)
      distribution :: [(Pixel8,Pixel8,Pixel8,Pixel8,Float)]
      distribution = zipWith zipper randomSubset probabilities
      toTuple (PixelRGBA8 r g b a) = (r,g,b,a)
      bgColour = toTuple $ allColours !! bgId
  return [("--customBgColour", show bgColour)
         ,("--customColours", show distribution)]
-- ** Defined versions (non-derived)
handleSimpleArg ("colours",colourScheme) = return [("--colourScheme", show colourSchemeId)]
  where colourSchemeId = case elemIndex colourScheme ["luxe","rad","golfSocks","baked",
                                                      "politique","cool","am","whiteOnCream",
                                                      "blackAndWhite"] of
                           Nothing -> error $ "Couldn't find colour scheme " ++ colourScheme
                           Just x -> x
-- Have margin
handleSimpleArg ("haveMargin","yes") = return [("--padding", "20")]
handleSimpleArg ("haveMargin","no") = return [("--padding", "-100")]
-- Outlined
handleSimpleArg ("outlined","yes") = return [("--strokeOrFill","2")]
handleSimpleArg ("outlined","no") = return [("--strokeOrFill","1")]
-- Scale
handleSimpleArg ("scale","small") = return [("--widths","[(30,0.005),(20,0.01),(15,0.05),(12,0.1),(8,0.4),(6,0.2),(4,0.2),(3,0.2)]")
                                    ,("--fertilities","(30,250,20)")
                                    ,("--maxCurves","1000")]
handleSimpleArg ("scale","medium") = return [("--widths","[(45,0.1),(30,0.2),(20,0.2),(15,0.2),(12,0.2),(8,0.1),(6,0.01),(4,0.001)]")
                                    ,("--fertilities","(30,900,20)")
                                    ,("--maxCurves","330")
                                    ,("--numGenerationAttempts","5000")
                                    ,("--minLength","30")]
handleSimpleArg ("scale","large") = return [("--widths","[(65,0.5),(40,0.2),(30,0.2),(20,0.1),(10,0.05)]")
                                    ,("--fertilities","(300,800,50)")
                                    ,("--minLength","40")
                                    ,("--maxCurves","80")]
handleSimpleArg ("scale","jumbo") = return [("--widths","[(135,0.003),(80,0.1),(60,0.2),(34,0.3),(20,0.3)]")
                                    ,("--fertilities","(400,1000,20)")
                                    ,("--maxCurves","100")
                                    ,("--minLength","100")
                                    ,("--numGenerationAttempts","5000")]
handleSimpleArg ("scale","jumboXL") = return [("--widths","[(225,0.01),(130,0.1),(100,0.3),(80,0.3),(60,0.05)]")
                                      ,("--fertilities","(300,1700,20)")
                                      ,("--maxCurves","35")
                                      ,("--minLength","130")
                                      ,("--numGenerationAttempts","10000")]
handleSimpleArg ("scale","microUniform") = return [("--widths","[4]")
                                           ,("--maxCurves","10000")
                                           ,("--maxSteps","200000")
                                           ,("--fertilities","(10,260,5)")
                                           ,("--minLength","5")]
handleSimpleArg ("scale","uniform") = return [("--widths","[30]")
                                      ,("--maxCurves","140")
                                      ,("--fertilities","(100,1500,100)")
                                      ,("--minLength","50")]
-- Shape angles
handleSimpleArg ("shapeAngles","curved") = return [("--vectorFieldStepPiDivisor","0")]
handleSimpleArg ("shapeAngles","sharp") = return [("--vectorFieldStepPiDivisor","4")]
-- Soft shapes
handleSimpleArg ("softShapes","yes") = return [("--drawSoftly","1")]
handleSimpleArg ("softShapes","no") = return [("--drawSoftly","0")]
-- Super blocks
handleSimpleArg ("superBlocks","yes") = return [("--squareBlocks","1")]
handleSimpleArg ("superBlocks","no") = return [("--squareBlocks","0")]
-- Turbulence
handleSimpleArg ("turbulence","none") = return [("--vectorFieldGenerator","Straight")]
handleSimpleArg ("turbulence","low") = return [("--vectorFieldGenerator","PerlinNoise 0.0002 -200")]
handleSimpleArg ("turbulence","mid") = return [("--vectorFieldGenerator","PerlinNoise 0.001 -200")]
handleSimpleArg ("turbulence","high") = return [("--vectorFieldGenerator","PerlinNoise 0.0036 -400")]

handleSimpleArg (name,value) =
    error $ "Unrecognized simple arg name and/or value" ++ show (name,value)
