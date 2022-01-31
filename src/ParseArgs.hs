module ParseArgs ( Args(..)
                 , helpString
                 , parseArgs
                 ) where

import Codec.Picture (PixelRGBA8( .. ), Pixel8)
import Data.List (elemIndex, sort, sortBy, isPrefixOf)
import System.Random (setStdGen, getStdRandom, randomR, mkStdGen, random)

import qualified Data.Map as Map (Map, fromList, findWithDefault, map, (!), keys, elems)

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
    show (ArgDesc (name,"",desc)) =
        name ++ " - " ++ desc ++ "."
    show (ArgDesc (name,_default,desc)) =
        name ++ " - " ++ desc ++ ". Default " ++ _default

simpleArgs :: Map.Map String ArgDesc
simpleArgs = Map.fromList $ map (\(x,y,v) -> (x, ArgDesc (x,y,v))) [
                   ("collisionCheck","","whether or not to allow collisions\n\
                                  \      Valid options are noOverlap, relaxed and anythingGoes")
                 , ("colours","","colour scheme\n\
                           \      Valid options are luxe,rad,golfSocks,baked,politique,\n\
                           \      cool,am,whiteOnCream,blackAndWhite and derived+(any of the above).\n\
                           \        e.g. derivedAm or derivedBlackAndWhite")
                 , ("haveMargin","","whether we are constrained to the frame or allowed to go outside")
                 , ("outlined","","whether to add an outline to each segment")
                 , ("scale","","defines the size distribution of the ribbons\n\
                         \      Valid options are small,medium,large,jumbo,jumboXL,microUniform and uniform")
                 , ("shapeAngles","","whether the turns in the ribbons are smooth and sharp\n\
                             \      Valid options are curved and sharp.")
                 , ("softShapes","","whether to draw the shapes with hundreds of small strokes, \n\
                              \      resulting in a soft, brush like looking stroke. Valid options are yes and no")
                 , ("superBlocks","","whether to make the segments along the ribbon square\n\
                               \      Valid options are yes and no")
                 , ("turbulence","","how drastic the shape turns are. A more technical explanation \n\
                              \      is that this parameter controls the frequency of the underlying \n\
                              \      perlin noise vector field. Valid options are none,low,mid and high")
                 ]
simpleArgsNames :: [String]
simpleArgsNames = map ("--"++) $ Map.keys simpleArgs

recognizedArgs :: Map.Map String ArgDesc
recognizedArgs = Map.fromList $ map (\(x,y,v) -> (x, ArgDesc (x,y,v))) [
                   ("help","","shows this help message")
                 , ("seed","0","seed for the random generator")
                 , ("width","1000","image width")
                 , ("height","1200","image height")
                 , ("collisionMargin","2","minimal distance between shapes")
                 , ("padding","20","minimal distance from a shape to the borders of the image")
                 , ("vectorFieldGenerator","PerlinNoise 0.001 -400","method for generating the underlying vector field\n\
                                                              \      Valid options are:\n\
                                                              \         - PerlinNoise {frequency} {offset}, where the offset\n\
                                                              \              is there to allow us to pass negative coordinates\n\
                                                              \              and still get a result\n\
                                                              \         - FromFile {filePath}, where the file has the follwoing forma:\n\
                                                              \              x11 y11 x12 y12 x13 y13 .. x1n y1n\n\
                                                              \              x21 y21 x22 y22 x23 y23 .. x2n y2n\n\
                                                              \              ..\n\
                                                              \              xm1 ym1 xm2 ym2 xm3 ym3 .. xmn ymn\n\
                                                              \\n\
                                                              \              the m and n numbers are not necessary to be the same as the\n\
                                                              \              width and height of the image as they will be lerped\n\
                                                              \         - Horizontal - every point in the vector field returns (1,0)\n\
                                                              \         - Straight {angle} - every point in the vector field retuns (1,0)\n\
                                                              \              rotated by {angle}")
                 , ("vectorFieldStepPiDivisor","0","if greater than 0, all angles will be rounded to \n\
                                             \      multiples of PI divided by this parameter")
                 , ("rotationOffset","0","rotates the whole vector field by this parameter")
                 , ("numGenerationAttempts","1000","how many attempts to make when generating a new \n\
                                             \      curve before giving up")
                 , ("maxSteps","100000","a way of limiting the execution, where one step is considered \n\
                                  \      the extension of any curve by one segment of length {stepLength}")
                 , ("maxCurves","200","a maximum number of curves before execution stops")
                 , ("minLength","15","the minimum number of {stepLength} long segments to accept \n\
                               \      as a valid generated curve")
                 , ("chunkSizes","(2,12,1)","the distribution of chunk/segment sizes")
                 , ("stopChunkingAt","(5,15)","controls the distance from start and end to stop \n\
                                        \      chunking the ribbon into segments at. The actual number is randomly \n\
                                        \      sampled between the left and right boundaries.\n\
                                        \      Set this to something massive if you don't want segments")
                 , ("squareBlocks","0.0","whether to make the segments along the ribbon square.\n\
                                   \      The range is (0,1)")
                 , ("avgBlockSize","0.0","whether to make all segments on a single curve the same.\n\
                                   \      The range is (0,1)")
                   -- generation options
                 , ("stepLength","1","how long of a step to make at every iteration.\n\
                               \      Inveresely proportional to resolution")
                 , ("widths","(2,60,4)","the distribution of curve widths")
                 , ("fertilities","(50,400,25)","the distribution of fertilities (how long a curve can get)")
                 , ("skewAngles","(-0.1,0.1,0.01)","the distribution of skewing angles applied to the segments\n\
                                             \      when drawing. One angle per curve")
                   -- drawing
                 , ("chunksOverlap","1","when splitting into segments, we can choose to make them overlap a bit\n\
                                   \     or choose to have a negative number which makes gaps between the segments")
                 , ("colourScheme","0","the colour scheme as an id. See the list of colours at the\n\
                                 \      Simple colour arg below. 9,10 and 11 are my own colour schemes")
                 , ("customBgColour","","the background colour in the format (r,g,b,a) with 0-255 values.\n\
                                  \      Overrides the background of any defined colour schemes.")
                 , ("customColours","[]","a custom colour palette in the format (r,g,b,a,probability)\n\
                                   \      with 0-255 values for r,g,b and a and any type of relative probability.\n\
                                   \      Overrides any colour scheme parameters")
                 , ("randomBoil","0","randomly pushes shape points with this amount")
                 , ("randomBoilSeed","0","the seed for the random boil. Uses a separate seed,\n\
                                   \      so we can make small, silly boiling animations")
                 , ("strokeOrFill","1","whether to outline each segment (0), fill the each segment (1),\n\
                                 \      outline and fill each segment (2) or fill each segment and\n\
                                 \      outline the whole ribbon")
                 , ("outlineColour","(10,10,10,255)","the colour of any outlines")
                 , ("outlineSize","1","the size of any outlines")
                   -- softness
                 , ("drawSoftly","0","whether to draw each shape by drawing many tiny strokes,\n\
                               \      rather than filling the whole shape, resulting in a soft,\n\
                               \      brush like looking shapes")
                 , ("softSeed","0","seed for the softness, so it can be animated")
                 , ("softStepLength","11","how many stes of {stepLen} to group into one step\n\
                                    \      for the tiny strokes")
                 , ("softMaxStrokes","220","maximum number of tiny strokes per curve")
                 , ("softNumStrokesWidthRatio","1.8","multiplies the width of a curve by this to\n\
                                               \      define the number of strokes per shape")
                 , ("softRandomOfsAlongPerp","2","how much to randomly push the tiny strokes\n\
                                           \      along the perpendicular (normal) of the curve")
                 , ("softRandomOfsAlongV","6","how much to randomly push the tiny strokes along\n\
                                        \      along the direction of the curve")
                 , ("softPerlinFreq","0.016","the frequency/turbulence of the soft strokes")
                 , ("softPerlinOfs","-300","the offset of the soft strokes vector field,\n\
                                     \      to allow (x,y) values outside of the img dimensions\n\
                                     \      to have valid results")
                 ]
allRecognizedArgNames,recognizedArgNames :: [String]
recognizedArgNames = map ("--"++) $ Map.keys recognizedArgs
allRecognizedArgNames = recognizedArgNames ++ simpleArgsNames

helpString :: String
helpString = "\n\
\A partial implementation of Tyler Hobbs's Fidenza algorithm, written in Haskell.\n\
\\n\
\Partial, as there currently is no support for creating spiral Fidenzas and for intuitive,\n\
\deterministic control over the density of Fidenzas.\n\
\\n\
\The control over the density is achieved by tweaking the maximum number of curves and\n\
\the curve widths distribution, as if the image is already mostly filled up by shapes\n\
\and {numGenerationAttempts} attemps have failed to generate a new random ribbon, then\n\
\the algorithm ends.\n\
\\n\
\A lot of arguments are of type distribution, which accepts 3 different kind of parameters:\n\
\   - a uniform distribution, specified by a list like so - [1,2,3,4,5,6], where\n\
\     each element has an equal probability of being chosen\n\
\   - a weighted distribution, specified by making each element a tuple like so:\n\
\     [(1,0.5),(2,0.1),(3,0.1),(4,0.1),(5,0.1),(6,0.1)], where the first member of\n\
\     each element is the value and the second is the probability of that value being chosen\n\
\   - a uniform distribution, specified by a range like so - (1,6,2), where\n\
\     the first number is the starting number, the second number is the end number and\n\
\     the third number is to step to keep taking from the starting number until you get to\n\
\     the end number. So (1,6,2) becomes [1,3,5].\n\
\\n\
\Usage: (./Fidenza|cabal run Fidenza -- ) [ARGS] \n\
\\n\
\Ex:\n\
\  ./Fidenza --seed 1\n\
\  cabal run Fidenza -- --seed 1\n\
\\n\
\Args:\n" ++ (concat . sort . Map.elems $ Map.map (("   --"++) . (++"\n\n") . show) recognizedArgs) ++ "\n\
\Simple Args:\n\
\  If the simple flags are used, then the arguments accepted are significantly\n\
\  simplified and act as switches on defined presets of the arguments above.\n\
\\n\
\  These presets are designed to resemble Tyler Hobbs's parameters as much as possible.\n\
\\n\
\  It's important to note that the presets are evaluated first, and then any of the main\n\
\  arguments above, which means preset values can be overwritten by the main arguments.\n\
\\n\
\  For example, the following command will NOT use the 'small' scale as specified by the\n\
\  preset, but instead use the widths provided afterwards.\n\
\\n\
\  ./Fidenza --scale small --widths  [80,90]\n\
\\n" ++ (concat . sort . Map.elems $ Map.map (("   --"++) . (++"\n\n") . show) simpleArgs)

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
