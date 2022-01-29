module VectorFieldGenerator ( VectorFieldGenerator(..)
                            , VectorFieldFunc
                            , readVectorFieldFromFile
                            ) where

import Data.List (isPrefixOf)

data VectorFieldGenerator = PerlinNoise Float Int
                          | FromFile String
                          | Straight
                          | Horizontal
                            deriving stock (Show)

instance Read VectorFieldGenerator where
    readsPrec _ "Straight" = [(Straight,"")]
    readsPrec _ "Horizontal" = [(Horizontal,"")]
    readsPrec _ str
      | isPrefixOf "PerlinNoise " str =
          let params = words $ drop (length "PerlinNoise ") str
           in [(PerlinNoise (read $ params !! 0) (read $ params !! 1), "")]
      | isPrefixOf "FromFile " str =
          [(FromFile $ drop (length "FromFile ") $ str, "")]
    readsPrec _ str = error $ "VectorFieldGenerator.read: Can't read " ++ str

type Vector = (Float,Float)
type VectorField = [[Vector]]
type VectorFieldFunc = (Vector -> Vector)

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
