module ColourSchemes ( colourSchemes
                     , readColour
                     , readColourProbs
                     ) where

import Codec.Picture( PixelRGBA8( .. ), Pixel8)
import Data.List.Split (splitOn)

import Distribution (Distribution(..), toWeightedDistribution)

type ColourScheme = (PixelRGBA8, Distribution PixelRGBA8)

luxe,rad,golfSocks,baked,politique :: ColourScheme
cool,am,whiteOnCream,blackAndWhite,vs1,vs2,vs3 :: ColourScheme
colourSchemes :: [ColourScheme]

luxe = (PixelRGBA8 235 228 216 255,
        toWeightedDistribution [(PixelRGBA8  84  62  46 255, 0.08)
                               ,(PixelRGBA8 184 217 206 255, 0.06)
                               ,(PixelRGBA8 224 215 197 255, 0.05)
                               ,(PixelRGBA8  59  43  32 255, 0.04)
                               ,(PixelRGBA8 252 188  25 255, 0.03)
                               ,(PixelRGBA8 219  79  84 255, 0.03)
                               ,(PixelRGBA8 252 210 101 255, 0.02)
                               ,(PixelRGBA8  31  51  89 255, 0.02)
                               ,(PixelRGBA8  41 166 145 255, 0.02)
                               ,(PixelRGBA8  49  95 140 255, 0.02)
                               ,(PixelRGBA8 209  42  47 255, 0.02)
                               ,(PixelRGBA8 247 177 161 255, 0.01)
                               ,(PixelRGBA8 124 169 191 255, 0.01)
                               ,(PixelRGBA8 230 125  50 255, 0.01)])

rad = (PixelRGBA8 235 228 216 255,
       toWeightedDistribution [(PixelRGBA8 250 248 245 255, 0.23)
                              ,(PixelRGBA8 252 210 101 255, 0.06)
                              ,(PixelRGBA8 219  79  84 255, 0.04)
                              ,(PixelRGBA8 124 169 191 255, 0.07)
                              ,(PixelRGBA8 84  62   46 255, 0.02)
                              ,(PixelRGBA8 230 125  50 255, 0.02)
                              ,(PixelRGBA8 224 215 197 255, 0.02)
                              ,(PixelRGBA8 252 188  25 255, 0.01)])

golfSocks = (PixelRGBA8 102 128 106 255,
             toWeightedDistribution [(PixelRGBA8 231 225 213 255, 0.35)
                                    ,(PixelRGBA8 41 79 48 255, 0.16)
                                    ,(PixelRGBA8 36 77 115 255, 0.04)
                                    ,(PixelRGBA8 234 144 143 255, 0.03)
                                    ,(PixelRGBA8 34 75 115 255, 0.02)
                                    ,(PixelRGBA8 232 144 142 255, 0.01)
                                    ,(PixelRGBA8 22 50 77 255, 0.06)
                                    ,(PixelRGBA8 237 196 191 255, 0.01)])

baked = (PixelRGBA8  63 140  70 255,
         toWeightedDistribution [(PixelRGBA8  63 140  70 255, 0.59)
                                 ,(PixelRGBA8 247 212 203 255, 0.16)
                                 ,(PixelRGBA8 250 248 245 255, 0.10)
                                 ,(PixelRGBA8 202 230 204 255, 0.05)
                                 ,(PixelRGBA8 247 177 161 255, 0.04)
                                 ,(PixelRGBA8  99  77  58 255, 0.00)])

politique = (PixelRGBA8 235 228 216 255,
             toWeightedDistribution [(PixelRGBA8 252 210 101 255, 0.16)
                                    ,(PixelRGBA8 250 248 245 255, 0.20)
                                    ,(PixelRGBA8 247 177 161 255, 0.09)
                                    ,(PixelRGBA8  49  95 140 255, 0.03)
                                    ,(PixelRGBA8 219  79  84 255, 0.03)])
cool = (PixelRGBA8 235 228 216 255,
        toWeightedDistribution [(PixelRGBA8 128 171 192 255, 0.20)
                               ,(PixelRGBA8  58 101 143 255, 0.20)
                               ,(PixelRGBA8  34  54  91 255, 0.20)
                               ,(PixelRGBA8  21  30  54 255, 0.20)
                               ,(PixelRGBA8 224 215 197 255, 0.04)
                               ,(PixelRGBA8 203 193 224 255, 0.05)
                               ,(PixelRGBA8 185 217 206 255, 0.05)
                               ,(PixelRGBA8  44 167 146 255, 0.04)
                               ,(PixelRGBA8 208 230 221 255, 0.05)
                               ,(PixelRGBA8  61  45  34 255, 0.02)
                               ,(PixelRGBA8 251 210 103 255, 0.01)])
am = (PixelRGBA8  61  54  77 255,
      toWeightedDistribution [(PixelRGBA8  44  41  51 255, 1.20)
                              ,(PixelRGBA8 127 114 127 255, 0.15)
                              ,(PixelRGBA8 204 185 184 255, 0.15)
                              ,(PixelRGBA8 142 178 148 255, 0.04)
                              ,(PixelRGBA8 231 211 172 255, 0.04)
                              ,(PixelRGBA8 224 141 135 255, 0.04)])

whiteOnCream = (PixelRGBA8 235 228 216 255,
                toWeightedDistribution [(PixelRGBA8 250 248 245 255, 1.0)])

blackAndWhite = (PixelRGBA8 235 228 216 255,
                  toWeightedDistribution [(PixelRGBA8 10 10 10 255, 1.0)])

vs1 = (PixelRGBA8 235 228 216 255,
        toWeightedDistribution [(PixelRGBA8 38 70 83 255, 0.2)
                               ,(PixelRGBA8 42 157 143 255, 0.2)
                               ,(PixelRGBA8 233 196 106 255, 0.05)
                               ,(PixelRGBA8 244 162 97 255, 0.05)
                               ,(PixelRGBA8 231 111 81 255, 0.06)])

vs2 = (PixelRGBA8 235 228 216 255,
        toWeightedDistribution [(PixelRGBA8 0 48 73 255, 0.5)
                               ,(PixelRGBA8 214 40 40 255, 0.5)
                               ,(PixelRGBA8 247 127 0 255, 0.5)
                               ,(PixelRGBA8 252 191 73 255, 0.5)
                               ,(PixelRGBA8 234 226 183 255, 0.5)])
vs3 = (PixelRGBA8 235 228 216 255,
        toWeightedDistribution [(PixelRGBA8 247 37 133 255, 0.2)
                               ,(PixelRGBA8 114 9 183 255, 1.5)
                               ,(PixelRGBA8 72 12 168 255, 3.5)
                               ,(PixelRGBA8 63 55 201 255, 3.5)
                               ,(PixelRGBA8 72 149 239 255, 1.5)
                               ,(PixelRGBA8 76 201 240 255, 0.2)])

colourSchemes = [luxe, rad, golfSocks, baked, politique, cool, am, whiteOnCream, blackAndWhite, vs1, vs2, vs3]

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
