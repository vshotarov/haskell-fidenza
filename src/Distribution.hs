module Distribution ( Distribution(..)
                    , toWeightedDistribution
                    , mapDistribution
                    , readDistribution
                    , sampleDistribution
                    , nullDistribution
                    ) where

import Data.List (sortBy)

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

nullDistribution :: Distribution a -> Bool
nullDistribution (Distribution []) = True
nullDistribution _ = False
