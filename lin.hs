module Lin (
  dotProd,
  transpose,
  matDot,
  matDotPure,
  randSeed,
  randList,
  randLayer,
  randList005,
  randLayer005,
  randListCstm,
  randLayerCstm,
  zeroList,
  zeroLayer,
  calcMean,
  backpropActiSoftFull
) where

import System.Random
-- import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Parallel.Strategies

{--
  backPropActiSoft w/ jacobian matrix
 let sO = [0.7, 0.1, 0.2]
 let sO2 = reshapeOnez sO
 $ output - [[0.7],[0.1],[0.2]]

 let dotOfSO = matDot sO2 (transpose sO2) [0, 0, 0]
 $ output - [[0.48999998,7.0e-2,0.14]] - python gave me the same thing BEFORe i did a trnapsoe, I am not getting the matDot proper on my transpose. T order does not matter on dot
 @NOTE THIS STEP IS WRONG ^^ IT IS ONLY A (1, 3) instead of a (3, 3)
 THIS IS WORKING 
    *Lin> diagFlat (transpose (matDot  (transpose sO2) sO2 [0, 0, 0]) !! 0)
      [[0.48999998,0.0,0.0],[0.0,7.0e-2,0.0],[0.0,0.0,0.14]]
    ORs
    *Lin> diagFlat ((matDot  sO2 (transpose sO2) [0, 0, 0]) !! 0)
    [[0.48999998,0.0,0.0],[0.0,7.0e-2,0.0],[0.0,0.0,0.14]]
  let dotOfSo = diagFlat ((matDot  sO2 (transpose sO2) [0, 0, 0]) !! 0)
 @TODO look into how to handle, or np does, for the 1x3x3 - 1x3
 let diagFlatSO = (diagFlat sO)
 $ output -  [[0.7,0.0,0.0],[0.0,0.1,0.0],[0.0,0.0,0.2]]
 diagFlatsSO `minusFuncToCreate` dotOfSO
 $ output [ [.., .., ..],  [.., .., ..],  [.., .., ..]]


--}

-- @ the bias is adding another output, might eed to modularize as looping is still newer. An array that is only [] and not [[]] also won't fire in dotV2, handle this @TODO

-- @NOTES --
-- *) dot product includes a bias addition already built in, no need for the extra loop/step
-- @TODO output relies on amount of weights so I need to create a funciton to handle this proper.
-- going to split apart dotProd and bias
-- b needs to be a list so we can call each bias
-- Dot Prod of **n arrays --
-- multiDot :: Fractional a => [[a]] -> [[a]] -> [a] -> [a]
-- multiDot xs ys zs = [dotProd x y z | (x, y, z) <- zip3 xs ys zs] -- my bias needs to be brought in rather than 1

{-
 TO PUMP A RONOM LIST IN RANGE WITH SEED
  - randomRs(-1, 1) (mkStdGen seed) :: [Double]
 TO PUMP A RANDOM NUMBER IN RANGE W/ A SEED
  - (randomR(-1, 1) . mkStdGen $ seed)

TO PUMP A RANDOM IO WITHOUT SEED IN RANGE
  - randomRIO(-1, 1)
-}

{- dotProd :: Num a => [a] -> [a] -> a -> a -}
dotProd :: [Double] -> [Double] -> Double -> Double
dotProd xs ys z = sum[x*y | (x,y) <- zip xs ys] + z  `using` rseq

-- leaving polymorphic for now
transpose:: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

{- matDot :: Num a => [[a]] -> [[a]] -> [a] -> [[a]] -}
matDot :: [[Double]] -> [[Double]] -> [Double] -> [[Double]]
matDot inputs weights bs = [[dotProd i w b|  i <- inputs] | (w, b) <- zip weights bs] `using` parList rseq

matDotPure :: [[Double]] -> [[Double]] -> [[Double]]
matDotPure inputs weights = [[dotProd i w 0 |  i <- inputs] | w <- weights] `using` parList rseq
-- prob can  uncurry(dotProd) <$> zip inputs weights

-- randSeed :: IO Int
-- randSeed = (round . (* 1000)) <$> getPOSIXTime
randSeed = randomIO :: IO Int

randList :: Int -> [Double]  --changing to [Double]
-- used to be (*0.10)
randList seed = (*0.34) <$> randoms (mkStdGen seed) :: [Double]


{-
  take 5 $ randList005 4333636
  [1.3772963491464979e-2,2.3010258726375576e-3,4.006180730618544e-2,2.760574653745005e-2,4.6459592847868596e-2]

  @TODO - MAKE THIS ALSO PUMP OUT NEGATIVE VALUES
-}

randListCstm :: Double -> Int -> [Double]  --changing to [Double]
randListCstm _mlt _seed = (*_mlt) <$> randomRs(-1, 1) (mkStdGen _seed) :: [Double]
randLayerCstm :: Int -> Int -> Double -> Int -> [[Double]]
randLayerCstm m n _mlt seed = [  take m $ randListCstm _mlt (seed+j) | j <- [1..n] ]

-- THIS ALSO PUMPS NEGATIVE NUMBERS - THIS ALSO PUMPS NEGATIVE NUMBERS - THIS ALSO PUMPS NEGATIVE NUMBERS
randList005 :: Int -> [Double]  --changing to [Double]
-- randList005 seed = (*0.05) <$> randoms (mkStdGen seed) :: [Double]
randList005 seed = (*0.05) <$> randomRs(-1, 1) (mkStdGen seed) :: [Double]

randLayer005:: Int -> Int -> Int -> [[Double]]
randLayer005 m n seed = [  take m $ randList005 (seed+j) | j <- [1..n] ]

randLayer:: Int -> Int -> Int -> [[Double]]
randLayer m n seed = [  take m $ randList (seed+j) | j <- [1..n] ]

zeroLayer:: Int -> Int -> [[Double]]
zeroLayer m n = [  [0.0 | i <- [1..n]] | j <- [1..m] ]

zeroList:: Int -> [Double]
zeroList n = [0.0 | i <- [1..n]]

diagFlat :: [Double] -> [[Double]]
diagFlat _xs = [
  [(_xs !! 0), 0.0, 0.0],
  [0.0,(_xs !! 1), 0.0],
  [0.0, 0.0, (_xs !! 2)]]

reshapeOnez :: [Double] -> [[Double]]
reshapeOnez _xs = [[x] | x <- _xs]

--}
-- FIX POLYMORPHISM 
offshapeDot :: [Double] -> [[Double]] -> [[Double]]
offshapeDot _xs _ys = sfx
 where 
   sfx = map concat fx
   fx  = map ((\x -> map (\y -> (*y) <$> x) _xs)) _ys
{- example: offshapeDot s0 (reshapeOnez s0) -}

backpropActiSoft :: [Double] -> [[Double]]
backpropActiSoft _xs = sfx `using` parList rseq
 where
   sfx = offshapeDot _xs fx
   fx  = reshapeOnez _xs

subMats :: [[Double]] -> [[Double]] -> [[Double]]
subMats i j= zipWith (zipWith (-)) i j `using` parList rseq

backpropActiSoftFull :: [Double] -> [[Double]]
-- (diagFlat _xs) `subMats` (backpropActiSoft _xs)
backpropActiSoftFull _xs = diagFlat _xs `subMats` backpropActiSoft _xs
{- @NOTES
 - jacobian amtrix only pumps in 3 vals at a time. I need to look into this as it is what I wanted. What do other AIs do on backprop acti softs? 
 - A jacobian matrix could be made dynamic by multiplying said matrice by a matrices of the same shape yet the diagonals are 1. This lets you expand the and contract via i-i^n in the fly bby. 

 - (diagFlat [0.7, 0.1, 0.2]) `subMats` (backpropActiSoft [0.7, 0.1, 0.2])
--}

-- @TODO fix this from throwing a NaN
calcMean :: [Double] -> Double
calcMean _xs = (sum _xs) / (fromIntegral $ length _xs) `using` rseq