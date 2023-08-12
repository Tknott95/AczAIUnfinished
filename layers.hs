module Layers (
  RELU(RELU),
  RELU2(RELU2),
  LeakyRELU(LeakyRELU),
  LeakyRELU2(LeakyRELU2),
  Layer(..),
  iLayerUpdateWeights,
  iLayerUpdateBiases,
  iLayerUpdateWeightsCstm,
  iLayerUpdateBiasesCstm,
  iDLayerFromInputs,
  iDense,
  iDenseWithInputs,
  iSetInputs
) where

import Lin

import Control.Parallel.Strategies

----- Layer Types
--RELU
-- - string, Num
--Softmax
-- etc


data RELU = RELU String Double deriving (Eq, Show, Read)
data LeakyRELU = LeakyRELU String Double deriving (Eq, Show, Read)

data RELU2 = RELU2 { reluName :: String, reluVal :: Double } deriving (Eq, Show, Read)
data LeakyRELU2 = LeakyRELU2 { leakyName :: String, leakyVal :: Double } deriving (Eq, Show, Read)

data Layer = Layer { 
  inputs :: [[Double]], 
  weights :: [[Double]], 
  biases :: [[Double]]
} deriving (Show)

-- data TLayer = TLayer {
--   layer :: Layer,
--   dLayer :: Layer
-- }
-- dinputes
-- dweights
-- dbiases

matMPlus :: [[Double]] -> [[Double]] -> [[Double]]
matMPlus = zipWith(zipWith(+))

iLayerUpdateWeights :: (Int, Int) -> Int -> [[Double]] -> [[Double]] --Layer
iLayerUpdateWeights _size _seed _weights  = _weights `matMPlus` (Lin.randLayer005 (fst _size) (snd _size) _seed)  `using` parList rseq


iLayerUpdateBiases :: (Int, Int) -> Int -> [[Double]] -> [[Double]] --Layer
-- only randomzing positives
iLayerUpdateBiases _size _seed _biases  = _biases `matMPlus` ([take (snd _size) $ Lin.randList005 _seed]) `using` parList rseq


iLayerUpdateWeightsCstm :: Double -> (Int, Int) -> Int -> [[Double]] -> [[Double]] --Layer
iLayerUpdateWeightsCstm _mlt _size _seed _weights  = _weights `matMPlus` (Lin.randLayerCstm (fst _size) (snd _size) _mlt _seed)  `using` parList rseq

iLayerUpdateBiasesCstm :: Double -> (Int, Int) -> Int -> [[Double]] -> [[Double]] --Layer
iLayerUpdateBiasesCstm _mlt _size _seed _biases  = _biases `matMPlus` ([take (snd _size) $ Lin.randListCstm _mlt _seed]) `using` parList rseq


{-  !@NOTE numOfWeights' is changing to numOfInuts in Lin.RandLyaer @DOUBLECHECK @TODO
 - OLD:  Lin.randLayer numOfInputs' numofWeights' randSeed',,
-}
iDLayerFromInputs :: [[Double]] -> Int -> Layer
iDLayerFromInputs _inputs _randSeed = 
  Layer {
      inputs  = _inputs,
      weights = Lin.randLayer numOfInputs' numOfInputs' randSeed',
      biases  = [take numOfInputs' $ Lin.randList  (randSeed'+1)] --[zeroList numOfInputs']
  } where 
    numOfInputs' = length $ head _inputs {- COLS -}
    numofWeights' = length _inputs {- ROWS -}
    randSeed' = _randSeed

{- Build weights and biases on the fly without inputs for neuron expansion -}
iDense _numOfInputs _numOfWeights _randSeed = 
  Layer {
      inputs  = [[1]],
      weights = Lin.randLayer numOfInputs' numOfInputs' randSeed',
      biases  = [take numOfInputs' $ Lin.randList  (randSeed'+1)] --[zeroList numOfInputs']
  } where 
    numOfInputs' = _numOfInputs {- COLS -}
    numofWeights' = _numOfWeights {- ROWS -}
    randSeed' = _randSeed

iDenseWithInputs  _inputs _numOfInputs _numOfWeights _randSeed = 
  Layer {
      inputs  = _inputs,
      weights = Lin.randLayer numOfInputs' numOfInputs' randSeed',
      biases  = [take numOfInputs' $ Lin.randList  (randSeed'+1)] --[zeroList numOfInputs']
  } where 
    numOfInputs' = _numOfInputs {- COLS -}
    numofWeights' = _numOfWeights {- ROWS -}
    randSeed' = _randSeed

iSetInputs :: [[Double]] -> Layer -> Layer
iSetInputs _inputs _layer = 
  Layer {
    inputs  = _inputs,
    weights = weights _layer,
    biases  = biases _layer 
  }
