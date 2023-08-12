module Optimizers where

data OptimizerAdaGrad = OptimizerAdaGrad {
  learningRate :: Float,
  decay        :: Float,
  epsilon      :: Float
}
