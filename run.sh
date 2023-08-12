#!/bin/sh

# GOOD DOCS ON COMPILER COMMANDS - https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/codegens.html#llvm-code-generator-fllvm# -A5m -N -Nn
# -O2 -fasm -fforce-recomp -fllvm -optc-O3
# ./   -xc -S -G1
ghc -threaded  -O -O2 -fforce-recomp -rtsopts $1.hs net.hs static_data.hs lin.hs layers.hs activations.hs colors.hs && time ./$1 -xc -RTS +RTS -A5m  && rm -f *.hi *.o $1
