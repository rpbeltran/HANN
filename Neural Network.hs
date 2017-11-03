


module NeuralNetwork (network, forward, sigmoid_activation, relu_activation, tanh_activation, bounded_activation, unbounded_activation) where

import Numeric.LinearAlgebra
import System.Random

-- Random Initialization Generator

data Rgen = Rgen Int StdGen

randWeights :: [Int] -> [Matrix Double]
randWeights layers = randWeightsHelper zipLayers 0 where

    zipLayers = zip layers (tail layers)

    randWeightsHelper :: [ (Int, Int) ] -> Int -> [Matrix Double]
    randWeightsHelper [] i = []
    randWeightsHelper zlayers i = rLayer : ( randWeightsHelper ( tail zlayers ) (i + 1) ) where

        rLayer = matrix np $ randSequence (p * np) ( i * 10000 * p )
        ( p, np ) = head zlayers

    -- Random Sequence Generation
    randSequence :: Int -> Int -> [ Double ]
    randSequence length randomStart = fst $ rseqhelper length initial_generator where

        initial_generator = Rgen val gen where
            ( val, gen ) = random (mkStdGen randomStart)

        rseqhelper :: Int -> Rgen -> ( [ Double ], Rgen )
        rseqhelper 0 gen = ( [], gen )

        rseqhelper length (Rgen v g) = ( 1.0 - 2.0 * ( fromIntegral (mod val 1000000) / 1000000 ) : fst( rseqhelper(length-1) nextRgen ), nextRgen ) where
            nextRgen = Rgen val gen
            ( val, gen ) = random g



-- Neural Network

data Network = Network [ Int ] [ Matrix Double ] ( ( Matrix Double ) -> ( Matrix Double ) )

network :: [ Int ] -> ( Double -> Double ) -> Network
network layers a = Network layers (randWeights layers) activation where
    activation = fromLists . ( map . map $ a ) . toLists


-- Forward Propagation

forward :: Network -> Matrix Double -> Matrix Double
forward (Network _ ws activation) x = foldl (\l w -> activation $ l <> w) x ws


-- Genetic Learning


-- Back Propagation Learning



-- Common Activation Functions

sigmoid_activation :: Double -> Double
sigmoid_activation x = 1 / ( exp x + 1 )

relu_activation :: Double -> Double
relu_activation = max 0

tanh_activation :: Double -> Double
tanh_activation = ( \u -> (1-u) / (1+u) ) . exp

bounded_activation :: Double -> Double
bounded_activation = max (-1)

unbounded_activation :: Double -> Double
unbounded_activation = id





