{-
  (C) 2017 David Lettier
  lettier.com
-}

module LinearRegression (
      runLinearRegression
    , runLinearRegressionOnce
    , hypothesis
    , cost
    , gradientForYIntercept
    , gradientForSlope
    , newYIntercept
    , newSlope
    , scaleXButNotY
    , unscaleYIntercept
    , unscaleSlope
    , standardDeviation
    , mean
    , xValues
    , yValues
  ) where

import Prelude

import Math (pow)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sum, foldl)

import Utils (lengthNum, arrayNumberHandler, firstOrSecondValues, first, second)

runLinearRegression :: Int -> Int -> Number -> Number -> Number -> Number -> Array (Array Number) -> Array Number
runLinearRegression _ _ _ _ yIntercept slope [] = [yIntercept, slope]
runLinearRegression
  currentIteration
  maxIterations
  maxCost
  learningRate
  yIntercept
  slope
  values
  =
    if cost' <= maxCost || currentIteration >= maxIterations
      then [newYIntercept', newSlope']
      else runLinearRegression (currentIteration + 1) maxIterations maxCost learningRate newYIntercept' newSlope' values
  where
    newYIntercept' = newYIntercept learningRate values yIntercept slope
    newSlope'      = newSlope      learningRate values yIntercept slope
    cost'          = cost hypothesis newYIntercept' newSlope' values

runLinearRegressionOnce :: Number -> Number -> Number -> Array (Array Number) -> Array Number
runLinearRegressionOnce _ yIntercept slope [] = [yIntercept, slope]
runLinearRegressionOnce
  learningRate
  yIntercept
  slope
  values
  = [newYIntercept', newSlope']
  where
    newYIntercept' = newYIntercept learningRate values yIntercept slope
    newSlope'      = newSlope      learningRate values yIntercept slope

hypothesis :: Number -> Number -> Number -> Number
hypothesis yIntercept slope x = yIntercept + slope * x

cost :: (Number -> Number -> Number -> Number) -> Number -> Number -> Array (Array Number) -> Number
cost _ _ _ [] = 0.0
cost hypothesisFunc yIntercept slope values = (sum <<< map error) values / size
  where
    size :: Number
    size = lengthNum values
    error :: Array Number -> Number
    error = arrayNumberHandler error'
    error' :: Number -> Number -> Number
    error' x y = pow (y - (hypothesisFunc yIntercept slope x)) 2.0

gradientForYIntercept :: Number -> Number -> Array (Array Number) -> Number
gradientForYIntercept yIntercept slope = gradient innerSum
  where
    innerSum :: Number -> Number -> Number
    innerSum x y = (yIntercept + slope * x) - y

gradientForSlope :: Number -> Number -> Array (Array Number) -> Number
gradientForSlope yIntercept slope = gradient innerSum
  where
    innerSum :: Number -> Number -> Number
    innerSum x y = ((yIntercept + slope * x) - y) * x

gradient :: (Number -> Number -> Number) -> Array (Array Number) -> Number
gradient _ []     = 0.0
gradient f values = (1.0 / size) * (sum <<< map innerSum) values
  where
    size :: Number
    size = lengthNum values
    innerSum :: Array Number -> Number
    innerSum = arrayNumberHandler f

newYIntercept :: Number -> Array (Array Number) -> Number -> Number -> Number
newYIntercept learningRate values oldYIntercept slope = newYIntercept'
  where
    gradientForYIntercept' :: Number
    gradientForYIntercept' = gradientForYIntercept oldYIntercept slope values
    newYIntercept' :: Number
    newYIntercept' = oldYIntercept - (learningRate * gradientForYIntercept')

newSlope :: Number -> Array (Array Number) -> Number -> Number -> Number
newSlope learningRate values yIntercept oldSlope = newSlope'
  where
    gradientForSlope' :: Number
    gradientForSlope' = gradientForSlope yIntercept oldSlope values
    newSlope' :: Number
    newSlope' = oldSlope - (learningRate * gradientForSlope')

scaleXButNotY :: Maybe Number -> Maybe Number -> Array (Array Number) -> Array (Array Number)
scaleXButNotY _             _            [] = []
scaleXButNotY mMean@(Just m) mStandardDeviation@(Just s) es = foldl innerFold [] es
  where
    innerFold acc [x, y] = acc <> [[fromMaybe 0.0 (scaleValue mMean mStandardDeviation x), y]]
    innerFold acc _      = acc
scaleXButNotY _             _            _  = []

scaleValue :: Maybe Number -> Maybe Number -> Number -> Maybe Number
scaleValue (Just mean') (Just 0.0)                unscaledValue = Nothing
scaleValue (Just mean') (Just standardDeviation') unscaledValue = Just ((unscaledValue - mean') / standardDeviation')
scaleValue _            _                         _             = Nothing

unscaleYIntercept :: Maybe Number -> Maybe Number -> Number -> Number -> Maybe Number
unscaleYIntercept (Just mean') (Just 0.0)  yInterceptScaled slopeScaled = Nothing
unscaleYIntercept (Just mean') (Just std') yInterceptScaled slopeScaled = Just (yInterceptScaled - (slopeScaled / std') * mean')
unscaleYIntercept _            _           _                _           = Nothing

unscaleSlope :: Maybe Number -> Number -> Maybe Number
unscaleSlope (Just 0.0) slopeScaled = Nothing
unscaleSlope (Just std) slopeScaled = Just (slopeScaled / std)
unscaleSlope _          _           = Nothing

standardDeviation :: Maybe Number -> Array Number -> Maybe Number
standardDeviation _           [] = Nothing
standardDeviation (Just mean') es = Just (pow base 0.5)
  where
    oneOverLength = 1.0 / lengthNum es
    summation     = sum $ map (\ e -> pow (e - mean') 2.0) es
    base          = oneOverLength * summation
standardDeviation _           _   = Nothing

mean :: Array Number -> Maybe Number
mean [] = Nothing
mean es = Just (sum es / lengthNum es)

xValues :: Array (Array Number) -> Array Number
xValues = firstOrSecondValues first

yValues :: Array (Array Number) -> Array Number
yValues = firstOrSecondValues second
