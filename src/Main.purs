{-
  (C) 2017 David Lettier
  lettier.com
-}

module Main where

import Prelude

import Data.Generic (gShow)
import Data.Ord (abs)
import Data.Foldable (foldr)
import Data.Array (drop, (:), length, head, last)
import Data.Maybe (Maybe(..), isNothing, fromMaybe, isJust)

import Control.Monad.Aff (Aff, later')
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import LinearRegression (
      runLinearRegressionOnce
    , cost
    , hypothesis
    , standardDeviation
    , mean
    , scaleXButNotY
    , unscaleYIntercept
    , unscaleSlope
    , xValues
  )
import Plot (PLOT, PlotData, emptyPlotData, makePlot)
import Utils (maybeNumberToString, stringToMaybeNumber, numberInvalid, arrayMinOrMax)

type Effects eff = H.HalogenEffects (plot :: PLOT, console :: CONSOLE | eff)

type Point = { id :: Int, x :: Maybe Number, y :: Maybe Number }

data Query a = PushPoint a | PopPoint a | RemovePoint Int a | UpdatePointCoordinate Int String String a | RunLinearRegression a

type State = { nextId :: Int, points :: Array Point, yIntercept :: Maybe Number, slope :: Maybe Number, running :: Boolean }

main :: Eff (Effects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  uiContainer <- selectElement "#uiContainer"
  H.runUI linearRegressionUIComponent initialState (fromMaybe body uiContainer)
  makePlot emptyPlotData

linearRegressionUIComponent :: forall eff. H.Component State Query (Aff (Effects eff))
linearRegressionUIComponent = H.component { render, eval }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_ [
            HH.div_ [
                  HH.div_ [
                        HH.b_ [
                            HH.text "Status: "
                          ]
                      , HH.text
                          if state.running
                            then "Running..."
                            else if hasValidPoints state then "Press run." else "Add points."
                    ]
                  , HH.div_ [
                          HH.b_ [
                              HH.text "Y-Intercept: "
                            ]
                        , HH.text
                          if isJust state.yIntercept && hasValidPoints state
                            then "" <> (maybeNumberToString state.yIntercept)
                            else "?"
                      ]
                  , HH.div_ [
                          HH.b_ [
                              HH.text "Slope: "
                            ]
                        , HH.text
                          if isJust state.slope && hasValidPoints state
                            then "" <> (maybeNumberToString state.slope)
                            else "?"
                      ]
              ]
          , HH.div_ [
                  HH.button [
                        HE.onClick (HE.input_ PushPoint)
                    ] [
                        HH.text "Push Point"
                    ]
                , HH.button [
                        HE.onClick (HE.input_ PopPoint)
                    ] [
                        HH.text "Pop Point"
                    ]
                , HH.button [
                        HE.onClick (HE.input_ RunLinearRegression)
                      , HP.class_ (className "runButton")
                    ] [
                        HH.text "Run"
                    ]
                , HH.div_ (
                      map (\ point ->
                          HH.li_ [
                                HH.input [
                                      HP.value (maybeNumberToString point.x)
                                    , HP.placeholder "Input the X Coordinate"
                                    , HE.onValueChange (HE.input (UpdatePointCoordinate point.id "x"))
                                  ]
                              , HH.input [
                                      HP.value (maybeNumberToString point.y)
                                    , HP.placeholder "Input the Y Coordinate"
                                    , HE.onValueChange (HE.input (UpdatePointCoordinate point.id "y"))
                                  ]
                              , HH.button [
                                      HE.onClick (HE.input_ (RemovePoint point.id))
                                    , HP.class_ (className "removeButton")
                                  ] [
                                      HH.text "X"
                                  ]
                            ]
                        )
                        state.points
                    )
              ]
        ]
    eval :: Query ~> H.ComponentDSL State Query (Aff (Effects eff))
    eval (PushPoint next) = do
      H.modify (\ state -> state {
              nextId = state.nextId + 1
            , points = newPoint state.nextId : state.points
            , yIntercept = Nothing
            , slope = Nothing
          }
        )
      pure next
    eval (PopPoint next) = do
      H.modify (\ state -> state {
              points = drop 1 state.points
            , yIntercept = Nothing
            , slope = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (RemovePoint id next) = do
      H.modify (\ state -> state {
              points = foldr (\ point acc ->
                  if point.id == id then acc else point : acc
                ) [] state.points
            , yIntercept = Nothing
            , slope = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (UpdatePointCoordinate id key value next) = do
      H.modify (\ state -> state {
              points = foldr (\ point acc ->
                  (if point.id == id then updatePointCoordinateFromString point key value else point) : acc
                ) [] state.points
            , yIntercept = Nothing
            , slope = Nothing
          }
        )
      currentState <- H.get
      _ <- H.fromAff (makePlot (makePlotDataFromState currentState))
      pure next
    eval (RunLinearRegression next) = do
      H.modify (\ state -> state { yIntercept = Nothing, slope = Nothing, running = true })
      currentState <- H.get
      result <- H.fromAff (later' 1 (runLinearRegressionAsync currentState 0.05))
      H.modify (\ state -> state { yIntercept = head result, slope = last result, running = false })
      if isJust (head result) && isJust (last result)
        then do
          currentState' <- H.get
          _ <- H.fromAff (makePlot (makePlotDataFromState currentState'))
          pure next
        else pure next
      pure next

initialState :: State
initialState = { nextId: 0, points: [], yIntercept: Nothing, slope: Nothing, running: false }

newPoint :: Int -> Point
newPoint id = { id: id, x: Nothing, y: Nothing }

updatePointCoordinateFromString :: Point -> String -> String -> Point
updatePointCoordinateFromString point "x" s = point { x = stringToMaybeNumber s }
updatePointCoordinateFromString point "y" s = point { y = stringToMaybeNumber s }
updatePointCoordinateFromString point  _  _ = point

runLinearRegressionAsync :: forall e. State -> Number -> Aff (console :: CONSOLE | e) (Array Number)
runLinearRegressionAsync { points: [] }             _               = pure []
runLinearRegressionAsync { points: [{x: _, y: _}] } _               = pure []
runLinearRegressionAsync state                      oldLearningRate = do
  let valuesUnscaled      = coords state
  let xValues'            = xValues valuesUnscaled
  let mean'               = mean xValues'
  let standardDeviation'  = standardDeviation mean' xValues'
  let valuesScaled        = scaleXButNotY mean' standardDeviation' valuesUnscaled
  let oldYInterceptScaled = fromMaybe 0.0 state.yIntercept
  let oldSlopeScaled      = fromMaybe 0.0 state.slope
  runLinearRegressionAsync'
    0
    mean'
    standardDeviation'
    oldLearningRate
    oldYInterceptScaled
    oldSlopeScaled
    valuesUnscaled
    valuesScaled

runLinearRegressionAsync' ::
  forall e.
  Int ->
  Maybe Number ->
  Maybe Number ->
  Number ->
  Number ->
  Number ->
  Array (Array Number) ->
  Array (Array Number) ->
  Aff (console :: CONSOLE | e) (Array Number)
runLinearRegressionAsync' _ _ _ _ _ _ []       _ = pure []
runLinearRegressionAsync' _ _ _ _ _ _ [[_, _]] _ = pure []
runLinearRegressionAsync'
  iterations
  mMean@(Just mean')
  mStandardDeviation@(Just standardDeviation')
  oldLearningRate
  oldYInterceptScaled
  oldSlopeScaled
  valuesUnscaled
  valuesScaled
  = do
    let result                = runLinearRegressionOnce oldLearningRate oldYInterceptScaled oldSlopeScaled valuesScaled
    let newYInterceptScaled   = fromMaybe 0.0 (head result)
    let newSlopeScaled        = fromMaybe 0.0 (last result)
    let oldYInterceptUnscaled = fromMaybe 0.0 $ unscaleYIntercept mMean mStandardDeviation oldYInterceptScaled oldSlopeScaled
    let oldSlopeUnscaled      = fromMaybe 0.0 $ unscaleSlope mStandardDeviation oldSlopeScaled
    if numberInvalid newYInterceptScaled || numberInvalid newSlopeScaled
      then do
        pure [oldYInterceptUnscaled, oldSlopeUnscaled]
      else do
        let newYInterceptUnscaled = fromMaybe 0.0 $ unscaleYIntercept mMean mStandardDeviation newYInterceptScaled newSlopeScaled
        let newSlopeUnscaled      = fromMaybe 0.0 $ unscaleSlope mStandardDeviation newSlopeScaled
        let prevCost              = cost hypothesis oldYInterceptUnscaled oldSlopeUnscaled valuesUnscaled
        let newCost               = cost hypothesis newYInterceptUnscaled newSlopeUnscaled valuesUnscaled
        when debug (log ("prevCost "               <> (gShow prevCost)))
        when debug (log ("newCost "                <> (gShow newCost)))
        when debug (log ("iterations "             <> (gShow iterations)))
        when debug (log ("abs prevCost - newCost " <> (gShow (abs (prevCost - newCost)))))
        if iterations >= 10000 || newCost <= 1e-11 || (abs (prevCost - newCost)) == 0.0
          then pure [newYInterceptUnscaled, newSlopeUnscaled]
          else do
            -- Bold Driver - http://www.willamette.edu/~gorr/classes/cs449/momrate.html
            let yInterceptScaled  = if prevCost < newCost then oldYInterceptScaled else newYInterceptScaled
            let slopeScaled       = if prevCost < newCost then oldSlopeScaled      else newSlopeScaled
            let newLearningRate   = if prevCost < newCost
                                      then oldLearningRate - (oldLearningRate * 0.5 )
                                      else oldLearningRate + (oldLearningRate * 0.05)
            when debug (log ("newLearningRate " <> (gShow newLearningRate)))
            later' 1 (
                runLinearRegressionAsync'
                  (iterations + 1)
                  mMean
                  mStandardDeviation
                  newLearningRate
                  yInterceptScaled
                  slopeScaled
                  valuesUnscaled
                  valuesScaled
              )
runLinearRegressionAsync' _ _ _ _ _ _ _ _ = pure []

coords :: State -> Array (Array Number)
coords { points: [] }     = []
coords { points: points } = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else [fromMaybe 0.0 point.x,  fromMaybe 0.0 point.y] : acc
  ) [] points

getXValuesFromState :: State -> Array (Number)
getXValuesFromState { points: points } = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else fromMaybe 0.0 point.x : acc
  ) [] points

makePlotDataFromState :: State -> PlotData
makePlotDataFromState state@{
      points: points
    , yIntercept: (Just yIntercept)
    , slope: (Just slope)
  } = {
      scatter: pointsToScatterData points
    , line: if length xValues > 0
      then [
            {
                x: minX
              , y: hypothesis yIntercept slope minX
            }
          , {
                x: maxX
              , y: hypothesis yIntercept slope maxX
            }
        ]
      else []
  }
  where
    xValues    = getXValuesFromState state
    minX       = arrayMinOrMax min 0.0 xValues
    maxX       = arrayMinOrMax max 0.0 xValues
makePlotDataFromState state@{
      points: points
    , yIntercept: Nothing
    , slope: Nothing
  } = {
      scatter: pointsToScatterData points
    , line: []
  }
makePlotDataFromState state@{
      points: points
    , yIntercept: (Just _)
    , slope: Nothing
  } = {
      scatter: pointsToScatterData points
    , line: []
  }
makePlotDataFromState state@{
      points: points
    , yIntercept: Nothing
    , slope: (Just _)
  } = {
      scatter: pointsToScatterData points
    , line: []
  }

pointsToScatterData :: Array Point -> Array { x :: Number, y :: Number }
pointsToScatterData [] = []
pointsToScatterData points = foldr (\ point acc ->
    if isNothing point.x || isNothing point.y
      then acc
      else { x: fromMaybe 0.0 point.x, y: fromMaybe 0.0 point.y } : acc
  ) [] points

hasValidPoints :: State -> Boolean
hasValidPoints { points: [] } = false
hasValidPoints state          = (length <<< coords) state > 0

debug :: Boolean
debug = false
