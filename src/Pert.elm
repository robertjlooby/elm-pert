module Pert where

import Html exposing (button, Html, div, input, text)
import Html.Attributes exposing (autofocus, placeholder, size, value)
import Html.Events exposing (on, onClick, targetValue)
import Maybe exposing (withDefault)
import Result exposing (toMaybe)
import StartApp exposing (start)
import String exposing (toInt)

main =
  StartApp.start
    { model = emptyModel
    , update = update
    , view = view
    }


-- Model
type alias Model =
  { optimistic : Int
  , realistic : Int
  , pessimistic : Int
  , combined : Float
  }

emptyModel : Model
emptyModel =
  { optimistic = 0
  , realistic = 0
  , pessimistic = 0
  , combined  = 0
  }

-- Update
type Action
  = Reset
  | UpdateOptimistic Int
  | UpdateRealistic Int
  | UpdatePessimistic Int

update : Action -> Model -> Model
update action model =
  case action of
    Reset ->
      emptyModel
    UpdateOptimistic value ->
      let newModel = { model | optimistic <- value }
      in  { newModel | combined <- calculateCombined newModel }

    UpdateRealistic value ->
      let newModel = { model | realistic <- value }
      in  { newModel | combined <- calculateCombined newModel }

    UpdatePessimistic value ->
      let newModel = { model | pessimistic <- value }
      in  { newModel | combined <- calculateCombined newModel }

calculateCombined : Model -> Float
calculateCombined model =
  if | model.optimistic == 0 -> 0
     | model.realistic == 0 -> 0
     | model.pessimistic == 0 -> 0
     | otherwise ->
       (rawScore model)
         |> (\score -> score * 4)
         |> round
         |> (\score -> (toFloat score) / 4)

rawScore : Model -> Float
rawScore model =
  (weightedMean model) + 2 * (standardDeviation model)

weightedMean : Model -> Float
weightedMean model =
  (toFloat (model.optimistic + 4 * model.realistic + model.pessimistic)) / 6

standardDeviation : Model -> Float
standardDeviation model =
  (toFloat (model.pessimistic - model.optimistic)) / 6

parseValue : String -> Int
parseValue str =
  toInt str |> toMaybe |> withDefault 0

valueDisplay : Int -> String
valueDisplay val =
  case val of
    0 -> ""
    _ -> toString val

view : Signal.Address Action -> Model -> Html
view address model = div []
                       [ input
                           [ placeholder "Optimistic"
                           , on "input" targetValue (parseValue >> UpdateOptimistic >> Signal.message address)
                           , value (valueDisplay model.optimistic)
                           , autofocus True
                           , size 11
                           ]
                           []
                       , input
                           [ placeholder "Realistic"
                           , on "input" targetValue (parseValue >> UpdateRealistic >> Signal.message address)
                           , value (valueDisplay model.realistic)
                           , size 11
                           ]
                           []
                       , input
                           [ placeholder "Pessimistic"
                           , on "input" targetValue (parseValue >> UpdatePessimistic >> Signal.message address)
                           , value (valueDisplay model.pessimistic)
                           , size 11
                           ]
                           []
                       , button [ onClick address Reset ] [ text "Reset" ]
                       , div [] [text (toString model.combined)]
                       ]
