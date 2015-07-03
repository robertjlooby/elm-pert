module Pert where

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
  = UpdateOptimistic Int
  | UpdateRealistic Int
  | UpdatePessimistic Int

update : Action -> Model -> Model
update action model =
  case action of
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
