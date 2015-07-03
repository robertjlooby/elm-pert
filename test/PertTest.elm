module PertTest where

import Pert exposing (..)
import ElmTest.Assertion exposing (assertEqual, assert, Assertion)
import ElmTest.Test exposing (test, suite)

tests =
  suite "Pert"
    [ (suite "emptyModel"
        [ (test "starts with an optimistic of 0" (ae 0  emptyModel.optimistic))
        , (test "starts with a realistic of 0" (ae 0 emptyModel.realistic))
        , (test "starts with a pessimistic of 0" (ae 0 emptyModel.pessimistic))
        , (test "starts with a combined of 0" (ae 0 emptyModel.combined))
        ])
    , (suite "update"
        [ (test "updates the optimistic value" (ae 2 (.optimistic (update (UpdateOptimistic 2) emptyModel))))
        , (test "updates the realistic value" (ae 3 (.realistic (update (UpdateRealistic 3) emptyModel))))
        , (test "updates the pessimistic value" (ae 4 (.pessimistic (update (UpdatePessimistic 4) emptyModel))))
        , (test "updates the combined value" (assertWithin 2.75 (.combined (update (UpdateOptimistic 1) (model 0 2 3))) 0.001))
        , (test "updates the combined value" (assertWithin 2.75 (.combined (update (UpdateRealistic 2) (model 1 0 3))) 0.001))
        , (test "updates the combined value" (assertWithin 2.75 (.combined (update (UpdatePessimistic 3) (model 1 2 0))) 0.001))
        ])
    , (suite "calculateCombined"
        [ (test "returns 0 if optimistic is 0" (ae 0 (calculateCombined (model 0 2 3))))
        , (test "returns 0 if realistic is 0" (ae 0 (calculateCombined (model 1 0 3))))
        , (test "returns 0 if pessimistic is 0" (ae 0 (calculateCombined (model 1 2 0))))
        , (test "returns 3 if all 3s" (ae 3 (calculateCombined (model 3 3 3))))
        , (test "returns 2.75 for 1/2/3" (assertWithin 2.75 (calculateCombined (model 1 2 3)) 0.001))
        , (test "returns 2.5 for 2/2/3" (ae 2.5 (calculateCombined (model 2 2 3))))
        , (test "returns 3.25 for 2/2/3" (ae 2.5 (calculateCombined (model 2 2 3))))
        ])
    , (suite "rawScore"
        [ (test "returns 0 for all 0s" (ae 0 (rawScore emptyModel)))
        , (test "returns 3 for all 3s" (ae 3 (rawScore (model 3 3 3))))
        , (test "returns 2.67 for 1/2/3" (ae (8 / 3) (rawScore (model 1 2 3))))
        , (test "returns 2.5 for 2/2/3" (ae 2.5 (rawScore (model 2 2 3))))
        , (test "returns 3.33 for 1/3/3" (assertWithin (10 / 3) (rawScore (model 1 3 3)) 0.001))
        ])
    ]

ae = assertEqual

assertWithin : number -> number -> number -> Assertion
assertWithin expected actual delta = assert <| delta > abs (actual - expected)

model o r p =
  { optimistic = o
  , realistic = r
  , pessimistic = p
  , combined = 0
  }
