module Main where

import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (Test, suite)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)


specs : Test
specs = suite "All elm-pert specs" []

console : IO ()
console = runDisplay specs

port requests : Signal Request
port requests = IO.Runner.run responses console

port responses : Signal Response
