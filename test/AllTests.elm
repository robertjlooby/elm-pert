module Main where

import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (Test, suite)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)

import PertTest

tests : Test
tests = suite "All elm-pert specs" [
                PertTest.tests
        ]

console : IO ()
console = runDisplay tests

port requests : Signal Request
port requests = IO.Runner.run responses console

port responses : Signal Response
