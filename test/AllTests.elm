module Main where

import ElmTest.Runner.Element exposing (runDisplay)
import ElmTest.Test exposing (Test, suite)

import PertTest

tests : Test
tests = suite "All elm-pert specs" [
                PertTest.tests
        ]

main = runDisplay tests
