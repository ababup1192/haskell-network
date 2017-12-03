module Tests exposing (..)

import Test exposing (..)
import TestExp exposing (..)
import Parser exposing (run)


-- Test target modules

import HaskellParser exposing (func, Func(..))


all : Test
all =
    describe "Parser Test"
        [ "Parse"
            => Parser.run func "doubleMe x = x"
            === Ok (Func "doubleMe" [ "x" ] "x")
        ]
