module Tests exposing (..)

import Domain.Map exposing (..)
import Domain.ShortestPath exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (p)
import Set
import Test exposing (..)


canParseValidCsv : Test
canParseValidCsv =
    describe "Reading valid content"
        [ test "should work for one line" <|
            \_ ->
                let
                    csv =
                        """A,B,km,speed
Cogburg,Copperhold,1047,91"""
                in
                csv
                    |> decoded
                    |> Expect.equal (Ok [ { a = "Cogburg", b = "Copperhold", distance = 1047, speed = 91 } ])
        , test "give the right linecount" <|
            \_ ->
                let
                    csv =
                        """A,B,km,speed
Cogburg,Copperhold,1047,91
Leverstorm,Irondale,629,89
Cogburg,Steamdrift,1269,89
Copperhold,Irondale,345,91
Copperhold,Leverstorm,569,91
Leverstorm,Gizbourne,866,91
Rustport,Cogburg,1421,93
Rustport,Steamdrift,1947,99
Rustport,Gizbourne,1220,96
Irondale,Gizbourne,526,97
Cogburg,Irondale,1034,98
Rustport,Irondale,1302,95"""
                in
                csv
                    |> decoded
                    |> Result.map List.length
                    |> Expect.equal (Ok 12)
        ]


mapCsv =
    """A,B,km,speed
Cogburg,Copperhold,1047,91
Cogburg,Steamdrift,1269,89
Copperhold,Irondale,345,91"""


mapTests : Test
mapTests =
    describe "Can build a map from a csv"
        [ test "Locations are unique" <|
            \_ ->
                mapCsv
                    |> parseMap
                    |> Result.map Domain.Map.allLocations
                    |> Result.map Set.size
                    |> Expect.equal (Ok 4)
        , test "Road count is correct" <|
            \_ ->
                mapCsv
                    |> parseMap
                    |> Result.map Domain.Map.allRoads
                    |> Result.map List.length
                    |> Expect.equal (Ok 6)
        , test "Roads from is correct" <|
            \_ ->
                mapCsv
                    |> parseMap
                    |> Result.map (\map -> Domain.Map.roadsFrom map "Copperhold")
                    |> Expect.equal
                        (Ok
                            [ { a = "Copperhold", b = "Cogburg", distance = 1047, speed = 91 }
                            , { a = "Copperhold", b = "Irondale", distance = 345, speed = 91 }
                            ]
                        )
        ]


sampleMap =
    [ Domain.Map.buildConnection "a" "b" 1
    , Domain.Map.buildConnection "b" "c" 1
    , Domain.Map.buildConnection "c" "e" 1
    , Domain.Map.buildConnection "b" "e" 1
    , Domain.Map.buildConnection "b" "f" 1000
    , Domain.Map.buildConnection "c" "f" 1
    ]
        |> Domain.Map.buildMap


shortestPathTests : Test
shortestPathTests =
    describe "Can calculate shortest Path"
        [ test "With direct connection" <|
            \_ ->
                Domain.ShortestPath.calculatePath sampleMap { from = "a", to = "b" }
                    |> Expect.equal
                        (Just [ "a", "b" ])
        , test "With one hop" <|
            \_ ->
                Domain.ShortestPath.calculatePath sampleMap { from = "a", to = "c" }
                    |> Expect.equal
                        (Just [ "a", "b", "c" ])
        , test "With two possibilities" <|
            \_ ->
                Domain.ShortestPath.calculatePath sampleMap { from = "a", to = "e" }
                    |> Expect.equal
                        (Just [ "a", "b", "e" ])
        , test "With two possibilities where the one with more milestones is cheaper" <|
            \_ ->
                Domain.ShortestPath.calculatePath sampleMap { from = "a", to = "f" }
                    |> Expect.equal
                        (Just [ "a", "b", "c", "f" ])
        ]


acceptenceTests : Test
acceptenceTests =
    describe "Decodes the provided example correctly"
        [ test "give the right linecount" <|
            \_ ->
                let
                    csv =
                        """A,B,km,speed
Cogburg,Copperhold,1047,91
Leverstorm,Irondale,629,89
Cogburg,Steamdrift,1269,89
Copperhold,Irondale,345,91
Copperhold,Leverstorm,569,91
Leverstorm,Gizbourne,866,91
Rustport,Cogburg,1421,93
Rustport,Steamdrift,1947,99
Rustport,Gizbourne,1220,96
Irondale,Gizbourne,526,97
Cogburg,Irondale,1034,98
Rustport,Irondale,1302,95"""
                in
                csv
                    |> parseMap
                    |> Result.map (\m -> Domain.ShortestPath.calculatePath m { from = "Steamdrift", to = "Leverstorm" })
                    |> Expect.equal
                        (Ok
                            (Just
                                [ "Steamdrift", "Cogburg", "Copperhold", "Leverstorm" ]
                            )
                        )
        ]
