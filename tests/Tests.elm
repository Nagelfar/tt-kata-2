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
                        """A,B,km
Cogburg,Copperhold,1047"""
                in
                csv
                    |> decoded
                    |> Expect.equal (Ok [ { a = "Cogburg", b = "Copperhold", distance = 1047 } ])
        , test "give the right linecount" <|
            \_ ->
                let
                    csv =
                        """A,B,km
Cogburg,Copperhold,1047
Leverstorm,Irondale,673
Cogburg,Steamdrift,1269
Copperhold,Irondale,345
Copperhold,Leverstorm,569
Leverstorm,Gizbourne,866
Rustport,Cogburg,1421
Rustport,Steamdrift,1947
Rustport,Gizbourne,1220
Irondale,Gizbourne,526
Cogburg,Irondale,1034
Rustport,Irondale,1302"""
                in
                csv
                    |> decoded
                    |> Result.map List.length
                    |> Expect.equal (Ok 12)
        ]


mapCsv =
    """A,B,km
Cogburg,Copperhold,1047
Cogburg,Steamdrift,1269
Copperhold,Irondale,345"""


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
                            [ ( "Copperhold", "Cogburg", 1047 )
                            , ( "Copperhold", "Irondale", 345 )
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
                        """A,B,km
Cogburg,Copperhold,1047
Leverstorm,Irondale,673
Cogburg,Steamdrift,1269
Copperhold,Irondale,345
Copperhold,Leverstorm,569
Leverstorm,Gizbourne,866
Rustport,Cogburg,1421
Rustport,Steamdrift,1947
Rustport,Gizbourne,1220
Irondale,Gizbourne,526
Cogburg,Irondale,1034
Rustport,Irondale,1302"""
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
