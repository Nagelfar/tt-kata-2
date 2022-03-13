module TrainTests exposing (..)

import Domain.Train exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)


canParseValidCsv : Test
canParseValidCsv =
    describe "Reading valid trainingset"
        [ test "should work for one line" <|
            \_ ->
                let
                    csv =
                        """TRANSPORT,TIME,A,B,SPEED
CARGO_15100,1859-11-25 10:16:40,Steamdrift,Rustport,74.09"""
                in
                csv
                    |> decoded
                    |> Expect.equal (Ok [ { transport = "CARGO_15100", time = "1859-11-25 10:16:40", road = { a = "Steamdrift", b = "Rustport" }, speed = 74.09 } ])
        , test "give the right linecount" <|
            \_ ->
                let
                    csv =
                        """TRANSPORT,TIME,A,B,SPEED
CARGO_15100,1859-11-25 10:16:40,Steamdrift,Rustport,74.09
CARGO_15101,1859-11-26 03:58:07,Rustport,Irondale,73.6
CARGO_15101,1859-11-26 13:02:07,Irondale,Leverstorm,69.37
CARGO_15102,1859-11-26 19:55:34,Leverstorm,Copperhold,82.58
CARGO_15102,1859-11-27 09:23:42,Copperhold,Cogburg,77.73
CARGO_15103,1859-11-28 02:30:18,Cogburg,Steamdrift,74.17
CARGO_15104,1859-11-28 19:45:01,Steamdrift,Cogburg,73.59
CARGO_15104,1859-11-29 08:41:30,Cogburg,Irondale,79.9
CARGO_15104,1859-11-29 14:59:00,Irondale,Gizbourne,83.6
CARGO_15105,1859-11-29 22:00:28,Gizbourne,Irondale,74.88"""
                in
                csv
                    |> decoded
                    |> Result.map List.length
                    |> Expect.equal (Ok 10)
        ]


canPredictSpeed : Test
canPredictSpeed =
    describe "can predicting speed"
        [ test "for a single value" <|
            \_ ->
                buildSample "t1" "time" "a" "b" 10.0
                    |> List.singleton
                    |> predictTravelSpeed
                    |> Expect.equalLists [ { road = { a = "a", b = "b" }, speed = 10.0 } ]
        , test "for a single transport on the same road by using the average" <|
            \_ ->
                [ buildSample "t1" "time" "a" "b" 10.0
                , buildSample "t1" "time" "a" "b" 20.0
                ]
                    |> predictTravelSpeed
                    |> Expect.equalLists [ { road = { a = "a", b = "b" }, speed = 15.0 } ]
        , test "for a two different transports on the same road by using the average" <|
            \_ ->
                [ buildSample "t1" "time" "a" "b" 10.0
                , buildSample "t2" "time" "a" "b" 20.0
                ]
                    |> predictTravelSpeed
                    |> Expect.equalLists [ { road = { a = "a", b = "b" }, speed = 15.0 } ]
        , test "for transports on the different roads by using the average" <|
            \_ ->
                [ buildSample "t1" "time" "a" "b" 10.0
                , buildSample "t2" "time" "a" "b" 20.0
                , buildSample "t1" "time" "c" "d" 5.0
                , buildSample "t2" "time" "c" "d" 15.0
                ]
                    |> predictTravelSpeed
                    |> Expect.equalLists
                        [ { road = { a = "a", b = "b" }, speed = 15.0 }
                        , { road = { a = "c", b = "d" }, speed = 10.0 }
                        ]
        ]


canCalculateAverage : Test
canCalculateAverage =
    describe "When calculating the average"
        [ test "it works for one value" <|
            \_ ->
                calculateAverageSpeed [ 10.0 ]
                    |> Expect.equal 10.0
        ]
