module Domain.Map exposing (Distance, Location, Map, Road, TravelTime, allLocations, allRoads, buildConnection, buildMap, decoded, parseMap, roadsFrom, travelTime)

import Csv.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Location =
    String


type alias Distance =
    Int


type alias Speed =
    Int


type alias TravelTime =
    Float


type alias Road =
    { a : Location
    , b : Location
    , distance : Distance
    , speed : Speed
    }


type alias Locations =
    Set Location


type alias Roads =
    Dict Location (Set ( Location, Distance, Speed ))


type Map
    = Map Locations Roads


toRoad from ( to, distance, speed ) =
    { a = from, b = to, distance = distance, speed = speed }


allRoads : Map -> List Road
allRoads (Map _ connections) =
    let
        asRoad ( from, tos ) =
            tos
                |> Set.toList
                |> List.map (toRoad from)
    in
    connections
        |> Dict.toList
        |> List.concatMap asRoad


roadsFrom : Map -> Location -> List Road
roadsFrom (Map _ connections) from =
    connections
        |> Dict.get from
        |> Maybe.withDefault Set.empty
        |> Set.toList
        |> List.map (toRoad from)


allLocations : Map -> Set Location
allLocations (Map locations _) =
    locations


travelTime : Road -> TravelTime
travelTime { distance, speed } =
    (toFloat distance) / (toFloat speed)


decoder : Decoder Road
decoder =
    Decode.into
        (\a b distance speed ->
            { a = a, b = b, distance = distance, speed = speed }
        )
        |> Decode.pipeline (Decode.column 0 Decode.string)
        |> Decode.pipeline (Decode.column 1 Decode.string)
        |> Decode.pipeline (Decode.column 2 Decode.int)
        |> Decode.pipeline (Decode.column 3 Decode.int)


decoded : String -> Result Decode.Error (List Road)
decoded csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv


buildConnection a b distance speed =
    { a = a, b = b, distance = distance, speed = speed }


buildMap : List Road -> Map
buildMap connections =
    let
        locations =
            connections
                |> List.concatMap (\c -> [ c.a, c.b ])
                |> Set.fromList

        addRoad from to distance speed roads =
            roads
                |> Dict.update
                    from
                    (\value ->
                        case value of
                            Just existing ->
                                existing
                                    |> Set.insert ( to, distance, speed )
                                    |> Just

                            Nothing ->
                                Set.singleton ( to, distance, speed )
                                    |> Just
                    )

        addRoads current roads =
            roads
                |> addRoad current.a current.b current.distance current.speed
                |> addRoad current.b current.a current.distance current.speed
    in
    Map locations (connections |> List.foldl addRoads Dict.empty)


parseMap : String -> Result Decode.Error Map
parseMap csv =
    csv
        |> decoded
        |> Result.map buildMap
