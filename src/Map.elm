module Map exposing (Distance, Location, Map, Road, allLocations, allRoads, decoded, parseMap, roadsFrom, buildMap, buildConnection)

import Csv.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Location =
    String


type alias Distance =
    Int


type alias Road =
    ( Location, Location, Distance )


type alias Connection =
    { a : Location
    , b : Location
    , distance : Distance
    }


type alias Locations =
    Set Location


type alias Connections =
    Dict Location (Set ( Location, Distance ))


type Map
    = Map Locations Connections


allRoads : Map -> List Road
allRoads (Map _ connections) =
    let
        asRoad ( from, tos ) =
            tos
                |> Set.toList
                |> List.map
                    (\( to, distance ) ->
                        ( from, to, distance )
                    )
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
        |> List.map (\( to, d ) -> ( from, to, d ))


allLocations : Map -> Set Location
allLocations (Map locations _) =
    locations


decoder : Decoder Connection
decoder =
    Decode.map3 Connection
        (Decode.column 0 Decode.string)
        (Decode.column 1 Decode.string)
        (Decode.column 2 Decode.int)


decoded : String -> Result Decode.Error (List Connection)
decoded csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv

buildConnection a b distance =
    { a = a, b= b, distance =distance}

buildMap : List Connection -> Map
buildMap connections =
    let
        locations =
            connections
                |> List.concatMap (\c -> [ c.a, c.b ])
                |> Set.fromList

        addRoad from to distance roads =
            roads
                |> Dict.update
                    from
                    (\value ->
                        case value of
                            Just existing ->
                                existing
                                    |> Set.insert ( to, distance )
                                    |> Just

                            Nothing ->
                                Set.singleton ( to, distance )
                                    |> Just
                    )

        addRoads current roads =
            roads
                |> addRoad current.a current.b current.distance
                |> addRoad current.b current.a current.distance
    in
    Map locations (connections |> List.foldl addRoads Dict.empty)


parseMap : String -> Result Decode.Error Map
parseMap csv =
    csv
        |> decoded
        |> Result.map buildMap
