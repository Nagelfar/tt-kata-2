module Domain.Train exposing (TravelSpeedSample, buildSample, calculateAverageSpeed, decoded, predictTravelSpeed)

import Csv.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import List.Extra as List


type alias TransportIdentifier =
    String


type alias Timestamp =
    String


type alias Location =
    String


type alias Speed =
    Float


type alias Road =
    { a : Location, b : Location }


type alias PredictedTravelSpeed =
    { road : Road
    , speed : Speed
    }


type alias TravelSpeedSample =
    { transport : TransportIdentifier
    , time : Timestamp
    , road : Road
    , speed : Speed
    }


buildSample : TransportIdentifier -> Timestamp -> Location -> Location -> Speed -> TravelSpeedSample
buildSample transport time a b speed =
    { transport = transport
    , time = time
    , road = { a = a, b = b }
    , speed = speed
    }


decoder : Decoder TravelSpeedSample
decoder =
    Decode.into
        buildSample
        |> Decode.pipeline (Decode.column 0 Decode.string)
        |> Decode.pipeline (Decode.column 1 Decode.string)
        |> Decode.pipeline (Decode.column 2 Decode.string)
        |> Decode.pipeline (Decode.column 3 Decode.string)
        |> Decode.pipeline (Decode.column 4 Decode.float)


decoded : String -> Result Decode.Error (List TravelSpeedSample)
decoded csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv


calculateAverageSpeed : List Speed -> Speed
calculateAverageSpeed speeds =
    (speeds |> List.sum) / toFloat (speeds |> List.length)


predictTravelSpeed : List TravelSpeedSample -> List PredictedTravelSpeed
predictTravelSpeed samples =
    samples
        |> List.groupWhile (\left right -> left.road.a == right.road.a && left.road.b == right.road.b)
        |> List.map
            (\( grouping, speeds ) ->
                { road = grouping.road
                , speed =
                    grouping
                        :: speeds
                        |> List.map .speed
                        |> calculateAverageSpeed
                }
            )
