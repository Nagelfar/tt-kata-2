module Domain.Itinerary exposing (Itinerary, Milestone, asString, fromPath)

import Domain.ETA exposing (Path(..))
import Domain.Map exposing (Distance, Location, Map, Road, TravelTime, travelTime)


type Event
    = Depart
    | Arrive


type alias Milestone =
    { duration : TravelTime
    , location : Location
    , event : Event
    }


type alias Itinerary =
    List Milestone


asString : Itinerary -> String
asString itinerary =
    itinerary
        |> List.map
            (\{ location, event, duration } ->
                [ duration |> String.fromFloat |> String.left 5
                , location |> String.padRight 10 ' '
                , case event of
                    Depart ->
                        "DEPART"

                    Arrive ->
                        "ARRIVE"
                ]
            )
        |> List.map (String.join "\t")
        |> String.join "\n"


fromPath : Path -> List Milestone
fromPath path =
    case path of
        Start l ->
            [ { duration = 0.0, location = l, event = Depart } ]

        City current duration next ->
            List.append (fromPath next) [ { location = current, duration = duration, event = Arrive } ]
