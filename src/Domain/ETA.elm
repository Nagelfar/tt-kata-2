module Domain.ETA exposing (Itinerary,Milestone, Transport, calculateEta, asString)

import Domain.Map exposing (Distance, Location, Map, Road, TravelTime, travelTime)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)
import String exposing (fromFloat)


type alias Transport =
    { from : Location, to : Location }


type Path
    = Start Location
    | City Location TravelTime Path


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


type alias State =
    { travels : PriorityQueue Journey
    , visited : Set Location
    , map : Map
    }


flatten : Path -> List Milestone
flatten path =
    case path of
        Start l ->
            [ { duration = 0.0, location = l, event = Depart } ]

        City current duration next ->
            List.append (flatten next)  [ { location = current, duration = duration, event = Arrive } ]


type alias Journey =
    ( TravelTime, Path )


asString : Itinerary -> String
asString itinerary =
    itinerary
        |> List.map
            (\{ location, event, duration } ->
                [ duration |> fromFloat |> String.left 5
                , location
                , case event of
                    Depart ->
                        "DEPART"

                    Arrive ->
                        "ARRIVE"
                ]
            )
        |> List.map (String.join "\t")
        |> String.join "\n"


asPriority : Journey -> Int
asPriority ( t, _ ) =
    ceiling (t * 10000)


beginJourneyAt : Location -> Journey
beginJourneyAt location =
    ( 0.0, Start location )


exploreOn : Road -> Journey -> Journey
exploreOn road ( duration, path ) =
    let
        totalDuration =
            duration + travelTime road
    in
    ( totalDuration, City road.b totalDuration path )


calculateEta : Map -> Transport -> Maybe Itinerary
calculateEta map { from, to } =
    { travels =
        PriorityQueue.fromList asPriority [ beginJourneyAt from ]
    , visited = Set.empty
    , map = map
    }
        |> calculateShortestPath to
        |> Maybe.map flatten


calculateShortestPath : Location -> State -> Maybe Path
calculateShortestPath end state =
    state.travels
        |> PriorityQueue.head
        |> Maybe.andThen
            (\journey ->
                let
                    ( _, path ) =
                        journey

                    currentLocation =
                        case path of
                            Start l ->
                                l

                            City c _ _ ->
                                c
                in
                if state.visited |> Set.member currentLocation then
                    calculateShortestPath end { state | travels = state.travels |> PriorityQueue.tail }

                else if currentLocation == end then
                    Just path

                else
                    let
                        visited =
                            state.visited
                                |> Set.insert currentLocation

                        travels =
                            Domain.Map.roadsFrom state.map currentLocation
                                |> List.map
                                    (\road -> exploreOn road journey)
                                |> List.foldl
                                    (\item queue ->
                                        queue |> PriorityQueue.insert item
                                    )
                                    (state.travels |> PriorityQueue.tail)
                    in
                    calculateShortestPath
                        end
                        { state
                            | travels = travels
                            , visited = visited
                        }
            )
