module Domain.ETA exposing (Path(..), Transport, calculateEta)

import Domain.Map exposing (Location, Map, Road, TravelTime, travelTime)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)
import String exposing (fromFloat)


type alias Transport =
    { from : Location, to : Location }


type Path
    = Start Location
    | City Location TravelTime Path


type alias State =
    { journeys : PriorityQueue Journey
    , visited : Set Location
    , map : Map
    }


type alias Journey =
    ( TravelTime, Path )


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


calculateEta : Map -> Transport -> Maybe Path
calculateEta map { from, to } =
    { journeys =
        PriorityQueue.fromList asPriority [ beginJourneyAt from ]
    , visited = Set.empty
    , map = map
    }
        |> calculateShortestPath to


calculateShortestPath : Location -> State -> Maybe Path
calculateShortestPath end state =
    state.journeys
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
                    calculateShortestPath end { state | journeys = state.journeys |> PriorityQueue.tail }

                else if currentLocation == end then
                    Just path

                else
                    let
                        visited =
                            state.visited
                                |> Set.insert currentLocation

                        journeys =
                            Domain.Map.roadsFrom state.map currentLocation
                                |> List.map
                                    (\road -> exploreOn road journey)
                                |> List.foldl
                                    (\item queue ->
                                        queue |> PriorityQueue.insert item
                                    )
                                    (state.journeys |> PriorityQueue.tail)
                    in
                    calculateShortestPath
                        end
                        { state
                            | journeys = journeys
                            , visited = visited
                        }
            )
