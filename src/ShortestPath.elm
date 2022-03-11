module ShortestPath exposing (Itinerary, calculatePath)

import Map exposing (Distance, Location, Map, Road)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)


type Path
    = Milestone Location (Maybe Path)


type alias Itinerary =
    List Location


type alias State =
    { travels : PriorityQueue Journey
    , visited : Set Location
    , map : Map
    }


flatten : Path -> List Location
flatten (Milestone current next) =
    current
        :: (case next of
                Just n ->
                    flatten n

                Nothing ->
                    []
           )


type alias Journey =
    ( Distance, Path )


beginJourneyAt : Location -> Journey
beginJourneyAt location =
    ( 0, Milestone location Nothing )


exploreOn : Road -> Journey -> Journey
exploreOn ( _, to, distance ) ( totalDistance, path ) =
    ( totalDistance + distance, Milestone to (Just path) )


calculatePath : Map -> Location -> Location -> Maybe Itinerary
calculatePath map start end =
    { travels =
        PriorityQueue.empty Tuple.first
            |> PriorityQueue.insert (beginJourneyAt start)
    , visited = Set.empty
    , map = map
    }
        |> calculateShortestPath end
        |> Maybe.map flatten
        |> Maybe.map List.reverse


calculateShortestPath : Location -> State -> Maybe Path
calculateShortestPath end state =
    state.travels
        |> PriorityQueue.head
        |> Maybe.andThen
            (\journey ->
                let
                    ( _, milestone ) =
                        journey

                    (Milestone currentLocation _) =
                        milestone
                in
                if state.visited |> Set.member currentLocation then
                    calculateShortestPath end { state | travels = state.travels |> PriorityQueue.tail }

                else if currentLocation == end then
                    Just milestone

                else
                    let
                        visited =
                            state.visited
                                |> Set.insert currentLocation

                        travels =
                            Map.roadsFrom state.map currentLocation
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
