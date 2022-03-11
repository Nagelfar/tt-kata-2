module Domain.ShortestPath exposing (Itinerary, Transport, calculatePath)

import Domain.Map exposing (Distance, Location, Map, Road)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)


type alias Transport =
    { from : Location, to : Location }


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
exploreOn { b,  distance, speed } ( totalDistance, path ) =
    ( totalDistance + distance, Milestone b (Just path) )


calculatePath : Map -> Transport -> Maybe Itinerary
calculatePath map { from, to } =
    { travels =
        PriorityQueue.empty Tuple.first
            |> PriorityQueue.insert (beginJourneyAt from)
    , visited = Set.empty
    , map = map
    }
        |> calculateShortestPath to
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
