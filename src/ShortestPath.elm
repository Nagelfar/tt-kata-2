module ShortestPath exposing (Itinerary, calculatePath)

import Map exposing (Distance, Location, Map)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)


type Journey
    = Milestone Location (Maybe Journey)


type alias Itinerary =
    List Location


type alias State =
    { travels : PriorityQueue ( Distance, Journey )
    , visited : Set Location
    , map : Map
    }


flatten : Journey -> List Location
flatten (Milestone current next) =
    current
        :: (case next of
                Just n ->
                    flatten n

                Nothing ->
                    []
           )


calculatePath : Map -> Location -> Location -> Maybe Itinerary
calculatePath map start end =
    { travels =
        PriorityQueue.empty Tuple.first
            |> PriorityQueue.insert ( 0, Milestone start Nothing )
    , visited = Set.empty
    , map = map
    }
        |> calculateShortestPath end
        |> Maybe.map flatten
        |> Maybe.map List.reverse


calculateShortestPath : Location -> State -> Maybe Journey
calculateShortestPath end state =
    case state.travels |> PriorityQueue.head of
        Nothing ->
            Nothing

        Just ( totalDistance, milestone ) ->
            let
                (Milestone currentLocation _ ) = milestone
            in if state.visited |> Set.member currentLocation then
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
                                (\( _, to, distance ) ->
                                    ( totalDistance + distance, Milestone to (Just milestone) )
                                )
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



-- class Milestone:
--     location: str
--     previous: Milestone
-- # start the travels
-- travels.put((0, new Milestone(start, None)))
-- while not travels.empty():
--   (distance, milestone) = travels.get_next()
--   if milestone.location in visited:
--     continue #skip
--   if milestone.location == end:
--     print(f"we arrived to {end}!")
--     # traverse the event to the origin
--     return
--   visited.append(milestone.location)
--   for road in road_map[milestone.location]:
--     distance_at = distance + road.length
--     travels.put((distance_at, new Milestone(road.destination, milestone)))
