module ShortestPath exposing (Journey, Travel(..), calculatePath)

import Map exposing (Distance, Location, Map, Road)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)



-- type Travel
--     = Start Location
--     | Milestone Location Travel


type Travel
    = Milestone Location (Maybe Travel)


type alias Journey =
    List Location


type alias State =
    { travels : PriorityQueue ( Distance, Travel )
    , visted : Set Location
    , map : Map
    }


flattenTravel : Travel -> List Location
flattenTravel (Milestone current next) =
    current
        :: (case next of
                Just n ->
                    flattenTravel n

                Nothing ->
                    []
           )


calculatePath : Map -> Location -> Location -> Maybe Journey
calculatePath map start end =
    let
        state =
            { travels =
                PriorityQueue.empty Tuple.first
                    |> PriorityQueue.insert ( 0, Milestone start Nothing )
            , visted = Set.empty
            , map = map
            }
    in
    calculateShortest end state
        |> Maybe.map flattenTravel
        |> Maybe.map List.reverse


calculateShortest : Location -> State -> Maybe Travel
calculateShortest end state =
    case state.travels |> PriorityQueue.head of
        Nothing ->
            Nothing

        Just ( distance, Milestone currentMilestone previous ) ->
            if state.visted |> Set.member currentMilestone then
                calculateShortest end { state | travels = state.travels |> PriorityQueue.tail }

            else if currentMilestone == end then
                Just (Milestone currentMilestone previous)

            else
                let
                    visited =
                        state.visted
                            |> Set.insert currentMilestone

                    travels =
                        Map.roadsFrom state.map currentMilestone
                            |> List.map
                                (\( _, to, t ) ->
                                    ( distance + t, Milestone to (Just (Milestone currentMilestone previous)) )
                                )
                            |> List.foldl
                                (\item queue ->
                                    queue |> PriorityQueue.insert item
                                )
                                state.travels
                in
                calculateShortest
                    end
                    { state
                        | travels = travels
                        , visted = visited
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
