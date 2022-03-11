module Main exposing (..)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Csv.Decode as Decode
import Domain.Map exposing (..)
import Domain.ShortestPath exposing (..)
import File
import Ports


type Msg
    = NoOp
    | File File.Event


type alias CliOptions =
    { arguments : List String
    }


type alias Flags =
    Program.FlagsIncludingArgv {}


type alias Model =
    Transport


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                -- |> with (Option.requiredPositionalArg "arguments")
                |> OptionsParser.withRestArgs (Option.restArgs "rest")
            )


update : CliOptions -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        File (File.FileLoaded content) ->
            ( model
            , case content |> Domain.Map.parseMap of
                Ok map ->
                    case Domain.ShortestPath.calculatePath map model of
                        Just itinerary ->
                            itinerary
                                |> String.join ", "
                                |> Ports.printAndExitSuccess

                        Nothing ->
                            Ports.printAndExitFailure "No path could be found"

                Err e ->
                    e
                        |> Decode.errorToString
                        |> Ports.printAndExitFailure
            )


init : Flags -> CliOptions -> ( Model, Cmd Msg )
init flags { arguments } =
    case arguments of
        [ from, to ] ->
            ( { from = from, to = to }
            , File.load "map.csv"
            )

        _ ->
            ( { from = "", to = "" }
            , Ports.printAndExitFailure "Invalid arguments"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    File.subscriptions |> Sub.map File


main : Program.StatefulProgram Model Msg CliOptions {}
main =
    Program.stateful
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = program
        , subscriptions = subscriptions
        , update = update
        }
