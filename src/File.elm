port module File exposing (Event(..), load, subscriptions)


port load : String -> Cmd msg


port onFileLoaded : (String -> msg) -> Sub msg


type Event
    = FileLoaded String


subscriptions : Sub Event
subscriptions =
    onFileLoaded FileLoaded
