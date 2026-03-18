module DiffTuiMinimal exposing (run)

import BackendTask exposing (BackendTask)
import Pages.Script as Script exposing (Script)
import Tui
import Tui.Effect as Effect
import Tui.Sub


type alias Model =
    { count : Int }


type Msg
    = KeyPressed Tui.KeyEvent


run : Script
run =
    Script.tui
        { data = BackendTask.succeed ()
        , init = \() -> ( { count = 0 }, Effect.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Tui.Sub.onKeyPress KeyPressed
        }


update : Msg -> Model -> ( Model, Effect.Effect Msg )
update msg model =
    case msg of
        KeyPressed event ->
            case event.key of
                Tui.Character 'q' ->
                    ( model, Effect.exit )

                Tui.Character 'j' ->
                    ( { model | count = model.count + 1 }, Effect.none )

                Tui.Character 'k' ->
                    ( { model | count = model.count - 1 }, Effect.none )

                _ ->
                    ( model, Effect.none )


view : Tui.Context -> Model -> Tui.Screen
view ctx model =
    Tui.lines
        [ Tui.text ""
        , Tui.text "  Minimal TUI Test"
        , Tui.text ""
        , Tui.text ("  Count: " ++ String.fromInt model.count)
        , Tui.text ""
        , Tui.text "  j/k to change, q to quit"
        , Tui.text ("  Terminal: " ++ String.fromInt ctx.width ++ "x" ++ String.fromInt ctx.height)
        ]
