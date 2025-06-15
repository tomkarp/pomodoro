port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Time


port playSound : () -> Cmd msg


type Phase
    = WorkTime
    | BreakTime


type alias Model =
    { workTime : Int
    , breakTime : Int
    , currentTime : Int
    , phase : Phase
    , isRunning : Bool
    }


type Msg
    = SetWorkTime Int
    | SetBreakTime Int
    | StartTimer
    | PauseTimer
    | ResetTimer
    | TogglePhase
    | Tick
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { workTime = 20
      , breakTime = 5
      , currentTime = 20 * 60 * 1000
      , isRunning = False
      , phase = WorkTime
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWorkTime minutes ->
            ( { model
                | workTime = minutes
                , currentTime =
                    if model.phase == WorkTime && model.currentTime == model.workTime * 60 * 1000 then
                        minutes * 60 * 1000

                    else
                        model.currentTime
              }
            , Cmd.none
            )

        SetBreakTime minutes ->
            ( { model
                | breakTime = minutes
                , currentTime =
                    if model.phase == BreakTime && model.currentTime == model.breakTime * 60 * 1000 then
                        minutes * 60 * 1000

                    else
                        model.currentTime
              }
            , Cmd.none
            )

        StartTimer ->
            ( { model | isRunning = True }
            , Cmd.none
            )

        PauseTimer ->
            ( { model | isRunning = False }
            , Cmd.none
            )

        ResetTimer ->
            ( { model
                | isRunning = False
                , phase = WorkTime
                , currentTime = model.workTime * 60 * 1000
              }
            , Cmd.none
            )

        TogglePhase ->
            if not model.isRunning then
                let
                    ( newPhase, newTime ) =
                        case model.phase of
                            WorkTime ->
                                ( BreakTime, model.breakTime * 60 * 1000 )

                            BreakTime ->
                                ( WorkTime, model.workTime * 60 * 1000 )
                in
                ( { model
                    | phase = newPhase
                    , currentTime = newTime
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Tick ->
            if model.isRunning then
                let
                    newTime =
                        max 0 (model.currentTime - 1000)
                in
                if newTime == 0 then
                    case model.phase of
                        WorkTime ->
                            ( { model
                                | currentTime = model.breakTime * 60 * 1000
                                , phase = BreakTime
                                , isRunning = False
                              }
                            , playSound ()
                            )

                        BreakTime ->
                            ( { model
                                | currentTime = model.workTime * 60 * 1000
                                , phase = WorkTime
                                , isRunning = False
                              }
                            , playSound ()
                            )

                else
                    ( { model | currentTime = newTime }
                    , Cmd.none
                    )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        digitInput attrs val onMsg minVal maxVal =
            input
                ([ Attr.type_ "text"
                 , Attr.value (String.fromInt val)
                 , Attr.min (String.fromInt minVal)
                 , Attr.max (String.fromInt maxVal)
                 , Attr.pattern "[0-9]*"
                 , Attr.attribute "inputmode" "numeric"
                 , onInput
                    (\str ->
                        let
                            trimmed =
                                String.trim str
                        in
                        if String.all Char.isDigit trimmed && not (String.isEmpty trimmed) then
                            case String.toInt trimmed of
                                Just n ->
                                    onMsg n

                                Nothing ->
                                    NoOp

                        else
                            NoOp
                    )
                 ]
                    ++ attrs
                )
                []
    in
    div [ Attr.class "container" ]
        [ h1 [] [ text "Pomodoro Timer" ]
        , div [ Attr.class "settings" ]
            [ div [ Attr.class "setting-group" ]
                [ label [] [ text "Arbeitszeit (Minuten):" ]
                , digitInput [] model.workTime SetWorkTime 1 60
                ]
            , div [ Attr.class "setting-group" ]
                [ label [] [ text "Pausenzeit (Minuten):" ]
                , digitInput [] model.breakTime SetBreakTime 1 30
                ]
            ]
        , div [ Attr.class "timer-display" ]
            [ div [ Attr.class "timer-container" ]
                [ div [ Attr.class "timer-circle" ]
                    [ text (formatTime model.currentTime) ]
                , div
                    [ Attr.class
                        ("timer-icon "
                            ++ (case model.phase of
                                    WorkTime ->
                                        "work"

                                    BreakTime ->
                                        "break"
                               )
                        )
                    , onClick TogglePhase
                    ]
                    []
                ]
            , div [ Attr.class "button-group" ]
                [ button
                    [ Attr.class "control-button"
                    , onClick
                        (if model.isRunning then
                            PauseTimer

                         else
                            StartTimer
                        )
                    ]
                    [ text
                        (if model.isRunning then
                            "Pause"

                         else
                            "Start"
                        )
                    ]
                , button
                    [ Attr.class "control-button"
                    , onClick ResetTimer
                    ]
                    [ text "Reset" ]
                ]
            ]
        , div [ Attr.class "keyboard-hints" ]
            [ p [] [ text "Leertaste = Start/Pause, R = Reset" ]
            ]
        ]


formatTime : Int -> String
formatTime milliseconds =
    let
        totalSeconds =
            milliseconds // 1000

        minutes =
            totalSeconds // 60

        seconds =
            modBy 60 totalSeconds
    in
    String.padLeft 2 '0' (String.fromInt minutes) ++ ":" ++ String.padLeft 2 '0' (String.fromInt seconds)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 (\_ -> Tick)
        , Browser.Events.onKeyDown
            (D.map2
                (\keyCode targetTag ->
                    case ( keyCode, targetTag ) of
                        ( 32, "BODY" ) ->
                            if model.isRunning then
                                PauseTimer

                            else
                                StartTimer

                        ( 82, "BODY" ) ->
                            ResetTimer

                        _ ->
                            NoOp
                )
                (D.field "keyCode" D.int)
                (D.at [ "target", "tagName" ] D.string)
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
