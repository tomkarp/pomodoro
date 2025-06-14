port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Time


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
    | Tick
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { workTime = 5
      , breakTime = 3
      , currentTime = 0
      , phase = WorkTime
      , isRunning = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWorkTime seconds ->
            ( { model
                | workTime = seconds
                , currentTime = seconds * 1000
                , phase = WorkTime
                , isRunning = False
              }
            , Cmd.none
            )

        SetBreakTime seconds ->
            ( { model
                | breakTime = seconds
                , currentTime = seconds * 1000
                , phase = BreakTime
                , isRunning = False
              }
            , Cmd.none
            )

        StartTimer ->
            ( { model
                | isRunning = True
                , currentTime =
                    case model.phase of
                        WorkTime ->
                            model.workTime * 1000

                        BreakTime ->
                            model.breakTime * 1000
              }
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
                , currentTime = model.workTime * 1000
              }
            , Cmd.none
            )

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
                                | currentTime = model.breakTime * 1000
                                , phase = BreakTime
                                , isRunning = False
                              }
                            , playSound ()
                            )

                        BreakTime ->
                            ( { model
                                | currentTime = model.workTime * 1000
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
    div [ Attr.class "container" ]
        [ h1 [] [ text "Pomodoro Timer" ]
        , div [ Attr.class "settings" ]
            [ div [ Attr.class "setting-group" ]
                [ label [] [ text "Arbeitszeit (Sekunden):" ]
                , input
                    [ Attr.type_ "number"
                    , Attr.value (String.fromInt model.workTime)
                    , onInput (String.toInt >> Maybe.map SetWorkTime >> Maybe.withDefault NoOp)
                    , Attr.min "1"
                    , Attr.max "60"
                    ]
                    []
                ]
            , div [ Attr.class "setting-group" ]
                [ label [] [ text "Pausenzeit (Sekunden):" ]
                , input
                    [ Attr.type_ "number"
                    , Attr.value (String.fromInt model.breakTime)
                    , onInput (String.toInt >> Maybe.map SetBreakTime >> Maybe.withDefault NoOp)
                    , Attr.min "1"
                    , Attr.max "30"
                    ]
                    []
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
            (D.map
                (\keyCode ->
                    case keyCode of
                        32 ->
                            -- Leertaste
                            if model.isRunning then
                                PauseTimer

                            else
                                StartTimer

                        82 ->
                            -- R
                            ResetTimer

                        _ ->
                            NoOp
                )
                (D.field "keyCode" D.int)
            )
        ]


port playSound : () -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
