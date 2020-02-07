module Main exposing (main)

--import Math.Vector2 as V2

import Browser
import Browser.Events
import Html exposing (Html, button, div, li, span, ul)
import Html.Attributes exposing (style)
import Html.Events as Events
import Json.Decode as D
import Tools.Game as Game



{- v2P : V2.Vec2 -> Position
   v2P v =
       ( floor (V2.getX v), floor (V2.getY v) )


   p2V : Position -> V2.Vec2
   p2V ( a, b ) =
       V2.vec2 (toFloat a) (toFloat b)
-}


type Msg
    = FromGame Game.Msg
    | JumpTo Model


type Model
    = Menu
    | Play Game.GameLevel


init : () -> ( Model, Cmd Msg )
init _ =
    ( Menu
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (\a -> ( a, Cmd.none )) <|
        case model of
            Play level ->
                case msg of
                    FromGame gMsg ->
                        if gMsg == Game.Exit then
                            Menu

                        else
                            Play (Game.update gMsg level)

                    JumpTo Menu ->
                        Menu

                    _ ->
                        model

            Menu ->
                case msg of
                    JumpTo (Play level1) ->
                        Play level1

                    _ ->
                        model


view : Model -> Html Msg
view model =
    div divGameStyle
        [ case model of
            Play level ->
                Html.map FromGame (Game.gameView level)

            Menu ->
                div divDefautStyle
                    (List.concat
                    [ buttonList "Select the Level" Game.levelList
                    , buttonList "These are Demos(No goal to win)" Game.myDemos
                    ])
        ]


divDefautStyle : List (Html.Attribute Msg)
divDefautStyle =
    [ style "width" "320px"
    , style "margin" "0 auto"
    , style "overflow" "auto"
    , style "background-color" "white"
    ]


divGameStyle : List (Html.Attribute Msg)
divGameStyle =
    [ style "width" "680px"
    , style "height" "480px"
    , style "margin" "0 auto"
    , style "overflow" "auto"
    , style "background-color" "white"
    ]


buttonList : String -> List Game.GameLevel -> List (Html Msg)
buttonList listName gameList =
    [ span [] [ Html.text listName ]
    , ul [ style "list-style-type" "none", style "overflow" "auto" ]
        (List.map selectLevelButton gameList)
    ]


selectLevelButton : Game.GameLevel -> Html Msg
selectLevelButton level =
    li [] [ button [ Events.onClick (JumpTo (Play level)) ] [ Html.text level.name ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map (\a -> FromGame (Game.keyboardControls a)) (D.field "code" D.string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
