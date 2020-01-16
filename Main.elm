module Main exposing (main)
import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (style,tabindex,autofocus)
import Browser
import Json.Decode as D
import Dict
--import Math.Vector2 as V2
import PixelEngine exposing (Area,toHtml)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Tools.Atlas as At




{- v2P : V2.Vec2 -> Position
v2P v =
    ( floor (V2.getX v), floor (V2.getY v) )


p2V : Position -> V2.Vec2
p2V ( a, b ) =
    V2.vec2 (toFloat a) (toFloat b) -}


type Msg
    = Move At.Direction
    | NoMove


type alias Model =
    { map : At.Atlas
    , gP : At.GlobalPos
    }





init : () -> ( Model, Cmd Msg )
init _ =
    ( { map = At.testMap1
      , gP = At.GlobalPos 0 ( 4, 4 )
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let
                res =
                    At.move model.map model.gP direction
            in
            case res of
                Ok ( True, tP ) ->
                    ( { model | gP = tP }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        NoMove ->
            ( model, Cmd.none )


playerTile : Tile Msg
playerTile =
    Tile.fromPosition ( 3, 0 )
        |> Tile.movable "player"


groundTile : Int -> Tile Msg
groundTile ind =
    if ind < 1 then
        Tile.fromPosition ( 3, 1 )

    else
        Tile.fromPosition ( 0, 1 )


blackTile : Tile Msg
blackTile =
    Tile.fromPosition ( 0, 2 )


areas : Model -> List (Area Msg)
areas { map, gP } =
    let
        mShowChart =
            Dict.get gP.chartId map.charts

        vision =
            At.defaultVisableArea map gP

        obstacle =
            Dict.get -1 map.charts
                |> Maybe.withDefault (At.Chart -1 [] [])
    in
    case mShowChart of
        Just _ ->
            [ PixelEngine.tiledArea
                { rows = boardSize
                , tileset =
                    { source = "tileset.png"
                    , spriteWidth = tileSize
                    , spriteHeight = tileSize
                    }
                , background =
                    PixelEngine.imageBackground
                        { height = width
                        , width = width
                        , source = "background.png"
                        }
                }
                --Show player and the chart it locates in.
                (List.concat
                    [ List.map (\a -> ( a.pos, groundTile a.chartId )) vision
                    , List.map (\a -> ( a, blackTile )) obstacle.blocks
                    , [ ( gP.pos, playerTile ) ]
                    ]
                )
            ]

        Nothing ->
            []


boardSize : Int
boardSize =
    10


tileSize : Int
tileSize =
    32


width : Float
width =
    toFloat <| boardSize * tileSize

options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.4

gameview : Model -> Html Msg
gameview model = 
    let
        cfg = 
            { options = Just options
            , width = width
            } 
        playGroud =
            areas model
    in
        toHtml cfg playGroud

view : Model -> Html Msg
view model =
    div 
    [ autofocus True
    , tabindex 0
    , style "outline" "none"
    , onKeyDown controls ]
    [ gameview model ]


onKeyDown: (Int -> msg) -> Attribute msg
onKeyDown keyControl =
    
    Events.custom "keydown" (D.map (\i -> { message = keyControl i, stopPropagation = True, preventDefault = True}) Events.keyCode)        
controls : Int ->  Msg
controls int =
    case int of
        38 ->
             Move  At.N

        40 ->
             Move  At.S

        37 ->
             Move  At.W

        39 ->
             Move  At.E

        _ ->
            NoMove


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
