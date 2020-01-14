module Main exposing (main)
import Dict exposing (Dict)
import Html exposing (Html,text)
import Array exposing (Array)
import Debug exposing(toString)
import Math.Vector2 as V2
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Random
import Time
import Tools.Atlas as At

type alias Position =
    ( Int, Int )

v2P: V2.Vec2 -> Position
v2P v =
    (floor (V2.getX v), floor (V2.getY v))
p2V: Position -> V2.Vec2    
p2V (a,b) =
    V2.vec2 (toFloat a)  (toFloat b)
type Msg = 
    Move At.Direction

 
type alias Model =
    {map: At.Atlas
    ,gP: At.GlobalPos}

type Property 
    =You
    |Push
    |Stop


type alias Object =
    {position : Position
    ,properties:List Property
    ,tile: Tile Msg
    }
init : () -> ( Model, Cmd Msg )
init _ =
    ( {map=At.testMap1
        ,gP= At.GlobalPos 0 (4,4)}
    ,Cmd.none)    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let 
                res = 
                    At.move model.map model.gP direction
            in
            case res of
                Ok (True, tP) ->
                    ( { model | gP = tP}, Cmd.none )
                _ ->
                    ( model, Cmd.none)

playerTile = 
    Tile.fromPosition ( 3, 0 )
        |>Tile.movable "player"

groundTile: Int ->Tile Msg
groundTile ind = 
    if ind < 1 then
        Tile.fromPosition ( 3, 1 )
    else
        Tile.fromPosition ( 0, 1 )
blackTile =
    Tile.fromPosition ( 0, 2 )
areas : Model -> List (Area Msg)
areas { map, gP } =
    let 
        mShowChart =
            Dict.get gP.chartId map.charts
        vision = At.defaultVisableArea map gP
        obstacle = 
            Dict.get -1 map.charts
                |>Maybe.withDefault (At.Chart -1 [] [])
    in
    case mShowChart of
        Just chart ->
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
                    [ List.map (\a ->(a.pos,groundTile a.chartId )) vision
                    , List.map (\a ->(a,blackTile )) obstacle.blocks
                    ,[(gP.pos,playerTile)]
                    ]
                )   
            
            ]
        Nothing->[]

boardSize : Int
boardSize =
    10

tileSize : Int
tileSize =
    32


width : Float
width =
    toFloat <| (boardSize) * tileSize


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Move At.N

        InputDown ->
            Just <| Move At.S

        InputLeft ->
            Just <| Move At.W

        InputRight ->
            Just <| Move At.E

        _ ->
            Nothing

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none           

options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.4


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "gou"
    , options = Just options
    , body = areas model
    }


main : PixelEngine () Model Msg
main =
    game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        , width = width
        }