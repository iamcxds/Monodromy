module Tools.GameObject exposing (Msg(..),GameLevel,update,gameView,myLevels)
import Html exposing (Html,div,button)
import Html.Events as Events
import Html.Attributes exposing (style,tabindex,autofocus)
import Json.Decode as D
import Dict
import PixelEngine exposing (Area,toHtml)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Tools.Atlas as At

{- type Property
    = You
    | Push
    | Stop -}

type Msg
    = Move At.Direction
    | Exit
    | NoChange
type alias GameLevel =
    { map : At.Atlas
    , gP : At.GlobalPos
    -- every tile associated to a ground tile index
    , groundPattern : At.GlobalPos -> Int 
    , name : String
    }

update : Msg -> GameLevel -> GameLevel
update msg gameLevel =
    case msg of
        Move direction ->
            let
                res =
                    At.move gameLevel.map gameLevel.gP direction
            in
            case res of
                Ok ( True, tP ) ->
                    { gameLevel | gP = tP }

                _ ->
                    gameLevel
        _ ->
             gameLevel

playerTile : Tile Msg
playerTile =
    Tile.fromPosition ( 1, 0 )
        |> Tile.movable "player"


groundTile : Int -> Tile Msg
groundTile ind =
    case ind of
        -- red
        0 ->
            Tile.fromPosition ( 2, 0 )

        -- orange
        1 ->
            Tile.fromPosition ( 3, 0 )

        -- yellow 
        2 ->
            Tile.fromPosition ( 0, 1 )
        -- green
        3 ->
            Tile.fromPosition ( 1, 1 )
        -- cyan
        4 ->
            Tile.fromPosition ( 2, 1 )
        -- blue
        5 ->
            Tile.fromPosition ( 3, 1 )
        -- purple
        6 ->
            Tile.fromPosition ( 0 , 2 )
        -- pink
        _ ->
            Tile.fromPosition ( 1 , 2 )

blackTile : Tile Msg
blackTile =
    Tile.fromPosition ( 2, 2 )


areas : GameLevel -> List (Area Msg)
areas {map,gP,groundPattern} =
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
                    [ List.map (\a -> ( a.pos, groundTile (groundPattern a) )) vision
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

onKeyDown : (Int -> msg) -> Html.Attribute msg
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

        87 ->
             Move  At.N

        83 ->
             Move  At.S

        65 ->
             Move  At.W

        68 ->
             Move  At.E

        27 ->
            Exit

        _ ->
            NoChange

gameView : GameLevel -> Html Msg
gameView level = 
    let
        cfg = 
            { options = Just options
            , width = width
            } 
        playGroud =
            areas level
    in
        div 
        [ autofocus True
        , tabindex 0
        , style "outline" "none"
        , onKeyDown controls ]
        [ div[style "width" "320px", style "margin" "0 auto"]
            [button [ Events.onClick Exit ][Html.text "Back to Menu"]]
        ,toHtml cfg playGroud 
        ]


getMapByName : String -> At.Atlas
getMapByName name =
    Maybe.withDefault At.emptyMap (Dict.get name At.myMaps)
myLevels : List GameLevel 
myLevels =
    [   
        { map = getMapByName "SquareRoot" 
        , gP = At.GlobalPos 0 ( 4, 4 )
        , groundPattern = \gP -> gP.chartId
        , name = "Square Root"
        }
        ,
        { map = getMapByName "EllipticCurve" 
        , gP = At.GlobalPos 0 ( 4, 4 )
        , groundPattern = \gP -> gP.chartId
        , name = "Elliptic Curve"
        }
        ,
        {map = getMapByName "SquareSquareRoot" 
        , gP = At.GlobalPos 0 ( 4, 4 )
        , groundPattern = \gP -> gP.chartId
        , name = "(y^2+1)^2=x"
        }
        
    ]        