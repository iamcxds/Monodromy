module Tools.Game exposing (Msg(..),GameLevel,update,gameView,myLevels,controls)
import Html exposing (Html,div,button)
import Html.Events as Events
import Html.Attributes exposing (style)
--import Json.Decode as D
import Dict exposing(Dict)
import PixelEngine exposing (Area,toHtml)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Tools.Atlas as At


type alias Position =
    ( Int, Int )
type Msg
    = Move At.Direction
    | Exit
    | NoChange

-- Dynamic vision Data
type alias VisionElement =
    {   
      visionMemory : Dict Position Int
    , shadows : List Position
    }
type alias GameLevel =
    { map : At.Atlas
    , gP : At.GlobalPos    
    , name : String
    -- every tile associated to a ground tile index
    , groundPattern : At.GlobalPos -> Int
    , visionData : VisionElement
    }

updateVision : At.Atlas -> At.GlobalPos -> VisionElement -> VisionElement
updateVision map gP {visionMemory} = 
    let
        currentVision = 
            At.defaultVisableArea map gP
                |> Dict.filter (\pos-> always (List.member pos allBaseBlocks))
        visionRange = Dict.keys currentVision

        onePosUpdate : Position -> Dict Position Int -> Dict Position Int
        onePosUpdate pos dict =
            Dict.update pos (always (Dict.get pos currentVision)) dict
        newMemory : Dict Position Int
        newMemory =
            List.foldl onePosUpdate visionMemory visionRange

        shadows =
            At.minusOfBlocks allBaseBlocks visionRange
                |>List.filter (\pos -> List.member pos (Dict.keys newMemory))
    in
        VisionElement newMemory shadows



update : Msg -> GameLevel -> GameLevel
update msg gameLevel =
    case msg of
        Move direction ->
            let
                res =
                    At.tryMoveSimple gameLevel.map gameLevel.gP direction

            in
                case res of
                    ( True, tP ) ->
                        let
                            newVisionData = updateVision gameLevel.map tP gameLevel.visionData
                        in
                            { gameLevel | gP = tP , visionData = newVisionData }

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
    if ind < 0 then 
        blackTile
    else
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

shadowTile : Tile Msg
shadowTile =
    Tile.fromPosition ( 3, 2 )

areas : GameLevel -> List (Area Msg)
areas {map,gP,groundPattern,visionData} =
    let
        
        mShowChart =
            Dict.get gP.chartId map.charts
        vision = Dict.toList visionData.visionMemory
        
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
                    [ --List.map (\pos -> (pos, blackTile)) allBaseBlocks ,
                     List.map (\(pos,id) -> ( pos, groundTile (groundPattern (At.GlobalPos id pos)))) vision
                    , List.map (\pos -> (pos, shadowTile)) visionData.shadows
                    , [ ( gP.pos, playerTile ) ]
                    ]
                )
            ]

        Nothing ->
            []
boardSize : Int
boardSize =
    10

allBaseBlocks : List Position
allBaseBlocks = 
    At.formRectangle (0,0) (boardSize-1,boardSize-1)

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

{- onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown keyControl =
    
    Events.custom "keydown" (D.map (\i -> { message = keyControl i, stopPropagation = True, preventDefault = True}) Events.keyCode)        
 -}

controls : String ->  Msg
controls keyCode =
    case keyCode of
        "ArrowUp" ->
             Move  At.N

        "ArrowDown" ->
             Move  At.S

        "ArrowLeft" ->
             Move  At.W

        "ArrowRight" ->
             Move  At.E

        "KeyW" ->
             Move  At.N

        "KeyS" ->
             Move  At.S

        "KeyA" ->
             Move  At.W

        "KeyD" ->
             Move  At.E

        "Escape" ->
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
        [ {- autofocus True
        , tabindex 0
        , style "outline" "none" -}
         ]
        [ div[style "width" "320px", style "margin" "0 auto"]
            [button [ Events.onClick Exit ][Html.text "Back to Menu"]]
        ,toHtml cfg playGroud 
        ]


getMapByName : String -> At.Atlas
getMapByName name =
    Maybe.withDefault At.emptyMap (Dict.get name At.myMaps)

levelGenerator : { map : At.Atlas
                , gP : At.GlobalPos    
                , name : String
                , groundPattern : At.GlobalPos -> Int } -> GameLevel
levelGenerator {map,gP,groundPattern,name} =
    { map = map 
    , gP = gP 
    , groundPattern = groundPattern
    , name = name 
    , visionData = updateVision map gP  (VisionElement Dict.empty [])

    }
myLevels : List GameLevel 
myLevels =
    List.map levelGenerator <|
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