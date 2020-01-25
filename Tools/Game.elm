module Tools.Game exposing (GameLevel, Msg(..), controls, gameView, myLevels, update)

--import Json.Decode as D

import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events as Events
import PixelEngine exposing (Area, toHtml)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import PixelEngine.Image as Image exposing (Image)

import Tools.Atlas as At exposing (Position, pX, pY)
import Tools.GameObject as Obj




type Msg
    = Move At.Direction
    | Exit
    | NoChange



-- Dynamic vision Data


type alias ThingsOnOnePosition =
    ( Int, List ( String, Int ) )


type alias VisionElements =
    { -- position (chartId, List (object name ,id)
      visionMemory : Dict Position ThingsOnOnePosition
    , shadows : List Position
    }


type alias GameLevel =
    { map : At.Atlas

    --, gP : At.GlobalPos
    , name : String

    -- every tile associated to a ground tile index
    , groundPattern : At.GlobalPos -> Int
    , visionData : VisionElements
    , objectsLayout : Obj.ObjectsLayout
    }


updateVision : At.Atlas -> Obj.ObjectsLayout -> VisionElements -> VisionElements
updateVision map layout { visionMemory, shadows } =
    let
        mGP =
            Dict.get ( "Player", 0 ) layout
                |> Maybe.map pY
    in
    case mGP of
        Just gP ->
            let
                currentVision : Dict Position ThingsOnOnePosition
                currentVision =
                    At.defaultVisableArea map gP
                        |> Dict.filter (\pos -> always (List.member pos allBaseBlocks))
                        |> Dict.map (\pos id -> ( id, Obj.getObjByGP (At.GlobalPos id pos) layout ))

                visionRange =
                    Dict.keys currentVision

                onePosUpdate : Position -> Dict Position ThingsOnOnePosition -> Dict Position ThingsOnOnePosition
                onePosUpdate pos dict =
                    Dict.update pos (always (Dict.get pos currentVision)) dict

                newMemory : Dict Position ThingsOnOnePosition
                newMemory =
                    List.foldl onePosUpdate visionMemory visionRange

                newShadows =
                    At.minusOfBlocks allBaseBlocks visionRange
                        |> List.filter (\pos -> List.member pos (Dict.keys newMemory))
            in
            VisionElements newMemory newShadows

        _ ->
            VisionElements visionMemory shadows


update : Msg -> GameLevel -> GameLevel
update msg gameLevel =
    case msg of
        Move direction ->
            let
                res =
                    Obj.onePlayerTryMove gameLevel.map direction gameLevel.objectsLayout
            in
            case res of
                ( True, newLayout ) ->
                    let
                        newVisionData =
                            updateVision gameLevel.map newLayout gameLevel.visionData
                    in
                    { gameLevel | objectsLayout = newLayout, visionData = newVisionData }

                _ ->
                    gameLevel

        _ ->
            gameLevel


objectTile : ( String, Int ) -> Tile Msg
objectTile ( name, ind ) =
    Tile.movable (name ++ String.fromInt ind) <|
        case name of
            "Player" ->
                Tile.fromPosition ( 1, 0 )
            "Crate" ->
                Tile.fromPosition ( 0, 3 )
            _ ->
                Tile.fromPosition ( 0, 0 )


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
                Tile.fromPosition ( 0, 2 )

            -- pink
            _ ->
                Tile.fromPosition ( 1, 2 )


blackTile : Tile Msg
blackTile =
    Tile.fromPosition ( 2, 2 )


shadowTile : Tile Msg
shadowTile =
    Tile.fromPosition ( 3, 2 )
        |>Tile.movable "shadow"|>Tile.jumping

controlsImage : String -> Image Msg
controlsImage keyName =
    let
        controlsTiles = 
            Tile.tileset
                { source = "controls.png"
                , spriteWidth = 64
                , spriteHeight = 64
                }
        image (x,y) = 
            Image.fromTile (Tile.fromPosition (x,y)) controlsTiles
    in
    
    case keyName of
        "Up" ->
            image (0,0)
                |>Image.clickable (Move At.N)
        "Down" ->
            image (1,0)
                |>Image.clickable (Move At.S)
        "Left" ->
            image (2,0)
                |>Image.clickable (Move At.W)
        "Right" ->
            image (3,0)
                |>Image.clickable (Move At.E)
            
    
        _ ->
            Image.fromSrc "no.png" 

dPad :(Float, Float) ->List ( (Float, Float) ,Image Msg)
dPad (x,y)=
    let
        --off set from up left to center for image
        offSet = 32
        (x0,y0)= (x-offSet,y-offSet)
        relative (a,b) = (a+x0,b+y0)
    in
    
     
        [( relative (0,-40) ,controlsImage "Up" )
        , ( relative (0,40) ,controlsImage "Down" )
        ,( relative (-40,0) ,controlsImage "Left" )
        ,( relative (40,0) ,controlsImage "Right" ) ]

emptyBackground : PixelEngine.Background
emptyBackground =
    PixelEngine.imageBackground
                { height = 0.0
                , width = 0.0
                , source = "no.png"
                }            
    
areas : GameLevel -> List (Area Msg)
areas { map, groundPattern, visionData } =
    let
        vision =
            Dict.toList visionData.visionMemory
        showOnePosition (pos, things) =
            let
                gdTile = groundTile (groundPattern (At.GlobalPos (pX things) pos))
                objTiles = List.map (\objNameAndId -> objectTile objNameAndId ) (pY things)

                allTiles = gdTile :: objTiles
            in
            
            List.map (Tuple.pair pos) allTiles
        visionRes =
            List.map showOnePosition vision
                |>List.concat
    in
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
              visionRes
            
            , List.map (\pos -> ( pos, shadowTile )) visionData.shadows
            ]
        )
    , PixelEngine.imageArea
        {
            height= 0
            , background = emptyBackground
           
        }

         (dPad (-90,-180)) 

    ]


boardSize : Int
boardSize =
    10


allBaseBlocks : List Position
allBaseBlocks =
    At.formRectangle ( 0, 0 ) ( boardSize - 1, boardSize - 1 )


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


controls : String -> Msg
controls keyCode =
    case keyCode of
        "ArrowUp" ->
            Move At.N

        "ArrowDown" ->
            Move At.S

        "ArrowLeft" ->
            Move At.W

        "ArrowRight" ->
            Move At.E

        "KeyW" ->
            Move At.N

        "KeyS" ->
            Move At.S

        "KeyA" ->
            Move At.W

        "KeyD" ->
            Move At.E

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
        [{- autofocus True
            , tabindex 0
            , style "outline" "none"
         -}
        ]
        [ div [ style "width" "320px", style "margin" "0 auto" ]
            [ button [ Events.onClick Exit ] [ Html.text "Back to Menu" ] ]
        , toHtml cfg playGroud
        ]


getMapByName : String -> At.Atlas
getMapByName name =
    Maybe.withDefault At.emptyMap (Dict.get name At.mapDict)


invokObject : String -> Int -> At.GlobalPos -> List ( ( String, Int ), ( Obj.Object, At.GlobalPos ) )
invokObject name id gP =
    let
        mObj =
            Dict.get name Obj.objDict
    in
    case mObj of
        Just obj ->
            [ ( ( name, id ), ( obj, gP ) ) ]

        _ ->
            []


invokObjectsByList : List ( String, Int, At.GlobalPos ) -> Obj.ObjectsLayout
invokObjectsByList list0 =
    List.map (\( name, id, gP ) -> invokObject name id gP) list0
        |> List.concat
        |> Dict.fromList


levelGenerator :
    { map : At.Atlas
    , name : String
    , groundPattern : At.GlobalPos -> Int
    , objectsLayout : Obj.ObjectsLayout
    }
    -> GameLevel
levelGenerator { map, groundPattern, name, objectsLayout } =
    { map = map
    , groundPattern = groundPattern
    , name = name
    , visionData = updateVision map objectsLayout (VisionElements Dict.empty [])
    , objectsLayout = objectsLayout
    }

defaultLayout1 : Obj.ObjectsLayout
defaultLayout1 =
    invokObjectsByList
                    [ ( "Player", 0, At.GlobalPos 0 ( 4, 4 ) ) , ( "Crate", 0, At.GlobalPos 0 ( 5, 4 ) ),( "Crate", 1, At.GlobalPos 0 ( 6, 4 ) )]

myLevels : List GameLevel
myLevels =
    List.map levelGenerator <|
        [ { map = getMapByName "SquareRoot"
          , groundPattern = \gP -> gP.chartId
          , name = "Square Root"
          , objectsLayout = defaultLayout1
                
          }
        , { map = getMapByName "EllipticCurve"
          , groundPattern = \gP -> gP.chartId
          , name = "Elliptic Curve"
          , objectsLayout = defaultLayout1
                
          }
        , { map = getMapByName "SquareSquareRoot"
          , groundPattern = \gP -> gP.chartId
          , name = "(y^2+1)^2=x"
          , objectsLayout = defaultLayout1
                
          }
        ]
