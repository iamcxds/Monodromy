module Tools.Game exposing (GameLevel, Msg(..), gameView, keyboardControls, myLevels, update)

--import Json.Decode as D

import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events as Events
import PixelEngine exposing (Area, Input(..), toHtml)
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Tools.Atlas as At exposing (Position, pX, pY)
import Tools.GameObject as Obj


type Msg
    = Move At.Direction
    | Exit
    | Undo
    | Reset
    | NoChange



-- Dynamic vision Data


type alias ThingsOnOnePosition =
    ( Int, List ( String, Int ) )


type alias VisionElements =
    { -- position (chartId, List (object name ,id)
      visionMemory : Dict Position ThingsOnOnePosition
    , shadows : List Position
    }


type alias GameState =
    { visionData : VisionElements
    , objectsLayout : Obj.ObjectsLayout
    }


type alias GameLevel =
    { map : At.Atlas

    --, gP : At.GlobalPos
    , name : String

    -- every tile associated to a ground tile index
    , groundPattern : At.GlobalPos -> Int
    , visionData : VisionElements
    , objectsLayout : Obj.ObjectsLayout
    , gameRecords : List GameState
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

                        oldState =
                            GameState gameLevel.visionData gameLevel.objectsLayout
                    in
                    { gameLevel
                        | objectsLayout = newLayout
                        , visionData = newVisionData
                        , gameRecords = oldState :: gameLevel.gameRecords
                    }

                _ ->
                    gameLevel

        Undo ->
            case gameLevel.gameRecords of
                [] ->
                    gameLevel

                x :: xs ->
                    { gameLevel
                        | objectsLayout = x.objectsLayout
                        , visionData = x.visionData
                        , gameRecords = xs
                    }

        Reset ->
            case List.reverse gameLevel.gameRecords of
                [] ->
                    gameLevel

                x :: _ ->
                    { gameLevel
                        | objectsLayout = x.objectsLayout
                        , visionData = x.visionData
                        , gameRecords = []
                    }

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
        |> Tile.movable "shadow"
        |> Tile.jumping


assetsPath : String
assetsPath =
    "Assets/"


controlsImage : String -> Image Msg
controlsImage keyName =
    let
        path =
            assetsPath ++ "Graphic/ControlButtons/"
    in
    case keyName of
        "Up" ->
            Image.fromSrc (path ++ "Up.png")
                |> Image.clickable (inputMsg InputUp)

        "Down" ->
            Image.fromSrc (path ++ "Down.png")
                |> Image.clickable (inputMsg InputDown)

        "Left" ->
            Image.fromSrc (path ++ "Left.png")
                |> Image.clickable (inputMsg InputLeft)

        "Right" ->
            Image.fromSrc (path ++ "Right.png")
                |> Image.clickable (inputMsg InputRight)

        _ ->
            Image.fromSrc "no.png"


dPad : ( Float, Float ) -> List ( ( Float, Float ), Image Msg )
dPad ( x, y ) =
    let
        --off set from up left to center for image
        offSet =
            32

        ( x0, y0 ) =
            ( x - offSet, y - offSet )

        relative ( a, b ) =
            ( a + x0, b + y0 )
    in
    [ ( relative ( 16, -40 ), controlsImage "Up" )
    , ( relative ( 16, 40 ), controlsImage "Down" )
    , ( relative ( -40, 16 ), controlsImage "Left" )
    , ( relative ( 40, 16 ), controlsImage "Right" )
    ]


emptyBackground : PixelEngine.Background
emptyBackground =
    PixelEngine.imageBackground
        { height = 0.0
        , width = 0.0
        , source = "no.png"
        }


areas : GameLevel -> List (Area Msg)
areas { groundPattern, visionData } =
    let
        imagePath =
            assetsPath ++ "Graphic/"

        vision =
            Dict.toList visionData.visionMemory

        showOnePosition ( pos, things ) =
            let
                gdTile =
                    groundTile (groundPattern (At.GlobalPos (pX things) pos))

                objTiles =
                    List.map (\objNameAndId -> objectTile objNameAndId) (pY things)

                allTiles =
                    gdTile :: objTiles
            in
            List.map (Tuple.pair pos) allTiles

        visionRes =
            List.map showOnePosition vision
                |> List.concat
    in
    [ PixelEngine.tiledArea
        { rows = boardSize
        , tileset =
            { source = imagePath ++ "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
        , background =
            PixelEngine.imageBackground
                { height = width
                , width = width
                , source = imagePath ++ "background.png"
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
        { height = 0
        , background = emptyBackground
        }
        (dPad ( -90, -180 ))
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


inputMsg : Input -> Msg
inputMsg input =
    case input of
        InputLeft ->
            Move At.W

        InputRight ->
            Move At.E

        InputUp ->
            Move At.N

        InputDown ->
            Move At.S

        _ ->
            NoChange


keyboardControls : String -> Msg
keyboardControls keyCode =
    case keyCode of
        "ArrowUp" ->
            inputMsg InputUp

        "ArrowDown" ->
            inputMsg InputDown

        "ArrowLeft" ->
            inputMsg InputLeft

        "ArrowRight" ->
            inputMsg InputRight

        "KeyW" ->
            inputMsg InputUp

        "KeyS" ->
            inputMsg InputDown

        "KeyA" ->
            inputMsg InputLeft

        "KeyD" ->
            inputMsg InputRight

        "KeyZ" ->
            Undo

        "KeyR" ->
            Reset

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
            [ button [ Events.onClick Exit ] [ Html.text "Back to Menu" ]
            , button [ Events.onClick Undo ] [ Html.text "Undo" ]
            , button [ Events.onClick Reset ] [ Html.text "Reset" ]
            ]
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
    , gameRecords = []
    }


defaultLayout1 : Obj.ObjectsLayout
defaultLayout1 =
    invokObjectsByList
        [ ( "Player", 0, At.GlobalPos 0 ( 4, 4 ) )
        , ( "Crate", 0, At.GlobalPos 0 ( 5, 4 ) )
        , ( "Crate", 1, At.GlobalPos 0 ( 6, 4 ) )
        ,( "Player", 1, At.GlobalPos 0 ( 7, 4 ) ) ]


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
