module Tools.Game exposing (GameLevel, Msg(..), gameView, keyboardControls, levelList, myDemos, update)

--import Json.Decode as D

import Array
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
    { playerGPos : At.GlobalPos
    , visionData : VisionElements
    , objectsLayout : Obj.ObjectsLayout
    , isWin : Bool
    }


type alias GameLevel =
    { name : String
    , map : At.Atlas
    , playerGPos : At.GlobalPos

    -- every tile associated to a ground tile index
    , groundPattern : At.GlobalPos -> Int
    , visionData : VisionElements
    , objectsLayout : Obj.ObjectsLayout
    , gameRecords : List GameState
    , introduction : String
    , winCondition : List ( String, Int )
    , isWin : Bool
    }


winJudge : At.GlobalPos -> List ( String, Int ) -> Obj.ObjectsLayout -> Bool
winJudge gP winCon layout =
    let
        winObj =
            Dict.filter (\a _ -> List.member a winCon) layout

        winGPos =
            Dict.values winObj
                |> List.map pY
    in
    List.member gP winGPos


updateVision : At.Atlas -> At.GlobalPos -> Obj.ObjectsLayout -> VisionElements -> VisionElements
updateVision map gP layout { visionMemory } =
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


updatePlayerGP : At.GlobalPos -> Obj.ObjectsLayout -> At.GlobalPos
updatePlayerGP oldGP layout =
    Dict.get ( "Player", 0 ) layout
        |> Maybe.map pY
        |> Maybe.withDefault oldGP


recoverLevel : GameState -> GameLevel -> GameLevel
recoverLevel x gameLevel =
    { gameLevel
        | playerGPos = x.playerGPos
        , isWin = x.isWin
        , objectsLayout = x.objectsLayout
        , visionData = x.visionData
    }


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
                        newGP =
                            updatePlayerGP gameLevel.playerGPos newLayout

                        newVisionData =
                            updateVision gameLevel.map newGP newLayout gameLevel.visionData

                        oldState =
                            GameState gameLevel.playerGPos gameLevel.visionData gameLevel.objectsLayout gameLevel.isWin

                        newIsWin =
                            winJudge newGP gameLevel.winCondition newLayout
                    in
                    { gameLevel
                        | objectsLayout = newLayout
                        , playerGPos = newGP
                        , visionData = newVisionData
                        , gameRecords = oldState :: gameLevel.gameRecords
                        , isWin = newIsWin
                    }

                _ ->
                    gameLevel

        Undo ->
            case gameLevel.gameRecords of
                [] ->
                    gameLevel

                x :: xs ->
                    let
                        oldLevel =
                            recoverLevel x gameLevel
                    in
                    { oldLevel | gameRecords = xs }

        Reset ->
            case List.reverse gameLevel.gameRecords of
                [] ->
                    gameLevel

                x :: _ ->
                    let
                        oldLevel =
                            recoverLevel x gameLevel
                    in
                    { oldLevel | gameRecords = [] }

        _ ->
            gameLevel


tileSetWidth : Int
tileSetWidth =
    5


tileNumberToPosition : Int -> Position
tileNumberToPosition id =
    let
        x =
            modBy tileSetWidth id

        y =
            id // tileSetWidth
    in
    ( x, y )


tileFromId : Int -> Tile Msg
tileFromId id =
    Tile.fromPosition (tileNumberToPosition id)


firstObjectTileId : Int
firstObjectTileId =
    13


objectTile : ( String, Int ) -> Tile Msg
objectTile ( name, ind ) =
    let
        objName =
            getObjName name

        transName =
            getTransName name
    in
    if name == "Player" then
        tileFromId firstObjectTileId
            |> Tile.movable "ZZZ"

    else
        Tile.movable (name ++ String.fromInt ind) <|
            case objName of
                "Crate" ->
                    tileFromId (firstObjectTileId + 1)

                "Wall" ->
                    tileFromId (firstObjectTileId + 2)

                "Butterfly" ->
                    let
                        defaultId =
                            firstObjectTileId + 3
                    in
                    case transName of
                        "F" ->
                            tileFromId (defaultId + 1)

                        _ ->
                            tileFromId defaultId

                "Triangle" ->
                    let
                        defaultId =
                            firstObjectTileId + 5
                    in
                    case transName of
                        "R" ->
                            tileFromId (defaultId + 1)

                        "RR" ->
                            tileFromId (defaultId + 2)

                        "F" ->
                            tileFromId (defaultId + 3)

                        "RF" ->
                            tileFromId (defaultId + 4)

                        "RRF" ->
                            tileFromId (defaultId + 5)

                        _ ->
                            tileFromId defaultId

                _ ->
                    tileFromId 0


firstGroundColorId : Int
firstGroundColorId =
    1


groundTile : Int -> Tile Msg
groundTile ind =
    if ind < 0 then
        if -ind < 4 then
            tileFromId (firstGroundColorId - ind)

        else
            --blackTile
            tileFromId firstGroundColorId
        {-
           black -1
           mirror -2
           rotate -3
        -}

    else
        let
            firstColorTile =
                firstGroundColorId + 4
        in
        if ind < 7 then
            tileFromId (firstColorTile + ind)

        else
            tileFromId (firstColorTile + 7)



{- red 0
   orange 1
   yellow 2
   green 3
   cyan 4
   blue 5
   purple 6
   pink else
-}


shadowTile : Tile Msg
shadowTile =
    tileFromId firstGroundColorId
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
        [ div divDefautStyle
            [ button [ Events.onClick Exit ] [ Html.text "Back to Menu" ]
            , button [ Events.onClick Undo ] [ Html.text "Undo" ]
            , button [ Events.onClick Reset ] [ Html.text "Reset" ]
            ]
        , toHtml cfg playGroud
        , div textDivDefautStyle
            [ Html.p [] (information level)
            ]
        ]


information : GameLevel -> List (Html Msg)
information { introduction, isWin } =
    (if isWin then
        [ Html.text "Congratulations, you win!", Html.br [] [] ]

     else
        []
    )
        ++ [ Html.text introduction, Html.br [] [], Html.text generalGuide ]


generalGuide : String
generalGuide =
    "Use the arrow Keys or WASD to move, Z to undo and R to reset, Esc to go back to menu"


divDefautStyle : List (Html.Attribute Msg)
divDefautStyle =
    [ style "width" "320px"
    , style "margin" "0 auto"
    , style "overflow" "auto"
    , style "background-color" "white"
    ]


textDivDefautStyle : List (Html.Attribute Msg)
textDivDefautStyle =
    [ style "width" "500px"
    , style "height" "110px"
    , style "margin" "0 auto"
    , style "overflow" "auto"
    , style "background-color" "white"
    ]


getMapByName : String -> At.Atlas
getMapByName name =
    Maybe.withDefault At.emptyMap (Dict.get name At.mapDict)



{- type alias InvokedObj =
   { objName : String
   , id : Int
   , extraName : Maybe String
   , groupId : Maybe Int
   }
-}


getObjName : String -> String
getObjName name =
    List.head (String.split "-" name)
        |> Maybe.withDefault ""


getTransName : String -> String
getTransName name =
    Array.fromList (String.split "-" name)
        |> Array.get 1
        |> Maybe.withDefault ""


invokObject : String -> Int -> At.GlobalPos -> Maybe Int -> List ( ( String, Int ), ( Obj.Object, At.GlobalPos ) )
invokObject name id gP mgrpId =
    let
        objName =
            getObjName name

        obj =
            Dict.get objName Obj.objDict
                |> Maybe.withDefault (Obj.Object objName [])
    in
    case mgrpId of
        Nothing ->
            [ ( ( name, id ), ( obj, gP ) ) ]

        Just grpId ->
            [ ( ( name, id ), ( Obj.addObjInGrp grpId obj, gP ) ) ]


invokObjectsByList : List ( String, Int, At.GlobalPos ) -> List ( ( String, Int ), At.GlobalPos, Int ) -> Obj.ObjectsLayout
invokObjectsByList listNoGrp listWithGrp =
    let
        resNoGrp =
            List.map (\( name, id, gP ) -> invokObject name id gP Nothing) listNoGrp
                |> List.concat

        resWithGrp =
            List.map (\( ( name, id ), gP, grpId ) -> invokObject name id gP (Just grpId)) listWithGrp
                |> List.concat
    in
    Dict.fromList (resNoGrp ++ resWithGrp)


levelGenerator :
    { map : At.Atlas
    , name : String
    , groundPattern : At.GlobalPos -> Int
    , objectsLayout : Obj.ObjectsLayout
    , introduction : String
    , winCondition : List ( String, Int )
    }
    -> GameLevel
levelGenerator { map, groundPattern, name, objectsLayout, introduction, winCondition } =
    let
        playerGP =
            updatePlayerGP (At.GlobalPos 0 ( -1, -1 )) objectsLayout
    in
    { map = map
    , groundPattern = groundPattern
    , name = name
    , playerGPos = playerGP
    , visionData = updateVision map playerGP objectsLayout (VisionElements Dict.empty [])
    , objectsLayout = objectsLayout
    , gameRecords = []
    , introduction = introduction
    , winCondition = winCondition
    , isWin = False
    }


defaultLayout1 : Obj.ObjectsLayout
defaultLayout1 =
    invokObjectsByList
        [ ( "Player", 0, At.GlobalPos 0 ( 4, 4 ) )
        , ( "Crate", 2, At.GlobalPos 0 ( 6, 4 ) )

        --, ( "Player", 1, At.GlobalPos 0 ( 7, 4 ) )
        ]
        [ ( ( "Butterfly", 0 ), At.GlobalPos 0 ( 5, 4 ), 0 )
        , ( ( "Butterfly-F", 1 ), At.GlobalPos 1 ( 5, 4 ), 0 )
        ]


levelList : List GameLevel
levelList =
    List.map levelGenerator <|
        [ { map = getMapByName "SquareRoot"
          , groundPattern =
                \gP ->
                    if gP.chartId < 0 then
                        case gP.pos of
                            -- mono action: mirror
                            ( 5, 5 ) ->
                                -2

                            _ ->
                                -1

                    else
                        case gP.pos of
                            -- left yello
                            ( 4, 2 ) ->
                                2

                            -- right cyan
                            ( 6, 2 ) ->
                                4

                            _ ->
                                3
          , name = "Find the Butterfly"
          , objectsLayout =
                invokObjectsByList
                    [ ( "Player", 0, At.GlobalPos 0 ( 4, 4 ) )
                    , ( "Butterfly", 0, At.GlobalPos 0 ( 5, 2 ) )
                    , ( "Butterfly-F", 1, At.GlobalPos 1 ( 5, 2 ) )
                    ]
                    []
          , introduction = " Try to turn the butterfly to right pattern,  then move onto it."
          , winCondition = [ ( "Butterfly-F", 1 ) ]
          }
        , { map = getMapByName "S3"
          , groundPattern =
                \gP ->
                    if gP.chartId < 0 then
                        case gP.pos of
                            --mono action : rotate
                            ( 3, 5 ) ->
                                -3

                            --mono action : mirror
                            ( 7, 5 ) ->
                                -2

                            _ ->
                                -1

                    else
                        case gP.pos of
                            -- left yello
                            ( 4, 2 ) ->
                                2

                            -- right bule
                            ( 6, 2 ) ->
                                5

                            -- up red
                            ( 5, 1 ) ->
                                0

                            _ ->
                                3
          , name = "Turn the Triangle"
          , objectsLayout =
                invokObjectsByList
                    [ ( "Player", 0, At.GlobalPos 0 ( 4, 4 ) )
                    , ( "Triangle", 0, At.GlobalPos 0 ( 5, 2 ) )
                    , ( "Triangle-R", 1, At.GlobalPos 1 ( 5, 2 ) )
                    , ( "Triangle-RR", 2, At.GlobalPos 2 ( 5, 2 ) )
                    , ( "Triangle-F", 3, At.GlobalPos 3 ( 5, 2 ) )
                    , ( "Triangle-RF", 4, At.GlobalPos 4 ( 5, 2 ) )
                    , ( "Triangle-RRF", 5, At.GlobalPos 5 ( 5, 2 ) )
                    ]
                    []
          , introduction = " Try to turn the Triangle to right pattern, then move onto it."
          , winCondition = [ ( "Triangle-RRF", 5 ) ]
          }
        ]


myDemos : List GameLevel
myDemos =
    List.map levelGenerator <|
        [ { map = getMapByName "SquareRoot"
          , groundPattern = \gP -> gP.chartId
          , name = "Square Root"
          , objectsLayout = defaultLayout1
          , introduction = " This is a level"
          , winCondition = []
          }
        , { map = getMapByName "EllipticCurve"
          , groundPattern = \gP -> gP.chartId
          , name = "Elliptic Curve"
          , objectsLayout = defaultLayout1
          , introduction = " This is a level"
          , winCondition = []
          }
        , { map = getMapByName "SquareSquareRoot"
          , groundPattern = \gP -> gP.chartId
          , name = "(y^2+1)^2=x"
          , objectsLayout = defaultLayout1
          , introduction = " This is a level"
          , winCondition = []
          }
        ]
