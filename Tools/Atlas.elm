module Tools.Atlas exposing (..)

import Dict exposing (Dict)
import EverySet as Set
import List.Unique as Unique


type alias Position =
    ( Int, Int )


pAdd : Position -> Position -> Position
pAdd ( a, b ) ( c, d ) =
    ( a + c, b + d )


type Direction
    = N
    | S
    | E
    | W


flipUpDown : Direction -> Direction
flipUpDown dir =
    case dir of
        N ->
            S

        S ->
            N

        i ->
            i


int2Dir : Int -> Direction
int2Dir n =
    --conterclockwise
    let
        i =
            modBy 4 n
    in
    case i of
        1 ->
            N

        2 ->
            W

        3 ->
            S

        _ ->
            E


d2Int : Direction -> Int
d2Int dir =
    case dir of
        N ->
            1

        W ->
            2

        S ->
            3

        E ->
            0


dirAdd : Direction -> Direction -> Direction
dirAdd dir1 dir2 =
    int2Dir (d2Int dir1 + d2Int dir2)

oppoRot : Direction -> Direction
oppoRot dir =
    int2Dir -(d2Int dir)

oppoDir : Direction -> Direction
oppoDir dir =
    case dir of
        N ->
            S

        S ->
            N

        E ->
            W

        W ->
            E


d2V : Direction -> Position
d2V dir =
    case dir of
        N ->
            ( 0, -1 )

        S ->
            ( 0, 1 )

        E ->
            ( 1, 0 )

        W ->
            ( -1, 0 )



{- type alias Block a=
   {position : Position
   ,label: a}
-}


type alias Chart =
    { chartId : Int
    , blocks : List Position
    , gaps : List HalfEdge
    }


type alias GlobalPos =
    { chartId : Int
    , pos : Position
    }


type alias HalfEdge =
    { chartId : Int
    , pos : Position
    , dir : Direction
    }


type alias EdgeLink =
    { from : HalfEdge
    , to : HalfEdge
    }


type alias Atlas =
    { charts : Dict Int Chart
    , links : List EdgeLink
    , atlasName : String
    }


move : Atlas -> GlobalPos -> Direction -> Result String ( Bool, GlobalPos )
move atl { chartId, pos } dir =
    let
        mcha : Maybe Chart
        mcha =
            atl.charts
                |> Dict.get chartId

        --edge be faced
        hE =
            HalfEdge chartId pos dir

        --target Position
        tP =
            pAdd (d2V dir) pos

        notMoveResult =
            Ok ( False, GlobalPos chartId pos )
    in
    case mcha of
        Nothing ->
            Err "Can not find the chart"

        Just cha ->
            if List.member pos cha.blocks then
                --if move out from chart or face the gaps
                if not (List.member tP cha.blocks) || List.member hE cha.gaps then
                    let
                        --try to find link from this direction
                        isFromHere : EdgeLink -> Bool
                        isFromHere eL =
                            eL.from == hE

                        mlK =
                            List.filter isFromHere atl.links
                                |> List.head
                    in
                    case mlK of
                        --can not move
                        Nothing ->
                            notMoveResult

                        --move to new chart
                        Just lK ->
                            let
                                mTargetChart =
                                    atl.charts
                                        |> Dict.get lK.to.chartId
                            in
                            case mTargetChart of
                                --target chart not exist
                                Nothing ->
                                    notMoveResult

                                Just tChart ->
                                    --target position is in target chart
                                    if List.member lK.to.pos tChart.blocks then
                                        Ok ( True, GlobalPos lK.to.chartId lK.to.pos )

                                    else
                                        notMoveResult

                else
                    Ok ( True, GlobalPos chartId tP )

            else
                Err "Position is out of chart"



--functions show the area you can see, the idea is scanning tiles by lines


scanLineDefault : Int -> Int -> List Direction
scanLineDefault n k =
    --line of y=k/n*x of k<n in (n+1)*(n+1)
    if (k < n) && (0 <= k) then
        let
            --define the line step by step
            step i =
                if i == 1 then
                    [ E ]

                else
                    let
                        --floor ((i-1/2)*k/n+1/2)
                        a =
                            (k * (2 * i - 1) + n) // (2 * n)

                        b =
                            (k * (2 * i - 3) + n) // (2 * n)
                    in
                    if (a - b) == 1 then
                        step (i - 1) ++ [ N, E ]

                    else
                        step (i - 1) ++ [ E ]
        in
        if k == (n - 1) then
            step n ++ [ N ]

        else
            step n

    else
        []


scanAreaByDir : Int -> Direction -> List (List Direction)
scanAreaByDir n dir =
    let
        defaultArea =
            List.range 1 (n - 1)
                |> List.map (scanLineDefault n)

        downArea =
            defaultArea
                |> List.map (List.map flipUpDown)

        eastArea =
            defaultArea ++ ( List.repeat n E :: downArea)
    in
    if dir == E then
        eastArea

    else
        eastArea
            |> List.map (List.map (dirAdd dir))

scanRange : Int
scanRange =
    9


allDirScans : List (List Direction)
allDirScans =
    List.map (scanAreaByDir scanRange) [ E, N, W, S ]
        |> List.concat


visableAreaByLine : Atlas -> List GlobalPos -> List Direction -> List GlobalPos
visableAreaByLine atl gPs line =
    let
        moveSimple : ( Maybe GlobalPos, Maybe Direction ) -> ( Bool, List GlobalPos )
        moveSimple maybewhat =
            case maybewhat of
                ( Just gP, Just dir ) ->
                    let
                        res =
                            move atl gP dir
                    in
                    case res of
                        Ok ( isMove, tGP ) ->
                            ( isMove, [ tGP ] )

                        _ ->
                            ( False, [] )

                _ ->
                    ( False, [] )

        myTail list =
            List.tail list
                |> Maybe.withDefault []
    in
    case moveSimple ( List.head gPs, List.head line ) of
        ( False, _ ) ->
            gPs

        ( True, newGP ) ->
            visableAreaByLine atl (newGP ++ gPs) (myTail line)


visableArea : Atlas -> GlobalPos -> List (List Direction) -> List GlobalPos
visableArea atl gP0 scans =
    List.map (visableAreaByLine atl [ gP0 ]) scans
        |> List.concat
        |> Unique.filterDuplicates

defaultVisableArea : Atlas -> GlobalPos -> List GlobalPos
defaultVisableArea atl gP0 =
    visableArea atl gP0 allDirScans



-- functions about links


createLink : Int -> Position -> Direction -> Int -> Position -> Direction -> List EdgeLink
createLink i1 p1 d1 i2 p2 d2 =
    [ EdgeLink (HalfEdge i1 p1 d1) (HalfEdge i2 p2 d2)
    , EdgeLink (HalfEdge i2 p2 d2) (HalfEdge i1 p1 d1)
    ]



--create a default link at bound of chart


defaultLink1 : Int -> Position -> Direction -> Int -> List EdgeLink
defaultLink1 i1 p1 d1 i2 =
    createLink i1 p1 d1 i2 (pAdd p1 (d2V d1)) (oppoDir d1)



--create a long default link between two position


createLongStraightLinks1 : Int -> Position -> Position -> Direction -> Int -> List EdgeLink
createLongStraightLinks1 i1 p1 p2 dir i2 =
    let
        ps =
            formRectangle p1 p2

        makelk x =
            defaultLink1 i1 x dir i2
    in
    List.map makelk ps
        |> List.concat


linksToGaps : Int -> List EdgeLink -> List HalfEdge
linksToGaps ind lks =
    let
        linkIsFromChartInd lk =
            lk.from.chartId == ind

        linksI =
            List.filter linkIsFromChartInd lks
    in
    List.map (\x -> x.from) linksI



--functions about gaps

createHalfGap : Int -> Position -> Direction -> List HalfEdge
createHalfGap i1 p1 d1 =
    --create a one-way gap like a cliff.
    [ HalfEdge i1 p1 d1 ]

defaultGaps1 : Int -> Position -> Direction -> List HalfEdge
defaultGaps1 i1 p1 d1 =
    --create a 2 sides gap
    [ HalfEdge i1 p1 d1
    , HalfEdge i1 (pAdd p1 (d2V d1)) (oppoDir d1)
    ]



--functions about chart


formRectangle : Position -> Position -> List Position
formRectangle ( a, b ) ( c, d ) =
    let
        xs =
            List.range a c

        ys =
            List.range b d

        f1 x =
            List.map (Tuple.pair x) ys
    in
    List.map f1 xs
        |> List.concat


createChartBlocks : List Position -> List Position -> List Position
createChartBlocks base holes =
    -- base - holes
    Set.diff (Set.fromList base) (Set.fromList holes)
        |> Set.toList



--functions generate Atlas
--n cover


createAtlasNCover : Int -> (Int -> List Position) -> List EdgeLink -> String -> Atlas
createAtlasNCover n base lks name =
    let
        --remove the links not match to chart
        isLinkExist lk =
            List.member lk.from.pos (base lk.from.chartId) && List.member lk.to.pos (base lk.to.chartId)

        newLks =
            List.filter isLinkExist lks

        --create gaps by new links
        createChartByInd ind =
            ( ind, Chart ind (base ind) (linksToGaps ind newLks) )
    in
    -- the -1 level as obstacle
    { charts =
        Dict.fromList <|
            List.map createChartByInd (List.range -1 (n - 1))
    , links = newLks
    , atlasName = name
    }


testMap1 : Atlas
testMap1 =
    let
        holes =
            [ ( 1, 1 ), ( 8, 5 ), ( 3, 5 ) ]

        base i =
            if i == -1 then
                holes

            else
                createChartBlocks (formRectangle ( 0, 0 ) ( 9, 9 )) holes

        lks =
            List.concat <|
                [ defaultLink1 0 ( 0, 1 ) N 1
                , defaultLink1 1 ( 0, 1 ) N 0
                , createLongStraightLinks1 0 ( 4, 5 ) ( 7, 5 ) S 1
                , createLongStraightLinks1 1 ( 4, 5 ) ( 7, 5 ) S 0
                ]
    in
    createAtlasNCover 2 base lks "elliptic curve"
