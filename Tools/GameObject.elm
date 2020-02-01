module Tools.GameObject exposing (Object, ObjectsLayout, addObjInGrp, getObjByGP, objDict, onePlayerTryMove)

--import Debug exposing (log)
import Dict exposing (Dict)
import Set exposing (Set)
import Tools.Atlas as At exposing (pX, pY)


type Property
    = You -- 0
    | Stop --1
    | Push --2
    | ConnectGroup Int -- -n


propertyList : List Property
propertyList =
    [ You, Stop, Push ]



{- type Msg
   = TryMove (String,Int) At.Direction
   | Message
-}


type alias Object =
    { name : String
    , properties : List Property
    }


type alias ObjectsLayout =
    Dict ( String, Int ) ( Object, At.GlobalPos )


type alias IntegratedObject =
    Dict ( String, Int ) At.GlobalPos


generatePropSets : ObjectsLayout -> Dict Int (Set ( String, Int ))
generatePropSets layout =
    let
        objWith ind prop =
            ( ind
            , getObjByProp prop layout
                |> Dict.keys
                |> Set.fromList
            )
    in
    List.indexedMap objWith propertyList
        |> Dict.fromList


generatePlayer : ObjectsLayout -> List IntegratedObject
generatePlayer layout =
    let
        objIsYou =
            getObjByProp You layout
                |> Dict.toList

        singlePlayer ( names, ( _, gP ) ) =
            Dict.singleton names gP
    in
    List.map singlePlayer objIsYou


maximalConnectGrp : Int
maximalConnectGrp =
    10


generateExObjs : ObjectsLayout -> List IntegratedObject
generateExObjs layout =
    let
        objInConnectGrp id =
            getObjByProp (ConnectGroup id) layout
                |> Dict.map (always pY)
    in
    List.map objInConnectGrp (List.range 0 maximalConnectGrp)


getObjByProp : Property -> ObjectsLayout -> ObjectsLayout
getObjByProp prop layout =
    Dict.filter (\_ o -> List.member prop (pX o).properties) layout


getObjByGP : At.GlobalPos -> ObjectsLayout -> List ( String, Int )
getObjByGP gP layout =
    Dict.filter (\_ o -> pY o == gP) layout
        |> Dict.keys


orBoolList : List ( Bool, a ) -> ( Bool, List a )
orBoolList list =
    let
        boolList =
            List.map pX list

        result =
            List.map pY list

        boolRes =
            List.foldl (||) False boolList
    in
    ( boolRes, result )


orBoolListFoldl : (a -> b -> ( Bool, b )) -> b -> List a -> ( Bool, b )
orBoolListFoldl fun y xs =
    let
        fun0 x0 ( v, y0 ) =
            fun x0 y0
                |> Tuple.mapFirst ((||) v)
    in
    List.foldl fun0 ( False, y ) xs



{- andBoolList : List (Bool,a) -> (Bool,List a)
   andBoolList list =
       let
           boolList = List.map pX list
           result = List.map pY list

           boolRes = List.foldl (&&) True boolList
       in
           (boolRes,result)
-}
{- onePlaceTryMove : Set ( String, Int ) -> At.GlobalPos -> At.Atlas -> At.Direction -> ObjectsLayout -> Dict Int (Set ( String, Int )) -> ( Bool, ObjectsLayout )
   onePlaceTryMove names gP atl dir layout propSets =
       let
           defaultEmpty =
               Maybe.withDefault Set.empty

           defaultCase =
               ( False, layout )

           getPropSet ind =
               Dict.get ind propSets
                   |> defaultEmpty

           --filterPlayer = Set.intersect (getPropSet 0)
           filterStop =
               Set.intersect (getPropSet 1)

           filterPush =
               Set.intersect (getPropSet 2)

           objsOn gP0 =
               getObjByGP gP0 layout
       in
       case At.tryMoveSimple atl gP dir of
           ( True, tGP ) ->
               let
                   --when success update the layout
                   updateGP ( name, id ) layout0 =
                       Dict.update ( name, id ) (Maybe.map (\( obj, _ ) -> ( obj, tGP ))) layout0

                   successCase =
                       ( True, Set.foldl updateGP layout names )

                   objOnTGP =
                       Set.fromList (objsOn tGP)

                   stopOnTGP =
                       filterStop objOnTGP

                   pushOnTGP =
                       filterPush objOnTGP
               in
               {- try move, if target place has stop object then stop.
                  f has push object then try to move push object
               -}
               if Set.isEmpty stopOnTGP then
                   if Set.isEmpty pushOnTGP then
                       successCase

                   else
                       let
                           pushPlayer =
                               filterPush names

                           pushPlayersAndPushObj =
                               Set.union pushPlayer pushOnTGP

                           propsAfterTryPush =
                               --update stop objects, add current player and push objects
                               Dict.update 1 (\stop -> Just (Set.union pushPlayersAndPushObj (defaultEmpty stop))) propSets

                           layoutAfterTryPush =
                               onePlaceTryMove pushOnTGP tGP atl dir layout propsAfterTryPush
                                   |> pY
                       in
                       onePlaceTryMove names gP atl dir layoutAfterTryPush propsAfterTryPush

               else
                   defaultCase

           _ ->
               defaultCase

-}


integratedMove : At.Atlas -> IntegratedObject -> At.Direction -> ( Bool, IntegratedObject )
integratedMove atl bigObj dir =
    case Dict.toList bigObj of
        [] ->
            ( True, Dict.empty )

        x :: xs ->
            let
                ( canMove, ( names, tGP ) ) =
                    At.tryMoveSimple atl (pY x) dir
                        |> Tuple.mapSecond (\a -> ( pX x, a ))

                tGPxs =
                    integratedMove atl (Dict.fromList xs) dir
            in
            if canMove && pX tGPxs then
                ( True, Dict.insert names tGP (pY tGPxs) )

            else
                ( False, bigObj )



{- move an integrated Object, if meet something attachable, then add it to object -}


integratedMoveAndUpdate : At.Atlas -> IntegratedObject -> At.Direction -> ObjectsLayout -> Dict Int (Set ( String, Int )) -> ( Bool, ( Bool, IntegratedObject ) )
integratedMoveAndUpdate atl bigObj dir layout propSets =
    let
        ( canMoved, tObj ) =
            integratedMove atl bigObj dir

        tGPs =
            Dict.values tObj

        defaultEmpty =
            Maybe.withDefault Set.empty

        getPropSet ind =
            Dict.get ind propSets
                |> defaultEmpty

        {- filterPlayer =
           Set.intersect (getPropSet 0)
        -}
        thingsInBigObj =
            --Dict.filter (\_ xGP->xGP==gP) bigObj
            Dict.keys bigObj
                |> Set.fromList

        filterStop someSet =
            Set.diff (Set.intersect (getPropSet 1) someSet) thingsInBigObj

        filterPush someSet =
            Set.diff (Set.intersect (getPropSet 2) someSet) thingsInBigObj

        --Update big object at one global position result not move
        integratedUpdateByOneGP : At.GlobalPos -> ( Bool, ( Bool, IntegratedObject ) ) -> ( Bool, ( Bool, IntegratedObject ) )
        integratedUpdateByOneGP gP ( isUpdated0, ( canMove0, bigObj0 ) ) =
            let
                objsOnGP =
                    getObjByGP gP layout
                        |> Set.fromList

                stopOnGP =
                    filterStop objsOnGP

                pushOnGP =
                    filterPush objsOnGP

                --the case it will be frozen at begining
                cannotMoveCase =
                    ( False, ( False, bigObj ) )
            in
            if canMove0 then
                if Set.isEmpty stopOnGP then
                    if Set.isEmpty pushOnGP then
                        {- log "not update1" -}
                        ( False || isUpdated0, ( True, bigObj0 ) )

                    else
                        let
                            updatedBigObj =
                                Set.toList pushOnGP
                                    |> List.map (\a -> ( a, gP ))
                                    |> Dict.fromList
                                    |> Dict.union bigObj0
                        in
                        {- log "update" -}
                        ( True, ( True, updatedBigObj ) )

                else
                    cannotMoveCase

            else
                cannotMoveCase

        integratedUpdateByTGPs =
            List.foldl integratedUpdateByOneGP ( False, ( canMoved, bigObj ) ) tGPs
    in
    if canMoved then
        --if object been updated
        case integratedUpdateByTGPs of
            ( True, ( _, newBigObj ) ) ->
                ( True, ( False, newBigObj ) )

            ( False, ( True, _ ) ) ->
                ( False, ( True, tObj ) )

            _ ->
                ( False, ( False, bigObj ) )

    else
        ( False, ( False, bigObj ) )


listIntegratedMoveAndUpdate : At.Atlas -> List IntegratedObject -> At.Direction -> ObjectsLayout -> Dict Int (Set ( String, Int )) -> ( Bool, List ( Bool, IntegratedObject ) )
listIntegratedMoveAndUpdate atl bigObjs dir layout propSets =
    let
        moveAndUpdateForOneObj obj =
            integratedMoveAndUpdate atl obj dir layout propSets

        moveAndUpdateList =
            List.map moveAndUpdateForOneObj bigObjs

        ( isUpdateForAll, movedObjsList ) =
            orBoolList moveAndUpdateList
    in
    if isUpdateForAll then
        let
            --this new function with stop any of BigObjects moving.
            newMoveResult obj =
                if pX (moveAndUpdateForOneObj obj) then
                    pY (moveAndUpdateForOneObj obj)

                else
                    ( False, obj )

            newMoveList =
                List.map newMoveResult bigObjs
        in
        ( True, newMoveList )

    else
        ( False, movedObjsList )


updateLayoutByIntegrated : ( Bool, IntegratedObject ) -> ObjectsLayout -> ( Bool, ObjectsLayout )
updateLayoutByIntegrated ( isMoved, bigObject ) layout =
    if isMoved then
        let
            smallObjs =
                Dict.toList bigObject

            oneObjUpdate ( name, gP ) layout0 =
                Dict.update name (Maybe.map (\( obj, _ ) -> ( obj, gP ))) layout0

            newLayout =
                List.foldl oneObjUpdate layout smallObjs
        in
        ( True, newLayout )

    else
        ( False, layout )


mergeIntergratedObjects : List IntegratedObject -> ( Bool, List IntegratedObject )
mergeIntergratedObjects bigObjList =
    case bigObjList of
        [] ->
            ( False, [] )

        x :: xs ->
            let
                ( isMerged, newxs ) =
                    mergeIntergratedObjects xs

                oneObjTryMerge : IntegratedObject -> List IntegratedObject -> ( Bool, List IntegratedObject )
                oneObjTryMerge obj list =
                    case list of
                        [] ->
                            ( False, [ obj ] )

                        y :: ys ->
                            if Dict.isEmpty (Dict.intersect obj y) then
                                let
                                    ( isMerged1, newys ) =
                                        oneObjTryMerge obj ys
                                in
                                ( isMerged1, y :: newys )

                            else
                                let
                                    newObj =
                                        Dict.union obj y
                                in
                                ( True, pY (oneObjTryMerge newObj ys) )

                ( isMerged2, newList ) =
                    oneObjTryMerge x newxs
            in
            ( isMerged || isMerged2, newList )



{- check and merge big objects and existed objects on map -}


tryMergeWithExistedIntegrated : List IntegratedObject -> List IntegratedObject -> ( Bool, List IntegratedObject )
tryMergeWithExistedIntegrated bigObjs objsOnMap =
    let
        twoObjsMerge mObj obj =
            if Dict.isEmpty (Dict.intersect obj mObj) || Dict.isEmpty (Dict.diff mObj obj) then
                ( False, obj )

            else
                ( True, Dict.union obj mObj )

        oneObjMerge obj =
            orBoolListFoldl twoObjsMerge obj objsOnMap
    in
    List.map oneObjMerge bigObjs
        |> orBoolList



{- moveLikeOneIntegrated : At.Atlas -> IntegratedObject -> At.Direction -> ObjectsLayout -> Dict Int (Set ( String, Int )) -> ( Bool, ObjectsLayout )
   moveLikeOneIntegrated atl bigObj dir layout propSets =
       let
           ( isUpdated, ( isMoved, newObj ) ) =
               integratedMoveAndUpdate atl bigObj dir layout propSets
       in
       if isUpdated then
           moveLikeOneIntegrated atl newObj dir layout propSets

       else
           updateLayoutByIntegrated ( isMoved, newObj ) ( False, layout )
-}


moveLikeIntegrated : At.Atlas -> List IntegratedObject -> At.Direction -> ObjectsLayout -> List IntegratedObject -> Dict Int (Set ( String, Int )) -> ( Bool, ObjectsLayout )
moveLikeIntegrated atl bigObjs dir layout exObjs propSets =
    let
        ( needMerge0, newList0 ) =
            {- log "merge1" -} mergeIntergratedObjects bigObjs

        ( needMerge1, newList ) =
            {- log "merge2" -} tryMergeWithExistedIntegrated newList0 exObjs
    in
    if needMerge0 || needMerge1 then
        moveLikeIntegrated atl newList dir layout exObjs propSets

    else
        let
            ( isUpdated, newObjsAndIsMoved ) =
                listIntegratedMoveAndUpdate atl bigObjs dir layout propSets
        in
        if isUpdated then
            moveLikeIntegrated atl (List.map pY newObjsAndIsMoved) dir layout exObjs propSets

        else
            orBoolListFoldl updateLayoutByIntegrated layout newObjsAndIsMoved


onePlayerTryMove : At.Atlas -> At.Direction -> ObjectsLayout -> ( Bool, ObjectsLayout )
onePlayerTryMove atl dir layout =
    let
        propSets =
            generatePropSets layout

        mGP =
            Dict.get ( "Player", 0 ) layout
                |> Maybe.map pY

        players =
            generatePlayer layout

        exObjs =
            generateExObjs layout
    in
    Result.withDefault ( False, layout ) <|
        case mGP of
            Just _ ->
                moveLikeIntegrated atl players dir layout exObjs propSets
                    --onePlaceTryMove (Set.singleton ( "Player", 0 )) gP atl dir layout (generatePropSets layout)
                    |> Ok

            Nothing ->
                Err {- log "ObjectError" -} "Can not find player"


player : Object
player =
    { name = "Player"
    , properties = [ You ]
    }


crate : Object
crate =
    { name = "Crate"
    , properties = [ Push ]
    }


addObjInGrp : Int -> Object -> Object
addObjInGrp id obj =
    { obj | properties = ConnectGroup id :: obj.properties }


objDict : Dict String Object
objDict =
    let
        f a =
            ( a.name, a )
    in
    Dict.fromList <|
        List.map f <|
            [ player
            , crate
            ]
