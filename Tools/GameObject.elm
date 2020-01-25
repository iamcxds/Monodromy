module Tools.GameObject exposing (Object, ObjectsLayout, onePlayerTryMove, objDict,getObjByGP)

{- import Debug exposing (log) -}

import Dict exposing (Dict)
import Set exposing (Set)
import Tools.Atlas as At exposing (pX, pY)


type Property
    = You -- 0
    | Stop --1
    | Push --2


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


generatePropSets : ObjectsLayout -> Dict Int (Set ( String, Int ))
generatePropSets layout =
    let
        objWith ind prop =
            ( ind
            , Dict.filter (\_ o -> List.member prop (pX o).properties) layout
                |> Dict.keys
                |> Set.fromList
            )
    in
    List.indexedMap objWith propertyList
        |> Dict.fromList

getObjByGP : At.GlobalPos -> ObjectsLayout -> List (String,Int)
getObjByGP gP layout =
    Dict.filter (\_ o -> pY o == gP) layout
        |>Dict.keys


onePlaceTryMove : Set ( String, Int ) -> At.GlobalPos -> At.Atlas -> At.Direction -> ObjectsLayout -> Dict Int (Set ( String, Int )) -> ( Bool, ObjectsLayout )
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
                        playersAndPushObj =
                            Set.union names pushOnTGP

                        propsAfterTryPush =
                            --update stop objects, add current player and push objects
                            Dict.update 1 (\stop -> Just (Set.union playersAndPushObj (defaultEmpty stop))) propSets

                        layoutAfterTryPush =
                            onePlaceTryMove pushOnTGP tGP atl dir layout propsAfterTryPush
                                |> pY
                    in
                    onePlaceTryMove names gP atl dir layoutAfterTryPush propsAfterTryPush

            else
                defaultCase

        _ ->
            defaultCase


onePlayerTryMove : At.Atlas -> At.Direction -> ObjectsLayout -> (Bool,ObjectsLayout)
onePlayerTryMove atl dir layout =
    let
        mGP =
            Dict.get ( "Player", 0 ) layout
                |> Maybe.map pY
    in
    Result.withDefault (False,layout) <|
        case mGP of
            Just gP ->
                onePlaceTryMove (Set.singleton ( "Player", 0 )) gP atl dir layout (generatePropSets layout)
                    |> Ok

            Nothing ->
                Err {- log "ObjectError" -} "Can not find player"



{- tryUpdateObjects : At.Atlas -> ObjectsLayOut -> Dict Int (Set (String,Int)) -> Msg -> (Bool,ObjectsLayOut)
   tryUpdateObjects atl layout propSets msg =
       let
           defaultCase = (False , layout)
           getPropSet ind =
               Dict.get ind propSets
                   |>Maybe.withDefault Set.empty
           filterPlayer = Set.intersect (getPropSet 0)
           filterStop = Set.intersect (getPropSet 1)
           filterPush = Set.intersect (getPropSet 2)
           objsOn gP = Dict.filter (\_ o -> ((pY o) == gP)) layout





       in
           case msg of
               TryMove (name,id) dir ->
                   case Dict.get (name,ind) layout of
                       Just (obj, gP) ->
                           ( True , upd )

                       Nothing -> defaultCase




               _ -> defaultCase
-}


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