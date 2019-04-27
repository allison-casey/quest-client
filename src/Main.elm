module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing (src)

import Browser
import Css exposing (auto, margin, maxWidth, px)
import Debounce exposing (Debounce)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, classList, css, placeholder, src, title)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import ParseWhere exposing (..)
import Quest.Enum.ComparisonOperator as Comparitor
import Quest.InputObject exposing (IntFilterType)
import Quest.Object
import Quest.Object.Armor as Armor
import Quest.Object.Weapon as Weapon
import Quest.Query as Query
import Quest.Scalar exposing (Id(..))
import Quest.Union
import Quest.Union.Item as Item
import RemoteData exposing (RemoteData)
import Task



---- MODEL ----


type alias Model =
    { value : String
    , debounce : Debounce String
    , items : { armors : List ArmorSpec, weapons : List WeaponSpec }
    , selectedTab : Tabs
    , graphqlUrl : String
    }


type alias Flags =
    { graphqlUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { value = ""
      , debounce = Debounce.init
      , items = { armors = [], weapons = [] }
      , selectedTab = WeaponTab
      , graphqlUrl = flags.graphqlUrl
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | UpdateSearch String
    | DebounceMsg Debounce.Msg
    | Saved String
    | ChangeTabs Tabs
    | NoOp


type Tabs
    = WeaponTab
    | ArmorTab


type alias ArmorSpec =
    { name : String
    , dt : Int
    , value : Int
    , weight : Int
    , traits : List String
    }


type alias WeaponSpec =
    { name : String
    , dmg : List String
    , acc : Int
    , str : Int
    , mag : Int
    , ammo : String
    , value : Int
    , weight : Int
    , traits : List String
    }


type Item
    = WeaponDetails WeaponSpec
    | ArmorDetails ArmorSpec


type alias Response =
    List Item


presentIfJust : Maybe a -> Graphql.OptionalArgument.OptionalArgument a
presentIfJust m =
    case m of
        Nothing ->
            Absent

        Just a ->
            Present a


query : ParseWhere.FilterDict -> SelectionSet (List Item) RootQuery
query filterDict =
    Query.findItems
        (\optionals ->
            { optionals
                | where_ =
                    Present
                        { name = presentIfJust <| nameDecoder filterDict
                        , limit = presentIfJust <| limitDecoder filterDict
                        , acc = presentIfJust <| comparitorDecoder "acc" filterDict
                        , value = presentIfJust <| comparitorDecoder "value" filterDict
                        , str = presentIfJust <| comparitorDecoder "str" filterDict
                        , mag = presentIfJust <| comparitorDecoder "mag" filterDict
                        , weight = presentIfJust <| comparitorDecoder "weight" filterDict
                        , dt = presentIfJust <| comparitorDecoder "dt" filterDict
                        }
            }
        )
        itemSelection


itemSelection : SelectionSet Item Quest.Union.Item
itemSelection =
    Item.fragments
        { onArmor = SelectionSet.map ArmorDetails armorSelection
        , onWeapon = SelectionSet.map WeaponDetails weaponSelection
        }


weaponSelection : SelectionSet WeaponSpec Quest.Object.Weapon
weaponSelection =
    SelectionSet.succeed WeaponSpec
        |> with Weapon.name
        |> with Weapon.dmg
        |> with Weapon.acc
        |> with Weapon.str
        |> with Weapon.mag
        |> with Weapon.ammo
        |> with Weapon.value
        |> with Weapon.weight
        |> with Weapon.traits


armorSelection : SelectionSet ArmorSpec Quest.Object.Armor
armorSelection =
    SelectionSet.map5 ArmorSpec
        Armor.name
        Armor.dt
        Armor.value
        Armor.weight
        Armor.traits


makeRequest : String -> String -> Cmd Msg
makeRequest urlHost searchString =
    buildQuery searchString
        |> Graphql.Http.queryRequest "http://localhost:4000/quest"
        -- |> Graphql.Http.queryRequest "/quest"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


buildQuery : String -> SelectionSet (List Item) RootQuery
buildQuery searchString =
    query (parse searchString)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success items ->
                    let
                        defaultValue =
                            { armors = [], weapons = [] }

                        combiner item acc =
                            case item of
                                ArmorDetails armor ->
                                    { acc | armors = armor :: acc.armors }

                                WeaponDetails weapon ->
                                    { acc | weapons = weapon :: acc.weapons }

                        folded =
                            List.foldl combiner defaultValue items
                    in
                    ( { model | items = folded }, Cmd.none )

                RemoteData.Loading ->
                    ( model, Cmd.none )

                RemoteData.NotAsked ->
                    ( model, Cmd.none )

                RemoteData.Failure e ->
                    -- let
                    --     _ =
                    --         Debug.log "error" e
                    -- in
                    ( model, Cmd.none )

        UpdateSearch search ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig search model.debounce
            in
            ( { model | value = search, debounce = debounce }, cmd )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        Saved s ->
            ( model, makeRequest model.graphqlUrl s )

        ChangeTabs tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


save : String -> Cmd Msg
save s =
    Task.perform Saved (Task.succeed s)



---- VIEW ----


viewItem : Item -> Html Msg
viewItem item =
    case item of
        WeaponDetails weapon ->
            viewWeapon weapon

        ArmorDetails armor ->
            viewArmor armor


viewWeapon : WeaponSpec -> Html Msg
viewWeapon { name, dmg, acc, str, mag, ammo, value, weight, traits } =
    tr []
        [ td [ attribute "data-title" "Name" ] [ text name ]
        , td [ attribute "data-title" "Damage" ] [ text <| String.join " " dmg ]
        , td [ attribute "data-title" "Accuracy" ] [ text <| String.fromInt acc ]
        , td [ attribute "data-title" "Strength" ] [ text <| String.fromInt str ]
        , td [ attribute "data-title" "Magazine" ] [ text <| String.fromInt mag ]
        , td [ attribute "data-title" "Ammo" ] [ text ammo ]
        , td [ attribute "data-title" "Value" ] [ text <| String.fromInt value ]
        , td [ attribute "data-title" "Weight" ] [ text <| String.fromInt weight ]
        , td [ attribute "data-title" "Traits" ] [ text <| String.join ", " traits ]
        ]


viewWeaponTable : List WeaponSpec -> Html Msg
viewWeaponTable weaponList =
    table [ class "table is-fullwidth is-striped" ]
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ abbr [ title "Damage" ] [ text "Dmg" ] ]
            , th [] [ abbr [ title "Accuracy" ] [ text "Acc" ] ]
            , th [] [ abbr [ title "Strength" ] [ text "Str" ] ]
            , th [] [ abbr [ title "Magazine" ] [ text "Mag" ] ]
            , th [] [ text "Ammo" ]
            , th [] [ text "Value" ]
            , th [] [ text "Weight" ]
            , th [] [ text "Traits" ]
            ]
        , tbody [] <|
            List.map
                viewWeapon
                weaponList
        ]


viewArmor : ArmorSpec -> Html Msg
viewArmor { name, dt, value, weight, traits } =
    tr []
        [ td [ attribute "data-title" "Name" ] [ text name ]
        , td [ attribute "data-title" "DT" ] [ text <| String.fromInt dt ]
        , td [ attribute "data-title" "Value" ] [ text <| String.fromInt value ]
        , td [ attribute "data-title" "Weight" ] [ text <| String.fromInt weight ]
        , td [ attribute "data-title" "Traits" ] [ text <| String.join ", " traits ]
        ]


viewArmorTable : List ArmorSpec -> Html Msg
viewArmorTable armorList =
    table [ class "table is-fullwidth is-striped" ]
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ abbr [ title "Damage Threshold" ] [ text "DT" ] ]
            , th [] [ text "Value" ]
            , th [] [ text "Weight" ]
            , th [] [ text "Traits" ]
            ]
        , tbody [] <|
            List.map
                viewArmor
                armorList
        ]


view : Model -> Html Msg
view { selectedTab, items } =
    let
        itemTable =
            case selectedTab of
                WeaponTab ->
                    viewWeaponTable items.weapons

                ArmorTab ->
                    viewArmorTable items.armors
    in
    nav
        [ class "panel"
        , css
            [ maxWidth (px 1200)
            , margin auto
            , Css.marginTop (px 10)
            , Css.boxShadow4 (px 10) (px 10) (px 5) (Css.rgba 0 0 0 0.25)
            ]
        ]
        [ p [ class "panel-heading" ] [ text "Quest Items" ]
        , div [ class "panel-block" ]
            [ p [ class "control has-icons-left" ]
                [ input [ class "input", placeholder "search", onInput UpdateSearch ] []
                , span [ class "icon is-left" ] [ i [ class "fas fa-search" ] [] ]
                ]
            ]
        , p [ class "panel-tabs" ]
            [ a
                [ classList [ ( "is-active", selectedTab == WeaponTab ) ]
                , onClick <| ChangeTabs WeaponTab
                ]
                [ text <| "Weapons " ++ (String.fromInt <| List.length items.weapons) ]
            , a
                [ classList [ ( "is-active", selectedTab == ArmorTab ) ]
                , onClick <| ChangeTabs ArmorTab
                ]
                [ text <| "Armors " ++ (String.fromInt <| List.length items.armors) ]
            ]
        , a [ class "panel-block", css [ Css.padding (px 0) ] ]
            [ itemTable ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
