module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing (src)

import Browser
import Css exposing (..)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onBlur, onInput)
import Quest.Enum.ComparisonOperator as Comparitor
import Quest.Object
import Quest.Object.Armor as Armor
import Quest.Query as Query
import Quest.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData)



---- MODEL ----


type alias Model =
    { armors : List Armor }



-- RemoteData (Graphql.Http.Error Response) Response
-- init : () -> ( Model, Cmd Msg )
-- init _ =
--     ( RemoteData.Loading, makeRequest )


init : ( Model, Cmd Msg )
init =
    ( { armors = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | UpdateSearch String
    | NoOp


type alias Armor =
    { name : String
    , dt : Int
    , value : Int
    , weight : Int
    }


type alias Response =
    List Armor


query : String -> SelectionSet (List Armor) RootQuery
query name =
    Query.findArmors
        (\optionals ->
            { optionals
                | where_ =
                    Present
                        { name = Present name
                        , dt = Absent
                        , value = Absent
                        , weight = Absent
                        , limit = Absent
                        }
            }
        )
        armorSelection


armorSelection : SelectionSet Armor Quest.Object.Armor
armorSelection =
    SelectionSet.map4 Armor
        Armor.name
        Armor.dt
        Armor.value
        Armor.weight


makeRequest : String -> Cmd Msg
makeRequest searchString =
    buildQuery searchString
        |> Graphql.Http.queryRequest "http://localhost:4000/quest"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


buildQuery : String -> SelectionSet (List Armor) RootQuery
buildQuery searchString =
    query searchString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success armors ->
                    ( { model | armors = armors }, Cmd.none )

                RemoteData.Loading ->
                    ( model, Cmd.none )

                RemoteData.NotAsked ->
                    ( model, Cmd.none )

                RemoteData.Failure e ->
                    let
                        _ =
                            Debug.log "error" e
                    in
                    ( model, Cmd.none )

        UpdateSearch search ->
            ( model, makeRequest search )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewArmor : Armor -> Html Msg
viewArmor armor =
    li []
        [ text
            (armor.name
                ++ " caps: "
                ++ String.fromInt armor.dt
                ++ " dt: "
                ++ String.fromInt armor.dt
                ++ " weight: "
                ++ String.fromInt armor.weight
            )
        ]


view : Model -> Html Msg
view model =
    div
        [ css
            [ maxWidth (px 600)
            , margin auto
            ]
        ]
        [ h1 [] [ text "Quest" ]
        , input
            [ css
                [ margin (px 0)
                , padding4 (px 16) (px 16) (px 16) (px 60)
                , outline none
                , border3 (px 1) solid (hex "000000")
                , width (pct 100)
                ]
            , onInput UpdateSearch
            ]
            [ text "A Button" ]
        , ul []
            (List.map viewArmor model.armors)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
