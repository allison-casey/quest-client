-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quest.Object.Weapon exposing (acc, ammo, dmg, id, mag, name, str, traits, value, weight)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Quest.InputObject
import Quest.Interface
import Quest.Object
import Quest.Scalar
import Quest.ScalarCodecs
import Quest.Union


{-| -}
id : SelectionSet Quest.ScalarCodecs.Id Quest.Object.Weapon
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Quest.ScalarCodecs.codecs |> Quest.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| -}
name : SelectionSet String Quest.Object.Weapon
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| -}
dmg : SelectionSet (List String) Quest.Object.Weapon
dmg =
    Object.selectionForField "(List String)" "dmg" [] (Decode.string |> Decode.list)


{-| -}
acc : SelectionSet Int Quest.Object.Weapon
acc =
    Object.selectionForField "Int" "acc" [] Decode.int


{-| -}
str : SelectionSet Int Quest.Object.Weapon
str =
    Object.selectionForField "Int" "str" [] Decode.int


{-| -}
mag : SelectionSet Int Quest.Object.Weapon
mag =
    Object.selectionForField "Int" "mag" [] Decode.int


{-| -}
ammo : SelectionSet String Quest.Object.Weapon
ammo =
    Object.selectionForField "String" "ammo" [] Decode.string


{-| -}
value : SelectionSet Int Quest.Object.Weapon
value =
    Object.selectionForField "Int" "value" [] Decode.int


{-| -}
weight : SelectionSet Int Quest.Object.Weapon
weight =
    Object.selectionForField "Int" "weight" [] Decode.int


{-| -}
traits : SelectionSet (List String) Quest.Object.Weapon
traits =
    Object.selectionForField "(List String)" "traits" [] (Decode.string |> Decode.list)
