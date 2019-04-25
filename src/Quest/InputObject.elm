-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quest.InputObject exposing (ArmorAttributes, ArmorAttributesRequiredFields, ArmorWhere, ArmorWhereOptionalFields, IntFilterType, IntFilterTypeRequiredFields, ItemWhere, ItemWhereOptionalFields, WeaponAttributes, WeaponAttributesRequiredFields, WeaponWhere, WeaponWhereOptionalFields, buildArmorAttributes, buildArmorWhere, buildIntFilterType, buildItemWhere, buildWeaponAttributes, buildWeaponWhere, encodeArmorAttributes, encodeArmorWhere, encodeIntFilterType, encodeItemWhere, encodeWeaponAttributes, encodeWeaponWhere)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Quest.Enum.ArmorType
import Quest.Enum.ComparisonOperator
import Quest.Interface
import Quest.Object
import Quest.Scalar
import Quest.ScalarCodecs
import Quest.Union


buildArmorAttributes : ArmorAttributesRequiredFields -> ArmorAttributes
buildArmorAttributes required =
    { name = required.name, type_ = required.type_, dt = required.dt, value = required.value, weight = required.weight, traits = required.traits }


type alias ArmorAttributesRequiredFields =
    { name : String
    , type_ : Quest.Enum.ArmorType.ArmorType
    , dt : Int
    , value : Int
    , weight : Int
    , traits : List String
    }


{-| Type for the ArmorAttributes input object.
-}
type alias ArmorAttributes =
    { name : String
    , type_ : Quest.Enum.ArmorType.ArmorType
    , dt : Int
    , value : Int
    , weight : Int
    , traits : List String
    }


{-| Encode a ArmorAttributes into a value that can be used as an argument.
-}
encodeArmorAttributes : ArmorAttributes -> Value
encodeArmorAttributes input =
    Encode.maybeObject
        [ ( "name", Encode.string input.name |> Just ), ( "type", Encode.enum Quest.Enum.ArmorType.toString input.type_ |> Just ), ( "dt", Encode.int input.dt |> Just ), ( "value", Encode.int input.value |> Just ), ( "weight", Encode.int input.weight |> Just ), ( "traits", (Encode.string |> Encode.list) input.traits |> Just ) ]


buildArmorWhere : (ArmorWhereOptionalFields -> ArmorWhereOptionalFields) -> ArmorWhere
buildArmorWhere fillOptionals =
    let
        optionals =
            fillOptionals
                { name = Absent, dt = Absent, value = Absent, weight = Absent, limit = Absent }
    in
    { name = optionals.name, dt = optionals.dt, value = optionals.value, weight = optionals.weight, limit = optionals.limit }


type alias ArmorWhereOptionalFields =
    { name : OptionalArgument String
    , dt : OptionalArgument IntFilterType
    , value : OptionalArgument IntFilterType
    , weight : OptionalArgument IntFilterType
    , limit : OptionalArgument Int
    }


{-| Type for the ArmorWhere input object.
-}
type alias ArmorWhere =
    { name : OptionalArgument String
    , dt : OptionalArgument IntFilterType
    , value : OptionalArgument IntFilterType
    , weight : OptionalArgument IntFilterType
    , limit : OptionalArgument Int
    }


{-| Encode a ArmorWhere into a value that can be used as an argument.
-}
encodeArmorWhere : ArmorWhere -> Value
encodeArmorWhere input =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input.name ), ( "dt", encodeIntFilterType |> Encode.optional input.dt ), ( "value", encodeIntFilterType |> Encode.optional input.value ), ( "weight", encodeIntFilterType |> Encode.optional input.weight ), ( "limit", Encode.int |> Encode.optional input.limit ) ]


buildIntFilterType : IntFilterTypeRequiredFields -> IntFilterType
buildIntFilterType required =
    { comparitor = required.comparitor, input = required.input }


type alias IntFilterTypeRequiredFields =
    { comparitor : Quest.Enum.ComparisonOperator.ComparisonOperator
    , input : Int
    }


{-| Type for the IntFilterType input object.
-}
type alias IntFilterType =
    { comparitor : Quest.Enum.ComparisonOperator.ComparisonOperator
    , input : Int
    }


{-| Encode a IntFilterType into a value that can be used as an argument.
-}
encodeIntFilterType : IntFilterType -> Value
encodeIntFilterType input =
    Encode.maybeObject
        [ ( "comparitor", Encode.enum Quest.Enum.ComparisonOperator.toString input.comparitor |> Just ), ( "input", Encode.int input.input |> Just ) ]


buildItemWhere : (ItemWhereOptionalFields -> ItemWhereOptionalFields) -> ItemWhere
buildItemWhere fillOptionals =
    let
        optionals =
            fillOptionals
                { name = Absent, limit = Absent, acc = Absent }
    in
    { name = optionals.name, limit = optionals.limit, acc = optionals.acc }


type alias ItemWhereOptionalFields =
    { name : OptionalArgument String
    , limit : OptionalArgument Int
    , acc : OptionalArgument IntFilterType
    }


{-| Type for the ItemWhere input object.
-}
type alias ItemWhere =
    { name : OptionalArgument String
    , limit : OptionalArgument Int
    , acc : OptionalArgument IntFilterType
    }


{-| Encode a ItemWhere into a value that can be used as an argument.
-}
encodeItemWhere : ItemWhere -> Value
encodeItemWhere input =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input.name ), ( "limit", Encode.int |> Encode.optional input.limit ), ( "acc", encodeIntFilterType |> Encode.optional input.acc ) ]


buildWeaponAttributes : WeaponAttributesRequiredFields -> WeaponAttributes
buildWeaponAttributes required =
    { name = required.name, dmg = required.dmg, acc = required.acc, str = required.str, value = required.value, mag = required.mag, ammo = required.ammo, weight = required.weight, traits = required.traits }


type alias WeaponAttributesRequiredFields =
    { name : String
    , dmg : List String
    , acc : Int
    , str : Int
    , value : Int
    , mag : Int
    , ammo : String
    , weight : Int
    , traits : List String
    }


{-| Type for the WeaponAttributes input object.
-}
type alias WeaponAttributes =
    { name : String
    , dmg : List String
    , acc : Int
    , str : Int
    , value : Int
    , mag : Int
    , ammo : String
    , weight : Int
    , traits : List String
    }


{-| Encode a WeaponAttributes into a value that can be used as an argument.
-}
encodeWeaponAttributes : WeaponAttributes -> Value
encodeWeaponAttributes input =
    Encode.maybeObject
        [ ( "name", Encode.string input.name |> Just ), ( "dmg", (Encode.string |> Encode.list) input.dmg |> Just ), ( "acc", Encode.int input.acc |> Just ), ( "str", Encode.int input.str |> Just ), ( "value", Encode.int input.value |> Just ), ( "mag", Encode.int input.mag |> Just ), ( "ammo", Encode.string input.ammo |> Just ), ( "weight", Encode.int input.weight |> Just ), ( "traits", (Encode.string |> Encode.list) input.traits |> Just ) ]


buildWeaponWhere : (WeaponWhereOptionalFields -> WeaponWhereOptionalFields) -> WeaponWhere
buildWeaponWhere fillOptionals =
    let
        optionals =
            fillOptionals
                { name = Absent, value = Absent, acc = Absent, str = Absent, mag = Absent, weight = Absent, limit = Absent }
    in
    { name = optionals.name, value = optionals.value, acc = optionals.acc, str = optionals.str, mag = optionals.mag, weight = optionals.weight, limit = optionals.limit }


type alias WeaponWhereOptionalFields =
    { name : OptionalArgument String
    , value : OptionalArgument IntFilterType
    , acc : OptionalArgument IntFilterType
    , str : OptionalArgument IntFilterType
    , mag : OptionalArgument IntFilterType
    , weight : OptionalArgument IntFilterType
    , limit : OptionalArgument Int
    }


{-| Type for the WeaponWhere input object.
-}
type alias WeaponWhere =
    { name : OptionalArgument String
    , value : OptionalArgument IntFilterType
    , acc : OptionalArgument IntFilterType
    , str : OptionalArgument IntFilterType
    , mag : OptionalArgument IntFilterType
    , weight : OptionalArgument IntFilterType
    , limit : OptionalArgument Int
    }


{-| Encode a WeaponWhere into a value that can be used as an argument.
-}
encodeWeaponWhere : WeaponWhere -> Value
encodeWeaponWhere input =
    Encode.maybeObject
        [ ( "name", Encode.string |> Encode.optional input.name ), ( "value", encodeIntFilterType |> Encode.optional input.value ), ( "acc", encodeIntFilterType |> Encode.optional input.acc ), ( "str", encodeIntFilterType |> Encode.optional input.str ), ( "mag", encodeIntFilterType |> Encode.optional input.mag ), ( "weight", encodeIntFilterType |> Encode.optional input.weight ), ( "limit", Encode.int |> Encode.optional input.limit ) ]
