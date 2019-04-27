module ParseWhere exposing
    ( Field
    , Filter(..)
    , FilterDict
    , arrayFieldDecoder
    , comparitorDecoder
    , createIntFilter
    , limitDecoder
    , nameDecoder
    , nameParser
    , parse
    , stringFilterDecoer
    , whereBlockParser
    , whereBlockParserHelp
    )

import Dict
import Parser exposing (..)
import Quest.Enum.ComparisonOperator as CO
import Quest.InputObject exposing (IntFilterType)


type Filter
    = Limit Int
    | Name String
    | ArrayField (List String)
    | Comparison CO.ComparisonOperator Int


type alias FilterDict =
    Dict.Dict String Filter


type alias Field =
    String


nameParser : Parser ( Field, Filter )
nameParser =
    let
        chompUntilColon =
            Parser.getChompedString <|
                succeed ()
                    |. chompUntilEndOr ":"
    in
    map (\a -> ( "name", Name a )) chompUntilColon


createIntFilter : (Int -> Filter) -> Field -> Parser ( Field, Filter )
createIntFilter constructor field =
    map (\a -> ( field, constructor a ))
        (succeed identity
            |. symbol ":"
            |. keyword field
            |. symbol "="
            |= Parser.int
            |. symbol ":"
            |. spaces
        )


createStringFilter : Field -> Parser ( Field, Filter )
createStringFilter field =
    map (\a -> ( field, Name a ))
        (succeed identity
            |. symbol ":"
            |. keyword field
            |. symbol "="
            |= (Parser.getChompedString <| succeed () |. chompUntil ":")
        )


createArrayFilter : Field -> Parser ( Field, Filter )
createArrayFilter field =
    map (\a -> ( field, ArrayField (String.split "," a) ))
        (succeed identity
            |. symbol ":"
            |. keyword field
            |. symbol "="
            |= (Parser.getChompedString <| succeed () |. chompUntil ":")
        )


encodeComparitor : Field -> ( String, Int ) -> Parser ( Field, Filter )
encodeComparitor field ( str, input ) =
    case str of
        "=" ->
            succeed ( field, Comparison CO.Eq input )

        "<" ->
            succeed ( field, Comparison CO.Lt input )

        ">" ->
            succeed ( field, Comparison CO.Gt input )

        ">=" ->
            succeed ( field, Comparison CO.Ge input )

        "<=" ->
            succeed ( field, Comparison CO.Ge input )

        "!=" ->
            succeed ( field, Comparison CO.Ne input )

        _ ->
            problem "operator isnt = or <"


createComparisonFilter : Field -> Parser ( Field, Filter )
createComparisonFilter field =
    let
        getChompedComparitor =
            Parser.getChompedString <|
                succeed ()
                    |. chompWhile (\c -> List.member c [ '=', '<', '>', '!' ])
    in
    succeed Tuple.pair
        |. symbol ":"
        |. symbol field
        |= getChompedComparitor
        |= Parser.int
        |. symbol ":"
        |. spaces
        |> andThen (encodeComparitor field)


whereBlockParser : Parser (List ( Field, Filter ))
whereBlockParser =
    nameParser
        |> andThen (whereBlockParserHelp [])


whereBlockParserHelp :
    List ( Field, Filter )
    -> ( Field, Filter )
    -> Parser (List ( Field, Filter ))
whereBlockParserHelp filterList filter =
    let
        recurse =
            \newFilter -> whereBlockParserHelp (filter :: filterList) newFilter

        limitparser =
            createIntFilter Limit "limit" |> andThen recurse

        comparisonParser field =
            createComparisonFilter field |> andThen recurse
    in
    oneOf
        [ backtrackable <| limitparser
        , backtrackable <| (createStringFilter "ammo" |> andThen recurse)
        , backtrackable <| (createArrayFilter "dmg" |> andThen recurse)
        , backtrackable <| (createArrayFilter "traits" |> andThen recurse)
        , backtrackable <| comparisonParser "value"
        , backtrackable <| comparisonParser "acc"
        , backtrackable <| comparisonParser "str"
        , backtrackable <| comparisonParser "mag"
        , backtrackable <| comparisonParser "weight"
        , backtrackable <| comparisonParser "dt"
        , lazy (\_ -> succeed (filter :: filterList))
        ]


filterDecoder : Field -> (Filter -> Maybe a) -> FilterDict -> Maybe a
filterDecoder field callback filterDict =
    let
        filter =
            Dict.get field filterDict
    in
    Maybe.andThen callback filter


nameDecoder : FilterDict -> Maybe String
nameDecoder =
    let
        callback a =
            case a of
                Name name ->
                    Just name

                _ ->
                    Nothing
    in
    filterDecoder "name" callback


limitDecoder : FilterDict -> Maybe Int
limitDecoder =
    let
        callback a =
            case a of
                Limit input ->
                    Just input

                _ ->
                    Nothing
    in
    filterDecoder "limit" callback


comparitorDecoder : Field -> FilterDict -> Maybe IntFilterType
comparitorDecoder field =
    let
        callback a =
            case a of
                Comparison comparitor input ->
                    Just <| IntFilterType comparitor input

                _ ->
                    Nothing
    in
    filterDecoder field callback


stringFilterDecoer : Field -> FilterDict -> Maybe String
stringFilterDecoer field =
    let
        callback a =
            case a of
                Name name ->
                    Just name

                _ ->
                    Nothing
    in
    filterDecoder field callback


arrayFieldDecoder : Field -> FilterDict -> Maybe (List String)
arrayFieldDecoder field =
    let
        callback a =
            case a of
                ArrayField arr ->
                    Just arr

                _ ->
                    Nothing
    in
    filterDecoder field callback


parse str =
    let
        parsedWhere =
            Parser.run whereBlockParser str
    in
    case parsedWhere of
        Ok lst ->
            Dict.fromList lst

        Err err ->
            Dict.fromList []
