module ParseWhere exposing
    ( Field
    , Filter(..)
    , FilterDict
    , comparitorDecoder
    , createIntFilter
    , limitDecoder
    , nameDecoder
    , nameParser
    , parse
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



-- filterToString : Filter -> Field
-- filterToString filter =
--     case filter of
--         Name _ ->
--             "name"
--
--         Limit _ ->
--             "limit"


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
        limitparser =
            createIntFilter Limit "limit"

        recurse =
            \newFilter -> whereBlockParserHelp (filter :: filterList) newFilter
    in
    oneOf
        [ backtrackable limitparser
            |> andThen recurse
        , createComparisonFilter "acc"
            |> andThen recurse
        , lazy (\_ -> succeed (filter :: filterList))
        ]



---- Old Parser ----
-- type alias TestWhere =
--     { name : Maybe String, limit : Maybe Int }
--
--
-- defaultTestWhere : TestWhere
-- defaultTestWhere =
--     { name = Nothing, limit = Nothing }
--
--
-- type alias Where =
--     { name : Maybe String
--     , limit : ( String, Maybe Int )
--     , test : ( String, Maybe Int )
--     }
-- type NewWhere
--     = Name (Maybe String)
--     | Limit (Maybe Int)
-- defaultWhere : Where
-- defaultWhere =
--     { name = Nothing, limit = ( "limit", Nothing ), test = ( "test", Nothing ) }
-- nameParser : Parser (Maybe String)
-- nameParser =
--     let
--         chompUntilColon =
--             Parser.getChompedString <|
--                 succeed ()
--                     |. chompUntilEndOr ":"
--     in
--     oneOf
--         [ succeed Just
--             |= chompUntilColon
--         , succeed Nothing
--         ]
--
--
-- limitParser : String -> Parser ( String, Maybe Int )
-- limitParser field =
--     oneOf
--         [ succeed Just
--             |. symbol ":"
--             |. keyword field
--             |. symbol "="
--             |= Parser.int
--             |. symbol ":"
--         , succeed Nothing
--         ]
--         |> andThen
--             (\code ->
--                 -- let
--                 --     _ =
--                 --         Debug.log "code" code
--                 -- in
--                 succeed
--                     ( field
--                     , code
--                     )
--             )
--
--
-- whereParser : Parser Where
-- whereParser =
--     succeed Where
--         |. spaces
--         |= nameParser
--         |= oneOf [ limitParser "test", limitParser "limit" ]
--         |= limitParser "test"


nameDecoder : Dict.Dict String Filter -> Maybe String
nameDecoder filterDict =
    let
        filter =
            Dict.get "name" filterDict
    in
    Maybe.andThen
        (\a ->
            case a of
                Name name ->
                    Just name

                _ ->
                    Nothing
        )
        filter


limitDecoder : Dict.Dict String Filter -> Maybe Int
limitDecoder filterDict =
    let
        filter =
            Dict.get "limit" filterDict
    in
    Maybe.andThen
        (\a ->
            case a of
                Limit name ->
                    Just name

                _ ->
                    Nothing
        )
        filter


comparitorDecoder : Field -> FilterDict -> Maybe IntFilterType
comparitorDecoder field filterDict =
    let
        filter =
            Dict.get field filterDict
    in
    Maybe.andThen
        (\a ->
            case a of
                Comparison comparitor input ->
                    Just <| IntFilterType comparitor input

                _ ->
                    Nothing
        )
        filter



-- parse : String -> Result (List Parser.DeadEnd) (Dict.Dict String Filter)
-- parse : String -> FilterDict


parse str =
    -- let
    --     whereblock =
    --         Parser.run whereBlockParser str
    --
    --     filters =
    --         case whereblock of
    --             Ok lst ->
    --                 Dict.fromList lst
    --
    --             Err err ->
    --                 Dict.fromList []
    --
    --     _ =
    --         Debug.log "name" filters
    --
    --     _ =
    --         Debug.log "limit" (limitDecoder filters)
    -- in
    -- Parser.run whereBlockParser str
    let
        parsedWhere =
            Parser.run whereBlockParser str
    in
    case parsedWhere of
        Ok lst ->
            Dict.fromList lst

        Err err ->
            Dict.fromList []
