module Tests exposing (all)

import Expect
import Parser exposing (..)
import Test exposing (..)



---- Parser


type alias Where =
    { name : Maybe String
    }


type Filter
    = Name String
    | Limit Int


defaultWhere : Where
defaultWhere =
    { name = Nothing, limit = ( "limit", Nothing ), test = ( "test", Nothing ) }


nameParser : Parser String
nameParser =
    let
        chompUntilColon =
            Parser.getChompedString <|
                succeed ()
                    |. chompUntilEndOr ":"
    in
    oneOf
        [ chompUntilColon
        , succeed identity ""
        ]


limitParser : Parser (Maybe Int)
limitParser =
    oneOf
        [ succeed Just
            |. symbol ":"
            |. keyword "limit"
            |. symbol "="
            |= Parser.int
            |. symbol ":"
        , succeed Nothing
        ]


whereParser : Parser Where
whereParser =
    succeed Where
        |. spaces
        |= oneOf [ nameParser ]



-- whereParserHelp


parse : String -> Result (List Parser.DeadEnd) Where
parse =
    Parser.run whereParser



---- Tests


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "This test should fail" <|
            \_ ->
                let
                    _ =
                        Debug.log "parse" (parse "power")
                in
                Expect.pass
        ]
