-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quest.ScalarCodecs exposing (Id, codecs)

import Json.Decode as Decode exposing (Decoder)
import Quest.Scalar exposing (defaultCodecs)


type alias Id =
    Quest.Scalar.Id


codecs : Quest.Scalar.Codecs Id
codecs =
    Quest.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        }
