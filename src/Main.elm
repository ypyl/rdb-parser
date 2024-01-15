module Main exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompIf, chompUntil, chompWhile, getChompedString, getCol, getIndent, keyword, loop, map, number, oneOf, problem, spaces, succeed, symbol, withIndent)
import Parser exposing (end)
import Parser exposing (chompUntilEndOr)


type alias Domain =
    { name : String
    , description : Maybe String
    , actors : Dict ViewElementKey Data
    , elements : Dict ViewElementKey Node
    }


type Node
    = Parent Data (Dict ViewElementKey Node)
    | Leaf Data


type alias Data =
    { name : String
    , description : Maybe String
    , relations : Maybe (List Relation)
    }


type alias Relation =
    ( String, String )


type alias View =
    { elements : Dict ViewElementKey ViewElement
    }


type alias ViewElementKey =
    String


type alias ViewElement =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , relations : Dict Relation (List ViewRelationPoint)
    }


type alias ViewRelationPoint =
    { x : Float
    , y : Float
    }


{-| Only ' ' or nothing -}
onlySpace : Parser ()
onlySpace =
    chompWhile (\c -> c == ' ')


{-| Only \n -}
isNewLine : Parser ()
isNewLine =
    oneOf
        [ symbol "\n"
        , end
        ]


isSeparator : Parser ()
isSeparator =
    symbol ":"


addIndent : Parser a -> Parser a
addIndent parser =
    getCol |> map (Debug.log "addIndent") |> andThen (\col -> withIndent col parser)


indented : Parser ()
indented =
    indented_ (>)


notIndented : Parser ()
notIndented =
    indented_ (==)


indented_ : (Int -> Int -> Bool) -> Parser ()
indented_ com =
    succeed (\indent column -> ( indent, column ))
        |= getIndent
        |= getCol
        |> andThen (checkIndentHelp_ com)


checkIndentHelp_ : (Int -> Int -> Bool) -> ( Int, Int ) -> Parser ()
checkIndentHelp_ com ( indent, column ) =
    if com column indent then
        succeed () |> withIndent indent

    else
        problem "indention is incorrect"


domain : Parser Domain
domain =
    succeed Domain
        |. onlySpace
        |. addIndent (nameNode "domain")
        |. onlySpace
        |. indented
        |= addIndent (namedNode "name")
        |= succeed Nothing
        |= succeed Dict.empty
        |= succeed Dict.empty


views : Parser (Dict String View)
views =
    succeed identity
        |. addIndent (nameNode "views")
        |= succeed Dict.empty


view : Parser View
view =
    succeed View
        |= succeed Dict.empty


namedNode : String -> Parser String
namedNode nodeNameValue =
    succeed identity
        |. keyword nodeNameValue
        |. isSeparator
        |. onlySpace
        |= value
        |. onlySpace
        |. isNewLine


nameNode : String -> Parser ()
nameNode name =
    succeed ()
        |. keyword name
        |. isSeparator
        |. onlySpace
        |. isNewLine



-- UTILS


multiLineValue : Parser (List String)
multiLineValue =
    loop (0, []) stepMultiLineValue


stepMultiLineValue : (Int, List String) -> Parser (Step (Int, List String) (List String))
stepMultiLineValue (indent, state) =
    succeed identity
        |. onlySpace
        |= (getCol |> map (Debug.log "getCol"))
        |> andThen (checkIndentHelp (indent, state))


checkIndentHelp : (Int, List String) -> Int -> Parser (Step (Int, List String) (List String))
checkIndentHelp (indent, state) column =
    let
        _ = Debug.log "state" (indent, state)
        _ = Debug.log "column" column
    in
    if indent <= column then
        succeed (\nextValue -> Loop (if indent == 0 then column else indent, (nextValue :: state)))
            |= value
            |. isNewLine

    else
        succeed () |> map (\_ -> Done (List.reverse state))


value : Parser String
value =
    succeed ()
        |. chompIf (\c -> c /= ' ')
        |. chompUntilEndOr "\n"
        |> getChompedString
        |> andThen (String.trim >> Debug.log "value" >> succeed)
