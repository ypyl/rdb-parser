module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (a)
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompIf, chompUntil, chompUntilEndOr, chompWhile, end, getChompedString, getCol, getIndent, keyword, loop, map, number, oneOf, problem, spaces, succeed, symbol, withIndent)


type alias Domain =
    { name : String
    , description : List String
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


{-| Only ' ' or nothing
-}
onlySpace : Parser ()
onlySpace =
    chompWhile (\c -> c == ' ')


{-| Only \\n or end
-}
isNewLineOrEnd : Parser ()
isNewLineOrEnd =
    oneOf
        [ symbol "\n"
        , end
        ]


{-| Only :
-}
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
        |. addIndent (keyValue "domain")
        |. onlySpace
        |. indented
        |= addIndent (namedNode "name")
        |= listOrEnd (namedMultiline "description")
        |= succeed Dict.empty
        |= succeed Dict.empty


maybeOrEnd : Parser (Maybe a) -> Parser (Maybe a)
maybeOrEnd innerParser =
    oneOf
        [ succeed identity
            |. onlySpace
            |. indented
            |= addIndent innerParser
        , succeed Nothing
            |. spaces
            |. end
        ]

listOrEnd : Parser (List a) -> Parser (List a)
listOrEnd innerParser =
    oneOf
        [ succeed identity
            |. onlySpace
            |. indented
            |= addIndent innerParser
        , succeed []
            |. spaces
            |. end
        ]


namedNodeOrNothing : String -> Parser (Maybe String)
namedNodeOrNothing nodeNameValue =
    oneOf
        [ succeed (\v -> Just v)
            |. keyword nodeNameValue
            |. isSeparator
            |. onlySpace
            |= oneLineValue
            |. onlySpace
            |. isNewLineOrEnd
        , succeed Nothing
        ]

namedMultiline : String -> Parser (List String)
namedMultiline nodeNameValue =
    oneOf
        [ succeed identity
            |. keyword nodeNameValue
            |. isSeparator
            |. onlySpace
            |= multiLineValue
            |. onlySpace
            |. isNewLineOrEnd
        , succeed []
        ]


views : Parser (Dict String View)
views =
    succeed identity
        |. addIndent (keyValue "views")
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
        |= oneLineValue
        |. onlySpace
        |. isNewLineOrEnd


keyValue : String -> Parser ()
keyValue name =
    succeed ()
        |. keyword name
        |. isSeparator
        |. onlySpace
        |. isNewLineOrEnd



-- UTILS


multiLineValue : Parser (List String)
multiLineValue =
    loop ( ( 0, 0 ), [] ) stepMultiLineValue


stepMultiLineValue : ( ( Int, Int ), List String ) -> Parser (Step ( ( Int, Int ), List String ) (List String))
stepMultiLineValue ( indent, state ) =
    oneOf
        [ succeed (Done (List.reverse state))
            |. end
        , succeed identity
            |. onlySpace
            |= (getCol |> map (Debug.log "getCol"))
            |> andThen (checkIndentHelp ( indent, state ))
        ]


checkIndentHelp : ( ( Int, Int ), List String ) -> Int -> Parser (Step ( ( Int, Int ), List String ) (List String))
checkIndentHelp ( ( col, row ), state ) column =
    let
        _ =
            Debug.log "state" ( ( col, row ), state )

        _ =
            Debug.log "column" column
    in
    if col <= column then
        -- we need to skip the first iteration as indent = 0 there
        succeed
            (\nextValue ->
                Loop
                    ( if col == 0 then
                        ( column, row )

                      else
                        ( col, row )
                    , nextValue :: state
                    )
            )
            |= oneLineValue
            |. oneOf [ isNewLineOrEnd, end ]

    else
        succeed () |> map (\_ -> Done (List.reverse state))


oneLineValue : Parser String
oneLineValue =
    succeed ()
        |. chompIf (\c -> c /= ' ')
        |. chompUntilEndOr "\n"
        |> getChompedString
        |> andThen (String.trim >> Debug.log "value" >> succeed)
