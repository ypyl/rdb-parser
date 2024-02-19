module Tests exposing (..)

import Dict
import Expect exposing (equal)
import Main exposing (Domain, domain, multiLineValue)
import Parser exposing (Problem(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "all"
        [ describe "domain"
            [ test "only name" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n") (Ok (Domain "Pastebin" [] Dict.empty Dict.empty))
            , test "only name with incorrect : after domain" <|
                \_ -> equal (Parser.run domain "domain   :   \n  name: Pastebin\n") (Err [ { col = 7, problem = ExpectingSymbol ":", row = 1 } ])
            , test "only name with spaces around" <|
                \_ -> equal (Parser.run domain "domain:   \n    name:    Pastebin\n") (Ok (Domain "Pastebin" [] Dict.empty Dict.empty))
            , test "only name with spaces around (second)" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin  \n") (Ok (Domain "Pastebin" [] Dict.empty Dict.empty))
            , test "only name with incorrect indention" <|
                \_ -> equal (Parser.run domain "domain:\nname: Pastebin\n") (Err [ { col = 1, problem = Problem "indention is incorrect", row = 2 } ])
            , test "name and one line description" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n  description: One line") (Ok (Domain "Pastebin" [ "One line" ] Dict.empty Dict.empty))
            , test "name and one line description (second)" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n  description: One line   \n   ") (Ok (Domain "Pastebin" [ "One line" ] Dict.empty Dict.empty))
            , test "name and two line description" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n  description: One line\n               second line") (Ok (Domain "Pastebin" [ "One line", "second line" ] Dict.empty Dict.empty))
            , test "name and two line description (second)" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n  description: One line\n               second line  \n   ") (Ok (Domain "Pastebin" [ "One line", "second line" ] Dict.empty Dict.empty))
            ]
        , describe "Utils"
            [ describe "multiLineValue"
                [ test "one line" <|
                    \_ -> equal (Parser.run multiLineValue "   very important\n") (Ok [ "very important" ])
                , test "two line" <|
                    \_ -> equal (Parser.run multiLineValue "   very important\n    very important2\n") (Ok [ "very important", "very important2" ])
                , test "two line without ending" <|
                    \_ -> equal (Parser.run multiLineValue "   very important\n    very important2") (Ok [ "very important", "very important2" ])
                , test "two line and new value later" <|
                    \_ -> equal (Parser.run multiLineValue "   very important\n    very important2\n new value") (Ok [ "very important", "very important2" ])
                ]
            ]
        ]
