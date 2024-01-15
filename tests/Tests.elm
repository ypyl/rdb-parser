module Tests exposing (..)

import Dict
import Expect exposing (equal)
import Main exposing (Domain,  domain)
import Parser exposing (Problem(..))
import Test exposing (Test, describe, test)
import Main exposing (multiLineValue)


suite : Test
suite =
    describe "Domain"
        [ describe "Initial"
            [ test "domain-1" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin\n") (Ok (Domain "Pastebin" Nothing Dict.empty Dict.empty))
            , test "domain-2" <|
                \_ -> equal (Parser.run domain "domain   :   \n  name: Pastebin\n") (Err [ { col = 7, problem = ExpectingSymbol ":", row = 1 } ])
            , test "domain-3" <|
                \_ -> equal (Parser.run domain "domain:   \n    name:    Pastebin\n") (Ok (Domain "Pastebin" Nothing Dict.empty Dict.empty))
            , test "domain-4" <|
                \_ -> equal (Parser.run domain "domain:\n  name: Pastebin  \n") (Ok (Domain "Pastebin" Nothing Dict.empty Dict.empty))
            , test "domain-5" <|
                \_ -> equal (Parser.run domain "domain:\nname: Pastebin\n") (Err [ { col = 1, problem = Problem "indention is incorrect", row = 2 } ])
            ]
        , describe "multiLineValue"
            [ test "one line" <|
                \_ -> equal (Parser.run multiLineValue "   very important\n") (Ok (["very important"]))
            , test "two line" <|
                \_ -> equal (Parser.run multiLineValue "   very important\n    very important2\n") (Ok (["very important", "very important2"]))
            , test "two line without ending" <|
                \_ -> equal (Parser.run multiLineValue "   very important\n    very important2") (Ok (["very important", "very important2"]))
            ]
        ]
