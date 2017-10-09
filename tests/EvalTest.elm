module EvalTest exposing (..)

import Dict exposing (Dict)
import Eval exposing (ValueGetter, evaluateExpression, exprToRpn)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type alias T =
    { expr : String, rpn : String, val : Result String Float, variables : ValueGetter }


none =
    always Nothing


getter : List ( String, Float ) -> String -> Maybe Float
getter variables varName =
    let
        varMap =
            Dict.fromList variables
    in
    Dict.get varName varMap


caseList : List T
caseList =
    [ T "3+2" "3 2 +" (Ok 5) none
    , T "10*100" "10 100 *" (Ok 1000) none
    , T "(1+(2*3))" "1 2 3 * +" (Ok 7) none
    , T "(4+5*4)/4" "4 5 4 * + 4 /" (Ok 6) none
    , T "2 ^ 10" "2 10 ^" (Ok 1024) none
    , T "((a+b)*(z+x))" "a b + z x + *" (Ok 30) (getter [ ( "a", 3 ), ( "b", 7 ), ( "z", 1 ), ( "x", 2 ) ])
    , T "((a+b)*(c+d))" "a b + c d + *" (Err "variable a is not defined") none
    , T "((a+b)*(c+e))" "a b + c e + *" (Err "variable b is not defined") (getter [ ( "a", 3 ) ])
    ]


caseListTests : List Test
caseListTests =
    List.concatMap
        (\{ expr, rpn, val, variables } ->
            [ test ("test conversion to RPN: " ++ expr) <|
                \() ->
                    Ok rpn |> Expect.equal (exprToRpn expr)
            , test ("calculate value: " ++ rpn) <|
                \() ->
                    val |> Expect.equal (evaluateExpression variables expr)
            ]
        )
        caseList


suite : Test
suite =
    describe "test formulas" <|
        caseListTests
