module EvalTest exposing (..)

import Eval exposing (evaluateExpression, exprToRpn)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type alias C =
    { expr : String, rpn : String, val : Float }


caseList : List C
caseList =
    [ C "3+2" "3 2 +" 5
    , C "10*100" "10 100 *" 1000
    ]


suite : Test
suite =
    describe "evaluate formulas"
        [ test "test conversion to RPN" <|
            \() ->
                Expect.equalLists
                    (List.map (\{ expr } -> exprToRpn expr) caseList)
                    (List.map (\{ rpn } -> Ok rpn) caseList)
        , test "calculate value" <|
            \() ->
                Expect.equalLists
                    (List.map (\{ expr } -> evaluateExpression (always Nothing) expr) caseList)
                    (List.map (\{ val } -> Ok val) caseList)
        ]
