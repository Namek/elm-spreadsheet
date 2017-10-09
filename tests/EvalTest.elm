module EvalTest exposing (..)

import Eval exposing (evaluateExpression, exprToRpn)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type alias T =
    { expr : String, rpn : String, val : Float }


caseList : List T
caseList =
    [ T "3+2" "3 2 +" 5
    , T "10*100" "10 100 *" 1000
    , T "(1+(2*3))" "1 2 3 * +" 7
    , T "(4+5*4)/4" "4 5 4 * + 4 /" 6
    , T "2 ^ 10" "2 10 ^" 1024
    ]


suite : Test
suite =
    describe "evaluate formulas"
        [ test "test conversion to RPN" <|
            \() ->
                Expect.equalLists
                    (List.map (\{ rpn } -> Ok rpn) caseList)
                    (List.map (\{ expr } -> exprToRpn expr) caseList)
        , test "calculate value" <|
            \() ->
                Expect.equalLists
                    (List.map (\{ val } -> Ok val) caseList)
                    (List.map (\{ expr } -> evaluateExpression (always Nothing) expr) caseList)
        ]
