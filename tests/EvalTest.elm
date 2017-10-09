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


caseListTests : List Test
caseListTests =
    List.concatMap
        (\{ expr, rpn, val } ->
            [ test ("test conversion to RPN: " ++ expr) <|
                \() ->
                    Ok rpn |> Expect.equal (exprToRpn expr)
            , test ("calculate value: " ++ rpn) <|
                \() ->
                    Ok val |> Expect.equal (evaluateExpression (always Nothing) expr)
            ]
        )
        caseList


suite : Test
suite =
    describe "test formulas" <|
        caseListTests
