module Eval
    exposing
        ( ValueGetter
        , evaluateExpression
        , exprToRpn
        )

import Stack exposing (Stack)


type RpnOp
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv


type Token
    = TOperator RpnOp
    | TNumber Float
    | TVariable String
    | TParenthesisOpen
    | TParenthesisClose


type EvalOperation
    = JustValue Float
    | GetValue String
    | DoMath RpnOp


type EvalItem
    = TmpNumber Float
    | TmpOperator RpnOp


type alias ValueGetter =
    String -> Maybe Float


evaluateExpression : ValueGetter -> String -> Result String Float
evaluateExpression getter expr =
    exprToOperations expr
        |> Result.andThen (evalOpsToValue getter)


exprToRpn : String -> Result String String
exprToRpn expr =
    exprToOperations expr
        |> Result.map (List.map evalOpToStr >> String.join " ")


evalOpToStr : EvalOperation -> String
evalOpToStr op =
    case op of
        JustValue float ->
            toString float

        GetValue varName ->
            varName

        DoMath rpnOp ->
            case rpnOp of
                OpAdd ->
                    "+"

                OpSub ->
                    "-"

                OpMul ->
                    "*"

                OpDiv ->
                    "/"


tokenizeExpr : String -> Result String (List Token)
tokenizeExpr expr =
    let
        numOrVar text =
            case String.toFloat text of
                Ok num ->
                    TNumber num

                Err _ ->
                    TVariable text

        finalizeCurString cur acc_tokens =
            if String.length cur > 0 then
                numOrVar cur :: acc_tokens
            else
                acc_tokens

        process char ( cur, acc_tokens ) =
            if char == ' ' then
                ( "", finalizeCurString cur acc_tokens )
            else if char == '+' then
                ( "", TOperator OpAdd :: finalizeCurString cur acc_tokens )
            else if char == '-' then
                ( "", TOperator OpSub :: finalizeCurString cur acc_tokens )
            else if char == '*' then
                ( "", TOperator OpMul :: finalizeCurString cur acc_tokens )
            else if char == '/' then
                ( "", TOperator OpDiv :: finalizeCurString cur acc_tokens )
            else if char == '(' then
                ( "", TParenthesisOpen :: finalizeCurString cur acc_tokens )
            else if char == ')' then
                ( "", TParenthesisClose :: finalizeCurString cur acc_tokens )
            else
                ( String.append cur (String.fromChar char), acc_tokens )

        tokens =
            String.foldl process ( "", [] ) expr
                |> uncurry finalizeCurString
                |> List.reverse
    in
    Ok tokens


{-| Convert string to list of Reversed Polish Notation operation list.
-}
exprToOperations : String -> Result String (List EvalOperation)
exprToOperations expr =
    tokenizeExpr expr
        |> Result.andThen
            (\tokens ->
                let
                    process : Token -> TransformState -> TransformState
                    process token ( stack, ops ) =
                        case token of
                            TNumber val ->
                                ( stack, JustValue val :: ops )

                            TVariable varName ->
                                ( stack, GetValue varName :: ops )

                            TParenthesisOpen ->
                                ( Stack.push token stack, ops )

                            TParenthesisClose ->
                                -- send all ops from stack until parenthesis open
                                let
                                    loop stack ops =
                                        case Stack.pop stack of
                                            ( Just TParenthesisOpen, newStack ) ->
                                                loop newStack (repackTokenToEvalOperation token :: ops)

                                            ( _, newStack ) ->
                                                ( newStack, ops )
                                in
                                loop stack ops

                            TOperator op ->
                                -- move all operators from the top of the stack to output
                                -- TODO operator priorities
                                let
                                    loop stack ops =
                                        case Stack.pop stack of
                                            ( Just (TOperator token_), newStack ) ->
                                                loop newStack (DoMath token_ :: ops)

                                            _ ->
                                                ( stack, ops )

                                    ( stack1, ops1 ) =
                                        loop stack ops
                                in
                                ( Stack.push (TOperator op) stack1, ops1 )

                    ( stack, ops ) =
                        List.foldl process ( Stack.initialise, [] ) tokens
                in
                Stack.toList stack
                    |> List.reverse
                    |> List.map (unpackTokenOp >> DoMath)
                    |> List.append (List.reverse ops)
                    |> Ok
            )


type alias TransformState =
    ( Stack Token, List EvalOperation )


unpackTokenOp : Token -> RpnOp
unpackTokenOp token =
    case token of
        TOperator rpnOp ->
            rpnOp

        _ ->
            Debug.crash "only operators are allowed to be here"


repackTokenToEvalOperation : Token -> EvalOperation
repackTokenToEvalOperation token =
    case token of
        TOperator rpnOp ->
            DoMath rpnOp

        TNumber val ->
            JustValue val

        TVariable varName ->
            GetValue varName

        _ ->
            Debug.crash "sorry, nope, doesn't make sense"


{-| Evaluate RPN.

Algorithm:

  - if number -> put on Stack
  - if operator -> get last two values from stack and evaluate -> put result on stack
  - if nothing else is left then result is on top of stack

-}
evalOpsToValue : ValueGetter -> List EvalOperation -> Result String Float
evalOpsToValue getter ops =
    let
        process : Stack EvalItem -> List EvalOperation -> Result String Float
        process stack ops =
            case ops of
                (JustValue val) :: restOps ->
                    process (Stack.push (TmpNumber val) stack) restOps

                (GetValue varName) :: restOps ->
                    case getter varName of
                        Just val ->
                            process (Stack.push (TmpNumber val) stack) restOps

                        _ ->
                            Err ("variable " ++ varName ++ " is not defined")

                (DoMath op) :: restOps ->
                    let
                        ( n1, stack1 ) =
                            Stack.pop stack

                        ( n2, stack2 ) =
                            Stack.pop stack1
                    in
                    case ( n1, n2 ) of
                        ( Just (TmpNumber n1), Just (TmpNumber n2) ) ->
                            let
                                val =
                                    doTheActualMath op n1 n2

                                stack3 =
                                    Stack.push (val |> TmpNumber) stack2
                            in
                            process stack3 restOps

                        _ ->
                            Err "expected two numbers after meeting the operator!"

                _ ->
                    case Stack.pop stack of
                        ( Just (TmpNumber val), _ ) ->
                            Ok val

                        _ ->
                            Err "expected final value on the top of the stack"
    in
    process Stack.initialise ops


doTheActualMath : RpnOp -> Float -> Float -> Float
doTheActualMath rpnOp n1 n2 =
    let
        op =
            case rpnOp of
                OpAdd ->
                    (+)

                OpSub ->
                    (-)

                OpMul ->
                    (*)

                OpDiv ->
                    (/)
    in
    op n1 n2


opPriority : RpnOp -> Int
opPriority rpnOp =
    case rpnOp of
        OpAdd ->
            1

        OpSub ->
            1

        OpMul ->
            2

        OpDiv ->
            2


hasLowerPriorityThan : RpnOp -> RpnOp -> Bool
hasLowerPriorityThan op1 op2 =
    opPriority op1 < opPriority op2
