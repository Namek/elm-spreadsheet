module Main exposing (..)

import Char
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput)
import Html.Lazy exposing (lazy)
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { grid : GridState
    }


type alias GridState =
    { values : GridValues
    , focusedCell : Maybe CellRef
    , settings :
        { rowCount : Int
        , colCount : Int
        }
    }


{-| 0-based indices
-}
type alias CellRef =
    { row : Int, col : Int }


type alias GridValues =
    Dict ViewCellId CellState


type alias CellState =
    { formula : String
    , cachedValue : String
    }



-- MODEL utils


init : ( Model, Cmd msg )
init =
    { grid =
        { values = Dict.empty
        , focusedCell = Nothing
        , settings =
            { rowCount = 9
            , colCount = 6
            }
        }
    }
        ! []


cellId : Int -> Int -> String
cellId rowIdx colIdx =
    colLetter rowIdx ++ (colIdx |> toString)


colLetter : Int -> String
colLetter index =
    Char.toCode 'A' + index |> Char.fromCode |> String.fromChar


getCellState : GridValues -> CellRef -> Maybe CellState
getCellState values cell =
    let
        id =
            cellId cell.row cell.col
    in
    Dict.get id values



-- VIEW MODEL


type alias ViewCellId =
    String


cellInputId : CellRef -> ViewCellId
cellInputId ref =
    "cell-" ++ cellId ref.row ref.col



-- UPDATE


type Msg
    = Omg
    | FocusCell CellRef
    | LostCell CellRef
    | SubmitCell CellRef String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { grid } =
            model
    in
    case msg of
        Omg ->
            ( model, Cmd.none )

        FocusCell cell ->
            { model | grid = { grid | focusedCell = Just cell } } ! []

        LostCell cell ->
            { model | grid = { grid | focusedCell = Nothing } } ! []

        SubmitCell cell text ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    let
        { settings } =
            model.grid

        colNameEl =
            colLetter >> text >> flip (::) [] >> th []
    in
    div []
        [ table []
            [ thead [] (td [] [ text "@" ] :: generateItems 0 settings.colCount colNameEl)
            , tbody [] (generateItems 0 settings.rowCount (viewRow model.grid))
            ]
        ]


{-| test
-}
viewRowWithCellNames : GridState -> Int -> Html Msg
viewRowWithCellNames grid num =
    let
        colLetter_ =
            colLetter num

        cell =
            toString
                >> (++) colLetter_
                >> text
                >> flip (::) []
                >> td []
    in
    tr [] <|
        th [] [ (num + 1) |> toString |> text ]
            :: generateItems 1 (grid.settings.colCount + 1) cell


viewRow : GridState -> Int -> Html Msg
viewRow grid rowIdx =
    let
        cell_old =
            toString
                >> (++) (colLetter rowIdx)
                >> text
                >> flip (::) []
                >> td []

        cell colIdx =
            lazy (viewCell grid) (CellRef rowIdx colIdx)

        cells =
            generateItems 1 (grid.settings.colCount + 1) cell
    in
    tr [] <|
        td [] [ (rowIdx + 1) |> toString |> text ]
            :: cells


viewCell : GridState -> CellRef -> Html Msg
viewCell grid ref =
    let
        id_ =
            cellId ref.row ref.col

        isFocused =
            case grid.focusedCell of
                Just focusedCell ->
                    ref == focusedCell

                Nothing ->
                    False

        -- TODO
        theText =
            id_

        attrs =
            [ id (cellInputId ref)
            , classList [ "wowz" => isFocused ]
            , style
                [ "background-color" => (isFocused |> either "#ccf" "")
                ]
            , onFocus (FocusCell ref)
            , onBlur (LostCell ref)
            , onInput (SubmitCell ref)
            ]

        attrsWhenFocused =
            not isFocused |> either (value theText :: attrs) attrs
    in
    td [] [ input attrsWhenFocused [] ]



-- VIEW utils


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


px : Int -> String
px number =
    toString number ++ "px"


either : a -> a -> Bool -> a
either valIf valElse cond =
    if cond then
        valIf
    else
        valElse



-- other utils


{-| Generate items in range `[start, until)`.
-}
generateItems : Int -> Int -> (Int -> a) -> List a
generateItems start until numToItem =
    let
        genNext num acc =
            if num == until then
                List.reverse acc
            else
                let
                    newItem =
                        numToItem num
                in
                genNext (num + 1) (newItem :: acc)
    in
    genNext start []
