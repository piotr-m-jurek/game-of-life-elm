module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events as Ev
import Random exposing (Generator)
import Time


gridSize : GridSize
gridSize =
    30


cellSize : CellSize
cellSize =
    15


type Cell
    = Alive
    | Dead


type alias Grid =
    Array Cell


takeRow : Grid -> Int -> List Cell
takeRow grid n =
    let
        cells =
            Array.toList grid

        skipCells =
            n * gridSize
    in
    cells |> List.drop skipCells |> List.take gridSize


idxFromXY : Int -> Int -> Int
idxFromXY x y =
    gridSize * y + x


getXY : Grid -> Int -> Int -> Maybe Cell
getXY grid x y =
    Array.get (idxFromXY x y) grid


setXY : Grid -> Int -> Int -> Cell -> Grid
setXY grid x y val =
    Array.set (idxFromXY x y) val grid


type alias Model =
    { grid : Grid }


type alias GridSize =
    Int


type alias CellSize =
    Int


type Msg
    = NoOp
    | InitialSeed (List Cell)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitialSeed d ->
            ( { model | grid = Array.fromList d }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ viewGrid model.grid
        , hr [] []
        , div [] [ model |> Debug.toString |> text ]
        ]


colorForCell : Cell -> String
colorForCell cell =
    case cell of
        Alive ->
            "red"

        Dead ->
            "black"


viewCell : Cell -> Html Msg
viewCell cell =
    div
        [ style "height" (String.fromInt cellSize ++ "px")
        , style "width" (String.fromInt cellSize ++ "px")
        , style "background" (colorForCell cell)
        , style "margin" "1px"
        ]
        [ text "" ]


viewRow : List Cell -> Html Msg
viewRow row =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        (List.map
            viewCell
            row
        )


viewGrid : Grid -> Html Msg
viewGrid grid =
    div
        [ class "grid"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        (List.range 0 (gridSize - 1)
            |> List.map (takeRow grid)
            |> List.map viewRow
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Array.fromList []
      }
    , Random.generate InitialSeed (generateGrid (gridSize * gridSize))
    )


generateCell : Generator Cell
generateCell =
    Random.uniform Dead [ Alive ]


generateGrid : Int -> Generator (List Cell)
generateGrid s =
    Random.list s generateCell


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
