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
    20


cellSize : CellSize
cellSize =
    15


type Cell
    = Alive
    | Dead

toggleCell : Cell -> Cell
toggleCell cell =
    case cell of
        Alive -> Dead             
    
        Dead -> Alive
            


type alias Grid =
    Array Cell


stepCell : Int -> Cell -> Cell
stepCell aliveNeighbours cell =
    case aliveNeighbours of
        3 ->
            if cell == Dead then
                Alive

            else
                cell

        2 ->
            cell

        _ ->
            Dead


getNeighbours : Grid -> ( Int, Int ) -> List Cell
getNeighbours grid position =
    let
        dist : ( Int, Cell ) -> ( Int, Cell )
        dist =
            Tuple.mapFirst <| distance position << xyFromIdx

        isNeighbour : ( Int, Cell ) -> Bool
        isNeighbour =
            Tuple.first >> (==) 1
    in
    grid
        |> Array.toIndexedList
        |> List.map dist
        |> List.filter isNeighbour
        |> List.map Tuple.second


countAliveNeighbours : Grid -> ( Int, Int ) -> Int
countAliveNeighbours grid position =
    let
        neighbours =
            getNeighbours grid position
    in
    neighbours
        |> List.filter ((==) Alive)
        |> List.length



-- ((x, y), Cell)
-- stepGrid : Grid -> Grid


stepGrid grid =
    let
        countAlive : Int -> Int
        countAlive pos =
            xyFromIdx pos |> countAliveNeighbours grid  
    in
    grid
        |> Array.indexedMap (\idx -> \cell -> stepCell (countAlive idx) cell)



-- |> Array.map (\( alive, cell ) -> stepCell cell alive)
-- List.length (List.filter (== Alive) (getNeighbours _ _))


takeRows : Int -> List Cell -> List (List Cell)
takeRows n cells =
    case cells of
        _ :: _ ->
            List.take n cells :: (takeRows n <| List.drop n cells)

        [] ->
            []


takeRowsFromGrid : Int -> Grid -> List (List Cell)
takeRowsFromGrid n =
    takeRows n << Array.toList


idxFromXY : Int -> Int -> Int
idxFromXY x y =
    gridSize * y + x


xyFromIdx : Int -> ( Int, Int )
xyFromIdx idx =
    ( modBy gridSize idx, idx // gridSize )


getXY : Grid -> (Int, Int) -> Cell
getXY grid (x, y) =
    grid
        |> Array.get (idxFromXY x y)
        |> Maybe.withDefault Dead


setXY : Grid -> Cell -> (Int, Int) -> Grid
setXY grid val (x, y) =
    Array.set (idxFromXY x y) val grid


distance : ( Int, Int ) -> ( Int, Int ) -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    max (abs <| x2 - x1) (abs <| y2 - y1)



type alias Model =
    { grid : Grid }


type alias GridSize =
    Int


type alias CellSize =
    Int


type Msg
    = NoOp
    | Seed (List Cell)
    | GenerateRandomSeed
    | Tick Time.Posix
    | Step
    | Toggle (Int, Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msgs" msg of
        NoOp ->
            ( model, Cmd.none )

        Seed d ->
            ( { model | grid = Array.fromList d }, Cmd.none )

        Tick _ ->
            ( model, Cmd.none )

        GenerateRandomSeed ->
            ( model, randomInitialSeedMsg )

        Step ->
            ( { model | grid = stepGrid model.grid }, Cmd.none )

        Toggle position ->
            ( { model | grid = setXY model.grid (toggleCell <| getXY model.grid position) position }, Cmd.none)

        



{--
-- Views
--}


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ viewGrid model.grid
        , viewControls
        ]


colorForCell : Cell -> String
colorForCell cell =
    case cell of
        Alive ->
            "black"

        Dead ->
            "white"


viewCell : Int -> Cell -> Html Msg
viewCell idx cell =
    div
        [ class "cell"
        , style "height" (String.fromInt cellSize ++ "px")
        , style "width" (String.fromInt cellSize ++ "px")
        , style "background" (colorForCell cell)
        , style "margin" "1px"
        , (Ev.onClick <| Toggle <| xyFromIdx idx)
        ]
        [ text "" ]


viewRow : Int -> List Cell -> Html Msg
viewRow rowIdx row =
    div
        [ style "display" "flex"
        , style "flex-direction" "row" 
        ]
        (List.indexedMap
            (\idx -> \cell -> viewCell (idxFromXY idx rowIdx) cell)
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
        (grid
            |> takeRowsFromGrid gridSize
            |> List.indexedMap viewRow
        )


viewControls : Html Msg
viewControls =
    div
        [ class "controls" ]
        [ button [ Ev.onClick GenerateRandomSeed ] [ text "Generate 🌍" ]
        , button [ Ev.onClick Step ] [ text "Step 🔜" ]
        ]



{-
   -- maintenance
-}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 <| \_ -> Step


randomInitialSeedMsg : Cmd Msg
randomInitialSeedMsg =
    Random.generate Seed (generateGrid (gridSize * gridSize))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Array.fromList []
      }
    , randomInitialSeedMsg
    )


generateCell : Generator Cell
generateCell =
    Random.uniform Dead [ Dead, Alive ]


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
