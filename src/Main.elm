module Main exposing (Model, Msg(..), view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)
import List exposing (..)


type alias Model =
    { count : Int, dragPos : Maybe Float }


type Msg
    = Inc
    | Dec
    | DragStarted
    | Follow


update : Msg -> Model -> Model
update msg model =
    case Debug.log "msg " msg of
        Inc ->
            { model | count = model.count + 1 }

        Dec ->
            { model | count = model.count - 1 }

        DragStarted ->
            model

        Follow ->
            model


bindBem : String -> ( List (Html.Html msg) -> Html.Html msg, String -> List (Html.Html msg) -> Html.Html msg )
bindBem className =
    let
        bindBlock =
            div [ class className ]

        bindElement =
            \e -> div [ class (className ++ "__" ++ e) ]
    in
    ( bindBlock, bindElement )


mainWrapper : List (Html Msg) -> Html Msg
mainWrapper =
    div
        [ style "height" "100%"
        , style "width" "100%"
        , style "display" "flex"
        , class "wrapper"
        , onMouseDown DragStarted
        ]


view : Model -> Html Msg
view model =
    let
        ( block, element ) =
            bindBem "Counter"
    in
    block
        [ element "Buttons"
            [ button [ onClick Inc ] [ text "+" ]
            , button [ onClick Dec ] [ text "-" ]
            ]
        , element "Output" [ text (String.fromInt model.count) ]
        ]


init : Model
init =
    { count = 0
    , dragPos = Nothing
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
