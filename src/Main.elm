module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import ImpParser as IP
import VM 
import Set as S
import Dict as D

-- main                               
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }
         
-- Model
type alias Model =
    { input : String
    , result : String
    , errors : List DeadEnd
    }

init : Model
init =
    { input =
          "<a := 2; if false then while 1 <= 2 & false do skip else skip; skip, {a -> 1, b -> 2}>"
    , result = ""
    , errors = []
    }

-- Update

type Msg
    = Change String
    | Eval String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Eval str ->
            case run IP.parser str of
                        Ok ast ->
                            { model | errors = []
                            , result = IP.showImp ast }

                        Err err -> { model | errors = err
                                   , result = "error" }
        Change str ->
            { model | input = str }

-- View

css path =
    node "link" [rel "stylesheet", href path ] []

view : Model -> Html Msg
view model =
    div [ class "interpreter" ]
        [ node "link"
              [rel "stylesheet"
              , href "https://fonts.googleapis.com/css2?family=Inconsolata:wght@300&display=swap"
              ] []
        , css "style.css"
        , div [ class "console" ]
            [ input [ class "reader"
                    , placeholder "input lambda expression \u{23CE}"
                    , value model.input, onInput Change ] []
            , button [ class "submitter"
                     , onClick <| Eval model.input ] [ text "run" ]
            , div [] [ text model.result ]
            , ul [] <|
                List.map (\err ->
                              li [] [ text <| IP.problem2String err.problem
                                    , div [] [ text <|
                                                   "at row: " ++ String.fromInt err.row
                                                   ++ ", col: " ++ String.fromInt err.col ]
                                    ]
                         ) model.errors
            ]
        ]
    
