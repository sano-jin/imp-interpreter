module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import ImpParser as IP
import VM as VM
import Set as S
import Dict as D
import Util exposing (..)

-- main                               
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }
         
-- Model
type alias Model =
    { input : String
    , result : Maybe VM.TransString
    , errors : List DeadEnd
    }

init : Model
init =
    { input =
          "<X := Y + 1; if X <= Z then Z := 2 * Y else Z:= 3 * Y, {X -> 0, Y -> 6, Z -> 0}>"
    , result = Nothing
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
                            , result = Just <| uncurry VM.evalThenShow <| Tuple.mapSecond D.fromList ast
                            }

                        Err err -> { model | errors = err
                                   , result = Nothing }
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
                    , placeholder "<Commands, State>"
                    , value model.input, onInput Change ] []
            , button [ class "submitter"
                     , onClick <| Eval model.input ] [ text "run" ]
            , div [] [ case model.result of
                           Just (VM.Trans beforeAfter (transName, _)) ->
                               text <| beforeAfter ++ " -- " ++ transName
                           Nothing -> text ""
                     ]
            , ul [] <|
                List.map (\err ->
                              li [] [ text <| IP.problem2String err.problem
                                    , div [] [ text <| ", col: " ++ String.fromInt err.col ]
                                    ]
                         ) model.errors
            ]
        ]
    

        
