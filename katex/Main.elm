module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li, textarea, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import ImpParser as IP
import VM as VM
import Set as S
import Dict as D
import Util exposing (..)
import Katex as K
    exposing
        ( Latex
        , human
        , inline
        , display
        )

viewMath : String -> Html a
viewMath str =
    let
        htmlGenerator _ stringLatex =
            span [] [ text stringLatex ]
    in
        K.generate htmlGenerator <| K.display str

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
          "<Z:=0;(Y:=1; while Z + 1 <= X do (Y:= 2 * Y; Z := Z + 1)), {X -> 2, Y -> 4, Z -> 2}>"
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

showTransString : VM.TransString -> Html Msg
showTransString transString =
    case transString of
        VM.Trans beforeAfter (transName, transList) ->
            span [ class "node" ] [
                 div [ class "children" ] <| List.map showTransString transList
                -- , div [ class "beforeAfter" ] [ text beforeAfter ]
                , div [ class "beforeAfter" ] [ viewMath beforeAfter ]
                , div [ class "trans" ] [ text transName ]
                ]

        
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
            , button [ class "submitter", id "submitter"
                     , onClick <| Eval model.input ] [ text "run" ]
            , div [ id "result" ] [ case model.result of
                           Just transString ->
                               showTransString transString
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
    

        
