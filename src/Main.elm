port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li, textarea, span)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onInput )
import Parser exposing ( run, DeadEnd, Problem (..) )
import Parsec exposing ( problem2String )
import ImpParser as IP
import VM as VM
import Set as S
import Dict as D
import Util exposing (..)
import Json.Encode as Encode exposing (..)

-- main                               
main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
    
-- Model
type alias Model =
    { input : String
    , result : Maybe VM.TransString
    , errors : List DeadEnd
    }

init : () -> ( Model, Cmd Msg )
init = \_ ->
       ( { input =
               "<Z:=0; (Y:=1; while Z+1 <= X do (Y:=2*Y; Z:=Z+1)), {X->2, Y->0, Z->0}>"
         , result = Nothing
         , errors = []
         }
       , Cmd.none )

port sendData : Value -> Cmd msg

-- Update
type Msg
    = Change String
    | Eval String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Eval str ->
            case run IP.parser str of
                        Ok ast ->
                            let proofTree = uncurry VM.evalThenShow <| Tuple.mapSecond D.fromList ast in
                            ( { model | errors = []
                              , result = Just proofTree
                              }, sendData <| jsonOfTransString proofTree  )

                        Err err -> ( { model | errors = err
                                    , result = Nothing }, sendData <| Encode.string "error"
                                   )
        Change str ->
            ( { model | input = str }, Cmd.none )

-- View
css path =
    node "link" [rel "stylesheet", href path ] []

{-|
showTransString : VM.TransString -> Html Msg
showTransString transString =
    case transString of
        VM.Trans beforeAfter (transName, transList) ->
            span [ class "node" ] [
                 div [ class "children" ]
                     <| List.intersperse (span [ class "padding" ] [])
                     <| List.map showTransString transList
                , div [ class "beforeAfter" ] [ text beforeAfter ]
                , div [ class "trans" ] [ text transName ]

                ]
        VM.Terminal expStr ->
            span [ class "terminal" ] [ text expStr ]
|-}             

jsonOfTransString : VM.TransString -> Encode.Value
jsonOfTransString transString =
    case transString of
        VM.Trans beforeAfter (transName, transList) ->
            Encode.object
                [ ( "antecedents", Encode.list jsonOfTransString transList )
                , ( "rule", Encode.string transName )
                , ( "consequent", Encode.string beforeAfter )
                ]
        VM.Terminal hypothesis ->
            Encode.object
                [ ( "consequent", Encode.string hypothesis ) ]
                
view : Model -> Html Msg
view model =
    div [ class "interpreter" ]
        [ 
{--
          node "link"
              [ rel "stylesheet"
              , href "https://fonts.googleapis.com/css2?family=Inconsolata:wght@300&display=swap"
              ] []
        , css "style.css"
--}
        div [ class "console" ]
            [ input [ class "reader"
                    , placeholder "<Commands, State>"
                    , value model.input, onInput Change ] []
            , button [ class "submitter"
                     , onClick <| Eval model.input ] [ text "run" ]
            , ul [] <|
                List.map (\err ->
                              li [] [ text <| problem2String err.problem
                                    , div [] [ text <| ", col: " ++ String.fromInt err.col ]
                                    ]
                         ) model.errors
            ]

        {--
        , div [ class "derivationTree" ]
              [ case model.result of
                    Just transString ->
                        showTransString transString
                    Nothing -> text ""
              ]
--}
        ]


        
