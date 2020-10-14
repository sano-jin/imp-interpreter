module Main exposing (main)

import Html as H exposing (Html)
import Browser exposing (..)
import Katex as K
    exposing
        ( Latex
        , human
        , inline
        , display
        )

init : ()
init = ()
        
passage : List Latex
passage =
    [ human "We denote by "
    , inline "\\phi"
    , human " the formula for which "
    , display "\\Gamma \\vDash \\phi"
    ]


view : Html a
view =
    let
        htmlGenerator isDisplayMode stringLatex =
            case isDisplayMode of
                Just True ->
                    H.div [] [ H.text stringLatex ]

                _ ->
                    H.span [] [ H.text stringLatex ]
    in
        passage
            |> List.map (K.generate htmlGenerator)
            |> H.div []

main : Program () () (() -> ())
main = Browser.sandbox
       { init = ()
       , view = always view 
       , update = identity
       }
