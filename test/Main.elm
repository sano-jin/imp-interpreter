module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Katex as K
    exposing
        ( Latex
        , display
        , human
        , inline
        )

init : () -> ()
init () = ()
        
passage : Latex
passage =
    inline "\\phi"


view : Html a
view =
    let
        htmlGenerator _ stringLatex =
            H.span [] [ H.text stringLatex ]
    in
    passage |> (K.generate htmlGenerator)


main : Program () (() -> ()) (() -> ())
main =
    Browser.sandbox
        { init = init
        , update = \b a -> always a b
        , view = always view
        }    


        
