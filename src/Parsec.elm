module Parsec exposing (..)

import Parser exposing (..)
import Char
import Util exposing (..)

-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

intLit : Parser Int
intLit =
    lexeme
    <| oneOf
        [ succeed negate
        |. lexeme (symbol "-")
        |= myInt
        , myInt
        ]

myInt : Parser Int
myInt =
    Parser.map (String.foldl (\c d -> d * 10 + Char.toCode c - 48) 0)
    <| getChompedString
    <| succeed ()
            |. chompIf Char.isDigit
            |. chompWhile Char.isDigit

lexSym : String -> Parser ()
lexSym = lexeme << symbol

lexKey : String -> Parser ()
lexKey = lexeme << keyword

trueLit : Parser Bool
trueLit =
    succeed True
        |. lexKey "true"
          
falseLit : Parser Bool
falseLit =
    succeed False
        |. lexKey "false"
        
boolLit : Parser Bool
boolLit =
    oneOf [ trueLit, falseLit ]
    
               
-- Parser
sandwitched : String -> String -> Parser a -> Parser a
sandwitched left right p =
    succeed identity
        |. lexSym left
        |= p
        |. lexSym right

paren : Parser a -> Parser a
paren = sandwitched "(" ")"

sepBy1 : String -> Parser a -> Parser (List a)
sepBy1 sep p =
    Parser.map List.singleton p |> andThen (flip loop <| sepByHelp sep p)

sepBy : String -> Parser a -> Parser (List a)
sepBy sep p =
    oneOf [ sepBy1 sep p, succeed [] ]
        
sepByHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByHelp sep p as_ =
    oneOf
    [ succeed (\a -> Loop (a::as_))
    |. lexSym sep
    |= p
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]

sepByEnd : String -> Parser a -> Parser (List a)
sepByEnd sep p =
    loop [] <| sepByEndHelp sep p

sepByEnd1 : String -> Parser a -> Parser (List a)
sepByEnd1 sep p =
    succeed (::)
         |= p
         |= oneOf [ succeed identity
                        |. lexSym sep
                        |= sepByEnd sep p
                  , succeed []
                  ]
            
        
sepByEndHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByEndHelp sep p as_ =
    oneOf
    [ p |> andThen (\a -> oneOf [ succeed (Loop (a::as_))
                                |. lexSym sep
                                , succeed (Done <| List.reverse <| a::as_)
                                ]
                   )
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]

sepBy1Foldl : (a -> a -> a) -> String -> Parser a -> Parser a
sepBy1Foldl f sep p =
    p |> andThen (flip loop <| sepBy1FoldlHelp f sep p)

sepBy1FoldlHelp : (a -> a -> a) -> String -> Parser a -> a -> Parser (Step a a)
sepBy1FoldlHelp f sep p as_ =
    oneOf
    [ succeed (\a -> Loop (f as_ a))
    |. lexSym sep
    |= p
    , succeed ()
    |> map (\_ -> Done as_)
    ]
            
sepBy1Foldr : (a -> a -> a) -> String -> Parser a -> Parser a
sepBy1Foldr f sep p =
    loop [] (sepBy1FoldrHelp f sep p)

sepBy1FoldrHelp : (a -> a -> a) -> String -> Parser a -> List a -> Parser (Step (List a) a)
sepBy1FoldrHelp f sep p as_ =
    p |> andThen
         (\a -> oneOf [ succeed (Loop <| a::as_)
                      |. lexSym sep
                      , succeed <| Done <| List.foldl (flip f) a as_
                      ]
         )

list : Parser a -> Parser (List a)
list p =
    sandwitched "[" "]" <| sepBy "," p 
      
-- show
problem2String : Problem -> String
problem2String problem = case problem of
                             Expecting str -> "expectiong " ++ str 
                             ExpectingInt -> "expecting int"
                             ExpectingNumber -> "expecting number"
                             ExpectingVariable -> "expecting variable"
                             ExpectingSymbol str -> "expecting symbol " ++ str
                             ExpectingKeyword str -> "expecting keyword " ++ str
                             ExpectingEnd -> "execting end"
                             UnexpectedChar -> "unexpected char"
                             Problem str -> "problem " ++ str
                             BadRepeat -> "badrepeat"
                             _ -> "error!"
