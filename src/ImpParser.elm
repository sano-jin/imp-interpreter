module ImpParser exposing (..)

import Parser exposing (..)
import Char
import Set
import Util exposing (..)

type AExp = Num Int
          | Var String
          | Sum AExp AExp
          | Mul AExp AExp
            
type BExp = Bool Bool
          | Le AExp AExp
          | Not BExp
          | And BExp BExp

type alias Commands = List Command
            
type Command = Skip
             | Update String AExp
             | If BExp Commands Commands
             | While BExp Commands

type alias State = List (String, Int)
   
                 
-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

varLit : Parser String
varLit =
    lexeme
    <| variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved =
            Set.fromList ["not", "if", "then", "else", "while", "do", "skip", "true", "false"]
        }
  
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
    Parser.map (String.foldr (\c d -> d * 10 + Char.toCode c - 48) 0)
    <| getChompedString
    <| succeed ()
            |. chompIf Char.isDigit
            |. chompWhile Char.isDigit

trueLit : Parser Bool
trueLit =
    succeed True
        |. lexeme (symbol "true")
          
falseLit : Parser Bool
falseLit =
    succeed False
        |. lexeme (symbol "false")
        
boolLit : Parser BExp
boolLit =
    Parser.map Bool <| oneOf [ trueLit, falseLit ]
    
               
-- Parser
sandwitched : String -> String -> Parser a -> Parser a
sandwitched left right p =
    succeed identity
        |. lexeme (symbol left)
        |= p
        |. lexeme (symbol right)

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
    |. lexeme (symbol sep)
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
                        |. lexeme (symbol sep)
                        |= sepByEnd sep p
                  , succeed []
                  ]
            
        
sepByEndHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByEndHelp sep p as_ =
    oneOf
    [ p |> andThen (\a -> oneOf [ succeed (Loop (a::as_))
                                |. lexeme (symbol sep)
                                , succeed (Done <| List.reverse <| a::as_)
                                ]
                   )
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]


sepBy1Fold : (a -> a -> a) -> String -> Parser a -> Parser a
sepBy1Fold f sep p =
    p |> andThen (flip loop <| sepBy1FoldHelp f sep p)

sepBy1FoldHelp : (a -> a -> a) -> String -> Parser a -> a -> Parser (Step a a)
sepBy1FoldHelp f sep p as_ =
    oneOf
    [ succeed (\a -> Loop (f as_ a))
    |. lexeme (symbol sep)
    |= p
    , succeed ()
    |> map (\_ -> Done as_)
    ]

parseUnaryAExp : Parser AExp
parseUnaryAExp =
    oneOf
    [ Parser.map Var varLit
    , Parser.map Num intLit
    , Parser.lazy (\_ -> paren parseSum) ]


    
parseMul : Parser AExp
parseMul =
   sepBy1Fold Mul "*" parseUnaryAExp

    
parseSum : Parser AExp
parseSum =
   sepBy1Fold Sum "+" parseMul

parseAExp = parseSum    

            
list : Parser a -> Parser (List a)
list p =
    sandwitched "[" "]" <| sepBy "," p 
      
parseUnaryBool : Parser BExp
parseUnaryBool =
    oneOf [ boolLit
          , (succeed Not 
            |. lexeme (symbol "not")
            |= Parser.lazy (\_ -> parseBExp)
            )
          , backtrackable
                (succeed Le
                |= parseAExp 
                |. lexeme (symbol "<=")
                |= parseAExp 
                )
          , paren <| Parser.lazy (\_ -> parseBExp) 
          ]

parseBExp : Parser BExp
parseBExp =
    sepBy1Fold And "&" parseUnaryBool

parseUnaryCommand : Parser Commands
parseUnaryCommand =
    oneOf [ Parser.map List.singleton <|
               oneOf [ succeed Skip |. lexeme (symbol "skip")
                     , (succeed Update
                       |= varLit
                       |. lexeme (symbol ":=")
                       |= parseAExp
                       )
                     , (succeed If
                       |. lexeme (symbol "if")
                       |= parseBExp
                       |. lexeme (symbol "then")
                       |= Parser.lazy (\_ -> parseCommands)
                       |. lexeme (symbol "else")
                       |= Parser.lazy (\_ -> parseUnaryCommand)
                       )
                     , (succeed While
                       |. lexeme (symbol "while")
                       |= parseBExp
                       |. lexeme (symbol "do")
                       |= Parser.lazy (\_ -> parseUnaryCommand)
                       )
                     ]
         , paren (Parser.lazy (\_ -> parseCommands))
         ]

parseCommands : Parser Commands
parseCommands =
    Parser.map List.concat <| sepByEnd1 ";" parseUnaryCommand

parseCorrespondence : Parser (String, Int)      
parseCorrespondence =
    succeed Tuple.pair
        |= varLit
        |. lexeme (symbol "->")
        |= intLit

parseState : Parser State
parseState =
    sandwitched "{" "}"
        <| sepBy "," parseCorrespondence

parseImp : Parser (List Command, State)
parseImp =
    sandwitched "<" ">"
    <| succeed Tuple.pair
        |= parseCommands
        |. lexeme (symbol ",")
        |= parseState

parser : Parser (List Command, State)
parser =
    succeed identity
       |. spaces
       |= parseImp
       |. end 

          
-- show
showCommands : Commands -> String
showCommands commands =
    String.join "; " <| List.map showCommand commands

showCommand : Command -> String
showCommand command =
    case command of
        Skip -> "skip"
        If bExp com1 com2 -> "if " ++ showBExp bExp
                             ++ " then " ++ showCommands com1
                             ++ " else " ++ showCommandsWithParen com2
        While bExp com -> "while " ++ showBExp bExp ++ " do " ++ showCommandsWithParen com
        Update var aExp -> var ++ " := " ++ showAExp aExp 10

showCommandsWithParen : Commands -> String
showCommandsWithParen commands =
    case commands of
        [ command ] -> showCommand command
        _ -> "(" ++ showCommands commands ++ ")"
    
showBExp : BExp -> String
showBExp bExp =
    case bExp of
         Bool bool -> if bool then "true" else "false"
         Le aExp1 aExp2 -> showAExp aExp1 5 ++ " <= " ++ showAExp aExp2 5
         Not b -> "not " ++ showBExp b
         And bExp1 bExp2 -> showBExp bExp1 ++ " & " ++ showBExp bExp2


showAExp : AExp -> Int -> String
showAExp aExp priority =
    case aExp of
        Num int -> String.fromInt int
        Var var -> var
        Sum aExp1 aExp2 ->
            let str = showAExp aExp1 4 ++ " + " ++ showAExp aExp2 4 in
            if priority < 4 then 
                "(" ++ str ++ ")"
            else str
        Mul aExp1 aExp2 ->
            let str = showAExp aExp1 3 ++ " * " ++ showAExp aExp2 3 in
            if priority < 3 then 
                "(" ++ str ++ ")"
            else str

showState : State -> String
showState state =
   String.join ", " <| List.map (\(var, int) -> var ++ " -> " ++ String.fromInt int) state

showImp : (List Command, State) -> String
showImp (commands, state) =
   "<" ++ showCommands commands ++ ", {" ++ showState state ++ "}>"
   
problem2String : Problem -> String
problem2String problem = case problem of
                             Expecting str -> "expectiong " ++ str 
                             ExpectingInt -> "expecting int"
                             ExpectingNumber -> "expecting number"
                             ExpectingVariable -> "expecting variable"
                             ExpectingSymbol str -> "expecting symbol " ++ str
                             ExpectingKeyword str -> "expecting keyword" ++ str
                             ExpectingEnd -> "execting end"
                             UnexpectedChar -> "unexpected char"
                             Problem str -> "problem " ++ str
                             BadRepeat -> "badrepeat"
                             _ -> "error!"

