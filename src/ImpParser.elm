module ImpParser exposing (..)

import Parser exposing (..)
import Parsec exposing (..)
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
        |. lexeme (keyword "true")
          
falseLit : Parser Bool
falseLit =
    succeed False
        |. lexeme (keyword "false")
        
boolLit : Parser BExp
boolLit =
    Parser.map Bool <| oneOf [ trueLit, falseLit ]
                   
-- Parser
parseUnaryAExp : Parser AExp
parseUnaryAExp =
    oneOf
    [ Parser.map Var varLit
    , Parser.map Num intLit
    , Parser.lazy (\_ -> paren parseSum) ]

    
parseMul : Parser AExp
parseMul =
   sepBy1Foldl Mul "*" parseUnaryAExp

    
parseSum : Parser AExp
parseSum =
   sepBy1Foldl Sum "+" parseMul

parseAExp = parseSum    
            
list : Parser a -> Parser (List a)
list p =
    sandwitched "[" "]" <| sepBy "," p 
      
parseUnaryBool : Parser BExp
parseUnaryBool =
    oneOf [ backtrackable boolLit
          , backtrackable
                (succeed Not 
                |. lexeme (keyword "not")
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
    sepBy1Foldr And "&" parseUnaryBool

parseUnaryCommand : Parser Commands
parseUnaryCommand =
    oneOf [ Parser.map List.singleton <|
               oneOf [ backtrackable
                           ( succeed Skip
                           |. lexeme (keyword "skip") )
                     , (succeed Update
                       |= varLit
                       |. lexeme (symbol ":=")
                       |= parseAExp
                       )
                     , (succeed If
                       |. lexeme (keyword "if")
                       |= parseBExp
                       |. lexeme (keyword "then")
                       |= Parser.lazy (\_ -> parseCommands)
                       |. lexeme (keyword "else")
                       |= Parser.lazy (\_ -> parseUnaryCommand)
                       )
                     , (succeed While
                       |. lexeme (keyword "while")
                       |= parseBExp
                       |. lexeme (keyword "do")
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

parseImp : Parser (Commands, State)
parseImp =
    sandwitched "<" ">"
    <| succeed Tuple.pair
        |= parseCommands
        |. lexeme (symbol ",")
        |= parseState

parser : Parser (Commands, State)
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

showImp : (Commands, State) -> String
showImp (commands, state) =
   "<" ++ showCommands commands ++ ", {" ++ showState state ++ "}>"
   
