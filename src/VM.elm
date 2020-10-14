module VM exposing (..)

import ImpParser as IP exposing ( AExp (..), BExp (..), Command (..), Commands, State )
import Set as S
import Dict as D
import Util exposing (..)

evalThenShow : Commands -> State -> TransString
evalThenShow commands state =
    let (commandsRule, state_) = evalCommands commands state in
    showCommTrans ((commands, state), state_, commandsRule)
     

type TransString = Trans String (String, List TransString)

type alias State = D.Dict String Int

showState : State -> String
showState = IP.showState << D.toList
    
type alias CommState = (Commands, State)
showCommState : CommState -> String
showCommState (commands, state) =
    "<" ++ IP.showCommands commands ++ ", {" ++ showState state ++ "}>"

type alias CommTrans = (CommState, State, CommRule)
showCommTrans : CommTrans -> TransString
showCommTrans (commState, state, commRule) =
    Trans
    (showCommState commState ++ " --> {" ++ showState state ++ "}")
    <| showCommRule commRule
    
type alias AExpState = (AExp, State)
type alias AExpTrans = (AExpState, Int, AExpRule)
showAExpState : AExpState -> String
showAExpState (aExp, state) =
    "<" ++ IP.showAExp aExp 10 ++ ", {" ++ showState state ++ "}>"

showAExpTrans : AExpTrans -> TransString
showAExpTrans (aExpState, num, aExpRule) =
    Trans
    (showAExpState aExpState ++ " --> " ++ String.fromInt num)
    <| showAExpRule aExpRule
    
type alias BExpState = (BExp, State)
type alias BExpTrans = (BExpState, Bool, BExpRule)
showBExpState : BExpState -> String
showBExpState (bExp, state) =
    "<" ++ IP.showBExp bExp ++ ", {" ++ showState state ++ "}>"

showBExpTrans : BExpTrans -> TransString
showBExpTrans (bExpState, bool, bExpRule) =
    let boolStr = if bool then "ture" else "false" in
    Trans
    (showBExpState bExpState ++ " --> " ++ boolStr)
    <| showBExpRule bExpRule
        
type CommRule = Upd AExpTrans
              | Skip 
              | Ift BExpTrans CommTrans
              | Iff BExpTrans CommTrans
              | Seq CommTrans CommTrans
              | Wht BExpTrans CommTrans CommTrans
              | Whf BExpTrans

showCommRule : CommRule -> (String, List TransString)
showCommRule commRule =
    case commRule of
        Upd at         -> ("upd", [ showAExpTrans at ])
        Skip           -> ("skip", [])
        Ift bt ct      -> ("ift", [ showBExpTrans bt, showCommTrans ct ])
        Iff bt ct      -> ("iff", [ showBExpTrans bt, showCommTrans ct ])
        Wht bt ct1 ct2 -> ("wht", [ showBExpTrans bt, showCommTrans ct1, showCommTrans ct2 ])
        Whf bt         -> ("wht", [ showBExpTrans bt ])
        Seq ct1 ct2    -> ("seq", [ showCommTrans ct1, showCommTrans ct2 ])
                
type BExpRule = Bool Bool
              | Let AExpTrans AExpTrans 
              | Lef AExpTrans AExpTrans 
              | Nott BExpTrans
              | Notf BExpTrans
              | And1 BExpTrans
              | And2 BExpTrans
              | And3 BExpTrans BExpTrans

showBExpRule : BExpRule -> (String, List TransString)
showBExpRule bExpRule =
    case bExpRule of
        Bool bool     -> if bool then ("true", []) else ("false", [])
        Let at1 at2   -> ("let", [ showAExpTrans at1, showAExpTrans at2 ])
        Lef at1 at2   -> ("lef", [ showAExpTrans at1, showAExpTrans at2 ])
        Nott bt       -> ("nott", [ showBExpTrans bt ])
        Notf bt       -> ("notf", [ showBExpTrans bt ])
        And1 bt       -> ("and1", [ showBExpTrans bt ])
        And2 bt       -> ("and2", [ showBExpTrans bt ])
        And3 bt1 bt2  -> ("and3", [ showBExpTrans bt1, showBExpTrans bt2 ])
        
                         
type AExpRule = Num 
              | Var 
              | Sum AExpTrans AExpTrans
              | Mul AExpTrans AExpTrans

showAExpRule : AExpRule -> (String, List TransString)
showAExpRule aExpRule =
    case aExpRule of
        Num         -> ("num", [])
        Var         -> ("var", [])
        Sum at1 at2 -> ("sum", [ showAExpTrans at1, showAExpTrans at2 ])
        Mul at1 at2 -> ("mul", [ showAExpTrans at1, showAExpTrans at2 ])
                
binop evalExp state exp1 exp2 = 
    (evalExp exp1 state, evalExp exp2 state)
                    
evalAExp : AExp -> State -> (AExpRule, Int)
evalAExp aExp state =
    let binopAExp = binop evalAExp state in
    case aExp of
        IP.Num num -> (Num, num)
        IP.Var var -> case D.get var state of
                          Just num -> (Var, num)
                          Nothing -> (Var, 0)
        IP.Sum aExp1 aExp2 ->
            let ((aExpRule1, result1), (aExpRule2, result2)) = binopAExp aExp1 aExp2 in
            (Sum ((aExp1, state), result1, aExpRule1)
                 ((aExp2, state), result2, aExpRule2)
            , result1 + result2)
        IP.Mul aExp1 aExp2 ->
            let ((aExpRule1, result1), (aExpRule2, result2)) = binopAExp aExp1 aExp2 in
            (Mul ((aExp1, state), result1, aExpRule1)
                 ((aExp1, state), result1, aExpRule1)
            , result1 * result2)
                
evalBExp : BExp -> State -> (BExpRule, Bool)
evalBExp bExp state =
    let  binopAExp = binop evalAExp state
         binopBExp = binop evalBExp state in
    case bExp of
        IP.Bool True -> (Bool True, True)
        IP.Bool False -> (Bool False, False)
        IP.Le aExp1 aExp2 -> 
            let ((aExpRule1, result1), (aExpRule2, result2)) = binopAExp aExp1 aExp2
                tup = 
                    (((aExp1, state), result1, aExpRule1)
                    , ((aExp1, state), result1, aExpRule1)
                    ) in
            if result1 <= result2
            then (uncurry Let tup, True)
            else (uncurry Lef tup, False)
        IP.And bExp1 bExp2 ->
            let ((bExpRule1, result1), (bExpRule2, result2)) = binopBExp bExp1 bExp2
                tup = 
                    (((bExp1, state), result1, bExpRule1)
                    , ((bExp1, state), result1, bExpRule1)
                    ) in
            case (result1, result2) of
                (False, _) -> (And1 ((bExp1, state), result1, bExpRule1), False)
                (_, False) -> (And2 ((bExp2, state), result2, bExpRule2), False)
                (True, True) -> (uncurry And3 tup, True) 
        IP.Not bExp_ ->
            let (bExpRule, result) = evalBExp bExp_ state in
            if not result
            then (Nott ((bExp_, state), True, bExpRule), True)
            else (Notf ((bExp_, state), False, bExpRule), False)
                
evalCommands : Commands -> State -> (CommRule, State)
evalCommands commands state =
    case commands of
        [] -> (Skip, state)  -- never reaches here
        [ h ] -> evalCommand h state
        h::t ->
            let (commRuleH, resultH) = evalCommand h state
                (commRuleT, resultT) = evalCommands t resultH in
            (Seq (([h], state), resultH, commRuleH) ((t, resultH), resultT, commRuleT)
            , resultT)
            
evalCommand : Command -> State -> (CommRule, State)
evalCommand command state =
    case command of
        IP.Skip -> (Skip, state)
        IP.Update var val ->
            let (aExpRule, result) = evalAExp val state in
            (Upd ((val, state), result, aExpRule), D.insert var result state)
        IP.If bExp comm1 comm2 ->
            let (bExpRule, result) = evalBExp bExp state
                bool = ((bExp, state), result, bExpRule) in
            if result
            then let (commRule1, result1) = evalCommands comm1 state in
                 (Ift bool ((comm1, state), result1, commRule1), result1)
            else let (commRule2, result2) = evalCommands comm2 state in
                 (Iff bool ((comm2, state), result2, commRule2), result2)
        IP.While bExp comm ->
            let (bExpRule, result) = evalBExp bExp state
                bool = ((bExp, state), result, bExpRule) in
            if result
            then let (commRuleDo, stateDo) = evalCommands comm state
                     (commRuleWh, stateWh) = evalCommand command stateDo
                 in
                 (Wht bool ((comm, state), stateDo, commRuleDo) (([ command ], stateDo), stateWh, commRuleWh), stateWh)
            else (Whf bool, state)
             
    
