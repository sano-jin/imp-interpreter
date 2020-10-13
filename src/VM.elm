module VM exposing (..)

import ImpParser as IP exposing ( AExp (..), BExp (..), Command (..), Commands, State )
import Set as S
import Dict as D
import Util exposing (..)

type Tree state trans = Node trans (List (state, (Tree state trans)))
type alias State = D.Dict String Int

type alias CommState = (Commands, State)
type alias CommTrans = (CommState, State, CommRule)
    
type alias AExpState = (AExp, State)
type alias AExpTrans = (AExpState, Int, AExpRule)
    
type alias BExpState = (BExp, State)
type alias BExpTrans = (BExpState, Bool, BExpRule)
    
type CommRule = Upd AExpTrans
              | Skip 
              | Ift BExpTrans CommTrans
              | Iff BExpTrans CommTrans
              | Seq CommTrans
              | Wht BExpTrans CommTrans
              | Whf BExpTrans CommTrans
                
type BExpRule = True
              | False
              | Let AExpTrans AExpTrans 
              | Lef AExpTrans AExpTrans 
              | Nott BExpTrans
              | Notf BExpTrans
              | And1 BExpTrans
              | And2 BExpTrans
              | And3 BExpTrans BExpTrans

type AExpRule = Num 
              | Var 
              | Sum AExpTrans AExpTrans
              | Mul AExpTrans AExpTrans

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
                 ((aExp1, state), result1, aExpRule1)
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
        IP.Bool Basics.True -> (True, Basics.True)
        IP.Bool Basics.False -> (False, Basics.False)
        IP.Le aExp1 aExp2 -> 
                let ((aExpRule1, result1), (aExpRule2, result2)) = binopAExp aExp1 aExp2
                    tup = 
                        (((aExp1, state), result1, aExpRule1)
                        , ((aExp1, state), result1, aExpRule1)
                        ) in
                if result1 <= result2
                then (uncurry Let tup, Basics.True)
                else (uncurry Lef tup, Basics.False)
        _ -> (True, Basics.True)
             
             
evalCommand : Command -> State -> (CommRule, State)
evalCommand command state =
    case command of
        IP.Skip -> (Skip, state)
        Update var val ->
            let (aExpRule, result) = evalAExp val state in
            (Upd ((val, state), result, aExpRule), D.insert var result state)
        _ -> (Skip, state)
             
    
