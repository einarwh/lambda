module Eval

open Types

type EvalResult = Reduced of Exp | Normal

let rec subst = function
    | t, s, App (f, a) -> App (subst (t, s, f), subst (t, s, a))
    | t, s, Lam (p, b) ->
        if p = s
        then Lam (p, b)
        else Lam (p, subst (t, s, b))
    | t, s, Var v ->
        if v = s
        then t
        else Var v

let rec reduce = function
    | Var v -> Normal
    | App (Lam (p, b), a) -> Reduced (subst (a, p, b))
    | App (f, a) ->
        match reduce f with
            | Reduced rf -> Reduced (App (rf, a))
            | _ ->
                match reduce a with
                    | Reduced ra -> Reduced (App (f, ra))
                    | _ -> Normal
    | Lam (p, b) ->
        match reduce b with
            | Reduced b -> Reduced (Lam (p, b))
            | _ -> Normal

