open Format
open Syntax
open Support.Error
open Support.Pervasive

(** Debugging **)

(** This function enable o disable trace printing*)
let check_trace ctx =
  let index = name2index dummyinfo ctx __TRACE__ in
  match List.nth ctx index with
      _, NameBind -> false
    | _, TmAbbBind(traceVal) -> (match traceVal with
        TmTrue(_) -> true
      | TmFalse(_) -> false
      | _ -> false)

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmFloat _  -> true
  | TmString _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_) -> true
  | TmRec(_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

(* Auxiliary function to indent stack traces *)
let rec indent = function
    0 -> ()
  | n -> print_string("  "); indent (n-1)

(** Auxiliary function for eval*)
let rec eval1 ctx t depth =
  if check_trace ctx
  then (
    indent depth;
    print_string "...";
    print_string (termTypeToString t);
    print_string " ( ";
    printtm ctx t;
    print_string " ) ";
    print_newline ()
  ) else ();
  match t with
    TmIf(_,TmTrue(_),t2,t3) ->
    t2
  | TmIf(_,TmFalse(_),t2,t3) ->
    t3
  | TmIf(fi,t1,t2,t3) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmIf(fi, t1', t2, t3)
  | TmVar(fi,n,_) ->
    (match getbinding fi ctx n with
       TmAbbBind(t) -> t
     | _ -> raise NoRuleApplies)
  | TmApp(fi,TmAbs(_,x,t12),v2) when isval ctx v2 ->
    termSubstTop v2 t12
  | TmApp(fi,TmRec(_,x,t12),v2) when isval ctx v2 ->
    termSubstTop v2 (applyToFPC ctx t12)
  | TmApp(fi,v1,t2) when isval ctx v1 ->
    let t2' = eval1 ctx t2 (depth+1) in
    TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmApp(fi, t1', t2)
  | TmRecord(fi,fields) ->
    let rec evalafield l = match l with
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi ->
        let rest' = evalafield rest in
        (l,vi)::rest'
      | (l,ti)::rest ->
        let ti' = eval1 ctx ti (depth+1) in
        (l, ti')::rest
    in let fields' = evalafield fields in
    TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
    (try List.assoc l fields
     with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmProj(fi, t1', l)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
    TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
    let t2' = eval1 ctx t2 (depth+1) in
    TmTimesfloat(fi,t1,t2')
  | TmTimesfloat(fi,t1,t2) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmTimesfloat(fi,t1',t2)
  | TmSucc(fi,t1) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
    TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
    nv1
  | TmPred(fi,t1) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
    TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
    TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmIsZero(fi, t1')
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
    termSubstTop v1 t2
  | TmLet(fi,x,t1,t2) ->
    let t1' = eval1 ctx t1 (depth+1) in
    TmLet(fi, x, t1', t2)
  | _ ->
    raise NoRuleApplies

(** Evaluation function: Provided with a context and a term, it evaluates that
term or returns it unchanged if there's no evaluation rule*)
let rec eval ctx t =
  try let t' = eval1 ctx t 0
    in eval ctx t'
  with NoRuleApplies -> t

(** Evaluation function for bindings. Evaluates a binding and modifies the given
context to include it. *)
let evalbinding ctx b = match b with
    TmAbbBind(t) -> let t' = eval ctx t in TmAbbBind(t')
  | bind -> bind

(* ---------------------------------------------------------------------- *)

(* Debugging *)
(**Output for the current context*)
let print_context ctx =
  let _ = List.fold_left
    ( fun seen (name, _) ->
      if List.mem name seen
      then seen
      else (
        let t = eval ctx (TmVar (dummyinfo, name2index dummyinfo ctx name, ctxlength ctx)) in
        pr name; pr " = "; printtm ctx t; print_newline ();
        name::seen
      )
    ) [] ctx in
  ()
(** Evaluation function for debugging commands. Evaluates a debugging command and modifies the given
context to include it.*)
let debugging ctx dbg = match dbg with
    DbgContextualize -> print_context ctx; ctx
  | DbgStartTrace -> addbinding ctx __TRACE__ __TRACE_ON__
  | DbgEndTrace -> addbinding ctx __TRACE__ __TRACE_OFF__
