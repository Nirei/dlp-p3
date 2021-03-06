(** Syntax trees and associated support functions *)

open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

(* Data type definitions *)
(** All the supported terms in the language, including an info component
    describing its appearance on the source file if possible. Some terms are
    compound, including other terms. *)
type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmString of info * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmLet of info * string * term * term
  | TmRec of info * string * term

(** Auxiliary function to recover the name of the term of a given type *)
let termTypeToString = function
    TmTrue (_) -> "TmTrue"
  | TmFalse (_) -> "TmFalse"
  | TmIf (_,_,_,_) -> "TmIf"
  | TmVar (_,_,_) -> "TmVar"
  | TmAbs (_,_,_) -> "TmAbs"
  | TmApp (_,_,_) -> "TmApp"
  | TmRecord (_,_) -> "TmRecord"
  | TmProj (_,_,_) -> "TmProj"
  | TmFloat (_,_) -> "TmFloat"
  | TmTimesfloat (_,_,_) -> "TmTimesfloat"
  | TmString (_,_) -> "TmString"
  | TmZero (_) -> "TmZero"
  | TmSucc (_,_) -> "TmSucc"
  | TmPred (_,_) -> "TmPred"
  | TmIsZero (_,_) -> "TmIsZero"
  | TmLet (_,_,_,_) -> "TmLet"
  | TmRec (_,_,_) -> "TmRec"

(** A binding is either a name (let ... in ) or an abstraction (... = ...) over
    a term *)
type binding =
    NameBind
  | TmAbbBind of term

(* Contexts *)
(** A context is a list of tuples string * binding type, where, if the binding
    is an abstract binding, it includes a term. *)
type context = (string * binding) list

(** Debug commands *)
type dbg =
  | DbgContextualize
  | DbgStartTrace
  | DbgEndTrace

(** A command is an order in the language, it can be either a binding or a term
    for evaluation *)
type command =
  | Eval of info * term
  | Bind of info * string * binding
  | Debug of info * dbg

(* ---------------------------------------------------------------------- *)
(* Context management *)

(* Name of the trace flag *)
let __TRACE__ = "__TRACE__"
(** Binding to activate the tracing facility *)
let __TRACE_ON__ = TmAbbBind(TmTrue(dummyinfo))
(** Binding to deactivate the tracing facility *)
let __TRACE_OFF__ = TmAbbBind(TmFalse(dummyinfo))
let initialTraceFlag = __TRACE__, __TRACE_OFF__

(* let us express the fixed point combinator as an evaluation tree *)
let __FPC__ = "__FPC__"
let __FPC_TERM__ = TmAbbBind(
  TmAbs(dummyinfo,"fun",
  	TmApp(dummyinfo,
  		TmAbs(dummyinfo,"fpc",
  			TmApp(dummyinfo,
  				TmVar(dummyinfo,1,3),
  				TmAbs(dummyinfo,"y",
  					TmApp(dummyinfo,
  						TmApp(dummyinfo,
  							TmVar(dummyinfo,1,4),
  							TmVar(dummyinfo,1,4)
  						),
  						TmVar(dummyinfo,0,4)
  					)
  				)
  			)
  		),
  		TmAbs(dummyinfo,"fpc",
  			TmApp(dummyinfo,
  				TmVar(dummyinfo,1,3),
  				TmAbs(dummyinfo,"y",
  					TmApp(dummyinfo,
  						TmApp(dummyinfo,
  							TmVar(dummyinfo,1,4),
  							TmVar(dummyinfo,1,4)
  						),
  						TmVar(dummyinfo,0,4)
  					)
  				)
  			)
  		)
  	)
  )
)
(* and make it into a binding *)
let strictFixedPointCombinator = __FPC__, __FPC_TERM__

(** Provides an empty context *)
let emptycontext = [strictFixedPointCombinator; initialTraceFlag]
(** Returns the length of the specified context *)
let ctxlength ctx = List.length ctx
(** Adds a binding to the given context *)
let addbinding ctx x bind = (x,bind)::ctx
(** Adds a name in the current context *)
let addname ctx x = addbinding ctx x NameBind

(** Checks if a name is bound in the given context *)
let rec isnamebound ctx x =
  match ctx with
    [] -> false
  | (y,_)::rest ->
    if y=x then true
    else isnamebound rest x

(** Generates a new name for an inner variable in a lambda expression if
  necessary to avoid name conflicts *)
let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

(** Given an index and a context, returns the variable name if it exists or
    raises an error otherwise *)
let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))
(** Given a name and a context, returns its index on that context *)
let rec name2index fi ctx x =
  match ctx with
    [] -> error fi ("Identifier " ^ x ^ " is unbound")
  | (y,_)::rest ->
    if y=x then 0
    else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tmmap onvar c t =
  let rec walk c t = match t with
      TmTrue(fi) as t -> t
    | TmFalse(fi) as t -> t
    | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
    | TmVar(fi,x,n) -> onvar fi c x n
    | TmAbs(fi,x,t2) -> TmAbs(fi,x,walk (c+1) t2)
    | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
    | TmProj(fi,t1,l) -> TmProj(fi,walk c t1,l)
    | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->
        (li,walk c ti))
        fields)
    | TmFloat _ as t -> t
    | TmTimesfloat(fi,t1,t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
    | TmString _ as t -> t
    | TmZero(fi)      -> TmZero(fi)
    | TmSucc(fi,t1)   -> TmSucc(fi, walk c t1)
    | TmPred(fi,t1)   -> TmPred(fi, walk c t1)
    | TmIsZero(fi,t1) -> TmIsZero(fi, walk c t1)
    | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
    | TmRec(fi,x,t2) -> TmRec(fi,x,walk (c+1) t2)
  in walk c t

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d))
    c t

(** Displaces a variable term's indices by an specified amount *)
let termShift d t = termShiftAbove d 0 t

(** Term shifting for bindings *)
let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TmAbbBind(t) -> TmAbbBind(termShift d t)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi c x n -> if x=j+c then termShift c s else TmVar(fi,x,n))
    0
    t

(** Substitutes every apparition of a variable inside term t with the value
  passed as s *)
let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Recursion *)

(** Applies the given term to the FPC allowing for recursion *)
let applyToFPC ctx t = match t with
  | TmRec(fi,x,v) ->
    TmApp(fi, TmVar(fi, name2index fi ctx __FPC__, ctxlength ctx), TmAbs(fi,x,v))
  | _ -> raise (Invalid_argument "Should only apply recusive terms to FPC")

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

(** Given an index and a context, returns the binding associated to that index *)
let getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

(** Extracts term filename, line number and column information *)
let tmInfo t = match t with
    TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmString(fi,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmRec(fi,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
   information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
   See the documentation for the Format module in the OCaml library for
   more details.
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t =
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printtm_Term outer ctx t = match t with
    TmIf(fi, t1, t2, t3) ->
    obox0();
    pr "if ";
    printtm_Term false ctx t1;
    print_space();
    pr "then ";
    printtm_Term false ctx t2;
    print_space();
    pr "else ";
    printtm_Term false ctx t3;
    cbox()
  | TmAbs(fi,x,t2) ->
    (let (ctx',x') = (pickfreshname ctx x) in
     obox(); pr "λ"; pr x'; pr ".";
     if (small t2) && not outer then break() else print_space();
     printtm_Term outer ctx' t2;
     cbox())
  | TmRec(fi,x,t2) -> (* printing function for recursive definitions *)
    (let (ctx',x') = (pickfreshname ctx x) in
     obox(); pr "Y λ"; pr x'; pr "."; (* we apply the lambda to the Y comb. *)
     if (small t2) && not outer then break() else print_space();
     printtm_Term outer ctx' t2;
     cbox())
  | TmLet(fi, x, t1, t2) ->
    obox0();
    pr "let "; pr x; pr " = ";
    printtm_Term false ctx t1;
    print_space(); pr "in"; print_space();
    printtm_Term false (addname ctx x) t2;
    cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
    obox0();
    printtm_AppTerm false ctx t1;
    print_space();
    printtm_ATerm false ctx t2;
    cbox()
  | TmTimesfloat(_,t1,t2) ->
    pr "timesfloat "; printtm_ATerm false ctx t1;
    pr " "; printtm_ATerm false ctx t2
  | TmPred(_,t1) ->
    pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
    pr "iszero "; printtm_ATerm false ctx t1
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
    printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_ATerm outer ctx t

(** Prints an atomic term (true, 9, zero...) *)
and printtm_ATerm outer ctx t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmVar(fi,x,n) ->
    if ctxlength ctx = n then
      pr (index2name fi ctx x)
    else
      pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
          ^ " in {"
          ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
          ^ " }]")
  | TmRecord(fi, fields) ->
    let pf i (li,ti) =
      if (li <> ((string_of_int i))) then (pr li; pr "=");
      printtm_Term false ctx ti
    in let rec p i l = match l with
          [] -> ()
        | [f] -> pf i f
        | f::rest ->
          pf i f; pr","; if outer then print_space() else break();
          p (i+1) rest
    in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmString(_,s) -> pr ("\"" ^ s ^ "\"")
  | TmZero(fi) ->
    pr "0"
  | TmSucc(_,t1) ->
    let rec f n t = match t with
        TmZero(_) -> pr (string_of_int n)
      | TmSucc(_,s) -> f (n+1) s
      | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
    in f 1 t1
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

(** Prints a term *)
let printtm ctx t = printtm_Term true ctx t

(** Prints a binding's name and associated terms *)
let prbinding ctx b = match b with
    NameBind -> ()
  | TmAbbBind(t) -> pr "= "; printtm ctx t
