(** Syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

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

(** A binding is either a name (let ... in ) or an abstraction (... = ...) over
    a term *)
type binding =
    NameBind
  | TmAbbBind of term

(* Debug commands *)
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

(* Contexts *)
(** A context is a list of tuples string * binding type, where, if the binding
    is an abstract binding, it includes a term. *)
type context

(** Provides an empty context *)
val emptycontext : context
(** Returns the length of the specified context *)
val ctxlength : context -> int
(** Adds a binding to the given context *)
val addbinding : context -> string -> binding -> context
(** Adds a name in the current context *)
val addname: context -> string -> context
(** Given an index and a context, returns the variable name if it exists or
    raises an error otherwise *)
val index2name : info -> context -> int -> string
(** Given an index and a context, returns the binding associated to that index *)
val getbinding : info -> context -> int -> binding
(** Given a name and a context, returns its index on that context *)
val name2index : info -> context -> string -> int
(** Checks if a name is bound in the given context *)
val isnamebound : context -> string -> bool


(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
(** Prints a term *)
val printtm: context -> term -> unit
(** Prints an atomic term (true, 9, zero...) *)
val printtm_ATerm: bool -> context -> term -> unit
(** Prints a binding's name and associated terms *)
val prbinding : context -> binding -> unit

(* Misc *)
(** Extracts term filename, line number and column information *)
val tmInfo: term -> info

(* Debugging *)
val debugging: context -> dbg -> context
