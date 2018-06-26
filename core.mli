(** Core typechecking and evaluation functions *)

open Syntax
open Support.Error

(** Evaluation function: Provided with a context and a term, it evaluates that
term or returns it unchanged if there's no evaluation rule*)
val eval : context -> term -> term

(** Evaluation function for bindings. Evaluates a binding and modifies the given
context to include it. *)
val evalbinding : context -> binding -> binding

(* Debugging *)
val print_context: context -> unit
val check_trace: context -> bool
val debugging: context -> dbg -> context
