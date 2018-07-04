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
(**Output for the current context*)
val print_context: context -> unit
(** This function enable o disable trace printing*)
val check_trace: context -> bool
(** Evaluation function for debugging commands. Evaluates a debugging command and modifies the given
context to include it.*)
val debugging: context -> dbg -> context
