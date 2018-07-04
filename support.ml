open Format
(* module Support
   Collects a number of low-level facilities used by the other modules
   in the typechecker/evaluator. 
*)
module Error = struct
  (** An exception raised by the low-level error printer; exported
     here so that it can be caught in module Main and converted into
     an exit status for the whole program. *)
  exception Exit of int


   (** An element of the type info represents a "file position": a 
     file name, line number, and character position within the line.  
     Used for printing error messages. *)
  type info = FI of string * int * int | UNKNOWN
    (** A convenient datatype for a "value with file info."  Used in
     the lexer and parser. *)
  type 'a withinfo = {i: info; v: 'a}

  let dummyinfo = UNKNOWN

  (** Create file position info: filename lineno column *)
  let createInfo f l c = FI(f, l, c)

  (** Print an error message and fail.  The printing function is called
     in a context where the formatter is processing an hvbox.  Insert
     calls to Format.print_space to print a space or, if necessary,
     break the line at that point. 
     @param f printing function.*)
  let errf f =
    print_flush();
    open_hbox (); f(); print_cut(); close_box(); print_newline();
    raise (Exit 1)

  let printInfo =
    (* In the text of the book, file positions in error messages are replaced
       with the string "Error:" *)
    function
      FI(f,l,c) ->
      print_string f;
      print_string ":";
      print_int l; print_string ".";
      print_int c; print_string ":"
    | UNKNOWN ->
      print_string "<Unknown file and line>: "

  (**Adds fileinfo to the printing function*)
  let errfAt fi f = errf (fun()-> printInfo fi; print_space(); f())
  (** Convenient wrappers for the above, for the common case where the
    action to be performed is just to print a given string. *)
  let err s = errf (fun()-> print_string "Error: "; print_string s)
  (** First step of the error chain. Adds the error string to the printing function*)
  let error fi s = errfAt fi (fun()-> print_string s;)
  (** Variants that print a message but do not fail afterwards *)
  let warning s =
    print_string "Warning: "; print_string s;
    print_newline()
  (** Variants that print a message but do not fail afterwards with fileinfo*)
  let warningAt fi s =
    printInfo fi; print_string " Warning: ";
    print_string s; print_newline()

end

(* ---------------------------------------------------------------------- *)

module Pervasive = struct

  type info = Error.info

  let pr = Format.print_string

end (* module pervasive *)
