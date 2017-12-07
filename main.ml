(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let parseArgs () =
  let inFile = ref (None : string option) in
  (* command line option names, descriptions and objects *)
  let argDefs = [
    "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path";
    "-f",
      Arg.String (fun s ->
        match !inFile with
          Some(_) -> err "You must specify exactly one input file"
        | None -> inFile := Some(s)),
      "Specify the input file"
    ] in
  Arg.parse argDefs
     (* unnamed arguments function *)
	 (fun _ -> err "Unrecognized arguments.")
     "";
  match !inFile with
      None -> "" (* interactive interpreter *)
    | Some(s) -> s (* input file specified *)

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let rec process_command ctx cmd = match cmd with
  | Eval(fi,t) -> 
      let t' = eval ctx t in
      printtm_ATerm true ctx t';
      force_newline();
      print_flush();
      ctx
  | Bind(fi,x,bind) -> 
      let bind' = evalbinding ctx bind in
      pr x; pr " "; prbinding ctx bind'; force_newline();
      addbinding ctx x bind'
  
let process_file f ctx =
  try
	  alreadyImported := f :: !alreadyImported;
	  let cmds,_ = parseFile f ctx in
	  let g ctx c =  
		open_hvbox 0;
		let results = process_command ctx c in
		close_box();
		print_flush();
		results
	  in
		List.fold_left g ctx cmds
  with Support.Error.Exit(code) -> exit code

let rec toplevel ctx =
  Printf.printf ":: "; (* prompt *)
  try
    let input = read_line () in
    (* creates a lexer buffer from the input string *)
	let lexbuf = Lexing.from_string input in
	let result = try
      (* parse the lexed input string, return a syntactic tree *)
	  Parser.toplevel Lexer.main lexbuf 
	  (* unless there's an error on the input string *)
	  with Parsing.Parse_error as e ->
	  	(* pretty print the error and scalate the exception *)
        print_flush();
        open_hvbox 0;
        printInfo (Lexer.info lexbuf);
        print_space ();
        print_string "Parse error";
        print_cut(); close_box(); print_newline();
        raise e;
	in
	(* we apply the semantic tree to a context to obtain a command list *)
	let cmds, _ = result ctx in
	
	(* this function allows us to evaluate a command in a given context *)
    let g ctx c =
      open_hvbox 0;
      let results = process_command ctx c in
      close_box();
      print_flush();
      results (* return context *)
    in
    (* we evaluate every command on the list on our current context while updating it with each execution *) 
	let new_ctx = List.fold_left g ctx cmds in
	(* after which, we clear the parser's stack *)
    Parsing.clear_parser ();
    (* and recall the interpreter's main loop with an updated context *)
    toplevel new_ctx
  (* interpreter will exit on EOF (Ctrl+D) *)
  with
    Parsing.Parse_error -> toplevel ctx
    | Support.Error.Exit(code) -> close_box(); print_flush(); toplevel ctx
    | End_of_file -> ()

let main () = 
  let inFile = parseArgs() in
  match inFile with
  	(* if no input file, launch interactive interpreter *)
  	"" -> toplevel emptycontext
  	(* else, execute file and exit *)
  	| s -> let _ = process_file inFile emptycontext in ()

let () = set_max_boxes 1000
let () = set_margin 67
(* Printexc.catch is OBSOLETE
let res = 
  Printexc.catch (fun () -> 
    try main(); 0 
    with Exit x -> x) 
  ()
*)
let () = print_flush()
(* 
let () = exit res
*)
let () = main ()
	
