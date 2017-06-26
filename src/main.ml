
module C = Cil
module F = Frontc
module E = Errormsg
module O = Option
module R = Rmtmpslbl

(* Parse a file to get a CIL file *)
let parseFile (filename: string) : C.file =
  F.parse filename ()


(* Output a CIL file, inserting a prefix string before it *)
let outputFile (filename: string option) (suffix: string) (f : C.file) : unit =
  let output_file, c = match filename with
    | None -> "standard output", stdout
    | Some f -> try
        f, open_out f
      with _ ->
        E.s (E.error "Couldn't open file %s" f)
  in
  C.print_CIL_Input := false;
  C.dumpFile (!C.printerForMaincil) c output_file f;
  Printf.fprintf c "%s\n\n" suffix;

  if filename <> None then close_out c


let main () =

  (* CIL options *)
  E.colorFlag := true;

  (* Parse command line arguments *)
  O.arg_parse ();

  (* Parse the specification *)
  let spec = Specification.from_file !O.specFile in

  (* Parse the code and remove unused entities (keeps labels) *)
  let cilFile = parseFile !O.srcFile in
  R.removeUnusedTemps cilFile;

  (* Call ltl2ba to generate a json version of the automaton representing
     the ltl formula from the specification.
  *)
  let cmd = Printf.sprintf
      "../ltl2ba/ltl2ba -f \"!( %s )\" -t json > auto.tmp"
      spec.Specification.ltl
  in
  (match Sys.command cmd with
  | 0 -> ()
  | r -> E.s (E.error "ltl2ba failed (return value: %d)" r)
  );

  (* Parse the automaton, translate it to C code *)
  let a = Automaton.from_file "auto.tmp" in

  (* If asked, output the automaton in dot *)
  if !O.output_dot then
    let dotfile = open_out_bin "auto.dot" in
    Automaton.output_dot_automaton dotfile a;

  let c_automaton = Mergeautomaton.create_automaton a cilFile spec in
  Mergeautomaton.add_result cilFile;

  (* Convert the specification, collecting CIL entities that are used *)
  let cil_props = Cilspecification.from_spec cilFile spec in

  (* Instrument the CIL file to synchronize the code with the automaton *)
  Instrumentation.add_instrumentation cilFile cil_props;

  (* Output the resulting code file *)
  outputFile !O.dstFile c_automaton cilFile;

;;

begin
  try
    main ()
  with E.Error -> ()
end;
exit (if !E.hadErrors then 1 else 0)
