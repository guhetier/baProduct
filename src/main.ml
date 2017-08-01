
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
  if !O.verbose then
    E.log "Parsing the specification file %s...\n" !O.specFile;
  let spec = Specification.from_file !O.specFile in

  (* Parse the code and remove unused entities (keeps labels) *)
  if !O.verbose then
    E.log "Parsing the file %s" spec.Specification.ltl;
  let cilFile = parseFile !O.srcFile in
  R.removeUnusedTemps cilFile;

  (* Call ltl2ba to generate a json version of the automaton representing
     the ltl formula from the specification.
  *)
  let cmd = Printf.sprintf
      "%s -f \"!( %s )\" -t json > %s/auto.tmp"
      !O.ltl2ba_path spec.Specification.ltl !O.tmp_path
  in
  if !O.verbose then
    E.log "Calling ltl2ba on the formula %s\n" spec.Specification.ltl;

  (match Sys.command cmd with
  | 0 -> ()
  | r -> E.s (E.error "ltl2ba failed (return value: %d)\n" r)
  );

  (* Parse the automaton, translate it to C code *)
  if !O.verbose then
    E.log "Parsing the automaton...\n";

  let a = Automaton.from_file (!O.tmp_path ^ "/auto.tmp") in

  (* If asked, output the automaton in dot *)
  if !O.output_dot then begin
    if !O.verbose then
      E.log "Printing the automaton in dot...\n";
    let dotfile = open_out_bin (!O.tmp_path ^ "/auto.dot") in
    Automaton.output_dot_automaton dotfile a
  end;

  if !O.verbose then
    E.log "Creating the automaton in C...\n";
  let c_automaton = Mergeautomaton.create_automaton a cilFile spec in
  Mergeautomaton.add_result cilFile;

  (* Convert the specification, collecting CIL entities that are used *)
  if !O.verbose then
    E.log "Collect CIL definitions...\n";
  let cil_props = Cilspecification.from_spec cilFile spec in

  (* Instrument the CIL file to synchronize the code with the automaton *)
  if !O.verbose then
    E.log "Instrument the file...\n";
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
