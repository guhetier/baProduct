
module C = Cil
module F = Frontc
module E = Errormsg
module O = Option
module R = Rmtmpslbl

let parseFile (filename: string) =
  F.parse filename ()

let outputFile (filename: string option) (f : C.file) : unit =
  let output_file, c = match filename with
    | None -> "standard output", stdout
    | Some f -> try
        f, open_out f
      with _ ->
        E.s (E.error "Couldn't open file %s" f)
  in
  C.print_CIL_Input := false;
  C.dumpFile (!C.printerForMaincil) c output_file f;
  if filename <> None then close_out c

let main () =

  E.colorFlag := true;

  O.arg_parse ();
  let spec = Specification.from_file !O.specFile in
  let cilFile = parseFile !O.srcFile in
  R.removeUnusedTemps cilFile;
  Mergeautomaton.initTruthVar cilFile spec;
  Mergeautomaton.add_result cilFile;
  let cil_props = Cilspecification.from_spec cilFile spec in
  Instrumentation.add_instrumentation cilFile cil_props;
  outputFile !O.dstFile cilFile
;;

begin
  try
    main ()
  with E.Error -> ()
end;
exit (if !E.hadErrors then 1 else 0)
