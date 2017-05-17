
module C = Cil
module F = Frontc
module E = Errormsg
module O = Option

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
  O.arg_parse ();
  let spec = Specification.from_file !O.specFile in
  print_string spec.Specification.ltl;
  print_newline ();
  let cilFile = parseFile !O.srcFile in
  Instrumentation.add_instrumentation cilFile spec;
  outputFile !O.dstFile cilFile


let () = main()
