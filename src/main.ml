
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

(* --- Dirty insertion of _ltl2ba_result, just to be sure the function is not deleted.
   Does not insert if there is no return in main
*)
class insert_result_visitor (result_call) = object(self)
  inherit C.nopCilVisitor
  method vstmt s =
    match s.C.skind with
    | C.Return _ -> let action s =
                      C.Block (C.mkBlock [C.mkStmtOneInstr result_call; s]) |> C.mkStmt
      in
      C.ChangeDoChildrenPost (s, action)
    | _ -> C.DoChildren
end

let insert_end_main rcal g =
  let rec getLast = function
    | [] -> assert false
    | x::[] -> x
    | x::q -> getLast q
  in
  match g with
  | C.GFun (f, _) when f.C.svar.C.vname = "main" ->
    let vis = new insert_result_visitor rcal in
    ignore(C.visitCilFunction vis f);
    let last = getLast (f.sbody.bstmts) in
    (match  last.skind with
    | C.Return _ -> ()
    | _ -> f.sbody.bstmts <- f.sbody.bstmts @ [C.mkStmtOneInstr rcal])
  | _ -> ()

let add_result f =
  let frtype = Baproductutils.mkFunctionType C.voidType [] in
  let fr = C.findOrCreateFunc f "_ltl2ba_result" frtype in
  let rcall = Baproductutils.mkFunctionCall fr None [] C.locUnknown in
  C.iterGlobals f (insert_end_main rcall)

(* ---- *)

let main () =
  O.arg_parse ();
  let spec = Specification.from_file !O.specFile in
  let cilFile = parseFile !O.srcFile in
  let cil_props = Cilspecification.from_spec cilFile spec in
  Instrumentation.add_instrumentation cilFile cil_props;
  add_result cilFile;
  Rmtmps.removeUnusedTemps cilFile;
  outputFile !O.dstFile cilFile


let () = main()
