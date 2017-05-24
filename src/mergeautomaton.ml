open Cil
open Baproductutils
module E = Errormsg
module S = Specification

(********* Insert a call to the result function at the end of the main *********)

class insert_result_visitor (fresult) = object(self)
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | Return _ -> let rc = mkFunctionCall fresult None [] (get_stmtLoc s.skind) in
      let ns = Block (mkBlock [mkStmtOneInstr rc; s]) |> mkStmt in
      ChangeTo ns
    | _ -> DoChildren
end


let insert_end_main rcal g =
  match g with
  | GFun (f, _) when f.svar.vname = "main" ->
    let vis = new insert_result_visitor rcal in
    ignore(visitCilFunction vis f);
  | _ -> ()


let add_result f =
  let frtype = Baproductutils.mkFunctionType voidType [] in
  let fr = findOrCreateFunc f "_ltl2ba_result" frtype in
  iterGlobals f (insert_end_main fr)


(********* Initialize proposition truth value variables **********)
(* TODO : Do this correctly when building the automaton in Ocaml *)

let initGlobalVar (f: file) (name: string) (i: init) =
  let aux (g: global) =
    match g with
    | GVar(vi, ii, _) when vi.vname = name ->
      ii.init <- Some i
    | _ -> ()
  in
  List.iter aux f.globals

let initTruthVar (f: file) (spec: S.spec) =
  let aux (p: S.atomic_prop) =
    initGlobalVar f ("_ltl2ba_atomic_" ^ p.S.name) (SingleInit (mkBool p.S.default_val))
  in
  List.iter aux (spec.S.props)
