open Cil
open Baproductutils
module E = Errormsg
module S = Specification

(********* Insert a call to the result function at the end of the main *********)

class insert_result_visitor (f_to_insert) = object(self)
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | Return _ ->
      let icalls = List.map
          (fun f -> mkFunctionCall f None [] (locUnknown))
          f_to_insert in
      let ns = Block (mkBlock [mkStmt (Instr icalls); s]) |> mkStmt in
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
  let f_atomic_b = findOrCreateFunc f "__ESBMC_atomic_begin" (mkFunctionType voidType []) in
  let f_atomic_e = findOrCreateFunc f "__ESBMC_atomic_end" (mkFunctionType voidType []) in
  let f_res = findOrCreateFunc f "_ltl2ba_result" (mkFunctionType voidType []) in
  iterGlobals f (insert_end_main [f_atomic_b; f_res; f_atomic_e])


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
