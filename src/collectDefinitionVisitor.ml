
(* Implementation of a visitor that collect definitions used in
   a specification.
   It allows the creation of `cil_prop` propositions from specification
   propositions.

   It collects :
   - functions used to define atomic propositions
   - variables given as parameters to these functions
   (- variables used to store propositions truth values) <- temporary,
   this binding should come from the automaton creation when done in CIL
*)

open Cil
module E = Errormsg
module S = Specification
module H = Hashtbl

let searchFunction (f: file) (name: string) : varinfo option =
  let rec search globals =
    match globals with
    | [] -> None
    | GFun({svar=vi}, _)::_ when vi.vname = name -> Some(vi)
    | _::rest -> search rest
  in
  search f.globals

let searchGlobVar (f: file) (name: string) : varinfo option =
  let rec search globals =
    match globals with
    | [] -> None
    | GVar(vi, _, _)::_ when vi.vname = name -> Some vi
    | _::rest -> search rest
  in
  search f.globals

class searchLocalVarVisitor (locname: string) = object(self)
  inherit nopCilVisitor

  val mutable vi_found = (None: varinfo option)

  method get_res () =
    vi_found

  method vvdec (vi: varinfo) =
    if vi_found = None && vi.vname = locname then
      vi_found <- Some vi;
    SkipChildren
end

let searchLocalVar (f: file) (fname: string) (vname: string) =
  let rec search globals =
    match globals with
    | [] -> None
    | GFun(fd, _)::_ when fd.svar.vname = fname ->
      let vis = new searchLocalVarVisitor(vname) in
      ignore(visitCilFunction (vis :> nopCilVisitor) fd);
      vis#get_res()
    | _::rest -> search rest
  in
  search f.globals


let collectFromSpecification (f: file) (s: S.spec) =
  let open S in
  let truth_var_cil = H.create 10 in
  let prop_fun_cil = H.create 10 in
  let param_prop_cil = H.create 10 in

  let collectParam (var: S.param) =
    if not (H.mem param_prop_cil var) then
      let vopt = match var with
      | Global(vname) -> searchGlobVar f vname
      | Local (fname, vname) -> searchLocalVar f fname vname
      in
      match vopt with
      | None -> assert false
      | Some vcil -> H.add param_prop_cil var vcil
  in
  let collectFromProp (p: S.atomic_prop) =
    let truth_var_name = "_ltl2ba_atomic_" ^ p.name in
    (match searchGlobVar f truth_var_name with
    | None -> E.s (E.error "Missing proposition truth variable %s"
      truth_var_name)
    | Some v -> H.add truth_var_cil p.name v
    );
    if not (H.mem prop_fun_cil p.expr) then begin
      match searchFunction f p.expr with
      | None -> E.s (E.error "Missing proposition defining function %s" p.expr)
      | Some v -> H.add prop_fun_cil p.expr v
    end;
    List.iter collectParam p.params
  in
  List.iter collectFromProp s.props;
  (truth_var_cil, prop_fun_cil, param_prop_cil)
