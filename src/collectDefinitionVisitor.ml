
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
  let state_var_cil = H.create 10 in
  let prop_fun_cil = H.create 10 in
  let param_prop_cil = H.create 10 in

  let create_prop_var_def (prop: atomic_prop) =
    (* Create the global variables marking the state of a proposition *)
    let tvar_inf = makeGlobalVar ("_ltl2ba_atomic_" ^ prop.S.name) intType  in
    let tvar_def = GVar(tvar_inf,
                        (Baproductutils.mkIntInit
                           (if prop.S.default_val then 1 else 0)),
                        locUnknown)
    in
    let svar_inf = makeGlobalVar ("_ltl2ba_active_" ^ prop.S.name) intType in
    let svar_def = GVar(svar_inf, (Baproductutils.mkIntInit 0), locUnknown)
    in
    f.globals <- svar_def :: tvar_def :: f.globals;
    H.add truth_var_cil prop.name tvar_inf;
    H.add state_var_cil prop.name svar_inf

  in List.iter create_prop_var_def s.props;

  let collectParam (var: param) =
    if not (H.mem param_prop_cil var) then
      let vopt, pname = match var with
      | Global(vname) -> searchGlobVar f vname, "_ltl2ba_pointer_" ^ vname
      | Local (fname, vname) -> searchLocalVar f fname vname,
                                "_ltl2ba_pointer_" ^ fname ^ "_" ^ vname
      in
      match vopt with
      | None -> assert false
      | Some vcil when not vcil.vglob ->
        let vpointer = makeGlobalVar (pname) (TPtr(vcil.vtype, [])) in
        let initvp = {init = Some (makeZeroInit vpointer.vtype)} in
        f.globals <- GVar(vpointer, initvp, locUnknown)::f.globals;
        H.add param_prop_cil var (vcil, Some(vpointer))
      | Some vcil ->
        H.add param_prop_cil var (vcil, None)
  in
  let collect_prop_fun (p: atomic_prop) =
    (* Collect the function defining the proposition value *)
    if not (H.mem prop_fun_cil p.expr) then begin
      match searchFunction f p.expr with
      | None -> E.s (E.error "Missing proposition defining function %s" p.expr)
      | Some v -> H.add prop_fun_cil p.expr v
    end;
    (* Collect the proposition parameters *)
    List.iter collectParam p.params
  in
  List.iter collect_prop_fun s.props;
  (truth_var_cil, state_var_cil, prop_fun_cil, param_prop_cil)
