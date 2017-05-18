
module C = Cil
module E = Errormsg

module H = Hashtbl
module S = Specification
module CS = CollectDefinitionVisitor

(* `cil_prop` represents the link between an atomic proposition
   from the specification and cil constructs *)
type cil_prop = {
  name : string; (* the name of the proposition *)
  start_label : string; (* name of the label starting the active span of the
                           property *)
  end_label : string; (* name of the label ending the active span of the
                           property *)
  prop_fun : C.varinfo; (* the function giving the proposition value in the
                           active span *)
  prop_params : C.varinfo list; (* the parameters of `prop_fun` *)
  default_val : bool; (* the value of the proposition out of the active span *)
  truth_var : C.varinfo; (* the global variable storing the current truth value
                           of the proposition *)
  state_var : C.varinfo; (* the global variable storing whether the proposition is
                            currently in its valid zone *)
}

let is_parameter (p: cil_prop) (v: C.varinfo) =
  let open C in
  let equal x y =
    x.vid = y.vid
  in
  List.exists (equal v) p.prop_params

(* Printing *)
let cil_prop_to_string (p: cil_prop) =
  p.name

(* Getters *)
let get_name (p: cil_prop) =
  p.name

let get_start_label (p: cil_prop) =
  p.start_label

let get_end_label (p: cil_prop) =
  p.end_label

let get_fun (p: cil_prop) =
  p.prop_fun

let get_params (p: cil_prop) =
  p.prop_params

let get_default (p: cil_prop) =
  p.default_val

let get_truth_var (p: cil_prop) =
  p.truth_var

let get_state_var (p: cil_prop) =
  p.state_var

(* Builder *)
let make_cil_prop name sl el pf pps def tv sv =
  {
    name = name;
    start_label = sl;
    end_label = el;
    prop_fun = pf;
    prop_params = pps;
    default_val = def;
    truth_var = tv;
    state_var = sv;
  }


(* `cil_prop_state` represents the state of atomic proposition
   during instrumentation.
   Invariants :
   - disabled_props and enabled_props are a partition of the set
   of all atomic propositions
   - a proposition is in `enabled_props` iff the current node in the AST
   is between the starting label and the ending label
*)
type cil_prop_state = {
  mutable disabled_props : cil_prop list;
  mutable enabled_props : cil_prop list;
}

(* Empty proposition state, for initialization *)
let empty = {
  disabled_props = [];
  enabled_props = [];
}

(* Getters *)
let get_enabled_props (s: cil_prop_state) =
  s.enabled_props

let get_disabled_props (s: cil_prop_state) =
  s.disabled_props

(* Modifiers *)
let enable_props (ps: cil_prop_state) (s: cil_prop list) =
  let to_keep p =
    not (List.exists (fun x -> x.name = p.name) s)
  in
  ps.disabled_props <- List.filter to_keep ps.disabled_props;
  ps.enabled_props <- List.append ps.enabled_props s

let disable_props (ps: cil_prop_state) (s: cil_prop list) =
  let to_keep p =
    not (List.exists (fun x -> x.name = p.name) s)
  in
  ps.enabled_props <- List.filter to_keep ps.enabled_props;
  ps.disabled_props <- List.append ps.disabled_props s

(* Build a cil_prop from a specification atomic_prop *)
let from_atomic_prop (truth_var, state_var, prop_fun, prop_params) (ap: S.atomic_prop) =
  let open S in
  let pf = try H.find prop_fun ap.expr with Not_found -> assert false in
  let tv = try H.find truth_var ap.name with Not_found -> assert false in
  let sv = try H.find state_var ap.name with Not_found -> assert false in
  let pps = List.map (fun var ->
      try H.find prop_params var with Not_found -> assert false)
      ap.params in
  make_cil_prop ap.name (fst ap.valid_span)
    (snd ap.valid_span) pf pps ap.default_val tv sv

(* Build a list of cil propositions from a specification *)
let from_spec (f: C.file) (spec: S.spec) : cil_prop list=
  let open Specification in
  let collected = CS.collectFromSpecification f spec in
  List.map (from_atomic_prop collected) spec.props
