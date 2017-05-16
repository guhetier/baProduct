
module C = Cil
module E = Errormsg
module S = Specification

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
  truth_var : C.varinfo (* the global variable storing the current truth value
                           of the proposition *)
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
