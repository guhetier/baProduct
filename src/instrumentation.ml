
open Cil
open Baproductutils

module E = Errormsg
module S = Specification
module CS = Cilspecification

let cilSpec : CS.cil_prop_state = CS.empty

let transition_fun_str = "_ltl2ba_transition"
let atomic_begin_fun_str = "__ESBMC_atomic_begin"
let atomic_end_fun_str = "__ESBMC_atomic_end"

let dummy_fun = makeVarinfo false "_dummy" voidType

type instr_fun = {
  mutable transition : varinfo;
  mutable atomic_begin : varinfo;
  mutable atomic_end : varinfo;
}

let instrFun = {
  transition = dummy_fun;
  atomic_begin = dummy_fun;
  atomic_end = dummy_fun;
}

(****************** Build-helper *******************************)

(* Build the instruction to update
   a truth value from the proposition function *)
let mkUpdateFunctionCall (prop: CS.cil_prop) (loc: location)=
  mkFunctionCall (CS.get_fun prop) (Some (CS.get_truth_var prop))
    (CS.get_params prop) loc

(* Build a call to the automaton transition function *)
let mkTransitionFunctionCall (loc: location) =
   mkFunctionCall instrFun.transition None [] loc

(* Build the instruction that set a truth value to its default value *)
let mkSetToDefaultInstr (prop: CS.cil_prop) (loc: location) =
  Set((Var((CS.get_truth_var prop)), NoOffset),
      (mkBool (CS.get_default prop)), loc)

let mkSetPropState (prop: CS.cil_prop) (loc: location) (s: bool) =
  Set((Var((CS.get_state_var prop)), NoOffset), mkBool s, loc)

(**************** Prop state manipulation functions *************)

(* Return the list of properties that must be recomputed after the
   modification of the variable `var` in the active zone of the prop.
   They are the properties that depends on `var` and are enabled.
*)
let active_props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_enabled_props cilSpec)

(* Return the list of properties that it may be needed to recompute after the
   modification of the variable `var` but that are not enabled.
   This case occurs when var is a global variable whose modification may
   impact the property in its active zone, in another thread
*)
let inactive_props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_disabled_props cilSpec)

(* Gather atomic propositions that start at a
   given label and set them enabled *)
let add_starting_prop (l: label) =
  let newProps = List.filter
      (fun p -> (CS.get_start_label p) = (get_label_name l))
      (CS.get_disabled_props cilSpec)
  in
  CS.enable_props cilSpec newProps;
  newProps

(* Gather atomic propositions that end at a given label and set them disabled *)
let remove_ending_prop (l:label) =
  let endingProps = List.filter
      (fun p -> (CS.get_end_label p) = (get_label_name l))
      (CS.get_enabled_props cilSpec)
  in
  CS.disable_props cilSpec endingProps;
  endingProps

(*************** Instrumentation visitor ********************)

(* This visitor instrument the code to build the product with a Büchi automaton.
   It updates truth values every time their value may change and call the
   automaton transition function
*)
class addInstrumentationVisitor = object(self)
  inherit nopCilVisitor

  (* When reaching a label, a proposition may be enabled / disabled.
     Update property status, and insert code to update truth value
  *)
  method vstmt (s: stmt) =
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    let endingProps = List.map remove_ending_prop labels |> List.flatten in

    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "Starting %s\n")
      startingProps;
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "Ending %s\n")
      endingProps;
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "%s is enabled\n")
      (CS.get_enabled_props cilSpec);
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "%s is disabled\n")
      (CS.get_disabled_props cilSpec);

    match startingProps, endingProps with
    | [], [] -> DoChildren
    | _ ->
      let loc = get_stmtLoc s.skind in
      let init_start =
        List.map (fun p -> mkUpdateFunctionCall p loc) startingProps in
      let init_end =
        List.map (fun p -> mkSetToDefaultInstr p loc) endingProps in
      let set_state_true =
        List.map (fun p -> mkSetPropState p loc true) startingProps in
      let set_state_false =
        List.map (fun p -> mkSetPropState p loc false) endingProps in
      let tr = mkTransitionFunctionCall loc in
      let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
      let ae = mkFunctionCall instrFun.atomic_end None [] loc in

      let action (s: stmt) =
        let instrStmt = mkStmt (Instr (
            [tr; ae] |> (@) set_state_true |> (@) init_start |> (@)
            set_state_false |> (@) init_end |> (@) [ab]))
        in
        let b = mkBlock (instrStmt::[s]) in
        mkStmt (Block b)
      in
      ChangeDoChildrenPost (s, action)

  (* If an instruction may change the truth value of a proposition, insert
     code to update the truth value *)
  method vinst (i: instr) =
      match i with
      | Set ((Var v, NoOffset), _, loc) ->
        E.log "%a : Set %a\n" d_loc loc d_instr i;
        (*
           TODO: Two cases.
           - a local variable : it cannot be modified out of this function (indirect access
           are not considered), so it is enough to update the proposition whenever the variable
           is modified in the valid span
           - a global variable : it can be modified out of the function by another thread.
           One must update the proposition every time the variable is modified in the program,
           but only if the proposition is active at the given time.
           *)
        let active_prop_to_update = active_props_to_update v in
        let inactive_prop_to_update =
          if v.vglob then inactive_props_to_update v else [] in
        if active_prop_to_update = [] && inactive_prop_to_update = [] then
          SkipChildren
        else begin
          List.iter
            (fun p -> p |> CS.cil_prop_to_string |> E.log "Needs to update %s\n")
            active_prop_to_update;
          List.iter
            (fun p -> p |> CS.cil_prop_to_string |> E.log "May needs to update %s\n")
            inactive_prop_to_update;

          let update_calls =
            List.map (fun p -> mkUpdateFunctionCall p loc) active_prop_to_update
          in
          (* Will need a statement... and insert it before the instruction... *)
          (* Explore instr by hand from every statement ??*)
          (* let may_update_calls = *)
          (*   List.map (fun p -> mkMayUpdateFunctionCall p loc) inactive_prop_to_update *)
          (* in *)
          let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
          let ae = mkFunctionCall instrFun.atomic_end None [] loc in
          let tr = mkTransitionFunctionCall loc in
          ChangeTo (ab::i::(update_calls@[tr; ae]))
        end
      | _ -> SkipChildren
end

let process_function (fd: fundec) (l: location) : unit =
  let vis = new addInstrumentationVisitor in
  ignore(visitCilFunction vis fd)

let only_functions (o: fundec -> location -> unit) (g: global) =
  match g with
  | GFun (fd, l) -> o fd l
  | _ -> ()

let add_instrumentation (f: file) (cs: CS.cil_prop list) =
  (* Initialize the proposition state *)
  cilSpec.CS.disabled_props <- cs;

  (* Create varinfo for function used in instrumentation *)
  instrFun.transition <- findOrCreateFunc f transition_fun_str (mkFunctionType voidType []);
  instrFun.atomic_begin <- findOrCreateFunc f atomic_begin_fun_str (mkFunctionType voidType []);
  instrFun.atomic_end <- findOrCreateFunc f atomic_end_fun_str (mkFunctionType voidType []);

  iterGlobals f (only_functions process_function)
