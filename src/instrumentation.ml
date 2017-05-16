
open Cil
open Baproductutils

module E = Errormsg
module S = Specification
module CS = Cilspecification

let cilSpec : CS.cil_prop_state = CS.empty

let trans_fun_str = "__ltl2ba_transition"
let trans_fun = ref (makeVarinfo false "_dummy" voidType)

(* Return the list of properties that must be recomputed after the
   modification of the variable `var`.
   They are the properties that depends on `var` and are enabled.
*)
let props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_enabled_props cilSpec)

let add_starting_prop (l: label) =
  let newProps = List.filter
      (fun p -> (CS.get_start_label p) = (get_label_name l))
      (CS.get_disabled_props cilSpec)
  in
  CS.enable_props cilSpec newProps;
  newProps

let remove_ending_prop (l:label) =
  let endingProps = List.filter
      (fun p -> (CS.get_end_label p) = (get_label_name l))
      (CS.get_enabled_props cilSpec)
  in
  CS.disable_props cilSpec endingProps;
  endingProps

let mkUpdateFunctionCall (prop: CS.cil_prop) (loc: location) =
  mkFunctionCall prop.prop_fun (Some prop.truth_var) prop.prop_params loc

let mkTransitionFunctionCall (loc: location) =
   mkFunctionCall !trans_fun None [] loc

let mkSetToDefaultInstr (prop: CS.cil_prop) (loc: location) =
  Set((Var(prop.truth_var), NoOffset), (mkBool prop.default_val), loc)

class addInstrumentationVisitor = object(self)
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    (* Set startingProps to their expr value right before this statement *)
    let endingProps = List.map remove_ending_prop labels |> List.flatten in
    (* Set endingsProps to their default value right after this statement *)

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
      let trans_call = mkTransitionFunctionCall loc in

      let action (s: stmt) =
        let instrStmt = mkStmt (Instr (init_start @ (init_end @ [trans_call])))
        in
        let b = mkBlock (instrStmt::[s]) in
        mkStmt (Block b)
      in
      ChangeDoChildrenPost (s, action)

  method vinst (i: instr) =
      match i with
      | Set ((Var v, NoOffset), _, loc) ->
        E.log "%a : Set %a\n" d_loc loc d_instr i;
        let to_update = props_to_update v in
        if to_update = [] then
          SkipChildren
        else begin
          List.iter
            (fun p -> p |> CS.cil_prop_to_string |> E.log "Needs to update %s\n")
            to_update;

          let update_calls =
            List.map (fun p -> mkUpdateFunctionCall p loc) to_update in
          ChangeTo (i::(update_calls@[mkTransitionFunctionCall loc]))
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

let add_instrumentation (f: file) (spec: Specification.spec) =
  (* specInfo.spec <- spec; *)
  let cs = CS.from_spec f spec in
  cilSpec.disabled_props <- cs.disabled_props;
  trans_fun := findOrCreateFunc f trans_fun_str (mkFunctionType voidType []);
  iterGlobals f (only_functions process_function)
