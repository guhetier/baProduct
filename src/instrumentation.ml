
open Cil

module E = Errormsg
module S = Specification
module CS = Cilspecification

let cilSpec : CS.cil_prop_state = CS.empty

let trans_fun_str = "__ltl2ba_transition"
let trans_fun = ref (makeVarinfo false "_dummy" voidType)

let mkFunctionType (rt: typ) (args: (string * typ) list) : typ =
  TFun (rt, (Some (List.map (fun a -> (fst a, snd a, [])) args)), false, [])

let mkFunctionCall (func: varinfo) (loc: location) : stmt =
    mkStmt (Instr [Call (None, Lval(Var func, NoOffset), [], loc)])

let get_label_name (l: label) =
  match l with
    | Label (n, _, _) -> n
    | _ -> assert false

let props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_enabled_props cilSpec)

let is_true_label (l: label) =
  match l with
  | Label(_, _, true) -> true
  | _ -> false

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

class addInstrumentationVisitor = object(self)
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    (* Set startingProps to their expr value right before this statement *)
    let endingProps = List.map remove_ending_prop labels |> List.flatten in
    (* Set endingsProps to their default value right after this statement *)

    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "Starting %s\n") startingProps;
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "Ending %s\n") endingProps;
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "%s is enabled\n") (CS.get_enabled_props cilSpec);
    List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "%s is disabled\n") (CS.get_disabled_props cilSpec);

    let action s =
      let c = mkFunctionCall !trans_fun locUnknown in
      let b = mkBlock (c::[s]) in
      mkStmt (Block b)
    in
    ChangeDoChildrenPost (s, action)

  method vinst (i: instr) =
    (
      match i with
      | Set ((Var v, NoOffset), _, l) -> E.log "%a : Set %a\n" d_loc l d_instr i;
        List.iter (fun p -> p |> CS.cil_prop_to_string |> E.log "Needs to update %s\n")
        (props_to_update v);
      | _ -> ()
    );
    SkipChildren
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
