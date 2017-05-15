
open Cil

module E = Errormsg
module S = Specification

type spec_info = {
  mutable spec : S.spec;
  mutable enabled_props : S.atomic_prop list;
}

let specInfo = {
  spec = S.emptySpec;
  enabled_props = [];
}

let get_label_name (l: label) =
  match l with
    | Label (n, _, _) -> n
    | _ -> assert false

let is_prop_parameter (var: varinfo) (prop: S.atomic_prop) =
  let compare (p: S.param) =
    match p with
    | Global n -> var.vglob && n = var.vname
    | Local (_, n) -> not var.vglob && n = var.vname
  in
  List.exists compare prop.params

let props_to_update (var: varinfo) =
  List.filter (is_prop_parameter var) specInfo.enabled_props

let is_true_label (l: label) =
  match l with
  | Label(_, _, true) -> true
  | _ -> false

let add_starting_prop (l: label) =
  let newProps = List.filter
      (fun p -> (S.get_prop_start p) = (get_label_name l))
      specInfo.spec.props
  in
  specInfo.enabled_props <- List.append specInfo.enabled_props newProps;
  newProps

let remove_ending_prop (l:label) =
  let endingProps, remaining = List.partition
      (fun p -> (S.get_prop_end p) = (get_label_name l))
      specInfo.enabled_props
  in
  specInfo.enabled_props <- remaining;
  endingProps

class addInstrumentationVisitor = object(self)
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    (* Set startingProps to their expr value right before this statement *)
    let endingProps = List.map remove_ending_prop labels |> List.flatten in
    (* Set endingsProps to their default value right after this statement *)

    List.iter (fun p -> p |> S.atomic_prop_to_string |> E.log "Starting %s\n") startingProps;
    List.iter (fun p -> p |> S.atomic_prop_to_string |> E.log "Ending %s\n") endingProps;

    DoChildren

  method vinst (i: instr) =
    (
      match i with
      | Set ((Var v, NoOffset), _, l) -> E.log "%a : Set %a\n" d_loc l d_instr i;
        List.iter (fun p -> p |> S.atomic_prop_to_string |> E.log "Needs to update %s\n")
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
  specInfo.spec <- spec;
  iterGlobals f (only_functions process_function)
