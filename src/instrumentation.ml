
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
  let argLVal = List.map (fun v -> Lval(Mem (Lval(Var v, NoOffset)), NoOffset))
  (CS.get_pointer_params prop) in
  Call (
    Some (Var (CS.get_truth_var prop), NoOffset),
    Lval(Var (CS.get_fun prop), NoOffset),
    argLVal,
    loc)

(* Build a statement `if` to update a truth value from the proposition
   function iff the proposition is currently active
*)
let mkMayUpdateFunctionCall (prop: CS.cil_prop) (loc: location) =
  let uc = mkUpdateFunctionCall prop loc in
  let ifBlock = mkBlock [mkStmtOneInstr uc] in
  let cond = Lval((Var (CS.get_state_var prop), NoOffset)) in
  mkStmt (If(cond, ifBlock, mkBlock [], loc))


(* Build a call to the automaton transition function *)
let mkTransitionFunctionCall (loc: location) =
   mkFunctionCall instrFun.transition None [] loc

(* Build the instruction that set a truth value to its default value *)
let mkSetToDefaultInstr (prop: CS.cil_prop) (loc: location) =
  Set((Var((CS.get_truth_var prop)), NoOffset),
      (mkBool (CS.get_default prop)), loc)

let mkSetPropState (prop: CS.cil_prop) (loc: location) (s: bool) =
  Set((Var((CS.get_state_var prop)), NoOffset), mkBool s, loc)

let mkSetPointer (var: CS.cil_prop_param) (loc: location) =
  Set((Var var.pointer, NoOffset), mkAddrOf (Var var.var, NoOffset), loc)

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

(* This visitor instrument code to build the product of a program
   and a Büchi automaton.
   Whenever a label marking the beginning or the end of a proposition
   active zone is reached, it updates the proposition value and triggers
   a transition. It also update and initialize variables used in other instrumentations :
   whether a proposition is in its active zone and pointers to variables used in
   propositions.
*)
class instrumentZoneChangeVisitor = object(self)
  inherit nopCilVisitor

  (* When reaching a label, a proposition may be enabled / disabled.
     Update property status and insert code to update truth values,
     pointers to variables, and property state variable.
  *)
  method vstmt (s: stmt) =
    (* Actualize property status from labels *)
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    let endingProps = List.map remove_ending_prop labels |> List.flatten in

    match startingProps, endingProps with
    | [], [] -> DoChildren
    | _ ->
      let loc = get_stmtLoc s.skind in
      (* Set the truth value of a starting property *)
      let init_start =
        List.map (fun p -> mkUpdateFunctionCall p loc) startingProps in
      (* Set the truth value of an ending property *)
      let init_end =
        List.map (fun p -> mkSetToDefaultInstr p loc) endingProps in
      (* Update pointers to variables used by a starting proposition *)
      let init_var_pointer =
        let vars = List.flatten (List.map (fun p -> p.CS.prop_params) startingProps) in
        List.map (fun p -> mkSetPointer p loc) vars in
      (* Update the flag stating the proposition is active for starting propositions *)
      let set_state_true =
        List.map (fun p -> mkSetPropState p loc true) startingProps in
      (* Update the flag stating the proposition is active for ending propositions *)
      let set_state_false =
        List.map (fun p -> mkSetPropState p loc false) endingProps in
      (* The transition function *)
      let tr = mkTransitionFunctionCall loc in
      (* atomic_begin *)
      let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
      (* atomic_end *)
      let ae = mkFunctionCall instrFun.atomic_end None [] loc in

      let action (s: stmt) =
        let instrStmt = mkStmt (Instr (
            [tr; ae] |> (@) set_state_true |> (@) init_start |> (@) init_var_pointer
            |> (@) set_state_false |> (@) init_end |> (@) [ab]))
        in
        let b = mkBlock (instrStmt::[s]) in
        mkStmt (Block b)
      in
      ChangeDoChildrenPost (s, action)
end

(* This visitor instrument variable assignation to build the product of
   a program code with a Büchi automaton.
   If an instruction may change the truth value of an atomic proposition,
   it insert code to update the truth value of the proposition and triggers
   a transition in the automaton.
*)
class instrumentVarChangeVisitor = object(self)
  inherit nopCilVisitor

  (* As instrumentation may requires to add statement, one cannot use the
     visitor directly to the instruction level. Instead, we use side effect to
     build a new statement from the instruction exploration and replace the
     statement at the end of the visit. The list of statements to insert is
     stored in `nstmts` in reverse order.
  *)
  val mutable nstmts: stmt list = []

  method private addStmt (s: stmt) : unit =
    nstmts <- s::nstmts

  method private addInst (i: instr) : unit =
    match nstmts with
    | ({skind = Instr l; _} as lstmt)::_ -> let ll = l@[i] in lstmt.skind <- Instr ll
    | _ -> nstmts <- (mkStmtOneInstr i)::nstmts

  method vstmt (s: stmt) =
    (* Update the active propositions *)
    let labels = List.filter is_true_label s.labels in
    ignore(List.map add_starting_prop labels);
    ignore(List.map remove_ending_prop labels);

    match s.skind with
    (* Whenever an instruction list is found, it may be replaced by several
       statements
    *)
    | Instr ll ->
      nstmts <- [];
      let action s =
        match nstmts with
        | [a] -> a
        | _ -> mkStmt (Block (mkBlock (List.rev nstmts)))
      in ChangeDoChildrenPost (s, action)
    | _ -> DoChildren

  method vinst (i: instr) =
    match i with
    | Set ((Var v, NoOffset), _, loc) ->
      (* We are currently in the active zone from theses properties :
         every change to a property's parameter must trigger an update
      *)
      let active_prop_to_update = active_props_to_update v in
      (* We are not in the active zone of theses properties, but the variable
         that is modified is global, so the modification may have an impact on
         the property value while it is active in another thread :
         we need to trigger an update iff the property is active at the instant
      *)
      let inactive_prop_to_update = if v.vglob then
          inactive_props_to_update v
        else []
      in
      if active_prop_to_update = [] && inactive_prop_to_update = [] then begin
        (* No update needed : just copy the instruction *)
        self#addInst i;
        SkipChildren
      end
      else begin
        (* Updates are needed :
           - create calls to update functions
           - set an atomic block around the instruction to enforce the
             transition as soon as the instruction is executed
        *)
        (* Update for prop in their validity zone *)
        let update_calls = List.map
            (fun p -> mkUpdateFunctionCall p loc) active_prop_to_update
        in
        (* Update for prop not their validity zone *)
        let may_update_calls =
          List.map (fun p -> mkMayUpdateFunctionCall p loc) inactive_prop_to_update
        in
        (* atomic_begin *)
        let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
        (* atomic_end *)
        let ae = mkFunctionCall instrFun.atomic_end None [] loc in
        (* transition function *)
        let tr = mkTransitionFunctionCall loc in

        (* build the new statement(s) *)
        self#addInst ab;
        self#addInst i;
        List.iter self#addInst update_calls;
        List.iter self#addStmt may_update_calls;
        self#addInst tr;
        self#addInst ae;
        SkipChildren
      end
    | _ -> self#addInst i;
      SkipChildren
end

let process_function (cs: CS.cil_prop list) (fd: fundec) (l: location) : unit =
  (* Initialize the proposition state *)
  cilSpec.CS.disabled_props <- cs;

  let visZone = new instrumentZoneChangeVisitor in
  ignore(visitCilFunction visZone fd);

  (* All active zone that have been opened in a function should have been closed
     before its end. It is essential for the second visitor to compute active zone
     correctly.
  *)
  if cilSpec.CS.enabled_props <> [] then
       E.s (E.error "Active zone for atomic propositions XXX TODO have been opened in\
                     %a but have not been closed");

  let visVar = new instrumentVarChangeVisitor in
  ignore(visitCilFunction visVar fd)

let add_instrumentation (f: file) (cs: CS.cil_prop list) =

  (* Create varinfo for function used in instrumentation *)
  instrFun.transition <- findOrCreateFunc f transition_fun_str (mkFunctionType voidType []);
  instrFun.atomic_begin <- findOrCreateFunc f atomic_begin_fun_str (mkFunctionType voidType []);
  instrFun.atomic_end <- findOrCreateFunc f atomic_end_fun_str (mkFunctionType voidType []);

  iterGlobals f (only_functions (process_function cs))
