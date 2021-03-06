
open Cil
open Baproductutils

module E = Errormsg
module CS = Cilspecification
module O = Option

(******************* Global parameters ****************************)
let cilSpec : CS.cil_prop_state = CS.empty

let transition_fun_str = "_ltl2ba_transition"

let dummy_fun = makeVarinfo false "_dummy" voidType

(* Function used for instrumentation *)
type instr_fun = {
  mutable transition : varinfo; (* Transition function in the Büchi automaton *)
  mutable atomic_begin : varinfo; (* Begin of an atomic block for the model
                                     checker *)
  mutable atomic_end : varinfo; (* End of an atomic block for the model
                                   checker *)
}

let instrFun = {
  transition = dummy_fun;
  atomic_begin = dummy_fun;
  atomic_end = dummy_fun;
}

(****************** Build-helper *******************************)

(* Build the instruction to update a truth value from the proposition
   function. Global pointers to arguments are used, so the call is valid
   in all contexts as long as parameters are in the memory.*)
let mkUpdateFunctionCall (prop: CS.cil_prop) (loc: location)=
  let argLVal = List.map
      (fun v -> match CS.get_pointer v with
         (* For a global variable, there is no pointer for an indirect access *)
         | None -> Lval(Var (CS.get_var v), NoOffset)
         (* For a local variable, we need to use a pointer for an indirect access *)
         | Some(p) -> Lval(Mem (Lval(Var p, NoOffset)), NoOffset))
      prop.CS.prop_params in
      (* (CS.get_pointer_params prop) in *)
  Call (
    Some (Var (CS.get_truth_var prop), NoOffset),
    Lval (Var (CS.get_fun prop), NoOffset),
    argLVal,
    loc)


(* Build a statement `if` to update a truth value from the proposition
   function iff the proposition is currently active *)
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

(* Build an instruction that set a proposition state variable to `s` *)
let mkSetPropState (prop: CS.cil_prop) (loc: location) (s: bool) =
  Set((Var((CS.get_state_var prop)), NoOffset), mkBool s, loc)


(* Build an instruction that set a parameter global pointer to the address of
   the parameter *)
let mkSetPointer (var: CS.cil_prop_param) (loc: location) =
  let v = match CS.get_pointer var with
    | Some v -> v
    | None -> E.s (E.error "The variable %s has no associated global pointer,\
                            but its initialization is requested."
                     (let v =  CS.get_var var in v.vname))
  in
  Set((Var v, NoOffset),
      mkAddrOf (Var (CS.get_var var), NoOffset), loc)


(**************** Prop state manipulation functions *************)

(* Return the list of the properties that are in their active zone and that
   depends on `var`. *)
let active_props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_enabled_props cilSpec)


(* Return the list of the properties that are not in their active zone and that
   depends on `var`. *)
let inactive_props_to_update (var: varinfo) =
  List.filter (fun p -> CS.is_parameter p var) (CS.get_disabled_props cilSpec)


(* Gather atomic propositions that start at label `l` and mark them as
   enabled *)
let add_starting_prop (l: label) =
  let newProps = List.filter
      (fun p -> (CS.get_start_label p) = (get_label_name l))
      (CS.get_disabled_props cilSpec)
  in
  CS.enable_props cilSpec newProps;
  newProps


(* Gather atomic propositions that end at label `l` and mark them as disabled *)
let remove_ending_prop (l:label) =
  let endingProps = List.filter
      (fun p -> (CS.get_end_label p) = (get_label_name l))
      (CS.get_enabled_props cilSpec)
  in
  CS.disable_props cilSpec endingProps;
  endingProps


(*************** Instrumentation visitor ********************)

(* This visitor instrument the entrance and the exit of properties in their
   validity zone.
   Whenever a label marking the beginning or the end of a proposition
   active zone is reached, it updates the proposition value and triggers a
   transition. It also update and initialize variables used in other
   instrumentations : whether a proposition is in its active zone and pointers
   to variables used in propositions.*)
class instrumentZoneChangeVisitor = object(self)
  inherit nopCilVisitor

  (* When reaching a label, a proposition may be enabled / disabled.
     Update property status and insert code to update truth values,
     pointers to variables, and property state variable.*)
  method vstmt (s: stmt) =
    (* Actualize property status (enabled or disabled) from the statement
       labels *)
    let labels = List.filter is_true_label s.labels in
    let startingProps = List.map add_starting_prop labels |> List.flatten in
    let endingProps = List.map remove_ending_prop labels |> List.flatten in

    if !O.verbose && labels <> [] then begin
      let ls = String.concat ", " (List.map (fun l -> get_label_name l) s.labels)
      in
      E.log "%a : Label %s reached.\n" d_loc (get_stmtLoc s.skind) ls;
      if startingProps <> [] then begin
        let ps = String.concat ", "
            (List.map (fun p -> CS.get_name p) startingProps)
        in
        E.log "> Proposition %s is entering their validity zone.\n" ps;
      end;
      if endingProps <> [] then begin
        let pe = String.concat ", "
            (List.map (fun p -> CS.get_name p) endingProps)
        in
        E.log "> Proposition %s is exiting their validity zone.\n" pe
      end
    end;

    (* Build the instruction to insert before the statement *)
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
      (* TODO: Delete doublon here. Use a set or a better data structure ?? *)
      let init_var_pointer =
        let vars =
          List.filter (fun p -> let v = CS.get_var p in not v.vglob)
          (List.flatten (List.map (fun p -> p.CS.prop_params) startingProps))
        in
        List.map (fun p -> mkSetPointer p loc) vars in
      (* Update the flag stating the proposition is active for starting
         propositions *)
      let set_state_true =
        List.map (fun p -> mkSetPropState p loc true) startingProps in
      (* Update the flag stating the proposition is active for ending
         propositions *)
      let set_state_false =
        List.map (fun p -> mkSetPropState p loc false) endingProps in
      (* The transition function call *)
      let tr = mkTransitionFunctionCall loc in
      (* atomic_begin call *)
      let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
      (* atomic_end call *)
      let ae = mkFunctionCall instrFun.atomic_end None [] loc in

      let action (s: stmt) =
        let instrStmt = mkStmt (Instr (
            [tr; ae] |> (@) set_state_true |> (@) init_start
            |> (@) init_var_pointer |> (@) set_state_false |> (@) init_end
            |> (@) [ab]))
        in
        let b = mkBlock (instrStmt::[s]) in
        mkStmt (Block b)
      in
      ChangeDoChildrenPost (s, action)
end

(* This visitor instrument variable assignations that may have an impact on
   atomic propositions.
   It insert code to update the truth value of the proposition and triggers
   a transition in the automaton after every assignation to a parameter of
   an atomic proposition.*)
class instrumentVarChangeVisitor = object(self)
  inherit nopCilVisitor

  (* The instrumentation may requires to insert a statement before an
     instruction. CIL does not handle this case. We rebuilt every statement by
     hand when visiting the instructions and insert the newly created statements
     once all the instructions of the statement have been explored.
     `nstmts` contains the list of statements that will replace the current
     statement, in reverse order. *)
  val mutable nstmts: stmt list = []


  (* Add a statement to the change list *)
  method private addStmt (s: stmt) : unit =
    nstmts <- s::nstmts


  (* Add an instruction to the change list. If possible, adding an instruction
     will not create a new statement. *)
  method private addInst (i: instr) : unit =
    match nstmts with
    | ({skind = Instr l; _} as lstmt)::_ ->
      let ll = l@[i] in lstmt.skind <- Instr ll
    | _ -> nstmts <- (mkStmtOneInstr i)::nstmts


  (* Update active propositions and replace statements by those constructed while
     exploring instructions *)
  method vstmt (s: stmt) =
    (* Update the active propositions *)
    let labels = List.filter is_true_label s.labels in
    ignore(List.map add_starting_prop labels);
    ignore(List.map remove_ending_prop labels);

    (* Whenever an instruction list statement is found, replace it by the
       statement(s) built while exploring instructions. *)
    match s.skind with
    | Instr ll ->
      nstmts <- [];
      let action s =
        let ns = match nstmts with
        | [a] -> a
        | _ -> mkStmt (Block (mkBlock (List.rev nstmts)))
        in ns.labels <- s.labels;
        ns
      in ChangeDoChildrenPost (s, action)
    | _ -> DoChildren


  (* Build instrumentation for assignation instructions *)
  method vinst (i: instr) =
    match i with
    | Set ((Var v, NoOffset), _, loc) ->
      (* We are currently in the active zone from theses properties :
         every change to a property's parameter must trigger an update *)
      let active_prop_to_update = active_props_to_update v in

      (* We are not in the active zone of theses properties, but the variable
         that is modified is global, so the modification may have an impact on
         the property value while it is active in another thread :
         we need to trigger an update iff the property is active at the
         instant *)
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

        if !O.verbose then begin
          E.log "%a : Assignation to variable %s.\n" d_loc (loc) v.vname;
          if active_prop_to_update <> [] then begin
            let ps = String.concat ", "
                (List.map (fun p -> CS.get_name p) active_prop_to_update)
            in
            E.log "> Property %s will surely be updated.\n" ps;
          end;
          if inactive_prop_to_update <> [] then begin
            let pm = String.concat ", "
                (List.map (fun p -> CS.get_name p) inactive_prop_to_update)
            in
            E.log "> Property %s will maybe be updated.\n" pm;
          end
        end;

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
        let may_update_calls = List.map
            (fun p -> mkMayUpdateFunctionCall p loc) inactive_prop_to_update
        in
        (* atomic_begin call *)
        let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
        (* atomic_end call *)
        let ae = mkFunctionCall instrFun.atomic_end None [] loc in
        (* transition function call *)
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

(* Initialize proposition state and start visitors *)
let process_function (cs: CS.cil_prop list) (fd: fundec) (l: location) : unit =
  (* Initialize the proposition state *)
  cilSpec.CS.disabled_props <- cs;
  let (global_prop, disabled_prop) = List.partition CS.is_prop_global cs in
  cilSpec.CS.disabled_props <- disabled_prop;
  cilSpec.CS.enabled_props <- global_prop;

  let visZone = new instrumentZoneChangeVisitor in
  ignore(visitCilFunction visZone fd);

  (* All active zone that have been opened in a function should have been closed
     before its end. It is essential for the second visitor to compute active
     zone correctly.
  *)
  if (CS.get_enabled_props cilSpec) <> global_prop then begin
    let pnames = List.fold_left (fun ss p -> (CS.get_name p) ^ " " ^ ss)
        "" (CS.get_enabled_props cilSpec)
    in E.s (E.error "Active zone for atomic propositions %s have been \
                  opened in %s but have not been closed" pnames fd.svar.vname)
  end;

  let visVar = new instrumentVarChangeVisitor in
  ignore(visitCilFunction visVar fd)

(* Initialize global proposition at the beginning of the main method *)
let insert_global_prop_init (cs: CS.cil_prop list) (func: Cil.fundec) (loc: Cil.location) =

  if func.svar.vname = "main" then
    begin
      let global_prop = List.filter CS.is_prop_global cs in
      match global_prop with
      | [] -> ()
      | _ ->
        (* Call the proposition initialization function *)
        let init_glob_instr =
          List.map (fun p -> mkUpdateFunctionCall p loc) global_prop in
        (* The transition function call *)
        let tr = mkTransitionFunctionCall loc in
        (* atomic_begin call *)
        let ab = mkFunctionCall instrFun.atomic_begin None [] loc in
        (* atomic_end call *)
        let ae = mkFunctionCall instrFun.atomic_end None [] loc in
        let init_glob_stmt = mkStmt (Instr (ab::(init_glob_instr @ [tr; ae]))) in
        func.sbody <- mkBlock (init_glob_stmt :: [mkStmt (Block func.sbody)])
    end


(* Instrument every function of the file to build the product with the Büchi
   automaton *)
let add_instrumentation (f: file) (cs: CS.cil_prop list) =
  (* Create varinfo for function used in instrumentation *)
  instrFun.transition <-
    findOrCreateFunc f transition_fun_str (mkFunctionType voidType []);
  instrFun.atomic_begin <-
    findOrCreateFunc f !O.checker_atomic_begin (mkFunctionType voidType []);
  instrFun.atomic_end <-
    findOrCreateFunc f !O.checker_atomic_end (mkFunctionType voidType []);

  (* Instrument every function of the file *)
  iterGlobals f (only_functions (process_function cs));

  (* Initialize global proposition at the beginning of the main method *)
  iterGlobals f (only_functions (insert_global_prop_init cs))
