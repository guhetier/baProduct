open Cil
open Baproductutils
module E = Errormsg
module S = Specification
module O = Option
module H = Hashtbl

open Automaton
module B = Buffer

let buffer = B.create 1000
let b = Format.formatter_of_buffer buffer
let printb = Format.fprintf

(* TODO :
   - Créer les variables contenant les propositions atomiques.
   Cela doit être fait à partir de la spécification et ajouté dans l'AST
   cil.
   - Créer des prototypes dans CIL pour toutes les fonctions écrites à la main
*)

(* Check if, given a proposition state, it is possible to use the edge e *)
let is_edge_valid (a: automaton) (e: edge_t) (sym_state: int) =
  let set_bit = (fun id s -> id lor (1 lsl (H.find a.symbols s))) in
  let sym_pos = List.fold_left set_bit 0 e.pos
  and sym_neg = List.fold_left set_bit 0 e.neg in
  ((sym_pos land sym_state) = sym_pos)
  && (sym_neg land sym_state = 0)

(* For each vertex of the list, indicate if it can reach a final cycle in
   the automaton *)
let reach_final_cyle (g: A.t) (v_sorted: A.vertex list) =
  let closure = OperAuto.transitive_closure ~reflexive:false g in
  let is_final v =
    v.final && A.mem_edge closure v v
  in
  let vertex_reach_final_cycle v =
    if is_final v then
      true
    else
      A.fold_succ (fun v r -> if is_final v then true else r) closure v false
  in
  List.map vertex_reach_final_cycle v_sorted

(* Compute surely accepting states.
   Use the algorithm from ltl2c, by Jeremie Morse *)
let surely_accepting_states (a: automaton) (v_sorted: A.vertex list) =

  (* For each state of the automaton, compute a set of set of edges.
     Each set of edge represent a choice of edges it is possible to
     use given an atomic proposition valuation. Only smaller sets (for the
     inclusion) are interesting, so we can delete the bigger ones.
  *)
  let pessimistic_transitions = Array.make a.nb_states VSetSet.empty in
  let add_to_setset ss v =
    let inserted = ref false in
    let ss = VSetSet.map (fun s ->
        if VSet.subset s v then
          (inserted := true; s)
        else if VSet.subset v s then
          (inserted := true; v)
        else
          s
      ) ss in
    if not !inserted then
      VSetSet.add v ss
    else
      ss
  in

  (* For each atomic propositions state, compute the set of vertexes that are
     directly accessible using a valid transition.
  *)
  for sym_state = 0 to (1 lsl a.nb_sym) do
    let valid_neighbour = Array.make a.nb_states VSet.empty in
    A.iter_edges_e (fun (s, e, d) ->
        if is_edge_valid a e sym_state then
          valid_neighbour.(s.id) <- (VSet.add d valid_neighbour.(s.id))
      ) a.graph;

    Array.iteri (fun i s ->
        pessimistic_transitions.(i) <-
          add_to_setset pessimistic_transitions.(i) s)
      valid_neighbour;
  done;

  (* The tree following recursive functions are use to find the set of vertices
     that are reachable from a vertex, whatever the evolution of the
     atomic propositions.
  *)
  (* First, whatever the atomic proposition, we need to reach the state :
     it has to be reachable using every set of transition (each set of
     transition can be seen as the transitions that are valid for a specific
     state of the atomic propositions *)
  let rec pessimistic_reach depth (setset: VSetSet.t) : VSet.t =
    List.fold_left VSet.inter
      (VSetSet.choose setset)
      (List.map (optimisitic_reach depth) (VSetSet.elements setset))

  (* Once the set is chosen, we can choose every transition in it, they are all
   valid. So we take the best one (union) *)
  and optimisitic_reach depth (set: VSet.t) : VSet.t =
    List.fold_left VSet.union
      VSet.empty
      (List.map (dept_increase depth) (VSet.elements set))

  (* Recursion as long as we have not ended the exploration (the number
     of state in the graph is an upper bound) *)
  and dept_increase depth (vertex: A.vertex) : VSet.t =
    match depth with
    | 0 -> VSet.singleton vertex
    | _ -> pessimistic_reach (depth-1) pessimistic_transitions.(vertex.id)
  in

  (* For every state, compute the set of state it can always reach *)
  let pess_reach = Array.make a.nb_states VSet.empty in
  for id_state = 0 to (a.nb_states-1) do
    pess_reach.(id_state) <-
      pessimistic_reach a.nb_states pessimistic_transitions.(id_state)
  done;

  (* Compute the set of set that are in a final pessimistic cycle *)
  let  final_cycle = List.filter
      (fun v -> VSet.mem v pess_reach.(v.id))
      v_sorted in
  let final_cycle = VSet.of_list final_cycle in
  List.map
    (fun v -> VSet.inter final_cycle pess_reach.(v.id) != VSet.empty)
    v_sorted


(* Print the treatment of one edge in the transition function *)
let print_c_transition_edge (a: automaton) (num_edge: int) (edge: A.edge) =
  let s, e, d = edge in
  let add_prefix = List.map (fun s -> "_ltl2ba_atomic_" ^ s) in
  let guard = match e with
    | {pos = []; neg=[]} -> "1"
    | _ -> set_to_c_string (add_prefix e.pos) (add_prefix e.neg)
  in
  printb b "@[<v 2>";
  if num_edge != 0 then printb b "\ else\ ";
  printb b "if (choice == %d) {@ " num_edge;
  printb b "%s(%s);@ " !O.checker_assume guard;
  printb b "_ltl2ba_state_var = %d;@]@ " d.id;
  printb b "}"


(* Print the treatment of one vertex in the transition function *)
let print_c_transition_vertex (a: automaton) (v: A.vertex) =
  printb b "@[<v 2>case %d:@ " v.id;
  let out_e = A.succ_e a.graph v in
  List.iteri (fun i e -> print_c_transition_edge a i e) out_e;

  printb b "@[<v 2>\ else {@ %s(0);@]@ }@ " !O.checker_assume;
  printb b "break;@]@ "


(* Print the transition function of the automaton *)
let print_c_transition_function (a: automaton) =
  printb b "@[<v 2>void _ltl2ba_transition() {@ ";
  (match a.nb_states with
   | 0 -> printb b "%s(0);@ " !O.checker_assume
   | _ ->
     printb b "int choice = %s(); @ " !O.checker_non_det;
     printb b "switch (_ltlba_state_var) {@ ";

     A.iter_vertex (print_c_transition_vertex a) a.graph;

     printb b "}"
  );
  printb b "@]";
  printb b "@ }@.\n"


(* Print the result function of the automaton *)
let print_c_conclusion_function (a: automaton) =
  printb b "@[<v 2>void _ltl2ba_result() {@ ";
  printb b "int reject_sure = _ltl2ba_surely_reject[_ltl2ba_state_var];@ ";
  printb b "%s(!reject_sure, \"ERROR SURE\");@ @ " !O.checker_assert;

  printb b "int id = _ltl2ba_sym_to_id();@ ";
  printb b "int accept_stutter =\
            _ltl2ba_stutter_accept[id * %d + ltl2ba_state_var];@ " a.nb_states;
  printb b "%s(!accept_stutter, \"ERROR MAYBE\");@ " !O.checker_assert;
  printb b "%s(accept_stutter, \"VALID MAYBE\");@]@ " !O.checker_assert;
  printb b "}@.\n"


(* Print the function that compute the id of a configuration of
   atomic propositions
*)
let print_c_sym_to_id_function (a: automaton) =
  printb b "@[<v 2>int _ltl2ba_sym_to_id() {@ ";
  printb b "int id = 0;@ ";
  H.iter (fun s i -> printb b "id |= (_ltl2ba_atomic_%s << %i);@ " s i)
    a.symbols;
  printb b "return id;@]@ ";
  printb b "@ }@.\n"


(* Print the array of surely rejecting states.
   These states correspond to state from which it is impossible to
   reach an accepting cycle.
   vertices_sorted is the list of all the vertices of a, sorted by id
*)
let print_c_surely_reject_table (a: automaton) (v_sorted: A.vertex list) =
  printb b "@[int _ltl2ba_surely_reject[%i] = {@," a.nb_states;
  let state_accept = List.map
      (function | true -> "0" | false -> "1")
      (reach_final_cyle a.graph v_sorted)
  in
  printb b "%s" (String.concat ", " state_accept);
  printb b "@]};@.\n"


(* For every state of the automaton and every configuration of atomic
   propositions, compute if it is possible to reach a final cycle without
   changing the configuration of propositions
*)
let print_c_stutter_accept_table (a: automaton) (v_sorted: A.vertex list) =
  let stutter_accept_sym (sym_state: int) =
    let subgraph = A.fold_edges_e
        (fun (s, e, d) sg -> if is_edge_valid a e sym_state then
                       A.add_edge_e sg (s, e, d) else sg)
        a.graph A.empty
    in
  let dotfile = open_out_bin (Printf.sprintf "mygraph_%i.dot" sym_state) in
  let closure = OperAuto.transitive_closure ~reflexive:false subgraph in
  Automaton.Dot.output_graph dotfile closure;

    reach_final_cyle subgraph v_sorted
  in
  printb b "@[<v 2>int _ltl2ba_stutter_accept[%i] = {"
    (a.nb_states * (1 lsl a.nb_sym));

  for sym_state = 0 to (1 lsl a.nb_sym) do
    let state_accept = List.map (function | true -> "1" | false -> "0")
      (stutter_accept_sym sym_state)
    in
    printb b "@ %s," (String.concat ", " state_accept);
  done;
  printb b "@]@ };@.\n"


(* Print the array of surely accepting states.
   These states correspond to states from which a final state can be
   reach whatever the future proposition values *)
let print_c_surely_accept_table (a: automaton) (v_sorted: A.vertex list) =
  printb b "@[int _ltl2ba_surely_accept[%i] = {@," a.nb_states;
  let state_accept = List.map
      (function | true -> "0" | false -> "1")
      (surely_accepting_states a v_sorted)
  in
  printb b "%s" (String.concat ", " state_accept);
  printb b "@]};@.\n"


(* Print in a string an automaton in C, as well as a function to analyze
   results *)
let print_c_automaton (a: automaton) =
  let v_list = A.fold_vertex List.cons a.graph [] in
  let v_list = List.sort Node.compare v_list in
  print_c_transition_function a;
  print_c_surely_reject_table a v_list;
  print_c_surely_accept_table a v_list;
  print_c_stutter_accept_table a v_list;
  print_c_sym_to_id_function a;
  print_c_conclusion_function a;

  B.contents buffer


(* Insert prototype for every function added in the automaton *)
let insert_automaton_prototype (cil: file) =
  let void_void_type = Baproductutils.mkFunctionType voidType [] in
  let int_void_type = Baproductutils.mkFunctionType intType [] in
  let _ = findOrCreateFunc cil "_ltl2ba_result" void_void_type in
  let _ = findOrCreateFunc cil "_ltl2ba_transition" void_void_type in
  let _ = findOrCreateFunc cil "_ltl2ba_sym_to_id" int_void_type in
  ()


(* Create the automaton corresponding to the specification.
   Returns the code of the automaton in a string.
*)
let create_automaton (a: automaton) (cil: file) (spec: S.spec) =
  let c_automaton = print_c_automaton a in
  insert_automaton_prototype cil;
  c_automaton


(********* Insert a call to the result function at the end of the main *********)

class insert_result_visitor (sresult) = object(self)
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | Return _ -> let ns = Block (mkBlock [sresult; s]) |> mkStmt in
      ChangeTo ns
    | _ -> DoChildren
end


let insert_end_main rcal g =
  match g with
  | GFun (f, _) when f.svar.vname = "main" ->
    let vis = new insert_result_visitor rcal in
    ignore(visitCilFunction vis f);
  | _ -> ()


let add_result f =
  let f_void_type = Baproductutils.mkFunctionType voidType [] in
  let fr = findOrCreateFunc f "_ltl2ba_result" f_void_type in
  let ab = findOrCreateFunc f !O.checker_atomic_begin f_void_type in
  let ae = findOrCreateFunc f !O.checker_atomic_end f_void_type in
  let fCalls = List.map (fun f -> mkFunctionCall f None [] (locUnknown))
      [ab; fr; ae]
  in
  iterGlobals f (insert_end_main (mkStmt (Instr fCalls)))
