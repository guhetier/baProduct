open Cil
open Baproductutils
module E = Errormsg
module S = Specification
module O = Option

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

let print_c_transition_edge (a: automaton) (num_edge: int) (edge: A.edge) =
  let s, e, d = edge in
  let add_prefix = List.map (fun s -> "_ltl2ba_atomic_" ^ s) in
  let guard = match e with
    | {pos = []; neg=[]} -> "1"
    | _ -> set_to_c_string (add_prefix e.pos) (add_prefix e.neg)
  in
  printb b "@[<v 2>if (choice == %d) {@ " num_edge;
  printb b "%s(%s);@ " !O.checker_assume guard;
  printb b "_ltl2ba_state_var = %d;@]@ " d.id;
  printb b "} else "

let print_c_transition_vertex (a: automaton) (v: A.vertex) =
  printb b "@[<v 2>case %d:@ " v.id;
  let out_e = A.succ_e a.graph v in

  List.iteri (print_c_transition_edge a) out_e;

  printb b "@[<v 2>@ %s(0);@]@ " !O.checker_assume;

  printb b "break;@]@ "

let print_c_transition_function (a: automaton) =
  printb b "@[<v 2>void _ltl2ba_transition() {@ ";
  (match a.nb_states with
  | 0 -> printb b "%s(0);@ " !O.checker_assume
  | _ ->
    printb b "int choice = %s(); @ " !O.checker_non_det;
    printb b "switch (_ltlba_state_var) {@ ";

    A.iter_vertex (print_c_transition_vertex a) a.graph;

    printb b "@[<v 2>default:@ ";
    printb b "break;@]";
    printb b "@ }"
  );
  printb b "@]";
  printb b "@ }\n\n"

let print_c_conclusion_function (a: automaton) =
  printb b "@[<v 2>void _ltl2ba_result() {@ ";
  printb b "int reject_sure = _ltl2ba_surely_reject[_ltl2ba_state_var];@ ";
  printb b "%s(!reject_sure, \"ERROR SURE\");@ @ " !O.checker_assert;

  printb b "int id = _ltl2ba_sym_to_id();@ ";
  printb b "int accept_stutter =\
            _ltl_stutter_accept[id * %d + ltl2ba_state_var];@ " a.nb_states;
  printb b "%s(!accept_stutter, \"ERROR MAYBE\");@ " !O.checker_assert;
  printb b "%s(accept_stutter, \"VALID MAYBE\");@]@ " !O.checker_assert;
  printb b "}@ "

let print_c_sym_to_id_function (a: automaton) =
  printb b "@[<v 2>void _ltl2ba_sym_to_id() {@ ";
  printb b "int id = 0;@ ";
  List.iteri (fun i s -> printb b "id |= (_ltl2ba_atomic_%s << %i);@ " s i)
    a.symbols;
  printb b "return id;@]@ ";
  printb b "@ }@ @ "

let print_c_automaton (a: automaton) =
  printb b "@[<v 2>";
  print_c_transition_function a;
  print_c_sym_to_id_function a;
  print_c_conclusion_function a;
  printb b "@.";

  B.output_buffer stdout buffer


(********* Insert a call to the result function at the end of the main *********)

class insert_result_visitor (fresult) = object(self)
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | Return _ -> let rc = mkFunctionCall fresult None [] (get_stmtLoc s.skind) in
      let ns = Block (mkBlock [mkStmtOneInstr rc; s]) |> mkStmt in
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
  let frtype = Baproductutils.mkFunctionType voidType [] in
  let fr = findOrCreateFunc f "_ltl2ba_result" frtype in
  iterGlobals f (insert_end_main fr)


(********* Initialize proposition truth value variables **********)
(* TODO : Do this correctly when building the automaton in Ocaml *)

let initGlobalVar (f: file) (name: string) (i: init) =
  let aux (g: global) =
    match g with
    | GVar(vi, ii, _) when vi.vname = name ->
      ii.init <- Some i
    | _ -> ()
  in
  List.iter aux f.globals

let initTruthVar (f: file) (spec: S.spec) =
  let aux (p: S.atomic_prop) =
    initGlobalVar f ("_ltl2ba_atomic_" ^ p.S.name) (SingleInit (mkBool p.S.default_val))
  in
  List.iter aux (spec.S.props)
