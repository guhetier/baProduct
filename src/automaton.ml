
module J = Yojson.Basic
module E = Errormsg
module H = Hashtbl

open J.Util

type node_t = {id: int; final: bool}
module Node = struct
  type t = node_t
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let hash v = Hashtbl.hash v.id
end

type edge_t = {pos: string list; neg: string list}
module Edge = struct
  type t = edge_t
  let compare = Pervasives.compare
  let default = {pos = []; neg = []}
end

module A = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

module VSet = Set.Make(Node)
module VSetSet = Set.Make(VSet)

(* module A = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge) *)

module OperAuto = Graph.Oper.P(A)

type automaton = {
  nb_states: int;
  nb_sym: int;
  symbols: (string, int) H.t;
  init_state: Node.t;
  graph: A.t;
}

let json_to_edge (json: J.json): Edge.t * Node.t =
  {
    pos = List.map to_string (json |> member "pos" |> to_list);
    neg = List.map to_string (json |> member "neg" |> to_list);
  },
  {
    id = json |> member "dest" |> to_int;
    final = false; (* Node are identified by their id only, so final
                      if not important here : all nodes have already been
                      inserted with correct values when adding the edges *)
  }


let json_to_state (json: J.json): Node.t =
  {
    id = json |> member "label" |> to_int;
    final = json |> member "final" |> to_bool;
  }


let json_to_state_trans (json: J.json): A.edge list =
  let vid = json |> member "label" |> to_int in
  let edges = List.map json_to_edge (json |> member "trans" |> to_list) in
  let v = {
    id = vid;
    final = json |> member "final" |> to_bool;
  } in
  List.map (fun (e,d) -> v, e, d) edges


exception NodeFound of Node.t


let from_json (json: J.json) : automaton =
  let graph = A.empty in

  (* Add states to the graph *)
  let states = List.map json_to_state (json |> member "states" |> to_list) in
  let graph = List.fold_left A.add_vertex graph states in

  (* Add edges to the graph *)
  let edges = List.map json_to_state_trans
      (json |> member "states" |> to_list) in
  let edges = List.flatten edges in
  let graph = List.fold_left A.add_edge_e graph edges in

  (* Get the first node back *)
  let init_id = json |> member "init_state" |> to_int in
  let init_state = try
      A.iter_vertex
        (fun v -> if v.id = init_id then raise (NodeFound v)) graph;
      raise Not_found;
    with | NodeFound v -> v
         | Not_found -> E.s (E.error "Initial node not found")
  in
  let nb_sym = json |> member "nb_sym" |> to_int in
  let symbols = H.create nb_sym in
  let sym_list = List.map to_string (json |> member "symbols" |> to_list) in
  List.iteri (fun i s -> H.add symbols s i) sym_list;
  {
    nb_states = json |> member "nb_state" |> to_int;
    nb_sym = nb_sym;
    symbols = symbols;
    init_state = init_state;
    graph = graph;
  }


let from_file (filename: string) : automaton =
  let json = try J.from_file filename
    with _ -> E.s (E.error "Couldn't parse the automaton %s" filename)
  in from_json json


(* The Dot module define a printer to output an automaton in dot *)
module Dot = Graph.Graphviz.Dot(struct
   include A
   let edge_attributes (a, e, b) =
     let l = Baproductutils.set_to_c_string e.pos e.neg in
     [`Label l]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes v =
     if v.final then
       [`Shape `Doublecircle]
     else
     [`Shape `Circle]
   let vertex_name v = string_of_int v.id
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)


(* Output an automaton in dot *)
let output_dot_automaton (out_c: out_channel) (a: automaton) =
  Dot.output_graph out_c a.graph
