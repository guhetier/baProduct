
module J = Yojson.Basic
module E = Errormsg
module H = Hashtbl

open J.Util

type label = int

type transition = {
  dest: label;
  pos: string list;
  neg: string list
}

and state = {
  label: label;
  final: bool;
  trans: transition list;
}

type automaton = {
  nb_states: int;
  init_state: state;
  states: (label, state) H.t;
}

let get_sate (a: automaton) (l: label) =
  H.find a.states l

let get_dest (a: automaton) (t: transition) =
  H.find a.states t.dest

let json_to_trans (json: J.json): transition =
  {
    dest = json |> member "dest" |> to_int;
    pos = List.map to_string (json |> member "pos" |> to_list);
    neg = List.map to_string (json |> member "neg" |> to_list);
  }

let json_to_state (json: J.json): state =
  {
    label = json |> member "label" |> to_int;
    final = json |> member "final" |> to_bool;
    trans = List.map json_to_trans (json |> member "trans" |> to_list);
  }

let from_json (json: J.json) : automaton =
  let nb_states = json |> member "nb_state" |> to_int in
  let state_list = List.map json_to_state (json |> member "states" |> to_list)
  in
  let states = H.create nb_states in
  List.iter (fun s -> H.add states s.label s) state_list;

  let init_label = json |> member "init_sate" |> to_int in
  let init = try H.find states init_label with Not_found ->
      E.s (E.error "Invalid label for initial state : %d" init_label)
  in
  {
    nb_states = nb_states;
    init_state = init;
    states = states
  }

let from_file (filename: string) : automaton =
  let json = try J.from_file filename
    with _ -> E.s (E.error "Couldn't parse the file %s" filename)
  in from_json json
