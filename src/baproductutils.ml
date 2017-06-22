open Cil

(* Create a function type from the return type and the arguments type *)
let mkFunctionType (rt: typ) (args: (string * typ) list) : typ =
  TFun (rt, (Some (List.map (fun a -> (fst a, snd a, [])) args)), false, [])

(* Create a function call from varinfo instead of lvalues.*)
let mkFunctionCall (func: varinfo) (destvar: varinfo option) (args: varinfo list)
    (loc: location) : instr =
  let destLVal = match destvar with
  | None -> None
  | Some vi -> Some (Var vi, NoOffset)
  in
  let argLVal = List.map (fun v -> Lval(Var v, NoOffset)) args in
  Call (destLVal, Lval(Var func, NoOffset), argLVal, loc)

(* Indicate if a label was initially in the code or if it has been added by
   CIL *)
let is_true_label (l: label) =
  match l with
  | Label(_, _, true) -> true
  | _ -> false

(* Return the text of a real label *)
let get_label_name (l: label) =
  match l with
    | Label (n, _, _) -> n
    | _ -> assert false

(* Construct a cil boolean *)
let mkBool (b: bool) =
  kinteger IBool (if b then 1 else 0)

(* Construct an integer init *)
let mkIntInit (i: int) =
  {init=Some(SingleInit(integer i))}

(* Filter global to apply `o` on functions only *)
let only_functions (o: fundec -> location -> unit) (g: global) =
  match g with
  | GFun (fd, l) -> o fd l
  | _ -> ()

(* Construct a string representing the condition of a transition of the
   automaton *)
let set_to_c_string (pos: string list) (neg: string list) =
  let neg = List.map (fun s -> "!"^s) neg in
  String.concat " && " (pos @ neg)
