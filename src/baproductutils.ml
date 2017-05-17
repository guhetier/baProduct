open Cil

(* Create a function type from the return type and the arguments type *)
let mkFunctionType (rt: typ) (args: (string * typ) list) : typ =
  TFun (rt, (Some (List.map (fun a -> (fst a, snd a, [])) args)), false, [])

(* Create a function call from the function type, the arguments and the variable
   in which the return value should be assigned
*)
let mkFunctionCall (func: varinfo) (destvar: varinfo option) (args: varinfo list)
    (loc: location) : instr =
  let destLVal = match destvar with
  | None -> None
  | Some vi -> Some (Var vi, NoOffset)
  in
  let argLVal = List.map (fun v -> Lval(Var v, NoOffset)) args in
  Call (destLVal, Lval(Var func, NoOffset), argLVal, loc)

(* Indicate if a label was initially in the code or
   if it has been added by CIL *)
let is_true_label (l: label) =
  match l with
  | Label(_, _, true) -> true
  | _ -> false

(* Return the text of a real label *)
let get_label_name (l: label) =
  match l with
    | Label (n, _, _) -> n
    | _ -> assert false

let mkBool (b: bool) =
  kinteger IBool (if b then 1 else 0)