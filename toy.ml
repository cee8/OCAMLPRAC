exception Error of string

type var = string
type value = Int of int | Bool of bool


type state = (var * value) list

let rec lookup (s: state) (k: var) =
  match s with
  | [] -> raise (Error "Variable not found")
  | (v, n) :: tl -> if k = v then n else lookup tl k

let initial_state = []

let convert_to_int v = 
  match v with
  | Int n -> n
  | _ -> raise (Error "Expected an integer")

let convert_to_bool v =
  match v with
  | Bool b -> b
  | _ -> raise (Error "Expected a boolean")

type aexp = Nat of int 
          | Plus of (aexp * aexp)
          | Minus of (aexp * aexp)
          | Mult of (aexp * aexp)
          | AId of var
          
let rec eval_aexp (e : aexp)(s: state) =
  match e with
  | Nat n -> Int n
  | Plus (e1, e2) -> 
      let n1 = convert_to_int(eval_aexp e1 s) in
      let n2 = convert_to_int(eval_aexp e2 s) in
      Int (n1 + n2)
  | Minus (e1, e2) ->
      let n1 = convert_to_int(eval_aexp e1 s) in
      let n2 = convert_to_int(eval_aexp e2 s) in
      Int (n1 - n2)
  | Mult (e1, e2) ->
      let n1 = convert_to_int(eval_aexp e1 s) in
      let n2 = convert_to_int(eval_aexp e2 s) in
      Int (n1 * n2)
  | AId variable -> lookup s variable

type bexp = True | False | Eq of (aexp * aexp) 
          | Not of bexp
          | And of (bexp * bexp)
          | BId of var

let rec eval_bexp (s: state) = function
  | True -> Bool true
  | False -> Bool false
  | Eq (a1, a2) -> 
      let n1 = convert_to_int(eval_aexp a1 s) in
      let n2 = convert_to_int(eval_aexp a2 s) in
      Bool (n1 = n2)
  | Not b -> 
      let v = convert_to_bool(eval_bexp s b) in
      if v = True then Bool False else Bool True
  | And (b1, b2) ->
      let v1 = convert_to_bool(eval_bexp s b1) in
      if v1 = False then Bool False else (
        let v2 = convert_to_bool(eval_bexp s b2) in
        Bool (v1 && v2))
  | BId variable -> lookup s variable


let bexample = Eq (Nat 42, Nat 32) (*False*)

type cmd = Skip | Assign of (var * aexp) | Seq of (cmd * cmd) 
                | If of (bexp * cmd * cmd) | While of (bexp * cmd)
    
ket eval_cmd c s = 
  match c with
  | Skip -> s
  | Assign (x, v) -> (x, v) :: s
  | Seq (c1, c2) -> let st1 = eval_cmd c1 s in
                    let st2 = eval_cmd c2 st1 in
                    st2
  | If (c1, b1, b2) -> let result = (eval_bexp c1 s) in
                      match result with
                        | Int n -> raise (Error "Expected a boolean")
                        | Bool b -> if b then eval_cmd b1 s else eval_cmd b2 s
  | While (c1, loop) -> let result = (eval_bexp s c1) in
                        (match result with
                        | Int n -> raise (Error "Expected a boolean")
                        | Bool b -> if b then (eval_cmd (Seq (loop, While (c1, loop))) ) else s)