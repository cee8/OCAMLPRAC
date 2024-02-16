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
      if v = true then Bool false else Bool true
  | And (b1, b2) ->
      let v1 = convert_to_bool(eval_bexp s b1) in
      if v1 = false then Bool false else (
        let v2 = convert_to_bool(eval_bexp s b2) in
        Bool (v1 && v2))
  | BId variable -> lookup s variable


let bexample = Eq (Nat 42, Nat 32) (*False*)

type cmd = Skip | Assign of (var * aexp) | CmdSequence of (cmd * cmd) 
                | If of (bexp * cmd * cmd) | While of (bexp * cmd)
    
let rec eval_cmd c s = 
  match c with
  | Skip -> s
  | Assign (x, a) -> (x, eval_aexp a s) :: s
  | CmdSequence (c1, c2) -> 
      let st1 = eval_cmd c1 s in
      eval_cmd c2 st1
  | If (b, c1, c2) -> 
      let result = eval_bexp s b in
      (match result with
       | Bool true -> eval_cmd c1 s
       | Bool false -> eval_cmd c2 s
       | _ -> raise (Error "Expected a boolean"))
  | While (b, c) -> 
      let result = eval_bexp s b in
      (match result with
       | Bool true -> eval_cmd (CmdSequence (c, While (b, c))) s
       | Bool false -> s
       | _ -> raise (Error "Expected a boolean"))

