open Types

(************** Provided functions - DO NOT MODIFY *****************)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v


(********************* Part 1: Evaluating expressions **********************)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let [@warning "-8"] rec eval_expr env e = match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s 
  | ID id -> lookup env id

  | Binop (op, e1, e2) -> eval_Binop env op e1 e2

  | Not e -> 
      (match (eval_expr env e) with 
      | Bool boo -> if boo then Bool false else Bool true
      | _ -> raise (TypeError "Expected type bool"))
      
  | If (guard, branch1, branch2) -> 
      (match (eval_expr env guard) with
        | Bool boo -> if boo then (eval_expr env branch1) else (eval_expr env branch2) 
        | _ -> raise (TypeError "eval_expr: If"))


  (* non-recursive let expression: let x = e1 in e2 *)
  | Let (x, false, e1, e2) -> 
      let v1 = eval_expr env e1 in
      let env' = extend env x v1 in 
      eval_expr env' e2

  (* recursive let expression: let rec x = e1 in e2 *)
  | Let (x, true, e1, e2) -> 
      let env1 = extend_tmp env x in 
      let v1 = eval_expr env1 e1 in 
      update env1 x v1 ; 
      eval_expr env1 e2

  (* anonymous func *)
  | Fun (x, funcbody) -> (Closure (env, x, funcbody))  

  (* function application *)    
  | App (expr1, expr2) -> 
      let e1 = eval_expr env expr1 in
      let e2 = eval_expr env expr2 in 
        (match e1 with 
        | Closure (env, x, e) -> 
            let env' = (extend env x e2) in 
            (eval_expr env' e)
        | _ -> raise (TypeError "eval_expr: App"))
        
  | Record list  ->  Record list 
        (* YOU DON'T NEED THIS !!!!!!!!!
          match list with 
          | [] -> Record ([])
          | (Lab l, expr)::t -> (Record (list))
        *)

  | Select (Lab id, expr) -> match (eval_expr env expr) with 
            | Record list -> select_helper list env id
            | _ -> raise (TypeError "Not a Record")

and select_helper recordlist env id = 
  match recordlist with 
  | (Lab id1, v1)::t -> if id1 = id then (eval_expr env v1) else select_helper t env id
  | [] -> raise (SelectError "Label not found")


and eval_Binop env op e1 e2 = 
  match op with
  | Or -> eval_Logic env Or (eval_expr env e1) (eval_expr env e2)
  | And -> eval_Logic env And (eval_expr env e1) (eval_expr env e2)
  | Add -> eval_Additive env Add (eval_expr env e1) (eval_expr env e2)
  | Sub -> eval_Additive env Sub (eval_expr env e1) (eval_expr env e2)
  | Mult -> eval_Multipicative env Mult (eval_expr env e1) (eval_expr env e2)
  | Div -> eval_Multipicative env Div (eval_expr env e1) (eval_expr env e2)
  | Equal -> eval_Equality env Equal (eval_expr env e1) (eval_expr env e2)
  | NotEqual -> eval_Equality env NotEqual (eval_expr env e1) (eval_expr env e2)                   
  | Less -> eval_Relational env Less (eval_expr env e1) (eval_expr env e2)
  | Greater -> eval_Relational env Greater (eval_expr env e1) (eval_expr env e2)
  | LessEqual -> eval_Relational env LessEqual (eval_expr env e1) (eval_expr env e2)
  | GreaterEqual -> eval_Relational env GreaterEqual (eval_expr env e1) (eval_expr env e2)
  | Concat -> eval_Concat env Concat (eval_expr env e1) (eval_expr env e2)
  | _ -> raise (TypeError "eval_Binop")

and eval_Logic env op e1 e2 = 
  match op, e1, e2 with
    | Or, Bool x, Bool y -> Bool (x || y)
    | And, Bool x, Bool y -> Bool (x && y)
    | _ -> raise (TypeError "Expected type bool")

and eval_Additive env op n1 n2 = 
  match op, n1, n2 with 
    | Add, Int x, Int y -> Int (x + y)
    | Sub, Int x, Int y -> Int (x - y)
    | _ -> raise (TypeError "Expected type int")

and eval_Multipicative env op n1 n2 = 
  match op, n1, n2 with 
    | Mult, Int x, Int y -> Int (x * y)
    | Div, Int x, Int y -> if y = 0 then raise (DivByZeroError)
                           else Int (x / y)
    | _ -> raise (TypeError "eval_Multipicative")

and eval_Equality env op v1 v2 = 
  match op, v1, v2 with 
    | Equal, Int x, Int y  -> Bool (x = y)
    | Equal, Bool x, Bool y   -> Bool (x = y)
    | Equal, String x, String y  -> Bool (x = y)
    | NotEqual, Int x, Int y  -> Bool ( x <> y)
    | NotEqual, Bool x, Bool y  -> Bool ( x <> y)
    | NotEqual, String x, String y  -> Bool ( x <> y)
    | _ -> raise (TypeError "Cannot compare types")

and eval_Relational env op v1 v2 = 
  match op, v1, v2 with
  | Less, Int x, Int y -> Bool (x < y)
  | Greater, Int x, Int y -> Bool (x > y)
  | LessEqual, Int x, Int y -> Bool (x <= y)
  | GreaterEqual, Int x, Int y -> Bool ( x >= y)
  | _ -> raise (TypeError "Expected type int")

and eval_Concat env op s1 s2 = 
  match s1, s2 with
    | String x, String y -> String (x ^ y)
    | _ -> raise (TypeError "Expected type string")
  
    
(******************** Part 2: Evaluating mutop directive *******************)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (x, e) -> 
      let env' = extend_tmp env x in
      let v = eval_expr env' e in update env' x v;
      (env', Some v)
  | Expr e -> 
      let v = eval_expr env e in
      (env, Some v)
  | NoOp -> (env, None)

(*  | _ -> raise (InvalidInputException "eval_mutop")    *)