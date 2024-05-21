open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

  (* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match


(*======================== WRITE YOUR CODE BELOW ==========================*)  

(*======= Part A2: Parsing expressions =======*)

let rec parse_expr toks = 
  let (toks, ast) = parse_Expr toks in 
    if toks = [] then (toks, ast)
    else raise (InvalidInputException "parse_expr")

and parse_Expr toks =
  match lookahead toks with  
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks

(*-----------------------*)  
and parse_LetExpr toks = 
  let toks_remain = match_token toks Tok_Let 
  in let (toks1, recursion) = 
    match lookahead toks_remain with
    | Some Tok_Rec -> (match_token toks_remain Tok_Rec, true)
    | _ -> (toks_remain, false) 
  in let (toks2, id) = 
    match lookahead toks1 with
    | Some Tok_ID id -> (match_token toks1 (Tok_ID id), id)
    | _ -> raise (InvalidInputException "parse_LetExpr: Tok_ID missing") 
  in let toks_remain1 = 
    match lookahead toks2 with
    | Some Tok_Equal -> match_token toks2 Tok_Equal
    | _ -> raise (InvalidInputException "parse_LetExpr: Tok_Equal missing") 
  in
  let (toks3, e3) = parse_Expr toks_remain1 in
  let toks_remain2 = match_token toks3 Tok_In in
  let (toks4, e4) = parse_Expr toks_remain2 in
  (toks4, Let(id, recursion, e3, e4))


and parse_IfExpr toks = 
  let toks_remain = match_token toks Tok_If in
  let (toks1, e1) = parse_Expr toks_remain in 
  let toks_remain1 = match_token toks1 Tok_Then in
  let (toks2, e2) = parse_Expr toks_remain1 in 
  let toks_remain2 = match_token toks2 Tok_Else in
  let (toks3, e3) = parse_Expr toks_remain2 in 
  (toks3, If(e1, e2, e3))


and parse_FunctionExpr toks = 
  let toks_remain = match_token toks Tok_Fun in
  let (toks1, id) = 
    match lookahead toks_remain with
    | Some Tok_ID id -> (match_token toks_remain (Tok_ID id), id)
    | _ -> raise (InvalidInputException "parse_FunctionExpr: Tok_ID missing") in 
  let toks_remain1 = match_token toks1 Tok_Arrow in
  let (toks2, e2) = parse_Expr toks_remain1 in
  (toks2, Fun(id, e2))


(*-----------------------*)  
and parse_OrExpr toks = 
  let (toks1, e1) = parse_AndExpr toks in
  match lookahead toks1 with
  | Some Tok_Or -> let toks_remain = match_token toks1 Tok_Or in
                   let (toks2, e2) = parse_OrExpr toks_remain in
                   (toks2, Binop(Or, e1, e2))

  | _ -> (toks1, e1)

and parse_AndExpr toks = 
  let (toks1, e1) = parse_EqualityExpr toks in
  match lookahead toks1 with
  | Some Tok_And -> let toks_remain = match_token toks1 Tok_And in
                    let (toks2, e2) = parse_AndExpr toks_remain in
                    (toks2, Binop(And, e1, e2))
  | _ -> (toks1, e1)

and parse_EqualityExpr toks = 
  let (toks1, e1) = parse_RelationalExpr toks in
  match lookahead toks1 with
  | Some Tok_Equal -> let toks_remain = match_token toks1 Tok_Equal in
                      let (toks2,e2) = parse_EqualityExpr toks_remain in
                      (toks2, Binop(Equal, e1, e2))
  | Some Tok_NotEqual -> let toks_remain = match_token toks1 Tok_NotEqual in
                        let (toks2,e2) = parse_EqualityExpr toks_remain in
                        (toks2, Binop(NotEqual, e1, e2))
  | _ -> (toks1, e1)

and parse_RelationalExpr toks =
  let (toks1, e1) = parse_AdditiveExpr toks in
  match lookahead toks1 with
  | Some Tok_Less -> let toks_remain = match_token toks1 Tok_Less in
                      let(toks2,e2) = parse_RelationalExpr toks_remain in
                      (toks2, Binop(Less,e1, e2))
  | Some Tok_Greater -> let toks_remain = match_token toks1 Tok_Greater in
                      let(toks2,e2) = parse_RelationalExpr toks_remain in
                      (toks2, Binop(Greater,e1, e2))
  | Some Tok_LessEqual -> let toks_remain = match_token toks1 Tok_LessEqual in
                          let(toks2,e2) = parse_RelationalExpr toks_remain in
                          (toks2, Binop(LessEqual,e1, e2))
  | Some Tok_GreaterEqual -> let toks_remain = match_token toks1 Tok_GreaterEqual in
                            let(toks2,e2) = parse_RelationalExpr toks_remain in
                            (toks2, Binop(GreaterEqual,e1, e2))
  | _ -> (toks1, e1) 

and parse_AdditiveExpr toks =
  let (toks1, e1) = parse_MultiplicativeExpr toks in
  match lookahead toks1 with
  | Some Tok_Add -> let toks_remain = match_token toks1 Tok_Add in
                    let(toks2,e2) = parse_AdditiveExpr toks_remain in
                    (toks2, Binop(Add, e1, e2))
  | Some Tok_Sub -> let toks_remain = match_token toks1 Tok_Sub in
                    let(toks2,e2) = parse_AdditiveExpr toks_remain in
                    (toks2, Binop(Sub, e1, e2))
  | _ -> (toks1, e1)

and parse_MultiplicativeExpr toks =
  let (toks1, e1) = parse_ConcatExpr toks in
  match lookahead toks1 with
  | Some Tok_Mult -> let toks_remain = match_token toks1 Tok_Mult in
                let (toks2,e2) = parse_MultiplicativeExpr toks_remain in
                (toks2, Binop(Mult, e1, e2))
  | Some Tok_Div -> let toks_remain = match_token toks1 Tok_Div in
                let (toks2, e2) = parse_MultiplicativeExpr toks_remain in
                if e2 = (Int 0) then raise (DivByZeroError) 
                else (toks2, Binop(Div, e1, e2))
  | _ -> (toks1, e1)

and parse_ConcatExpr toks =
  let (toks1, e1) = parse_UnaryExpr toks in
  match lookahead toks1 with
  | Some Tok_Concat -> let toks_remain = match_token toks1 Tok_Concat in
                  let (tok2, e2) = parse_ConcatExpr toks_remain in
                  (tok2, Binop(Concat, e1, e2))
  | _ -> (toks1, e1)

and parse_UnaryExpr toks =
  match lookahead toks with
  | Some Tok_Not -> let toks_remain = match_token toks Tok_Not in
                let (toks1, e1) = parse_UnaryExpr toks_remain in
                (toks1, Not(e1))
  | _ -> let (toks1, e1) = parse_AppExpr toks in (toks1, e1)
  

(* function application or descend to the next branch *)  
and parse_AppExpr toks = 
  let (toks1, e1) = parse_SelectExpr toks in
  match lookahead toks1 with 
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly) -> 
        let (toks2, e2) = parse_PrimaryExpr toks1 in
        (toks2, App(e1, e2))
  | _ -> (toks1, e1)


and parse_PrimaryExpr toks =
  match lookahead toks with
  | Some Tok_Int n -> let toks1 = match_token toks (Tok_Int n) in
                      (toks1, Int n)
  | Some Tok_Bool b -> let toks1 = match_token toks (Tok_Bool b) in
                      (toks1, Bool b)
  | Some Tok_String s -> let toks1 = match_token toks (Tok_String s) in
                        (toks1, String s)
  | Some Tok_ID id -> let toks1 = match_token toks (Tok_ID id) in
                      (toks1, ID id)
  | Some Tok_LParen -> let toks1 = match_token toks (Tok_LParen) in
                      let (toks2, e2) = parse_Expr toks1 in
                      let toks3 = match_token toks2 (Tok_RParen) in 
                      (toks3, e2)
    
  | Some Tok_LCurly -> parse_RecordExpr toks 

  | _ -> raise (InvalidInputException "parse_PrimaryExpr")


and parse_SelectExpr toks = 
  let (toks1, e1) = parse_PrimaryExpr toks in
  (match lookahead toks1 with 
  | Some Tok_Dot -> let toks2 = match_token toks1 Tok_Dot in 
      (match lookahead toks2 with 
      | Some Tok_ID id -> let toks3 = (match_token toks2 (Tok_ID id)) in 
                    (toks3, Select(Lab id, e1))
      | _ -> raise (InvalidInputException "parse_SelectExpr"))
  | _ -> (toks1, e1))

  
and parse_RecordExpr toks = 
  match lookahead toks with
  | Some Tok_LCurly -> let toks_remain = match_token toks Tok_LCurly in  
    (match lookahead toks_remain with 
      | Some Tok_RCurly -> ([], Record [])
      | _ ->
          let (toks1, e1) = parse_RecordBodyExpr toks_remain [] in
          let toks2 = match_token toks1 Tok_RCurly in  (* !!! *)
          (toks2, e1))
  | _ -> raise (InvalidInputException "parse_RecordExpr")

and parse_RecordBodyExpr toks list = 
  match lookahead toks with 
  | Some Tok_ID id -> let toks1 = match_token toks (Tok_ID id) in
      (match lookahead toks1 with
        | Some Tok_Equal -> let toks2 = match_token toks1 (Tok_Equal) in
          let (toks3, e1) = parse_Expr toks2 in (* !!! *)
          let list_new = ((Lab id, e1) :: list) in 
            (match lookahead toks3 with 
              | Some Tok_Semi -> let toks4 = match_token toks3 (Tok_Semi) in    
                  parse_RecordBodyExpr toks4 list_new
              | _ -> (toks3, Record (List.rev(list_new)))
            )
        | _ -> raise (InvalidInputException "parse_RecordBodyExpr: Tok_Equal missing")
      )
  | _ -> raise (InvalidInputException "parse_recordBodyExpr: Tok_ID missing") 


(*======================= Part A3: Parsing mutop =========================*)

let rec delete_last_element lst =
  match List.rev lst with
  | [] -> []
  | _ :: tl -> List.rev tl

let rec parse_mutop toks = 
  let (toks, ast) = parse_Mutop toks in 
  if toks = [] then
    (toks, ast)
  else
    raise (InvalidInputException "parse_mutop")

and parse_Mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_DefMutop toks
  | Some Tok_DoubleSemi -> ([], NoOp)
  | _ -> let new_toks = delete_last_element toks in
          let (toks1,e1) = parse_expr new_toks in
          (toks1, Expr(e1))

and parse_DefMutop toks = 
  let toks_remain = match_token toks Tok_Def in
  let (toks1, id) = 
  (match lookahead toks_remain with
  | Some (Tok_ID id) -> (match_token toks_remain (Tok_ID id), id)
  | _ -> raise (InvalidInputException "Tok_ID missing in parse_DefMutop"))  
in 
 let toks_remain1 = match_token toks1 Tok_Equal in
 let new_toks = delete_last_element toks_remain1 in
 let (toks2, e2) = parse_expr new_toks in
 (toks2, Def(id, e2))