open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

exception InvalidInputException of string

let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lcurly = Str.regexp "{"
let re_rcurly = Str.regexp "}"

let re_dot = Str.regexp "\\."  (* !! *)

let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="

let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"

let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"

let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "\\-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"

let re_concat = Str.regexp "\\^" (* !! *)
let re_let = Str.regexp "let"
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_semi = Str.regexp ";"
let re_doublesemi = Str.regexp ";;"

let re_int_pos = Str.regexp "[0-9]+"
let re_int_neg = Str.regexp "(-[0-9]+)"
let re_bool = Str.regexp "true\\|false"  (* !! *)
let re_string = Str.regexp "\"[^\"]*\"" (* strings are surronded by "" ans should accept any chars except quotes within them *)
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_spaces = Str.regexp "[ \t\n]"


(* converts input string into a list of tokens *)
let tokenize input = 
  let rec tokenize_aux pos s =
    if pos >= String.length s then []

    else if Str.string_match re_int_neg s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      let token_core = String.sub token 1 (len - 2) in
      Tok_Int (int_of_string token_core) :: tokenize_aux (pos + len) s
 
    else if Str.string_match re_int_pos s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      Tok_Int (int_of_string token) :: tokenize_aux (pos + len) s

    else if Str.string_match re_lparen s pos then Tok_LParen :: tokenize_aux (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tokenize_aux (pos + 1) s
    else if Str.string_match re_lcurly s pos then Tok_LCurly :: tokenize_aux (pos + 1) s
    else if Str.string_match re_rcurly s pos then Tok_RCurly :: tokenize_aux (pos + 1) s

    else if Str.string_match re_dot s pos then Tok_Dot :: tokenize_aux (pos + 1) s
    else if Str.string_match re_arrow s pos then Tok_Arrow :: tokenize_aux (pos + 2) s
    else if Str.string_match re_concat s pos then Tok_Concat :: tokenize_aux (pos + 1) s      
    else if Str.string_match re_doublesemi s pos then Tok_DoubleSemi :: tokenize_aux (pos + 2) s
    else if Str.string_match re_semi s pos then Tok_Semi :: tokenize_aux (pos + 1) s

    else if Str.string_match re_greaterequal s pos then Tok_GreaterEqual :: tokenize_aux (pos + 2) s
    else if Str.string_match re_lessequal s pos then Tok_LessEqual :: tokenize_aux (pos + 2) s
    else if Str.string_match re_notequal s pos then Tok_NotEqual :: tokenize_aux (pos + 2) s
    else if Str.string_match re_equal s pos then Tok_Equal :: tokenize_aux (pos + 1) s
    else if Str.string_match re_less s pos then Tok_Less :: tokenize_aux (pos + 1) s
    else if Str.string_match re_greater s pos then Tok_Greater :: tokenize_aux (pos + 1) s
   

    else if Str.string_match re_or s pos then Tok_Or :: tokenize_aux (pos + 2) s
    else if Str.string_match re_and s pos then Tok_And :: tokenize_aux (pos + 2) s
    else if Str.string_match re_not s pos then Tok_Not :: tokenize_aux (pos + 3) s
  
    else if Str.string_match re_if s pos then Tok_If :: tokenize_aux (pos + 2) s
    else if Str.string_match re_then s pos then Tok_Then :: tokenize_aux (pos + 4) s
    else if Str.string_match re_else s pos then Tok_Else :: tokenize_aux (pos + 4) s

    else if Str.string_match re_add s pos then Tok_Add :: tokenize_aux (pos + 1) s
    else if Str.string_match re_sub s pos then Tok_Sub :: tokenize_aux (pos + 1) s
    else if Str.string_match re_mult s pos then Tok_Mult :: tokenize_aux (pos + 1) s
    else if Str.string_match re_div s pos then Tok_Div :: tokenize_aux (pos + 1) s

    else if Str.string_match re_let s pos then Tok_Let :: tokenize_aux (pos + 3) s
    else if Str.string_match re_rec s pos then Tok_Rec :: tokenize_aux (pos + 3) s
    else if Str.string_match re_in s pos then Tok_In :: tokenize_aux (pos + 2) s
    else if Str.string_match re_def s pos then Tok_Def :: tokenize_aux (pos + 3) s
    else if Str.string_match re_fun s pos then Tok_Fun :: tokenize_aux (pos + 3) s

    else if Str.string_match re_bool s pos then 
      let token = Str.matched_string s in 
      let len = String.length token in 
      Tok_Bool (bool_of_string token) :: tokenize_aux (pos + len) s

    else if Str.string_match re_string s pos then  (* ----?? *)
      let token = Str.matched_string s in 
      let len = String.length token in 
      let new_tok = String.sub token 1 (len - 2) in
      Tok_String new_tok :: tokenize_aux (pos + len) s

    else if Str.string_match re_id s pos then 
      let token = Str.matched_string s in 
      let len = String.length token in 
      Tok_ID token :: tokenize_aux (pos + len) s

    else if Str.string_match re_spaces s pos then 
      let token = Str.matched_string s in 
      let len = String.length token in  
      tokenize_aux (pos + len) s

    else raise (InvalidInputException "tokenize")
  in
  tokenize_aux 0 input
