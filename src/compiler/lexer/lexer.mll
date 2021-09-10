(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

(*I'm NOT saying that I'm the coolest girl in the world, but...!!!!!!!!!!!!!!*)


{
  open Tigerparser.Parser  
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)

  type token =
    | EOF
    | INT of int
    | ID of string
    | NEWLINE
    | REC
    | PLUS 
    | MINUS 
    | TIMES 
    | DIVIDE 
    | REMAINDER 
    | ASSIGN 
    | LPAREN 
    | RPAREN 
    | LSQBRACE 
    | RSQBRACE 
    | LCUBRACE 
    | RCUBRACE 
    | DOT
} 

let digits=['0'-'9']+

(* add more named regexps here *)

(* an entrypoint with a few starting regexps *)
rule token = parse
  [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
  | eof                 { EOF }
  | ','                 { COMMA }
  | ';'                 { SEMICOLON }
  | ":="                { ASSIGN }
  | ":"                 { COLON }
  | "."                 { DOT }
  | "("                 { paren 0 lexbuf }
  | "["                 { LSQBRACK }
  | "]"                 { RSQBRACK }
  | "{"                 { LCUBRACK }
  | "}"                 { RCUBRACK }
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "*"                 { TIMES }
  | "="                 { EQ }
  | "<>"                { NEQ }
  | "<"                 { LT }  
  | "<="                { LEQ }  
  | ">"                 { GT }
  | ">="                { GEQ }
  | "&"                 { AND }
  | "|"                 { OR }
  | "while"             { WHILE }
  | "for"               { FOR }
  | "to"                { TO }
  | "break"             { BREAK }
  | "let"               { IN }
  | "end"               { END }
  | "function"          { FUNCTION }
  | "var"               { VAR }
  | "type"              { TYPE }
  | "array"             { ARRAY }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "do"                { DO }
  | "of"                { OF }
  | "nil"               { NIL }  
  | "//"                { single_line_comment lexbuf}
  | "/*"                { multi_line_comment 0 lexbuf}
  | "/"                 { DIVIDE }
  | id as id            { ID id }
  | digits as i         { INT (int_of_string i) }
  | _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }

and single_line_comment = parse
  eof       { EOF }
| '\n'      { token lexbuf }
| _		      { single_line_comment lexbuf }

and multi_line_comment level = parse
  eof     { error lexbuf "Missing close of multi-line-comment" }
| "*/"		{ if level = 0 then token lexbuf else multi_line_comment (level - 1) lexbuf }
| "/*"		{ multi_line_comment (level + 1) lexbuf }
| _		    { multi_line_comment level lexbuf }

and paren level = parse
  
| ")"                 { RPAREN }
| _		    {  }