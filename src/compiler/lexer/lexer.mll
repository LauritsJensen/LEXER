(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

(*I'm NOT saying that I'm the coolest girl in the world, but...!!!!!!!!!!!!!!*)
(* Jeg snaker Dansker*)
(*Di pizza ær fardig i di minutå*)
(*Don't fear! Asha is here!*)


{
  open Tigerparser.Parser  
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
} 

let digits=['0'-'9']+
let ident=['a'-'z''A'-'Z']['_''0'-'9''a'-'z''A'-'Z']* 
(* add more named regexps here *)

rule token = parse
    [' ' '\t' ]         { token lexbuf }     (* skip blanks *)
  | eof                 { EOF }
  | ','                 { COMMA }
  | ';'                 { SEMICOLON }
  | ":="                { ASSIGN }
  | ":"                 { COLON }
  | "."                 { DOT }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "["                 { LBRACK }   
  | "]"                 { RBRACK }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | "+"                 { PLUS }
  | "-"                 { MINUS } (* What about NEG? *)
  | "*"                 { TIMES }
  | "="                 { EQ }
  | "<>"                { NEQ }
  | "<"                 { LT }  
  | "<="                { LE }  
  | ">"                 { GT }
  | ">="                { GE }
  | "&"                 { AND }
  | "|"                 { OR }
  | "while"             { WHILE }
  | "for"               { FOR }
  | "to"                { TO }
  | "break"             { BREAK }
  | "let"               { LET }
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
  | "in"                { IN }
  | "\""                { stringHandler lexbuf.Lexing.lex_start_p "" }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }  
  | "//"                { single_line_comment lexbuf }
  | "/*"                { multi_line_comment 0 lexbuf }
  | "/"                 { DIVIDE }
  | ident as i          { ID i }
  | digits as i         { INT (int_of_string i) }
  | _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }


STRING 1 "ggfgfg"
and single_line_comment = parse
  eof       { EOF }
| '\n'      { token lexbuf }
| _		      { single_line_comment lexbuf }

and multi_line_comment level = parse
  eof     { error lexbuf "Missing close of multi-line-comment" }
| "*/"		{ if level = 0 then token lexbuf else multi_line_comment (level - 1) lexbuf }
| "/*"		{ multi_line_comment (level + 1) lexbuf }
| '\n'    { Lexing.new_line lexbuf; multi_line_comment level lexbuf }
| _		    { multi_line_comment level lexbuf }
