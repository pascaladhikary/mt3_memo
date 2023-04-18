{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)
let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let alpha = ['a' - 'z' 'A' - 'Z']
let alphanum = alpha | numeric

let printable_char = alphanum
                      | ' ' | '~' | '`' | '!' | '@' | '#' | '$' 
                      | '%' | '^' | '&' | '*' | '(' | ')' | '-' | '_'
                      | '+' | '=' | '{' | '[' | '}' | ']' | '|'
                      | ':' | ';' | '\'' | '<' | ',' | '>' | '.' | '?' | '/'


let whitespace = [' ' '\t' '\n']
rule token = parse
  | [' ' '\t'] { token lexbuf }  (* skip over whitespace *)
  | ['\n']     { token lexbuf }  (* skip over whitespace *)
  | eof        { EOF          }

(* your rules go here *)
  | "~"     { NEG }
  | "+"     { PLUS  }
  | "-"     { MINUS  }
  | "/"     { DIV  }
  | "+."    { DPLUS  }
  | "-."    { DMINUS  }
  | "*."    { DTIMES  }
  | "/."    { DDIV  }
  | "^"     { CARAT  }
  | "<"     { LT  }
  | ">"     { GT  }
  | "<="    { LEQ  }
  | ">="    { GEQ  }
  | "<>"    { NEQ }
  | "|"     { PIPE  }
  | "->"    { ARROW  }
  | "::"    { DCOLON  }
  | ";;"    { DSEMI }
  | "@"     { AT  }
  | "[]"    { NIL }
  | "let"   { LET  }
  | "rec"   { REC  }
  | "and"   { AND}
  | "end"   { END}
  | "in"    { IN  }
  | "if"    { IF  }
  | "else"  { ELSE  }
  | "fun"   { FUN  }
  | "mod"   { MOD  }
  | "raise" { RAISE }
  | "with"  { WITH }
  | "&&"    { LOGICALAND}
  | "||"    { LOGICALOR}
  | "["     { LBRAC  }
  | "]"     { RBRAC  }
  | "()"    { UNIT }
  | "("     { LPAREN  }
  | ")"     { RPAREN  }
  | ","     { COMMA  }
  | "_"     { UNDERSCORE }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "()"    { UNIT }
  | "*" 	  { TIMES }
  | "="	    { EQUALS }
  | ";"	    { SEMI }
  | "try"	  { TRY }
  | "then"  { THEN }
  | "not"	  { NOT }

  | numeric+ as s                                         { INT (int_of_string s) }
  | numeric+'.'(numeric*) as s                            { FLOAT (float_of_string s) }
  | numeric+'.'(numeric*)'e'(numeric+) as s               { FLOAT (float_of_string s) }
  | lowercase((alpha | numeric | '_' | '\'')*) as s       { IDENT s }
  | "\""                                                  { string "" lexbuf }

(* your rules go here *)

and string ins = parse
  | printable_char+ as s                    { string (ins ^ s) lexbuf }
  | "\""                                    { STRING ins }
  | "\\"                                    { escaped_string ins lexbuf }
  
and escaped_string ins = parse
  | "\\"                                    { string (ins ^ "\\") lexbuf }
  | "\'"                                    { string (ins ^ "\'") lexbuf }
  | "\""                                    { string (ins ^ "\"") lexbuf }
  | "t"                                     { string (ins ^ "\t") lexbuf }
  | "n"                                     { string (ins ^ "\n") lexbuf }
  | "r"                                     { string (ins ^ "\r") lexbuf }
  | "b"                                     { string (ins ^ "\b") lexbuf }
  | "\ "                                    { string (ins ^ "\ ") lexbuf }
  | ""(('0'|'1')numeric numeric as s )      { string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }
  | ""('2'['0' - '4']numeric as s)          { string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }
  | ""("25"['0' - '5'] as s)                { string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }
  | "\n"(' '|'\t')*                         { string ins lexbuf }

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }

