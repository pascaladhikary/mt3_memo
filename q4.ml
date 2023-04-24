(* INT *)
expression: 
  l2 { $1 }

l0:
  | l0 TIMES atomic_expression { BinOpAppExp (IntTimesOp, $1, $3) }
  | atomic_expression { $1 }

l1:
  | l1 PLUS l0 { BinOpAppExp (IntPlusOp, $1, $3) }
  | l1 MINUS l0 { BinOpAppExp (IntMinusOp, $1, $3) }
  | l0 { $1 }

l2:
  | FUN IDENT ARROW expression { FunExp ($2, $4) }
  | l3_pm { $1 }
  | l2_m { $1 }
  | l1 { $1 }

l2_m:
  | l0 TIMES FUN IDENT ARROW expression { BinOpAppExp (IntTimesOp, $1, FunExp ($4, $6))}

l3_pm:
  | l1 PLUS FUN IDENT ARROW expression { BinOpAppExp (IntPlusOp, $1, FunExp ($4, $6))}
  | l1 MINUS FUN IDENT ARROW expression { BinOpAppExp (IntMinusOp, $1, FunExp ($4, $6))}
  | l1 PLUS l2_m { BinOpAppExp (IntPlusOp, $1, $3) }
  | l1 MINUS l2_m { BinOpAppExp (IntMinusOp, $1, $3) }

atomic_expression: 
  | IDENT            { VarExp $1 }
  | INT { ConstExp (IntConst $1) }
  | LPAREN expression RPAREN { $2 }
  
(* ADD *)
expression: 
  | l0 { $1 }
  | add { $1 }
  | add add_o l0 { BinOpAppExp ($2, $1, $3) }
  | exponent_let { $1 }

l0:
  | LET IDENT EQUALS expression IN expression { LetInExp ($2, $4, $6) }

add:
  | add add_o exponent { BinOpAppExp ($2, $1, $3) }
  | exponent { $1 }

add_o:
  | DPLUS { FloatPlusOp }
  | DMINUS { FloatMinusOp }

exponent:
  | atomic_expression EXP exponent { BinOpAppExp (ExpoOp, $1, $3) }
  | atomic_expression { $1 }

exponent_let:
  | atomic_expression EXP exponent_let { BinOpAppExp (ExpoOp, $1, $3) }
  | atomic_expression EXP l0 { BinOpAppExp (ExpoOp, $1, $3) }
  
atomic_expression:
  | IDENT            { VarExp $1 }
  | FLOAT { ConstExp (FloatConst $1) }
  | LPAREN expression RPAREN { $2 }
  
(* MULT *)
expression: 
  c {$1}
  | div {$1}
  | exp_c {$1}
  | div DDIV c { BinOpAppExp(FloatDivOp, $1, $3) }
  | div DTIMES c { BinOpAppExp(FloatTimesOp, $1, $3) }

c:
  expression EQUALS div { BinOpAppExp(EqOp, $1, $3) }
  | expression GT div { BinOpAppExp(GreaterOp, $1, $3) }
  | expression LT div { BinOpAppExp(GreaterOp, $3, $1) }

div:
  div DTIMES exp { BinOpAppExp(FloatTimesOp, $1, $3) }
  | div DDIV exp { BinOpAppExp(FloatDivOp, $1, $3) }
  | exp { $1 }

exp: 
  atomic_expression EXP exp { BinOpAppExp(ExpoOp, $1, $3) }
  | atomic_expression { $1 }

exp_c:
  atomic_expression EXP exp_c { BinOpAppExp(ExpoOp, $1, $3) }
  | atomic_expression EXP c { BinOpAppExp(ExpoOp, $1, $3) }

atomic_expression: 
  | UNIT            { ConstExp UnitConst }
  | IDENT { VarExp $1 }
  | TRUE  { ConstExp (BoolConst true) }
  | FALSE { ConstExp (BoolConst false) }
  | FLOAT { ConstExp (FloatConst $1) }
  | LPAREN expression RPAREN {$2}
