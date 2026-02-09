%{
open Asm
%}

%token <int> LINENBR
%token <int64> INT64
%token <Asm.reg> REG
%token <string> STRING
%token MOVE ADD SUB MUL DIV MOD JUMP JUMP_EQ JUMP_NEQ JUMP_L JUMP_LE JUMP_G JUMP_GE PUSH POP CALL RET PRINT PRINTLN MALLOC HALT
%token COMMA PLUS MINUS LPAREN RPAREN LBRACKET RBRACKET
%token EOF

%start <Asm.prog> program
%%

program:
    | is=list(line) EOF { is }

line:
    | l=LINENBR opc=print_opcode LPAREN ops=separated_list(PLUS, str_operand) RPAREN { (l, (opc, ops)) }
    | l=LINENBR opc=print_opcode op=str_operand { (l, (opc, [op])) }
    | l=LINENBR opc=opcode ops=separated_list(COMMA, operand) { (l, (opc, ops)) }

print_opcode:
    | PRINT { Print }
    | PRINTLN { Println }

opcode:
    | MOVE { Move }
    | ADD { Add }
    | SUB { Sub }
    | MUL { Mul }
    | DIV { Div }
    | MOD { Mod }
    | JUMP { Jump }
    | JUMP_EQ { Jump_eq }
    | JUMP_NEQ { Jump_neq }
    | JUMP_L { Jump_l }
    | JUMP_LE { Jump_le }
    | JUMP_G { Jump_g }
    | JUMP_GE { Jump_ge }
    | PUSH { Push }
    | POP { Pop }
    | CALL { Call }
    | RET { Ret }
    | MALLOC { Malloc }
    | HALT { Halt }

operand:
    | n=INT64 { Imm n }
    | MINUS n=INT64 { Imm (Int64.neg n) }
    | r=REG { Reg r }
    | LBRACKET r=REG RBRACKET { Ind r }
    | LBRACKET r=REG PLUS n=INT64 RBRACKET { IndImm (r, n) }
    | LBRACKET r=REG MINUS n=INT64 RBRACKET { IndImm (r, Int64.neg n) }
    | LBRACKET r1=REG PLUS r2=REG RBRACKET { IndReg (r1, r2) }

str_operand:
    | op=operand { op }
    | s=STRING { Str s }
