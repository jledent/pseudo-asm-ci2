%{
open Asm
%}

%token <int> LINENBR
%token <int64> INT64
%token <Asm.reg> REG
%token <string> STRING
%token MOVE ADD SUB MUL DIV MOD JUMP JUMP_EQ JUMP_NEQ JUMP_L JUMP_LE JUMP_G JUMP_GE PRINT HALT
%token COMMA PLUS LPAREN RPAREN
%token EOF

%start <Asm.prog> program
%%

program:
    | is=list(line) EOF { is }

line:
    | l=LINENBR PRINT LPAREN ops=separated_list(PLUS, str_operand) RPAREN { (l, (Print, ops)) }
    | l=LINENBR opc=opcode ops=separated_list(COMMA, operand) { (l, (opc, ops)) }

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
    | HALT { Halt }

operand:
    | n=INT64 { Imm n }
    | r=REG { Reg r }

str_operand:
    | n=INT64 { Imm n }
    | r=REG { Reg r }
    | s=STRING { Str s }