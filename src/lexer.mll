{
    open Common
    open Asm
    open Parser
    let int_of_char c = Char.code c - Char.code '0'
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let register = 'R' digit
let number = ('-')?digit+
let string = '"' [^ '"']* '"'
let comment = "//" [^ '\n']*
let spaces = [' ' '\t']

rule token = parse
    | [' ' '\t' '\r' '\n'] { token lexbuf }
    | comment { token lexbuf }
    | (number as n) spaces* ':' { LINENBR (int_of_string n) }
    | number as n { match Int64.of_string_opt n with
                       | Some j -> INT64 j
                       | None -> raise (ParseError ("Invalid number: " ^ n)) }
    | register as r {
        let k = int_of_char r.[1] in
        if k < 0 || k > 7
        then raise (ParseError ("Invalid register: " ^ r))
        else REG (reg_of_int k)
      }
    | string as s { STRING (String.sub s 1 (String.length s - 2)) }
    | "move" { MOVE }
    | "add" { ADD }
    | "sub" { SUB }
    | "mul" { MUL }
    | "div" { DIV }
    | "mod" { MOD }
    | "jump" { JUMP }
    | "jump_eq" { JUMP_EQ }
    | "jump_neq" { JUMP_NEQ }
    | "jump_l" { JUMP_L }
    | "jump_le" { JUMP_LE }
    | "jump_g" { JUMP_G }
    | "jump_ge" { JUMP_GE }
    | "print" { PRINT }
    | "halt" { HALT }
    | ',' { COMMA }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '+' { PLUS }
    | eof { EOF }
    | _ as c { raise (ParseError (Printf.sprintf "Unexpected character : %c" c)) }