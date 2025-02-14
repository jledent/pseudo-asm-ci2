type reg =
  R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
| PC | SP | BP

let reg_of_int = function
  | 0 -> R0 | 1 -> R1 | 2 -> R2 | 3 -> R3
  | 4 -> R4 | 5 -> R5 | 6 -> R6 | 7 -> R7
  | _ -> failwith "Invalid register"

type operand =
| Imm of int64
| Reg of reg
| Str of string (* For the Print instruction *)

type opcode = 
| Move
| Add | Sub | Mul | Div | Mod
| Jump | Jump_eq | Jump_neq | Jump_l | Jump_le | Jump_g | Jump_ge
| Push | Pop
| Call | Ret
| Halt | Print | Malloc

type linenumber = int
type instr = opcode * operand list
type line = linenumber * opcode * operand list
type prog = line list

(* Pretty printing ----------------------------------------------------------- *)

let string_of_reg = function
  | R0 -> "R0" | R1 -> "R1" | R2 -> "R2" | R3 -> "R3"
  | R4 -> "R4" | R5 -> "R5" | R6 -> "R6" | R7 -> "R7"
  | PC -> "PC" | SP -> "SP" | BP -> "BP"

let string_of_operand = function
  | Imm i -> Int64.to_string i
  | Reg r -> string_of_reg r
  | Str s -> "\"" ^ s ^ "\""

let string_of_opcode = function
  | Move -> "move"
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Mod -> "mod"
  | Jump -> "jump" | Jump_eq -> "jump_eq" | Jump_neq -> "jump_neq"
  | Jump_l -> "jump_l" | Jump_le -> "jump_le" | Jump_g -> "jump_g" | Jump_ge -> "jump_ge"
  | Push -> "push" | Pop -> "pop"
  | Call -> "call" | Ret -> "ret"
  | Halt -> "halt" | Print -> "print" | Malloc -> "malloc"

let string_of_ins (l, opc, ops) = match opc with
  | Print -> Printf.sprintf "%d: %s (%s)" l (string_of_opcode opc) (String.concat " + " (List.map string_of_operand ops))
  | _ -> Printf.sprintf "%d: %s %s" l (string_of_opcode opc) (String.concat ", " (List.map string_of_operand ops))

let string_of_prog p =
  String.concat "\n" (List.map string_of_ins p)