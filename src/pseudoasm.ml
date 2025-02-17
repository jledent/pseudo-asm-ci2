open Common
open Asm

module Asm = Asm
exception Error = Error

(* module Memory = Bigarray.Array1 *)
module InstrMap = Map.Make(Int)

type state = {
  instr_map : instr InstrMap.t;
  reg_table : (reg, int64) Hashtbl.t;
  mutable output : string;
  mutable jmp_flag : bool;
}

let dummy_state = {
  instr_map = InstrMap.empty;
  reg_table = Hashtbl.create 11;
  output = "";
  jmp_flag = false;
}

let initialize (prog : Asm.prog) =
  let instr_map =
    List.fold_left (fun m (l, (op, args)) ->
      if InstrMap.mem l m then
        raise (ParseError (Printf.sprintf "Duplicate line number: %d" l))
      else
        InstrMap.add l (op, args) m
      )
      InstrMap.empty
      prog
  in
  let reg_table = Hashtbl.create 11 in
  List.iter (fun r -> Hashtbl.add reg_table r 0L)
    [ R0; R1; R2; R3; R4; R5; R6; R7; PC; SP; BP ];
  { instr_map; reg_table; output=""; jmp_flag=false }

let reset (s : state) =
  Hashtbl.iter (fun r _ -> Hashtbl.replace s.reg_table r 0L) s.reg_table;
  s.output <- "";
  s.jmp_flag <- false

let validate_instr line (op, args) = match op with
  | Move -> (
    match args with
    | [ Reg _; _ ] -> ()
    | [ _; _ ] -> raise (Error ("The destination of move must be a register", line))
    | _ -> raise (Error ("move must have two arguments", line))
  )
  | Add | Sub | Mul | Div | Mod -> (
    match args with
    | [ Reg _; _; _ ] -> ()
    | [ _; _; _ ] -> raise (Error ("The destination of " ^ string_of_opcode op ^ " must be a register", line))
    | _ -> raise (Error (string_of_opcode op ^ " must have three arguments", line))
  )
  | Jump -> (
    match args with
    | [ _ ] -> ()
    | _ -> raise (Error ("jump must have one argument", line))
  )
  | Jump_eq | Jump_neq | Jump_l | Jump_le | Jump_g | Jump_ge -> (
    match args with
    | [ _; _; _ ] -> ()
    | _ -> raise (Error (string_of_opcode op ^ " must have three arguments", line))
  )
  | Print -> ()
  | Halt -> ()
  | _ -> raise (Error ("Unsupported instruction", line))

let validate_prog prog =
  List.iter (fun (l, (op, args)) -> validate_instr l (op, args)) prog

let eval_operand (s : state) = function
  | Imm i -> i
  | Reg r -> Hashtbl.find s.reg_table r
  | Str _ -> failwith "String operands are not supported"

let eval_instr (s : state) (line : int) (op, args) =
  match op with
  | Move -> (
    match args with
    | [ Reg r; arg2 ] ->
      let v = eval_operand s arg2 in
      Hashtbl.replace s.reg_table r v
    | _ -> assert false
  )
  | Add | Sub | Mul | Div | Mod -> (
    match args with
    | [ Reg r; arg1; arg2 ] ->
      let v1 = eval_operand s arg1 in
      let v2 = eval_operand s arg2 in
      let res = match op with
        | Add -> Int64.add v1 v2
        | Sub -> Int64.sub v1 v2
        | Mul -> Int64.mul v1 v2
        | Div -> Int64.div v1 v2
        | Mod -> Int64.rem v1 v2
        | _ -> assert false
      in
      Hashtbl.replace s.reg_table r res
      | _ -> assert false
  )
  | Jump -> (
    match args with
    | [ arg1 ] -> (
      let l64 = eval_operand s arg1 in
      let l = Int64.to_int l64 in
      match InstrMap.find_opt l s.instr_map with
      | Some _ -> s.jmp_flag <- true;
          Hashtbl.replace s.reg_table PC l64
      | None -> raise (Error (Printf.sprintf "Unknown line number: %d" l, line))
    )
    | _ -> assert false
  )
  | Jump_eq | Jump_neq | Jump_l | Jump_le | Jump_g | Jump_ge -> (
    match args with
    | [ arg1; arg2; dest ] ->
      let v1 = eval_operand s arg1 in
      let v2 = eval_operand s arg2 in
      let l64 = eval_operand s dest in
      let l = Int64.to_int l64 in
      let cmp = match op with
        | Jump_eq -> v1 = v2
        | Jump_neq -> v1 <> v2
        | Jump_l -> v1 < v2
        | Jump_le -> v1 <= v2
        | Jump_g -> v1 > v2
        | Jump_ge -> v1 >= v2
        | _ -> assert false
      in  
      if cmp then
        begin
        match InstrMap.find_opt l s.instr_map with
        | Some _ -> s.jmp_flag <- true;
            Hashtbl.replace s.reg_table PC l64
        | None -> raise (Error (Printf.sprintf "Unknown line number: %d" l, line))
        end
    | _ -> assert false
  )
  | Print -> (
    let print_args arg_list =
      String.concat "" (List.map (
        function
          | Str s -> s
          | x -> Int64.to_string (eval_operand s x)
        ) arg_list
      )
    in
    s.output <- s.output ^ Printf.sprintf "%s\n" (print_args args)
  )
  | Halt -> raise Exit
  | _ -> raise (Error ("Unsupported instruction", line))

let one_step (s : state) =
  let line64 = Hashtbl.find s.reg_table PC in
  let line = Int64.to_int line64 in
  match InstrMap.find_opt line s.instr_map with
  | Some (instr) ->
      eval_instr s line instr;
      if not s.jmp_flag then
        begin
          match InstrMap.find_first_opt (fun l -> l > line) s.instr_map with
          | Some (next_line, _) -> Hashtbl.replace s.reg_table PC (Int64.of_int next_line)
          | None -> raise (Error ("Reached the end of the program without halting", line + 1))
        end
      else
        s.jmp_flag <- false
  | None -> raise (Error ("Missing instruction", line))

let run s =
  try
    while true do
      one_step s
    done
  with Exit -> ()

type load_result =
| ParseError of string
| Ok of state

let load_the_code (code : string) = 
  let lexbuf = Lexing.from_string code in
  try
    let prog = Parser.program Lexer.token lexbuf in
    validate_prog prog;
    let state = initialize prog in
    Ok (state)
  with
  | ParseError (msg) -> ParseError (msg)
  | Error (msg, line) ->
    let msg = Printf.sprintf "Error at line %d: %s" line msg in
    ParseError (msg)
  | _ -> ParseError ("Syntax error")

let test_parser () =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Lexer.token lexbuf in
  Printf.printf "Parse:\n%s\n" (string_of_prog prog)
