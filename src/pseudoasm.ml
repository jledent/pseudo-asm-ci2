open Common
open Asm

module Asm = Asm
exception Error = Error

module InstrMap = Map.Make(Int)
module BlockMap = Map.Make(Int)

let stack_size = 65536

(* Keep track of whether values are addresses or not, for printing purposes. *)
type value =
| Val of int64
| Addr of int64

let string_of_value = function
| Val i -> Int64.to_string i
| Addr i -> Printf.sprintf "@%08Lx" i

let int64_of_value = function
| Val i -> i
| Addr i -> i

let int_of_value = function
| Val i -> Int64.to_int i
| Addr i -> Int64.to_int i

let operate op v1 v2 = match (v1, v2) with
| Val x, Val y -> Val (op x y)
| (Addr x | Val x), (Addr y | Val y) -> Addr (op x y)
let add = operate Int64.add
let sub = operate Int64.sub
let mul = operate Int64.mul
let div = operate Int64.div
let rem = operate Int64.rem

type memory_block = {
  start_address: int;
  size: int;
  data: value array;
}

type state = {
  (* Loaded program: assigns instructions to line numbers *)
  instr_map : instr InstrMap.t;
  (* Register table *)
  reg_table : (reg, value) Hashtbl.t;
  (* Allocated blocks of memory ; also contains the stack at address 0 *)
  mutable memory : memory_block BlockMap.t;
  (* stdout *)
  mutable output : string;
  (* Jump flag set to true when a jump is performed, to avoid incrementing PC *)
  mutable jmp_flag : bool;
  (* Secretly stores the old BP when a Call instruction is used *)
  bp_stack : value Stack.t;
}

let dummy_state = {
  instr_map = InstrMap.empty;
  reg_table = Hashtbl.create 11;
  memory = BlockMap.empty;
  output = "";
  jmp_flag = false;
  bp_stack = Stack.create ();
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
  List.iter (fun r -> Hashtbl.add reg_table r (Val 0L))
    [ R0; R1; R2; R3; R4; R5; R6; R7; PC; SP ];
  Hashtbl.add reg_table BP (Val (-1L));
  let stack = Array.make stack_size (Val 0L) in
  let block = { start_address = 0; size = stack_size; data = stack } in
  let memory = BlockMap.singleton 0 block in
  let bp_stack = Stack.create () in
  { instr_map; reg_table; memory; output=""; jmp_flag=false; bp_stack}

let reset (s : state) =
  Hashtbl.iter (fun r _ -> Hashtbl.replace s.reg_table r (Val 0L)) s.reg_table;
  Hashtbl.replace s.reg_table BP (Val (-1L));
  let stack = Array.make stack_size (Val 0L) in
  let block = { start_address = 0; size = stack_size; data = stack } in
  s.memory <- BlockMap.singleton 0 block;
  s.output <- "";
  s.jmp_flag <- false;
  Stack.clear s.bp_stack

let read_memory memory addr line =
  let result = ref None in
  BlockMap.iter (fun start_addr block ->
    if start_addr <= addr && addr < start_addr + block.size then
      let offset = addr - start_addr in
      result := Some block.data.(offset)
  ) memory;
  match !result with
  | Some v -> v
  | None -> let msg = Printf.sprintf "Segmentation fault! The memory address @%08x is invalid, or was not allocated" addr in
    raise (Error (msg, line))

let write_memory memory addr value line =
  try
    BlockMap.iter (fun start_addr block ->
      if start_addr <= addr && addr < start_addr + block.size then
        let offset = addr - start_addr in
        block.data.(offset) <- value;
        raise Exit;
    ) memory;
    let msg = Printf.sprintf "Segmentation fault! The memory address @%08x is invalid, or was not allocated" addr in
    raise (Error (msg, line))
  with Exit -> ()

let can_allocate_block memory start_addr size =
  let end_addr = start_addr + size - 1 in
  BlockMap.for_all (fun block_start block ->
    let block_end = block_start + block.size - 1 in
    end_addr < block_start || block_end < start_addr
  ) memory

let search_for_free_block memory size line =
  let rec search candidate = function
  | [] -> if candidate + size < max_int then candidate
          else raise (Error ("Not enough space on the heap", line))
  | (start_addr, block) :: rest ->
    if candidate + size <= start_addr then
      candidate
    else
      search (start_addr + block.size) rest
  in
  search 0 (BlockMap.bindings memory)

let allocate_new_block (s : state) (size : int) line =
  let rec try_random_address n =
    if n = 0 then
      raise Not_found
    else
    let start_addr = Random.int (1 lsl 30 - 1) in
    if can_allocate_block s.memory start_addr size then
      start_addr
    else
      try_random_address (n - 1)
  in
  let start_addr =
    try try_random_address 10 
    with Not_found -> search_for_free_block s.memory size line
  in
  let block = { start_address = start_addr; size = size; data = Array.make size (Val 0L) } in
  s.memory <- BlockMap.add start_addr block s.memory;
  Addr (Int64.of_int start_addr)

let validate_instr line (op, args) = match op with
  | Move -> (
    match args with
    | [ Imm _; _ ] -> raise (Error ("The destination of move must be a register or a memory address", line))
    | [ _; _ ] -> ()
    | _ -> raise (Error ("move must have two arguments", line))
  )
  | Add | Sub | Mul | Div | Mod -> (
    match args with
    | [ Imm _; _; _ ] -> raise (Error ("The destination of " ^ string_of_opcode op ^ " must be a register or a memory address", line))
    | [ _; _; _ ] -> ()
    | _ -> raise (Error (string_of_opcode op ^ " must have three arguments", line))
  )
  | Jump | Push | Call -> (
    match args with
    | [ _ ] -> ()
    | _ -> raise (Error (string_of_opcode op ^ " must have one argument", line))
  )
  | Jump_eq | Jump_neq | Jump_l | Jump_le | Jump_g | Jump_ge -> (
    match args with
    | [ _; _; _ ] -> ()
    | _ -> raise (Error (string_of_opcode op ^ " must have three arguments", line))
  ) 
  | Pop -> (
    match args with
    | [ Imm _ ] -> raise (Error ("The destination of pop must be a register or a memory address", line))
    | [ _ ] -> ()
    | _ -> raise (Error ("pop must have one argument", line))
  )
  | Malloc -> (
    match args with
    | [ Imm _; _ ] -> raise (Error ("The destination of malloc must be a register or a memory address", line))
    | [ _; _ ] -> ()
    | _ -> raise (Error ("malloc must have two arguments", line))
  )
  | Halt | Ret -> (
    match args with
    | [] -> ()
    | _ -> raise (Error (string_of_opcode op ^ " must have no arguments", line))
  )
  | Print | Println -> ()

let validate_prog prog =
  List.iter (fun (l, (op, args)) -> validate_instr l (op, args)) prog

let get_address s = function
  | Ind r -> int_of_value (Hashtbl.find s.reg_table r)
  | IndImm (r, i) -> int_of_value (add (Hashtbl.find s.reg_table r) (Val i))
  | IndReg (r1, r2) -> int_of_value (add
                                     (Hashtbl.find s.reg_table r1)
                                     (Hashtbl.find s.reg_table r2))
  | _ -> assert false

let store_at s dest value line =
  match dest with
  | Imm _ | Str _ -> assert false
  | Reg r -> Hashtbl.replace s.reg_table r value
  | Ind _ | IndImm _ | IndReg _ ->
    let addr = get_address s dest in
    write_memory s.memory addr value line

let eval_operand s op line =
  match op with
  | Imm i -> Val i
  | Reg r -> Hashtbl.find s.reg_table r
  | Ind _ | IndImm _ | IndReg _ ->
    let addr = get_address s op in
    read_memory s.memory addr line
  | Str _ -> raise (Error ("String operands are not supported", line))

(* Evaluate an instruction by modifying the state.
 * The [line] argument is only used for error messages.
 * eval_instr is recursive to allow Call and Ret to directly use Push / Jump / Pop
 *)
let rec eval_instr (s : state) (line : int) (op, args) =
  match op with
  | Move -> (
    match args with
    | [ dest; arg1 ] ->
      let v = eval_operand s arg1 line in
      store_at s dest v line
    | _ -> assert false
  )
  | Add | Sub | Mul | Div | Mod -> (
    match args with
    | [ dest; arg1; arg2 ] ->
      let v1 = eval_operand s arg1 line in
      let v2 = eval_operand s arg2 line in
      let res = match op with
        | Add -> add v1 v2
        | Sub -> sub v1 v2
        | Mul -> mul v1 v2
        | Div -> div v1 v2
        | Mod -> rem v1 v2
        | _ -> assert false
      in
      store_at s dest res line
    | _ -> assert false
  )
  | Jump -> (
    match args with
    | [ arg1 ] -> (
      let l64 = eval_operand s arg1 line in
      let l = int_of_value l64 in
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
      let v1 = eval_operand s arg1 line in
      let v2 = eval_operand s arg2 line in
      let l64 = eval_operand s dest line in
      let l = int_of_value l64 in
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
  | Push -> (
    match args with
    | [ arg1 ] ->
      let v = eval_operand s arg1 line in
      let sp = Hashtbl.find s.reg_table SP in
      if int_of_value sp >= stack_size then
        raise (Error ("Stack overflow", line));
      let new_sp = add sp (Val 1L) in
      Hashtbl.replace s.reg_table SP new_sp;
      write_memory s.memory (int_of_value sp) v line
    | _ -> assert false
  )
  | Pop -> (
    match args with
    | [ dest ] ->
      let sp = Hashtbl.find s.reg_table SP in
      if int_of_value sp <= 0 then
        raise (Error ("Cannot pop an empty stack", line));
      let new_sp = sub sp (Val 1L) in
      Hashtbl.replace s.reg_table SP new_sp;
      let v = read_memory s.memory (int_of_value new_sp) line in
      store_at s dest v line
    | _ -> assert false
  )
  | Call -> (
    let sp = Hashtbl.find s.reg_table SP in
    let bp = Hashtbl.find s.reg_table BP in
    Stack.push bp s.bp_stack;
    Hashtbl.replace s.reg_table BP sp;
    eval_instr s line (Push, [ Reg PC ]);
    eval_instr s line (Jump, args)
  )
  | Ret -> (
    eval_instr s line (Pop, [ Reg PC ]);
    let bp = Stack.pop s.bp_stack in
    Hashtbl.replace s.reg_table BP bp
  )
  | Malloc -> (
    match args with
    | [ dest; arg ] ->
      let size64 = eval_operand s arg line in
      let size = int_of_value size64 in
      if size < 0 || size > 1 lsl 20 then
        begin
          let msg = Printf.sprintf "Cannot allocate a memory block of size %Ld" (int64_of_value size64) in
          raise (Error (msg, line))
        end;
      let res = allocate_new_block s size line in
      store_at s dest res line
    | _ -> assert false
  )
  | Print | Println -> (
    let print_args arg_list =
      String.concat "" (List.map (
        function
          | Str s -> s
          | x -> string_of_value (eval_operand s x line)
        ) arg_list
      )
    in
    match op with
    | Print -> s.output <- s.output ^ Printf.sprintf "%s" (print_args args)
    | Println -> s.output <- s.output ^ Printf.sprintf "%s\n" (print_args args)
    | _ -> assert false
  )
  | Halt -> raise Exit

let one_step (s : state) =
  let line64 = Hashtbl.find s.reg_table PC in
  let line = int_of_value line64 in
  match InstrMap.find_opt line s.instr_map with
  | Some (instr) ->
      eval_instr s line instr;
      if not s.jmp_flag then
        begin
          (* Need to recompute the current line because of Ret *)
          let line64 = Hashtbl.find s.reg_table PC in
          let line = int_of_value line64 in        
          match InstrMap.find_first_opt (fun l -> l > line) s.instr_map with
          | Some (next_line, _) -> Hashtbl.replace s.reg_table PC (Val (Int64.of_int next_line))
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