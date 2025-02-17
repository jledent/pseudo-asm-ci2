open Pseudoasm
open Asm
open Js_of_ocaml
module Html = Dom_html

let state = ref dummy_state

let hide error_message =
  error_message##.textContent := Js.null;
  error_message##.classList##add (Js.string "hidden")

let display_error error_message msg =
  error_message##.textContent := Js.some (Js.string msg);
  error_message##.classList##remove (Js.string "hidden")

let load_program input loaded_program error_message =
  let input_content = Js.to_string input##.value in
  match load_the_code input_content with
  | ParseError msg -> 
    display_error error_message msg;
    loaded_program##.textContent := Js.null;
    state := dummy_state
  | Ok (st) -> 
    hide error_message;
    state := st

let run_program error_message =
  try
    run !state
  with
  | Error (msg, line) ->
    let msg = Printf.sprintf "Error at line %d: %s" line msg in
    display_error error_message msg
  | _ ->
    display_error error_message "Unknown error"

let next_instruction error_message =
  try
    one_step !state
  with
  | Exit -> ()
  | Error (msg, line) ->
    let msg = Printf.sprintf "Error at line %d: %s" line msg in
    display_error error_message msg
  | _ ->
    display_error error_message "Unknown error"

let display_registers doc registers =
  registers##.innerHTML := Js.string "";
  let table = Html.createTable doc in
  table##.className := Js.string "registers-table w-full border-collapse";
  let row1 = Html.createTr doc in
  let row2 = Html.createTr doc in
  List.iter (fun reg ->
    let reg_value = Hashtbl.find !state.reg_table reg in
    let cell1 = Html.createTh doc in
    let cell2 = Html.createTd doc in
    cell1##.textContent := Js.some (Js.string (Asm.string_of_reg reg));
    cell2##.textContent := Js.some (Js.string (Int64.to_string reg_value));
    Dom.appendChild row1 cell1;
    Dom.appendChild row2 cell2
  ) [R0; R1; R2; R3; R4; R5; R6; R7; PC; SP; BP];
  Dom.appendChild table row1;
  Dom.appendChild table row2;
  Dom.appendChild registers table

let display_loaded_program doc loaded_program =
  let current_line = Int64.to_int (Hashtbl.find !state.reg_table PC) in
  loaded_program##.textContent := Js.null;
  InstrMap.iter (fun line instr ->
    let str = Asm.string_of_ins (line, instr) in
    let div = Html.createDiv doc in
    if line = current_line then
      div##.classList##add (Js.string "bg-blue-200");
    div##.classList##add (Js.string "px-2");
    div##.textContent := Js.some (Js.string str);
    Dom.appendChild loaded_program div
  ) !state.instr_map

let onload _ =
  let doc = Html.document in
  let input =
    Js.coerce_opt (doc##getElementById (Js.string "program-input")) Html.CoerceTo.textarea (fun _ -> assert false)
  in
  let load_button =
    Js.coerce_opt (doc##getElementById (Js.string "load-btn")) Html.CoerceTo.button (fun _ -> assert false)
  in
  let run_button =
    Js.coerce_opt (doc##getElementById (Js.string "run-btn")) Html.CoerceTo.button (fun _ -> assert false)
  in
  let next_button =
    Js.coerce_opt (doc##getElementById (Js.string "next-btn")) Html.CoerceTo.button (fun _ -> assert false)
  in
  let reset_button =
    Js.coerce_opt (doc##getElementById (Js.string "reset-btn")) Html.CoerceTo.button (fun _ -> assert false)
  in
  let loaded_program =
    Js.coerce_opt (doc##getElementById (Js.string "loaded-program")) Html.CoerceTo.div (fun _ -> assert false)
  in
  let error_message =
    Js.coerce_opt (doc##getElementById (Js.string "error-message")) Html.CoerceTo.div (fun _ -> assert false)
  in
  let registers =
    Js.coerce_opt (doc##getElementById (Js.string "registers")) Html.CoerceTo.div (fun _ -> assert false)
  in
  let output =
    Js.coerce_opt (doc##getElementById (Js.string "output")) Html.CoerceTo.div (fun _ -> assert false)
  in
  let show_state () =
    display_registers doc registers;
    display_loaded_program doc loaded_program;
    output##.textContent := Js.some (Js.string !state.output);
  in
  load_button##.onclick := Html.handler (fun _ ->
    load_program input loaded_program error_message;
    show_state ();
    Js._true
  );
  run_button##.onclick := Html.handler (fun _ ->
    run_program error_message;
    show_state ();
    Js._true
  );
  next_button##.onclick := Html.handler (fun _ ->
    next_instruction error_message;
    show_state ();
    Js._true
  );
  reset_button##.onclick := Html.handler (fun _ ->
    reset !state;
    hide error_message;
    show_state ();
    Js._true
  );
  Js._true

let _ = Html.window##.onload := Html.handler onload