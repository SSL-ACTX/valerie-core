(* Vale Native Compiler CLI Driver - valerie-core *)
open Ast

let usage_msg = "valerie-compiler [options] <source.vaf>\nOptions:"

let in_file = ref ""
let out_fstar = ref ""
let out_fsti = ref ""
let out_ir = ref ""
let out_pulse = ref ""
let out_asm = ref ""
let arch_target = ref Valerie_Compiler_AST.TargetAArch64
let fstar_text_only = ref false

let speclist = [
  ("-in", Arg.Set_string in_file, "Input .vaf source file");
  ("-out", Arg.Set_string out_fstar, "Output F* (.fst) implementation file");
  ("-outi", Arg.Set_string out_fsti, "Output F* (.fsti) interface file");
  ("-pulse", Arg.Set_string out_pulse, "Output Pulse separation logic (.fst) module");
  ("-ir", Arg.Set_string out_ir, "Output Vale IR AST dump");
  ("-asm", Arg.Set_string out_asm, "Output textual target assembly (.s)");
  ("-fstarText", Arg.Set fstar_text_only, "Emit F* text output");
  ("-arch", Arg.String (fun s ->
      match String.lowercase_ascii s with
      | "aarch64" | "arm64" -> arch_target := Valerie_Compiler_AST.TargetAArch64
      | "arm32" | "arm" -> arch_target := Valerie_Compiler_AST.TargetArm32
      | "x64" | "x86_64" -> arch_target := Valerie_Compiler_AST.TargetX64
      | "riscv64" | "riscv" -> arch_target := Valerie_Compiler_AST.TargetRiscv64
      | _ -> Printf.eprintf "Warning: unrecognized architecture '%s', defaulting to aarch64\n" s
    ), "Target architecture (aarch64, arm32, x64, riscv64)");
]

let write_file path content =
  if path <> "" then begin
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Printf.printf "Wrote %d bytes to %s\n" (String.length content) path
  end

let () =
  Arg.parse speclist (fun anon -> if !in_file = "" then in_file := anon) usage_msg;
  if !in_file = "" then begin
    Printf.eprintf "valerie-compiler: error: no input .vaf file specified\n";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  let fn = !in_file in
  let ic = try open_in fn with Sys_error msg ->
    Printf.eprintf "valerie-compiler: error: cannot open '%s': %s\n" fn msg;
    exit 1
  in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;

  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fn };

  let (includes, decls) =
    try Parse.start Lex.lextoken lexbuf
    with exn ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "valerie-compiler: parse error in %s at line %d, col %d: %s\n"
        pos.Lexing.pos_fname pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        (Printexc.to_string exn);
      exit 1
  in

  let mod_name = ref (Filename.remove_extension (Filename.basename fn)) in
  let verbatim_interface = ref [] in
  let verbatim_impl = ref [] in

  List.iter (fun (_loc, d) ->
    match d with
    | DPragma (ModuleName m) -> mod_name := m
    | DPragma (Target t) ->
        (match String.lowercase_ascii t with
         | "aarch64" | "arm64" -> arch_target := Valerie_Compiler_AST.TargetAArch64
         | "arm32" | "arm" -> arch_target := Valerie_Compiler_AST.TargetArm32
         | "x64" | "x86_64" -> arch_target := Valerie_Compiler_AST.TargetX64
         | "riscv64" | "riscv" -> arch_target := Valerie_Compiler_AST.TargetRiscv64
         | _ -> ())
    | DVerbatim (attrs, lines) ->
        let is_interface = List.exists (function
          | (Id "interface", _) -> true
          | _ -> false
        ) attrs in
        if is_interface then
          verbatim_interface := !verbatim_interface @ lines
        else
          verbatim_impl := !verbatim_impl @ lines
    | _ -> ()
  ) decls;

  let vaf_mod = Ast_bridge.bridge_decls !mod_name includes decls !arch_target in
  let res = Valerie_Compiler_Driver.compile_vaf_module vaf_mod !arch_target in

  let interface_header = String.concat "\n" !verbatim_interface in
  let impl_header = String.concat "\n" !verbatim_impl in

  let full_fsti = Printf.sprintf "module %s\n\n%s\n" !mod_name interface_header in
  let full_fst =
    if String.length res.Valerie_Compiler_Driver.cr_fstar >= 7 &&
       String.sub res.Valerie_Compiler_Driver.cr_fstar 0 7 = "module " then
      let rest = String.sub res.Valerie_Compiler_Driver.cr_fstar
                   (String.index res.Valerie_Compiler_Driver.cr_fstar '\n' + 1)
                   (String.length res.Valerie_Compiler_Driver.cr_fstar - String.index res.Valerie_Compiler_Driver.cr_fstar '\n' - 1)
      in
      if !out_fsti <> "" then
        Printf.sprintf "module %s\n\n%s\n\n%s\n" !mod_name impl_header rest
      else
        Printf.sprintf "module %s\n\n%s\n\n%s\n\n%s\n" !mod_name interface_header impl_header rest
    else
      if !out_fsti <> "" then
        Printf.sprintf "module %s\n\n%s\n\n%s\n" !mod_name impl_header res.Valerie_Compiler_Driver.cr_fstar
      else
        Printf.sprintf "module %s\n\n%s\n\n%s\n\n%s\n" !mod_name interface_header impl_header res.Valerie_Compiler_Driver.cr_fstar
  in

  write_file !out_fsti full_fsti;
  write_file !out_fstar full_fst;
  write_file !out_pulse res.Valerie_Compiler_Driver.cr_pulse;
  write_file !out_ir res.Valerie_Compiler_Driver.cr_ir;
  write_file !out_asm res.Valerie_Compiler_Driver.cr_asm;

  if !out_fstar = "" && !out_fsti = "" && !out_pulse = "" && !out_ir = "" && !out_asm = "" then begin
    print_string full_fst
  end
