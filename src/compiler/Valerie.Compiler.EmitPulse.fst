module Valerie.Compiler.EmitPulse

open FStar.String
open Valerie.Compiler.AST

let is_digit_char (c:FStar.Char.char) : bool =
  let n = FStar.Char.int_of_char c in
  n >= 48 && n <= 57

let rec all_digits_list (cs:list FStar.Char.char) : Tot bool (decreases cs) =
  match cs with
  | [] -> true
  | c :: rest -> is_digit_char c && all_digits_list rest

let all_digits (s:string) : bool =
  let cs = list_of_string s in
  Cons? cs && all_digits_list cs

let reg_idx_of_name (name:string) : string =
  let clean =
    if length name >= 16 && sub name 0 15 = "(va_op_reg_reg " then
      sub name 15 (length name - 16)
    else if length name >= 16 && sub name 0 15 = "(va_op_vec_reg " then
      sub name 15 (length name - 16)
    else name
  in
  let lower = lowercase clean in
  if lower = "rax" then "0sz"
  else if lower = "rcx" then "1sz"
  else if lower = "rdx" then "2sz"
  else if lower = "rbx" then "3sz"
  else if lower = "rsp" then "4sz"
  else if lower = "rbp" then "5sz"
  else if lower = "rsi" then "6sz"
  else if lower = "rdi" then "7sz"
  else if lower = "r8" then "8sz"
  else if lower = "r9" then "9sz"
  else if lower = "r10" then "10sz"
  else if lower = "r11" then "11sz"
  else if lower = "r12" then "12sz"
  else if lower = "r13" then "13sz"
  else if lower = "r14" then "14sz"
  else if lower = "r15" then "15sz"
  else if lower = "zero" then "0sz"
  else if lower = "ra" then "1sz"
  else if lower = "sp" then "2sz"
  else if lower = "gp" then "3sz"
  else if lower = "tp" then "4sz"
  else if lower = "t0" then "5sz"
  else if lower = "t1" then "6sz"
  else if lower = "t2" then "7sz"
  else if lower = "s0" then "8sz"
  else if lower = "s1" then "9sz"
  else if lower = "a0" then "10sz"
  else if lower = "a1" then "11sz"
  else if lower = "a2" then "12sz"
  else if lower = "a3" then "13sz"
  else if lower = "a4" then "14sz"
  else if lower = "a5" then "15sz"
  else if lower = "a6" then "16sz"
  else if lower = "a7" then "17sz"
  else if lower = "s2" then "18sz"
  else if lower = "s3" then "19sz"
  else if lower = "s4" then "20sz"
  else if lower = "s5" then "21sz"
  else if lower = "s6" then "22sz"
  else if lower = "s7" then "23sz"
  else if lower = "s8" then "24sz"
  else if lower = "s9" then "25sz"
  else if lower = "s10" then "26sz"
  else if lower = "s11" then "27sz"
  else if lower = "t3" then "28sz"
  else if lower = "t4" then "29sz"
  else if lower = "t5" then "30sz"
  else if lower = "t6" then "31sz"
  else if length clean >= 4 && sub clean 0 3 = "xmm" && all_digits (sub clean 3 (length clean - 3)) then
    concat "" [sub clean 3 (length clean - 3); "sz"]
  else if length clean >= 2 then
    let first = sub clean 0 1 in
    let rest = sub clean 1 (length clean - 1) in
    if (first = "x" || first = "r" || first = "X" || first = "R" || first = "v" || first = "V") && all_digits rest then
      concat "" [rest; "sz"]
    else "0sz"
  else "0sz"

let is_reg_name (name:string) : bool =
  let lower = lowercase name in
  lower = "rax" || lower = "rcx" || lower = "rdx" || lower = "rbx" ||
  lower = "rsp" || lower = "rbp" || lower = "rsi" || lower = "rdi" ||
  lower = "r8" || lower = "r9" || lower = "r10" || lower = "r11" ||
  lower = "r12" || lower = "r13" || lower = "r14" || lower = "r15" ||
  lower = "zero" || lower = "ra" || lower = "sp" || lower = "gp" || lower = "tp" ||
  lower = "t0" || lower = "t1" || lower = "t2" || lower = "t3" || lower = "t4" || lower = "t5" || lower = "t6" ||
  lower = "s0" || lower = "s1" || lower = "s2" || lower = "s3" || lower = "s4" || lower = "s5" || lower = "s6" || lower = "s7" || lower = "s8" || lower = "s9" || lower = "s10" || lower = "s11" ||
  lower = "a0" || lower = "a1" || lower = "a2" || lower = "a3" || lower = "a4" || lower = "a5" || lower = "a6" || lower = "a7" ||
  (length name >= 4 && sub name 0 3 = "xmm" && all_digits (sub name 3 (length name - 3))) ||
  (length name >= 16 && (sub name 0 15 = "(va_op_reg_reg " || sub name 0 15 = "(va_op_vec_reg ")) ||
  (length name >= 2 &&
   (let first = sub name 0 1 in
    (first = "x" || first = "r" || first = "X" || first = "R" || first = "v" || first = "V") &&
    all_digits (sub name 1 (length name - 1))))

let reg_idx_of_operand (op:operand) : string =
  match op with
  | OReg (RegNumbered n) -> concat "" [FStar.UInt32.to_string n; "sz"]
  | _ -> "0sz"

let is_buffer_typ (t:vaf_typ) : bool =
  match t with
  | TBuffer _ _ -> true
  | _ -> false

let buffer_elem_pulse_type (t:vaf_typ) : string =
  match t with
  | TBuffer TU8 _ -> "U8.t"
  | TBuffer TU32 _ -> "U32.t"
  | TBuffer TU64 _ -> "U64.t"
  | _ -> "U64.t"

let rec get_buffer_formals (formals:list pformal) : list pformal =
  match formals with
  | [] -> []
  | f :: rest ->
      if is_buffer_typ f.pformal_typ then f :: get_buffer_formals rest
      else get_buffer_formals rest

let string_of_bop (op:bop) : string =
  match op with
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BAnd -> "&&"
  | BOr -> "||"
  | BEq -> "=="
  | BNe -> "<>"
  | BLt -> "<"
  | BGt -> ">"
  | BLe -> "<="
  | BGe -> ">="
  | BImply -> "==>"
  | BEquiv -> "=="

let rec string_of_pulse_expr (e:expr) : Tot string (decreases e) =
  match e with
  | EVar v -> v
  | EConst c -> string_of_int c
  | EBoolConst b -> if b then "true" else "false"
  | EUop UNot inner -> concat "" ["(not "; string_of_pulse_expr inner; ")"]
  | EUop UNeg inner -> concat "" ["(- "; string_of_pulse_expr inner; ")"]
  | EUop UOld inner -> string_of_pulse_expr inner
  | EBop op e1 e2 -> concat "" ["("; string_of_pulse_expr e1; " "; string_of_bop op; " "; string_of_pulse_expr e2; ")"]
  | EApply f args -> concat "" [f; "("; concat_pulse_args args; ")"]

and concat_pulse_args (es:list expr) : Tot string (decreases es) =
  match es with
  | [] -> ""
  | [x] -> string_of_pulse_expr x
  | x :: xs -> concat "" [string_of_pulse_expr x; ", "; concat_pulse_args xs]

let clean_pulse_arg (e:expr) : string =
  match e with
  | EVar v ->
      if is_reg_name v then reg_idx_of_name v
      else v
  | EConst imm -> concat "" [string_of_int imm; "uL"]
  | _ -> string_of_pulse_expr e

let rec concat_pulse_call_args (es:list expr) : Tot string (decreases es) =
  match es with
  | [] -> ""
  | x :: xs -> concat "" [" "; clean_pulse_arg x; concat_pulse_call_args xs]

let is_mem_tag (f:string) : bool =
  f = "Mem" || f = "Mem64" || f = "Mem32" || f = "Mem128"

let extract_mem_operand (e:expr) : option (string & string) =
  match e with
  | EApply f [EVar buf; EConst off] ->
      if is_mem_tag f then
        let off_val = if off >= 8 && off % 8 = 0 then off / 8 else off in
        Some (buf, concat "" [string_of_int off_val; "sz"])
      else None
  | EApply f [EVar buf] ->
      if is_mem_tag f then
        Some (buf, "0sz")
      else None
  | _ -> None

let rec string_of_pulse_invs (invs:list expr) : Tot string (decreases invs) =
  match invs with
  | [] -> ""
  | inv :: rest ->
      concat "" [" **\n    pure ("; string_of_pulse_expr inv; ")"; string_of_pulse_invs rest]

let rec emit_pulse_stmt (indent:string) (s:stmt) : Tot string (decreases s) =
  match s with
  (* Direct Low*-clean instruction AST *)
  | SIns op operands ->
      (match op with
       | OpMov ->
           (match operands with
            | [dst; src] ->
                concat "" [indent; "pulse_step_mov regs "; reg_idx_of_operand dst; " "; reg_idx_of_operand src; ";\n"]
            | _ -> "")
       | OpAdd ->
           (match operands with
            | [dst; s1; s2] ->
                concat "" [indent; "pulse_step_add regs "; reg_idx_of_operand dst; " "; reg_idx_of_operand s1; " "; reg_idx_of_operand s2; ";\n"]
            | _ -> "")
       | OpSub ->
           (match operands with
            | [dst; s1; s2] ->
                concat "" [indent; "pulse_step_sub regs "; reg_idx_of_operand dst; " "; reg_idx_of_operand s1; " "; reg_idx_of_operand s2; ";\n"]
            | _ -> "")
       | OpXor ->
           (match operands with
            | [dst; s1; s2] ->
                concat "" [indent; "pulse_step_xor regs "; reg_idx_of_operand dst; " "; reg_idx_of_operand s1; " "; reg_idx_of_operand s2; ";\n"]
            | _ -> "")
       | OpLoad ->
           (match operands with
            | [dst; OMemBuffer base off] ->
                concat "" [indent; "pulse_step_load regs (mem_of_reg "; reg_idx_of_operand (OReg base); ") "; reg_idx_of_operand dst; " "; FStar.UInt32.to_string off; "sz;\n"]
            | _ -> "")
       | OpStore ->
           (match operands with
            | [OMemBuffer base off; src] ->
                concat "" [indent; "pulse_step_store regs (mem_of_reg "; reg_idx_of_operand (OReg base); ") "; reg_idx_of_operand src; " "; FStar.UInt32.to_string off; "sz;\n"]
            | _ -> "")
       | _ -> "")

  (* Procedure and macro calls *)
  | SCall name args ->
      (match name with
       | "Add" | "IAdd" | "Add64" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_add regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_add_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_add_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Add32" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_add32 regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Sub" | "ISub" | "Sub64" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_sub regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_sub_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_sub_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Sub32" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_sub32 regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "And" | "IAnd" | "And64" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_and regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_and_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_and_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Or" | "IOr" | "Or64" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_or regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_or_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_or_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Eor" | "IEor" | "Xor" | "Xor64" | "Pxor" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_xor regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_xor_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EVar s1] ->
                concat "" [indent; "pulse_step_xor regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; reg_idx_of_name s1; ";\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_xor_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Eor32" | "Xor32" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_xor32 regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Shl" | "IShl" | "Shl64" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_shl regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "ul;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_shl regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "ul;\n"]
            | _ -> "")
       | "Shr" | "IShr" | "Shr64" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_shr regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "ul;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_shr regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "ul;\n"]
            | _ -> "")
       | "Ror" | "IRor" | "Ror64" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_ror regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "ul;\n"]
            | _ -> "")
       | "Rol32" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_rol32 regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "ul;\n"]
            | _ -> "")
       | "Ror32" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_ror32 regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "ul;\n"]
            | _ -> "")
       | "Mov" | "IMov" | "Mov64" ->
           (match args with
            | [EVar dst; mem_e] ->
                (match extract_mem_operand mem_e with
                 | Some (buf, off_sz) ->
                     concat "" [indent; "pulse_step_load regs "; buf; " "; reg_idx_of_name dst; " "; off_sz; ";\n"]
                 | None ->
                     (match mem_e with
                      | EConst imm -> concat "" [indent; "pulse_write_reg regs "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
                      | EVar src -> concat "" [indent; "pulse_step_mov regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
                      | _ -> ""))
            | [mem_e; EVar src] ->
                (match extract_mem_operand mem_e with
                 | Some (buf, off_sz) ->
                     concat "" [indent; "pulse_step_store regs "; buf; " "; reg_idx_of_name src; " "; off_sz; ";\n"]
                 | None -> "")
            | _ -> "")
       | "Load64" | "Ldr" | "Ldr64" ->
           (match args with
            | [EVar dst; EVar buf; EConst offset] ->
                concat "" [indent; "pulse_step_load regs "; buf; " "; reg_idx_of_name dst; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Store64" | "Str" | "Str64" ->
           (match args with
            | [EVar buf; EConst offset; EVar src] ->
                concat "" [indent; "pulse_step_store regs "; buf; " "; reg_idx_of_name src; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Load32" | "Ldr32" ->
           (match args with
            | [EVar dst; EVar buf; EConst offset] ->
                concat "" [indent; "pulse_step_load32 regs "; buf; " "; reg_idx_of_name dst; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Store32" | "Str32" ->
           (match args with
            | [EVar buf; EConst offset; EVar src] ->
                concat "" [indent; "pulse_step_store32 regs "; buf; " "; reg_idx_of_name src; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Load8" | "Ldr8" | "Ldrb" ->
           (match args with
            | [EVar dst; EVar buf; EConst offset] ->
                concat "" [indent; "pulse_step_load8 regs "; buf; " "; reg_idx_of_name dst; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Store8" | "Str8" | "Strb" ->
           (match args with
            | [EVar buf; EConst offset; EVar src] ->
                concat "" [indent; "pulse_step_store8 regs "; buf; " "; reg_idx_of_name src; " "; string_of_int offset; "sz;\n"]
            | _ -> "")
       | "Addi" ->
           (match args with
            | [EVar dst; EVar s1; EConst imm] ->
                concat "" [indent; "pulse_step_add_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; string_of_int imm; "uL;\n"]
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_step_add_imm regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "FmovFromGpr" | "FmovToGpr" | "MovVecLane" | "MovLaneToGpr" | "DupVec" ->
           (match args with
            | [EVar dst; EVar src] ->
                concat "" [indent; "pulse_step_mov regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
            | _ -> "")
       | "MovImm" ->
           (match args with
            | [EVar dst; EConst imm] ->
                concat "" [indent; "pulse_write_reg regs "; reg_idx_of_name dst; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Pmull" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_pmull regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Pmull2" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_pmull2 regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Veor" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_xor regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | [EVar dst; EVar s1] ->
                concat "" [indent; "pulse_step_xor regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; reg_idx_of_name s1; ";\n"]
            | _ -> "")
       | "Veor3" ->
           (match args with
            | [EVar rd; EVar rn; EVar rm; EVar ra] ->
                concat "" [indent; "pulse_step_xor3 regs "; reg_idx_of_name rd; " "; reg_idx_of_name rn; " "; reg_idx_of_name rm; " "; reg_idx_of_name ra; ";\n"]
            | _ -> "")
       | "Vext8" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_vext8 regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Rev8" | "Rbit" ->
           (match args with
            | [EVar dst; EVar src] ->
                concat "" [indent; "pulse_step_rev regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
            | _ -> "")
       | "Sha256sum0" | "Sha256sum1" ->
           (match args with
            | [EVar dst; EVar src] ->
                concat "" [indent; "pulse_step_sha256sum0 regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
            | _ -> "")
       | "Aes64esm" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_aes64esm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Aes64dsm" ->
           (match args with
            | [EVar dst; EVar s1; EVar s2] ->
                concat "" [indent; "pulse_step_aes64dsm regs "; reg_idx_of_name dst; " "; reg_idx_of_name s1; " "; reg_idx_of_name s2; ";\n"]
            | _ -> "")
       | "Aesenc" | "Aesdec" ->
           (match args with
            | [EVar dst; EVar src] ->
                concat "" [indent; "pulse_step_aesenc regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
            | _ -> "")
       | "Pclmulqdq" ->
           (match args with
            | [EVar dst; EVar src; EConst imm] ->
                concat "" [indent; "pulse_step_pclmulqdq regs "; reg_idx_of_name dst; " "; reg_idx_of_name src; " "; string_of_int imm; "uL;\n"]
            | _ -> "")
       | "Paddd" ->
           (match args with
            | [EVar dst; EVar src] ->
                concat "" [indent; "pulse_step_add regs "; reg_idx_of_name dst; " "; reg_idx_of_name dst; " "; reg_idx_of_name src; ";\n"]
            | _ -> "")
       (* Real Procedure Call in Pulse *)
       | proc_call ->
           let extra_args = concat_pulse_call_args args in
           concat "" [indent; "va_pulse_"; proc_call; " regs"; extra_args; ";\n"])

  (* Block of statements *)
  | SBlock stmts ->
      emit_pulse_stmts indent stmts

  (* Assignment in Pulse *)
  | SAssign id expr ->
      (match expr with
       | EConst imm ->
           concat "" [indent; "pulse_write_reg regs "; reg_idx_of_name id; " "; string_of_int imm; "uL;\n"]
       | EVar src ->
           concat "" [indent; "pulse_step_mov regs "; reg_idx_of_name id; " "; reg_idx_of_name src; ";\n"]
       | _ ->
           concat "" [indent; "let "; id; " = "; string_of_pulse_expr expr; ";\n"])

  (* Conditional Branching in Pulse *)
  | SIfElse cond t_branch f_branch ->
      concat "" [
        indent; "if ("; string_of_pulse_expr cond; ") {\n";
        emit_pulse_stmts (concat "" [indent; "  "]) t_branch;
        indent; "} else {\n";
        emit_pulse_stmts (concat "" [indent; "  "]) f_branch;
        indent; "}\n"
      ]

  (* While Loop in Pulse with Separation Logic Invariant *)
  | SWhile cond invs body ->
      let inv_clauses = string_of_pulse_invs invs in
      concat "" [
        indent; "while ("; string_of_pulse_expr cond; ")\n";
        indent; "  invariant b.\n";
        indent; "    (exists* s_loop. pts_to regs s_loop ** pure (Seq.length s_loop == 32)"; inv_clauses; ")\n";
        indent; "{\n";
        emit_pulse_stmts (concat "" [indent; "  "]) body;
        indent; "}\n"
      ]

  (* Assert in Pulse *)
  | SAssert expr ->
      concat "" [indent; "assert pure ("; string_of_pulse_expr expr; ");\n"]

  (* Assume in Pulse *)
  | SAssume expr ->
      concat "" [indent; "assume pure ("; string_of_pulse_expr expr; ");\n"]

  | _ -> ""

and emit_pulse_stmts (indent:string) (stmts:list stmt) : Tot string (decreases stmts) =
  match stmts with
  | [] -> ""
  | s :: rest -> concat "" [emit_pulse_stmt indent s; emit_pulse_stmts indent rest]

let rec emit_buf_params (bufs:list pformal) : string =
  match bufs with
  | [] -> ""
  | b :: rest ->
      concat "" ["  ("; b.pformal_name; ": array "; buffer_elem_pulse_type b.pformal_typ; ")\n"; emit_buf_params rest]

let rec emit_buf_ghosts (bufs:list pformal) : string =
  match bufs with
  | [] -> ""
  | b :: rest ->
      let elem_t = buffer_elem_pulse_type b.pformal_typ in
      let len_req = match b.pformal_typ with
        | TBuffer _ len -> concat "" ["{Seq.length s_"; b.pformal_name; " == "; string_of_int len; "}"]
        | _ -> ""
      in
      let perm_param =
        match b.pformal_kind with
        | FormalIn -> concat "" ["  (#p_"; b.pformal_name; ": perm)\n"]
        | _ -> ""
      in
      concat "" [perm_param; "  (#s_"; b.pformal_name; ": Ghost.erased (Seq.seq "; elem_t; ")"; len_req; ")\n"; emit_buf_ghosts rest]

let rec emit_buf_reqs (bufs:list pformal) : string =
  match bufs with
  | [] -> ""
  | b :: rest ->
      let len_clause = match b.pformal_typ with
        | TBuffer _ len ->
            if len > 0 then concat "" [" **\n    pure (Seq.length s_"; b.pformal_name; " >= "; string_of_int len; ")"]
            else ""
        | _ -> ""
      in
      let perm_clause =
        match b.pformal_kind with
        | FormalIn -> concat "" [" #p_"; b.pformal_name]
        | _ -> ""
      in
      concat "" [" **\n    pts_to "; b.pformal_name; perm_clause; " s_"; b.pformal_name; len_clause; emit_buf_reqs rest]

let rec emit_buf_ens (bufs:list pformal) : string =
  match bufs with
  | [] -> ""
  | b :: rest ->
      match b.pformal_kind with
      | FormalIn ->
          concat "" [" **\n    pts_to "; b.pformal_name; " #p_"; b.pformal_name; " s_"; b.pformal_name; emit_buf_ens rest]
      | _ ->
          concat "" [" **\n    (exists* s_"; b.pformal_name; "_final. pts_to "; b.pformal_name; " s_"; b.pformal_name; "_final)"; emit_buf_ens rest]

let is_imm_formal (f:pformal) : bool =
  f.pformal_kind = FormalInline ||
  f.pformal_name = "imm" ||
  f.pformal_typ = TInt ||
  f.pformal_typ = TU64

let rec emit_ins_params (formals:list pformal) : string =
  match formals with
  | [] -> ""
  | f :: rest ->
      let typ_str =
        if is_buffer_typ f.pformal_typ then concat "" ["array "; buffer_elem_pulse_type f.pformal_typ]
        else if is_imm_formal f then "U64.t"
        else "SZ.t"
      in
      concat "" ["  ("; f.pformal_name; ": "; typ_str; ")\n"; emit_ins_params rest]

let emit_pulse_proc (p:proc_decl) : string =
  let fn_name = concat "" ["va_pulse_"; p.proc_name] in
  let bufs = get_buffer_formals p.proc_formals in
  if p.proc_is_ins then ""
  else
    concat "" [
      "fn "; fn_name; "\n";
      "  (regs: reg_array)\n";
      emit_buf_params bufs;
      "  (#s0: Ghost.erased (Seq.seq U64.t){Seq.length s0 == 32})\n";
      emit_buf_ghosts bufs;
      "  requires\n";
      "    pts_to regs s0"; emit_buf_reqs bufs; "\n";
      "  returns _: unit\n";
      "  ensures\n";
      "    (exists* s1. pts_to regs s1 ** pure (Seq.length s1 == 32))"; emit_buf_ens bufs; "\n";
      "{\n";
      emit_pulse_stmts "  " p.proc_body;
      "  ()\n";
      "}\n\n"
    ]

let rec emit_pulse_procs (procs:list proc_decl) : Tot string (decreases procs) =
  match procs with
  | [] -> ""
  | p :: rest -> concat "" [emit_pulse_proc p; emit_pulse_procs rest]

let is_perm_formal (f:pformal) : bool =
  is_buffer_typ f.pformal_typ && f.pformal_kind = FormalIn

let rec formals_has_perm (formals:list pformal) : Tot bool (decreases formals) =
  match formals with
  | [] -> false
  | f :: rest -> is_perm_formal f || formals_has_perm rest

let rec procs_has_perm (procs:list proc_decl) : Tot bool (decreases procs) =
  match procs with
  | [] -> false
  | p :: rest -> formals_has_perm p.proc_formals || procs_has_perm rest

let is_u8_formal (f:pformal) : bool =
  match f.pformal_typ with
  | TBuffer TU8 _ -> true
  | _ -> false

let rec formals_has_u8 (formals:list pformal) : Tot bool (decreases formals) =
  match formals with
  | [] -> false
  | f :: rest -> is_u8_formal f || formals_has_u8 rest

let rec procs_has_u8 (procs:list proc_decl) : Tot bool (decreases procs) =
  match procs with
  | [] -> false
  | p :: rest -> formals_has_u8 p.proc_formals || procs_has_u8 rest

let is_u32_formal (f:pformal) : bool =
  match f.pformal_typ with
  | TBuffer TU32 _ -> true
  | _ -> false

let rec formals_has_u32 (formals:list pformal) : Tot bool (decreases formals) =
  match formals with
  | [] -> false
  | f :: rest -> is_u32_formal f || formals_has_u32 rest

let rec procs_has_u32 (procs:list proc_decl) : Tot bool (decreases procs) =
  match procs with
  | [] -> false
  | p :: rest -> formals_has_u32 p.proc_formals || procs_has_u32 rest

let emit_pulse_module (m:module_decl) : string =
  let perm_header =
    if procs_has_perm m.mod_procs then "open PulseCore.FractionalPermission\n" else ""
  in
  let u8_alias =
    if procs_has_u8 m.mod_procs then "module U8 = FStar.UInt8\n" else ""
  in
  let u32_alias =
    if procs_has_u32 m.mod_procs then "module U32 = FStar.UInt32\n" else ""
  in
  concat "" [
    "module "; m.mod_name; "_Pulse\n";
    "#lang-pulse\n\n";
    "open Pulse.Lib.Pervasives\n";
    "open Pulse.Lib.Array\n";
    perm_header;
    "open Valerie.Pulse.Machine\n";
    u8_alias;
    u32_alias;
    "module U64 = FStar.UInt64\n";
    "module SZ = FStar.SizeT\n";
    "module Seq = FStar.Seq\n\n";
    emit_pulse_procs m.mod_procs
  ]
