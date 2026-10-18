open Prims
let is_digit_char (c : FStar_Char.char) : Prims.bool=
  let n = FStar_Char.int_of_char c in
  (n >= (Prims.of_int 48)) && (n <= (Prims.of_int 57))
let rec all_digits_list (cs : FStar_Char.char Prims.list) : Prims.bool=
  match cs with
  | [] -> true
  | c::rest -> (is_digit_char c) && (all_digits_list rest)
let all_digits (s : Prims.string) : Prims.bool=
  let cs = FStar_String.list_of_string s in
  (match cs with | hd::tl -> true | uu___ -> false) && (all_digits_list cs)
let reg_idx_of_name (name : Prims.string) : Prims.string=
  let clean =
    if
      ((FStar_String.strlen name) >= (Prims.of_int 16)) &&
        ((FStar_String.sub name Prims.int_zero (Prims.of_int 15)) =
           "(va_op_reg_reg ")
    then
      FStar_String.sub name (Prims.of_int 15)
        ((FStar_String.strlen name) - (Prims.of_int 16))
    else
      if
        ((FStar_String.strlen name) >= (Prims.of_int 16)) &&
          ((FStar_String.sub name Prims.int_zero (Prims.of_int 15)) =
             "(va_op_vec_reg ")
      then
        FStar_String.sub name (Prims.of_int 15)
          ((FStar_String.strlen name) - (Prims.of_int 16))
      else name in
  let lower = FStar_String.lowercase clean in
  if lower = "rax"
  then "0sz"
  else
    if lower = "rcx"
    then "1sz"
    else
      if lower = "rdx"
      then "2sz"
      else
        if lower = "rbx"
        then "3sz"
        else
          if lower = "rsp"
          then "4sz"
          else
            if lower = "rbp"
            then "5sz"
            else
              if lower = "rsi"
              then "6sz"
              else
                if lower = "rdi"
                then "7sz"
                else
                  if lower = "r8"
                  then "8sz"
                  else
                    if lower = "r9"
                    then "9sz"
                    else
                      if lower = "r10"
                      then "10sz"
                      else
                        if lower = "r11"
                        then "11sz"
                        else
                          if lower = "r12"
                          then "12sz"
                          else
                            if lower = "r13"
                            then "13sz"
                            else
                              if lower = "r14"
                              then "14sz"
                              else
                                if lower = "r15"
                                then "15sz"
                                else
                                  if lower = "zero"
                                  then "0sz"
                                  else
                                    if lower = "ra"
                                    then "1sz"
                                    else
                                      if lower = "sp"
                                      then "2sz"
                                      else
                                        if lower = "gp"
                                        then "3sz"
                                        else
                                          if lower = "tp"
                                          then "4sz"
                                          else
                                            if lower = "t0"
                                            then "5sz"
                                            else
                                              if lower = "t1"
                                              then "6sz"
                                              else
                                                if lower = "t2"
                                                then "7sz"
                                                else
                                                  if lower = "s0"
                                                  then "8sz"
                                                  else
                                                    if lower = "s1"
                                                    then "9sz"
                                                    else
                                                      if lower = "a0"
                                                      then "10sz"
                                                      else
                                                        if lower = "a1"
                                                        then "11sz"
                                                        else
                                                          if lower = "a2"
                                                          then "12sz"
                                                          else
                                                            if lower = "a3"
                                                            then "13sz"
                                                            else
                                                              if lower = "a4"
                                                              then "14sz"
                                                              else
                                                                if
                                                                  lower =
                                                                    "a5"
                                                                then "15sz"
                                                                else
                                                                  if
                                                                    lower =
                                                                    "a6"
                                                                  then "16sz"
                                                                  else
                                                                    if
                                                                    lower =
                                                                    "a7"
                                                                    then
                                                                    "17sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s2"
                                                                    then
                                                                    "18sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s3"
                                                                    then
                                                                    "19sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s4"
                                                                    then
                                                                    "20sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s5"
                                                                    then
                                                                    "21sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s6"
                                                                    then
                                                                    "22sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s7"
                                                                    then
                                                                    "23sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s8"
                                                                    then
                                                                    "24sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s9"
                                                                    then
                                                                    "25sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s10"
                                                                    then
                                                                    "26sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "s11"
                                                                    then
                                                                    "27sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "t3"
                                                                    then
                                                                    "28sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "t4"
                                                                    then
                                                                    "29sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "t5"
                                                                    then
                                                                    "30sz"
                                                                    else
                                                                    if
                                                                    lower =
                                                                    "t6"
                                                                    then
                                                                    "31sz"
                                                                    else
                                                                    if
                                                                    (((FStar_String.strlen
                                                                    clean) >=
                                                                    (Prims.of_int
                                                                    4)) &&
                                                                    ((FStar_String.sub
                                                                    clean
                                                                    Prims.int_zero
                                                                    (Prims.of_int
                                                                    3)) =
                                                                    "xmm"))
                                                                    &&
                                                                    (all_digits
                                                                    (FStar_String.sub
                                                                    clean
                                                                    (Prims.of_int
                                                                    3)
                                                                    ((FStar_String.strlen
                                                                    clean) -
                                                                    (Prims.of_int
                                                                    3))))
                                                                    then
                                                                    FStar_String.concat
                                                                    ""
                                                                    [
                                                                    FStar_String.sub
                                                                    clean
                                                                    (Prims.of_int
                                                                    3)
                                                                    ((FStar_String.strlen
                                                                    clean) -
                                                                    (Prims.of_int
                                                                    3));
                                                                    "sz"]
                                                                    else
                                                                    if
                                                                    (FStar_String.strlen
                                                                    clean) >=
                                                                    (Prims.of_int
                                                                    2)
                                                                    then
                                                                    (let first
                                                                    =
                                                                    FStar_String.sub
                                                                    clean
                                                                    Prims.int_zero
                                                                    Prims.int_one in
                                                                    let rest
                                                                    =
                                                                    FStar_String.sub
                                                                    clean
                                                                    Prims.int_one
                                                                    ((FStar_String.strlen
                                                                    clean) -
                                                                    Prims.int_one) in
                                                                    if
                                                                    ((((((first
                                                                    = "x") ||
                                                                    (first =
                                                                    "r")) ||
                                                                    (first =
                                                                    "X")) ||
                                                                    (first =
                                                                    "R")) ||
                                                                    (first =
                                                                    "v")) ||
                                                                    (first =
                                                                    "V")) &&
                                                                    (all_digits
                                                                    rest)
                                                                    then
                                                                    FStar_String.concat
                                                                    ""
                                                                    [rest;
                                                                    "sz"]
                                                                    else
                                                                    "0sz")
                                                                    else
                                                                    "0sz"
let is_reg_name (name : Prims.string) : Prims.bool=
  let lower = FStar_String.lowercase name in
  ((((((((((((((((((((((((((((((((((((((((((((((((((lower = "rax") ||
                                                     (lower = "rcx"))
                                                    || (lower = "rdx"))
                                                   || (lower = "rbx"))
                                                  || (lower = "rsp"))
                                                 || (lower = "rbp"))
                                                || (lower = "rsi"))
                                               || (lower = "rdi"))
                                              || (lower = "r8"))
                                             || (lower = "r9"))
                                            || (lower = "r10"))
                                           || (lower = "r11"))
                                          || (lower = "r12"))
                                         || (lower = "r13"))
                                        || (lower = "r14"))
                                       || (lower = "r15"))
                                      || (lower = "zero"))
                                     || (lower = "ra"))
                                    || (lower = "sp"))
                                   || (lower = "gp"))
                                  || (lower = "tp"))
                                 || (lower = "t0"))
                                || (lower = "t1"))
                               || (lower = "t2"))
                              || (lower = "t3"))
                             || (lower = "t4"))
                            || (lower = "t5"))
                           || (lower = "t6"))
                          || (lower = "s0"))
                         || (lower = "s1"))
                        || (lower = "s2"))
                       || (lower = "s3"))
                      || (lower = "s4"))
                     || (lower = "s5"))
                    || (lower = "s6"))
                   || (lower = "s7"))
                  || (lower = "s8"))
                 || (lower = "s9"))
                || (lower = "s10"))
               || (lower = "s11"))
              || (lower = "a0"))
             || (lower = "a1"))
            || (lower = "a2"))
           || (lower = "a3"))
          || (lower = "a4"))
         || (lower = "a5"))
        || (lower = "a6"))
       || (lower = "a7"))
      ||
      ((((FStar_String.strlen name) >= (Prims.of_int 4)) &&
          ((FStar_String.sub name Prims.int_zero (Prims.of_int 3)) = "xmm"))
         &&
         (all_digits
            (FStar_String.sub name (Prims.of_int 3)
               ((FStar_String.strlen name) - (Prims.of_int 3))))))
     ||
     (((FStar_String.strlen name) >= (Prims.of_int 16)) &&
        (((FStar_String.sub name Prims.int_zero (Prims.of_int 15)) =
            "(va_op_reg_reg ")
           ||
           ((FStar_String.sub name Prims.int_zero (Prims.of_int 15)) =
              "(va_op_vec_reg "))))
    ||
    (((FStar_String.strlen name) >= (Prims.of_int 2)) &&
       (let first = FStar_String.sub name Prims.int_zero Prims.int_one in
        ((((((first = "x") || (first = "r")) || (first = "X")) ||
             (first = "R"))
            || (first = "v"))
           || (first = "V"))
          &&
          (all_digits
             (FStar_String.sub name Prims.int_one
                ((FStar_String.strlen name) - Prims.int_one)))))
let reg_idx_of_operand (op : Valerie_Compiler_AST.operand) : Prims.string=
  match op with
  | Valerie_Compiler_AST.OReg (Valerie_Compiler_AST.RegNumbered n) ->
      FStar_String.concat "" [FStar_UInt32.to_string n; "sz"]
  | uu___ -> "0sz"
let is_buffer_typ (t : Valerie_Compiler_AST.vaf_typ) : Prims.bool=
  match t with
  | Valerie_Compiler_AST.TBuffer (uu___, uu___1) -> true
  | uu___ -> false
let buffer_elem_pulse_type (t : Valerie_Compiler_AST.vaf_typ) : Prims.string=
  match t with
  | Valerie_Compiler_AST.TBuffer (Valerie_Compiler_AST.TU8, uu___) -> "U8.t"
  | Valerie_Compiler_AST.TBuffer (Valerie_Compiler_AST.TU32, uu___) ->
      "U32.t"
  | Valerie_Compiler_AST.TBuffer (Valerie_Compiler_AST.TU64, uu___) ->
      "U64.t"
  | uu___ -> "U64.t"
let rec get_buffer_formals
  (formals : Valerie_Compiler_AST.pformal Prims.list) :
  Valerie_Compiler_AST.pformal Prims.list=
  match formals with
  | [] -> []
  | f::rest ->
      if is_buffer_typ f.Valerie_Compiler_AST.pformal_typ
      then f :: (get_buffer_formals rest)
      else get_buffer_formals rest
let string_of_bop (op : Valerie_Compiler_AST.bop) : Prims.string=
  match op with
  | Valerie_Compiler_AST.BAdd -> "+"
  | Valerie_Compiler_AST.BSub -> "-"
  | Valerie_Compiler_AST.BMul -> "*"
  | Valerie_Compiler_AST.BDiv -> "/"
  | Valerie_Compiler_AST.BMod -> "%"
  | Valerie_Compiler_AST.BAnd -> "&&"
  | Valerie_Compiler_AST.BOr -> "||"
  | Valerie_Compiler_AST.BEq -> "=="
  | Valerie_Compiler_AST.BNe -> "<>"
  | Valerie_Compiler_AST.BLt -> "<"
  | Valerie_Compiler_AST.BGt -> ">"
  | Valerie_Compiler_AST.BLe -> "<="
  | Valerie_Compiler_AST.BGe -> ">="
  | Valerie_Compiler_AST.BImply -> "==>"
  | Valerie_Compiler_AST.BEquiv -> "=="
let rec string_of_pulse_expr (e : Valerie_Compiler_AST.expr) : Prims.string=
  match e with
  | Valerie_Compiler_AST.EVar v -> v
  | Valerie_Compiler_AST.EConst c -> Prims.string_of_int c
  | Valerie_Compiler_AST.EBoolConst b -> if b then "true" else "false"
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNot, inner) ->
      FStar_String.concat "" ["(not "; string_of_pulse_expr inner; ")"]
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNeg, inner) ->
      FStar_String.concat "" ["(- "; string_of_pulse_expr inner; ")"]
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UOld, inner) ->
      string_of_pulse_expr inner
  | Valerie_Compiler_AST.EBop (op, e1, e2) ->
      FStar_String.concat ""
        ["(";
        string_of_pulse_expr e1;
        " ";
        string_of_bop op;
        " ";
        string_of_pulse_expr e2;
        ")"]
  | Valerie_Compiler_AST.EApply (f, args) ->
      FStar_String.concat "" [f; "("; concat_pulse_args args; ")"]
and concat_pulse_args (es : Valerie_Compiler_AST.expr Prims.list) :
  Prims.string=
  match es with
  | [] -> ""
  | x::[] -> string_of_pulse_expr x
  | x::xs ->
      FStar_String.concat ""
        [string_of_pulse_expr x; ", "; concat_pulse_args xs]
let clean_pulse_arg (e : Valerie_Compiler_AST.expr) : Prims.string=
  match e with
  | Valerie_Compiler_AST.EVar v ->
      if is_reg_name v then reg_idx_of_name v else v
  | Valerie_Compiler_AST.EConst imm ->
      FStar_String.concat "" [Prims.string_of_int imm; "uL"]
  | uu___ -> string_of_pulse_expr e
let rec concat_pulse_call_args (es : Valerie_Compiler_AST.expr Prims.list) :
  Prims.string=
  match es with
  | [] -> ""
  | x::xs ->
      FStar_String.concat ""
        [" "; clean_pulse_arg x; concat_pulse_call_args xs]
let is_mem_tag (f : Prims.string) : Prims.bool=
  (((f = "Mem") || (f = "Mem64")) || (f = "Mem32")) || (f = "Mem128")
let extract_mem_operand (e : Valerie_Compiler_AST.expr) :
  (Prims.string * Prims.string) FStar_Pervasives_Native.option=
  match e with
  | Valerie_Compiler_AST.EApply
      (f, (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
       off)::[])
      ->
      if is_mem_tag f
      then
        let off_val =
          if
            (off >= (Prims.of_int 8)) &&
              (((mod) off (Prims.of_int 8)) = Prims.int_zero)
          then off / (Prims.of_int 8)
          else off in
        FStar_Pervasives_Native.Some
          (buf, (FStar_String.concat "" [Prims.string_of_int off_val; "sz"]))
      else FStar_Pervasives_Native.None
  | Valerie_Compiler_AST.EApply (f, (Valerie_Compiler_AST.EVar buf)::[]) ->
      if is_mem_tag f
      then FStar_Pervasives_Native.Some (buf, "0sz")
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
let rec string_of_pulse_invs (invs : Valerie_Compiler_AST.expr Prims.list) :
  Prims.string=
  match invs with
  | [] -> ""
  | inv::rest ->
      FStar_String.concat ""
        [" **\n    pure (";
        string_of_pulse_expr inv;
        ")";
        string_of_pulse_invs rest]
let rec emit_pulse_stmt (indent : Prims.string)
  (s : Valerie_Compiler_AST.stmt) : Prims.string=
  match s with
  | Valerie_Compiler_AST.SIns (op, operands) ->
      (match op with
       | Valerie_Compiler_AST.OpMov ->
           (match operands with
            | dst::src::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_operand dst;
                  " ";
                  reg_idx_of_operand src;
                  ";\n"]
            | uu___ -> "")
       | Valerie_Compiler_AST.OpAdd ->
           (match operands with
            | dst::s1::s2::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add regs ";
                  reg_idx_of_operand dst;
                  " ";
                  reg_idx_of_operand s1;
                  " ";
                  reg_idx_of_operand s2;
                  ";\n"]
            | uu___ -> "")
       | Valerie_Compiler_AST.OpSub ->
           (match operands with
            | dst::s1::s2::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub regs ";
                  reg_idx_of_operand dst;
                  " ";
                  reg_idx_of_operand s1;
                  " ";
                  reg_idx_of_operand s2;
                  ";\n"]
            | uu___ -> "")
       | Valerie_Compiler_AST.OpXor ->
           (match operands with
            | dst::s1::s2::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_operand dst;
                  " ";
                  reg_idx_of_operand s1;
                  " ";
                  reg_idx_of_operand s2;
                  ";\n"]
            | uu___ -> "")
       | Valerie_Compiler_AST.OpLoad ->
           (match operands with
            | dst::(Valerie_Compiler_AST.OMemBuffer (base, off))::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load regs (mem_of_reg ";
                  reg_idx_of_operand (Valerie_Compiler_AST.OReg base);
                  ") ";
                  reg_idx_of_operand dst;
                  " ";
                  FStar_UInt32.to_string off;
                  "sz;\n"]
            | uu___ -> "")
       | Valerie_Compiler_AST.OpStore ->
           (match operands with
            | (Valerie_Compiler_AST.OMemBuffer (base, off))::src::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store regs (mem_of_reg ";
                  reg_idx_of_operand (Valerie_Compiler_AST.OReg base);
                  ") ";
                  reg_idx_of_operand src;
                  " ";
                  FStar_UInt32.to_string off;
                  "sz;\n"]
            | uu___ -> "")
       | uu___ -> "")
  | Valerie_Compiler_AST.SCall (name, args) ->
      (match name with
       | "Add" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "IAdd" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Add64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Add32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Sub" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "ISub" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Sub64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Sub32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sub32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "And" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "IAnd" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "And64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_and_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Or" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "IOr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Or64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_or_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Eor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "IEor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Xor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Xor64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Pxor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Eor32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Xor32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Shl" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "IShl" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Shl64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shl regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Shr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "IShr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Shr64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_shr regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Ror" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_ror regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "IRor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_ror regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Ror64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_ror regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Rol32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_rol32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Ror32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_ror32 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "ul;\n"]
            | uu___ -> "")
       | "Mov" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::mem_e::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_load regs ";
                       buf;
                       " ";
                       reg_idx_of_name dst;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None ->
                     (match mem_e with
                      | Valerie_Compiler_AST.EConst imm ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_write_reg regs ";
                            reg_idx_of_name dst;
                            " ";
                            Prims.string_of_int imm;
                            "uL;\n"]
                      | Valerie_Compiler_AST.EVar src ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_step_mov regs ";
                            reg_idx_of_name dst;
                            " ";
                            reg_idx_of_name src;
                            ";\n"]
                      | uu___ -> ""))
            | mem_e::(Valerie_Compiler_AST.EVar src)::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_store regs ";
                       buf;
                       " ";
                       reg_idx_of_name src;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None -> "")
            | uu___ -> "")
       | "IMov" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::mem_e::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_load regs ";
                       buf;
                       " ";
                       reg_idx_of_name dst;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None ->
                     (match mem_e with
                      | Valerie_Compiler_AST.EConst imm ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_write_reg regs ";
                            reg_idx_of_name dst;
                            " ";
                            Prims.string_of_int imm;
                            "uL;\n"]
                      | Valerie_Compiler_AST.EVar src ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_step_mov regs ";
                            reg_idx_of_name dst;
                            " ";
                            reg_idx_of_name src;
                            ";\n"]
                      | uu___ -> ""))
            | mem_e::(Valerie_Compiler_AST.EVar src)::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_store regs ";
                       buf;
                       " ";
                       reg_idx_of_name src;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None -> "")
            | uu___ -> "")
       | "Mov64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::mem_e::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_load regs ";
                       buf;
                       " ";
                       reg_idx_of_name dst;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None ->
                     (match mem_e with
                      | Valerie_Compiler_AST.EConst imm ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_write_reg regs ";
                            reg_idx_of_name dst;
                            " ";
                            Prims.string_of_int imm;
                            "uL;\n"]
                      | Valerie_Compiler_AST.EVar src ->
                          FStar_String.concat ""
                            [indent;
                            "pulse_step_mov regs ";
                            reg_idx_of_name dst;
                            " ";
                            reg_idx_of_name src;
                            ";\n"]
                      | uu___ -> ""))
            | mem_e::(Valerie_Compiler_AST.EVar src)::[] ->
                (match extract_mem_operand mem_e with
                 | FStar_Pervasives_Native.Some (buf, off_sz) ->
                     FStar_String.concat ""
                       [indent;
                       "pulse_step_store regs ";
                       buf;
                       " ";
                       reg_idx_of_name src;
                       " ";
                       off_sz;
                       ";\n"]
                 | FStar_Pervasives_Native.None -> "")
            | uu___ -> "")
       | "Load64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Ldr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Ldr64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Store64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Str" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Str64" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Load32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load32 regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Ldr32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load32 regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Store32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store32 regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Str32" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store32 regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Load8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Ldr8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Ldrb" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                buf)::(Valerie_Compiler_AST.EConst offset)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_load8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Store8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Str8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Strb" ->
           (match args with
            | (Valerie_Compiler_AST.EVar buf)::(Valerie_Compiler_AST.EConst
                offset)::(Valerie_Compiler_AST.EVar src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_store8 regs ";
                  buf;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int offset;
                  "sz;\n"]
            | uu___ -> "")
       | "Addi" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add_imm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "FmovFromGpr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "FmovToGpr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "MovVecLane" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "MovLaneToGpr" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "DupVec" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_mov regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "MovImm" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EConst
                imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_write_reg regs ";
                  reg_idx_of_name dst;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Pmull" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_pmull regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Pmull2" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_pmull2 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Veor" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  ";\n"]
            | uu___ -> "")
       | "Veor3" ->
           (match args with
            | (Valerie_Compiler_AST.EVar rd)::(Valerie_Compiler_AST.EVar
                rn)::(Valerie_Compiler_AST.EVar
                rm)::(Valerie_Compiler_AST.EVar ra)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_xor3 regs ";
                  reg_idx_of_name rd;
                  " ";
                  reg_idx_of_name rn;
                  " ";
                  reg_idx_of_name rm;
                  " ";
                  reg_idx_of_name ra;
                  ";\n"]
            | uu___ -> "")
       | "Vext8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_vext8 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Rev8" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_rev regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Rbit" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_rev regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Sha256sum0" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sha256sum0 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Sha256sum1" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_sha256sum0 regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Aes64esm" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_aes64esm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Aes64dsm" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                s1)::(Valerie_Compiler_AST.EVar s2)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_aes64dsm regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name s1;
                  " ";
                  reg_idx_of_name s2;
                  ";\n"]
            | uu___ -> "")
       | "Aesenc" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_aesenc regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Aesdec" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_aesenc regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | "Pclmulqdq" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::(Valerie_Compiler_AST.EConst imm)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_pclmulqdq regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  " ";
                  Prims.string_of_int imm;
                  "uL;\n"]
            | uu___ -> "")
       | "Paddd" ->
           (match args with
            | (Valerie_Compiler_AST.EVar dst)::(Valerie_Compiler_AST.EVar
                src)::[] ->
                FStar_String.concat ""
                  [indent;
                  "pulse_step_add regs ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name dst;
                  " ";
                  reg_idx_of_name src;
                  ";\n"]
            | uu___ -> "")
       | proc_call ->
           let extra_args = concat_pulse_call_args args in
           FStar_String.concat ""
             [indent; "va_pulse_"; proc_call; " regs"; extra_args; ";\n"])
  | Valerie_Compiler_AST.SBlock stmts -> emit_pulse_stmts indent stmts
  | Valerie_Compiler_AST.SAssign (id, expr) ->
      (match expr with
       | Valerie_Compiler_AST.EConst imm ->
           FStar_String.concat ""
             [indent;
             "pulse_write_reg regs ";
             reg_idx_of_name id;
             " ";
             Prims.string_of_int imm;
             "uL;\n"]
       | Valerie_Compiler_AST.EVar src ->
           FStar_String.concat ""
             [indent;
             "pulse_step_mov regs ";
             reg_idx_of_name id;
             " ";
             reg_idx_of_name src;
             ";\n"]
       | uu___ ->
           FStar_String.concat ""
             [indent; "let "; id; " = "; string_of_pulse_expr expr; ";\n"])
  | Valerie_Compiler_AST.SIfElse (cond, t_branch, f_branch) ->
      FStar_String.concat ""
        [indent;
        "if (";
        string_of_pulse_expr cond;
        ") {\n";
        emit_pulse_stmts (FStar_String.concat "" [indent; "  "]) t_branch;
        indent;
        "} else {\n";
        emit_pulse_stmts (FStar_String.concat "" [indent; "  "]) f_branch;
        indent;
        "}\n"]
  | Valerie_Compiler_AST.SWhile (cond, invs, body) ->
      let inv_clauses = string_of_pulse_invs invs in
      FStar_String.concat ""
        [indent;
        "while (";
        string_of_pulse_expr cond;
        ")\n";
        indent;
        "  invariant b.\n";
        indent;
        "    (exists* s_loop. pts_to regs s_loop ** pure (Seq.length s_loop == 32)";
        inv_clauses;
        ")\n";
        indent;
        "{\n";
        emit_pulse_stmts (FStar_String.concat "" [indent; "  "]) body;
        indent;
        "}\n"]
  | Valerie_Compiler_AST.SAssert expr ->
      FStar_String.concat ""
        [indent; "assert pure ("; string_of_pulse_expr expr; ");\n"]
  | Valerie_Compiler_AST.SAssume expr ->
      FStar_String.concat ""
        [indent; "assume pure ("; string_of_pulse_expr expr; ");\n"]
  | uu___ -> ""
and emit_pulse_stmts (indent : Prims.string)
  (stmts : Valerie_Compiler_AST.stmt Prims.list) : Prims.string=
  match stmts with
  | [] -> ""
  | s::rest ->
      FStar_String.concat ""
        [emit_pulse_stmt indent s; emit_pulse_stmts indent rest]
let rec emit_buf_params (bufs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match bufs with
  | [] -> ""
  | b::rest ->
      FStar_String.concat ""
        ["  (";
        b.Valerie_Compiler_AST.pformal_name;
        ": array ";
        buffer_elem_pulse_type b.Valerie_Compiler_AST.pformal_typ;
        ")\n";
        emit_buf_params rest]
let rec emit_buf_ghosts (bufs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match bufs with
  | [] -> ""
  | b::rest ->
      let elem_t = buffer_elem_pulse_type b.Valerie_Compiler_AST.pformal_typ in
      let len_req =
        match b.Valerie_Compiler_AST.pformal_typ with
        | Valerie_Compiler_AST.TBuffer (uu___, len) ->
            FStar_String.concat ""
              ["{Seq.length s_";
              b.Valerie_Compiler_AST.pformal_name;
              " == ";
              Prims.string_of_int len;
              "}"]
        | uu___ -> "" in
      let perm_param =
        match b.Valerie_Compiler_AST.pformal_kind with
        | Valerie_Compiler_AST.FormalIn ->
            FStar_String.concat ""
              ["  (#p_"; b.Valerie_Compiler_AST.pformal_name; ": perm)\n"]
        | uu___ -> "" in
      FStar_String.concat ""
        [perm_param;
        "  (#s_";
        b.Valerie_Compiler_AST.pformal_name;
        ": Ghost.erased (Seq.seq ";
        elem_t;
        ")";
        len_req;
        ")\n";
        emit_buf_ghosts rest]
let rec emit_buf_reqs (bufs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match bufs with
  | [] -> ""
  | b::rest ->
      let len_clause =
        match b.Valerie_Compiler_AST.pformal_typ with
        | Valerie_Compiler_AST.TBuffer (uu___, len) ->
            if len > Prims.int_zero
            then
              FStar_String.concat ""
                [" **\n    pure (Seq.length s_";
                b.Valerie_Compiler_AST.pformal_name;
                " >= ";
                Prims.string_of_int len;
                ")"]
            else ""
        | uu___ -> "" in
      let perm_clause =
        match b.Valerie_Compiler_AST.pformal_kind with
        | Valerie_Compiler_AST.FormalIn ->
            FStar_String.concat ""
              [" #p_"; b.Valerie_Compiler_AST.pformal_name]
        | uu___ -> "" in
      FStar_String.concat ""
        [" **\n    pts_to ";
        b.Valerie_Compiler_AST.pformal_name;
        perm_clause;
        " s_";
        b.Valerie_Compiler_AST.pformal_name;
        len_clause;
        emit_buf_reqs rest]
let rec emit_buf_ens (bufs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match bufs with
  | [] -> ""
  | b::rest ->
      (match b.Valerie_Compiler_AST.pformal_kind with
       | Valerie_Compiler_AST.FormalIn ->
           FStar_String.concat ""
             [" **\n    pts_to ";
             b.Valerie_Compiler_AST.pformal_name;
             " #p_";
             b.Valerie_Compiler_AST.pformal_name;
             " s_";
             b.Valerie_Compiler_AST.pformal_name;
             emit_buf_ens rest]
       | uu___ ->
           FStar_String.concat ""
             [" **\n    (exists* s_";
             b.Valerie_Compiler_AST.pformal_name;
             "_final. pts_to ";
             b.Valerie_Compiler_AST.pformal_name;
             " s_";
             b.Valerie_Compiler_AST.pformal_name;
             "_final)";
             emit_buf_ens rest])
let is_imm_formal (f : Valerie_Compiler_AST.pformal) : Prims.bool=
  (((f.Valerie_Compiler_AST.pformal_kind = Valerie_Compiler_AST.FormalInline)
      || (f.Valerie_Compiler_AST.pformal_name = "imm"))
     || (f.Valerie_Compiler_AST.pformal_typ = Valerie_Compiler_AST.TInt))
    || (f.Valerie_Compiler_AST.pformal_typ = Valerie_Compiler_AST.TU64)
let rec emit_ins_params (formals : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match formals with
  | [] -> ""
  | f::rest ->
      let typ_str =
        if is_buffer_typ f.Valerie_Compiler_AST.pformal_typ
        then
          FStar_String.concat ""
            ["array ";
            buffer_elem_pulse_type f.Valerie_Compiler_AST.pformal_typ]
        else if is_imm_formal f then "U64.t" else "SZ.t" in
      FStar_String.concat ""
        ["  (";
        f.Valerie_Compiler_AST.pformal_name;
        ": ";
        typ_str;
        ")\n";
        emit_ins_params rest]
let emit_pulse_proc (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let fn_name =
    FStar_String.concat "" ["va_pulse_"; p.Valerie_Compiler_AST.proc_name] in
  let bufs = get_buffer_formals p.Valerie_Compiler_AST.proc_formals in
  if p.Valerie_Compiler_AST.proc_is_ins
  then ""
  else
    FStar_String.concat ""
      ["fn ";
      fn_name;
      "\n";
      "  (regs: reg_array)\n";
      emit_buf_params bufs;
      "  (#s0: Ghost.erased (Seq.seq U64.t){Seq.length s0 == 32})\n";
      emit_buf_ghosts bufs;
      "  requires\n";
      "    pts_to regs s0";
      emit_buf_reqs bufs;
      "\n";
      "  returns _: unit\n";
      "  ensures\n";
      "    (exists* s1. pts_to regs s1 ** pure (Seq.length s1 == 32))";
      emit_buf_ens bufs;
      "\n";
      "{\n";
      emit_pulse_stmts "  " p.Valerie_Compiler_AST.proc_body;
      "  ()\n";
      "}\n\n"]
let rec emit_pulse_procs (procs : Valerie_Compiler_AST.proc_decl Prims.list)
  : Prims.string=
  match procs with
  | [] -> ""
  | p::rest ->
      FStar_String.concat "" [emit_pulse_proc p; emit_pulse_procs rest]
let is_perm_formal (f : Valerie_Compiler_AST.pformal) : Prims.bool=
  (is_buffer_typ f.Valerie_Compiler_AST.pformal_typ) &&
    (f.Valerie_Compiler_AST.pformal_kind = Valerie_Compiler_AST.FormalIn)
let rec formals_has_perm (formals : Valerie_Compiler_AST.pformal Prims.list)
  : Prims.bool=
  match formals with
  | [] -> false
  | f::rest -> (is_perm_formal f) || (formals_has_perm rest)
let rec procs_has_perm (procs : Valerie_Compiler_AST.proc_decl Prims.list) :
  Prims.bool=
  match procs with
  | [] -> false
  | p::rest ->
      (formals_has_perm p.Valerie_Compiler_AST.proc_formals) ||
        (procs_has_perm rest)
let is_u8_formal (f : Valerie_Compiler_AST.pformal) : Prims.bool=
  match f.Valerie_Compiler_AST.pformal_typ with
  | Valerie_Compiler_AST.TBuffer (Valerie_Compiler_AST.TU8, uu___) -> true
  | uu___ -> false
let rec formals_has_u8 (formals : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.bool=
  match formals with
  | [] -> false
  | f::rest -> (is_u8_formal f) || (formals_has_u8 rest)
let rec procs_has_u8 (procs : Valerie_Compiler_AST.proc_decl Prims.list) :
  Prims.bool=
  match procs with
  | [] -> false
  | p::rest ->
      (formals_has_u8 p.Valerie_Compiler_AST.proc_formals) ||
        (procs_has_u8 rest)
let is_u32_formal (f : Valerie_Compiler_AST.pformal) : Prims.bool=
  match f.Valerie_Compiler_AST.pformal_typ with
  | Valerie_Compiler_AST.TBuffer (Valerie_Compiler_AST.TU32, uu___) -> true
  | uu___ -> false
let rec formals_has_u32 (formals : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.bool=
  match formals with
  | [] -> false
  | f::rest -> (is_u32_formal f) || (formals_has_u32 rest)
let rec procs_has_u32 (procs : Valerie_Compiler_AST.proc_decl Prims.list) :
  Prims.bool=
  match procs with
  | [] -> false
  | p::rest ->
      (formals_has_u32 p.Valerie_Compiler_AST.proc_formals) ||
        (procs_has_u32 rest)
let emit_pulse_module (m : Valerie_Compiler_AST.module_decl) : Prims.string=
  let perm_header =
    if procs_has_perm m.Valerie_Compiler_AST.mod_procs
    then "open PulseCore.FractionalPermission\n"
    else "" in
  let u8_alias =
    if procs_has_u8 m.Valerie_Compiler_AST.mod_procs
    then "module U8 = FStar.UInt8\n"
    else "" in
  let u32_alias =
    if procs_has_u32 m.Valerie_Compiler_AST.mod_procs
    then "module U32 = FStar.UInt32\n"
    else "" in
  FStar_String.concat ""
    ["module ";
    m.Valerie_Compiler_AST.mod_name;
    "_Pulse\n";
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
    emit_pulse_procs m.Valerie_Compiler_AST.mod_procs]
