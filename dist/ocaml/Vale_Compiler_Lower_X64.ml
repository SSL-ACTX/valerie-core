open Prims
let reg_of_operand (r : Vale_Compiler_AST.reg_operand) :
  Vale_X64_Machine.reg=
  match r with
  | Vale_Compiler_AST.RegNumbered n ->
      if n = Stdint.Uint32.zero
      then Vale_X64_Machine.Rax
      else
        if n = Stdint.Uint32.one
        then Vale_X64_Machine.Rbx
        else
          if n = (Stdint.Uint32.of_int 2)
          then Vale_X64_Machine.Rcx
          else
            if n = (Stdint.Uint32.of_int 3)
            then Vale_X64_Machine.Rdx
            else
              if n = (Stdint.Uint32.of_int 4)
              then Vale_X64_Machine.Rsi
              else
                if n = (Stdint.Uint32.of_int 5)
                then Vale_X64_Machine.Rdi
                else
                  if n = (Stdint.Uint32.of_int 6)
                  then Vale_X64_Machine.Rbp
                  else
                    if n = (Stdint.Uint32.of_int 7)
                    then Vale_X64_Machine.Rsp
                    else
                      if n = (Stdint.Uint32.of_int 8)
                      then Vale_X64_Machine.R8
                      else
                        if n = (Stdint.Uint32.of_int 9)
                        then Vale_X64_Machine.R9
                        else
                          if n = (Stdint.Uint32.of_int 10)
                          then Vale_X64_Machine.R10
                          else
                            if n = (Stdint.Uint32.of_int 11)
                            then Vale_X64_Machine.R11
                            else
                              if n = (Stdint.Uint32.of_int 12)
                              then Vale_X64_Machine.R12
                              else
                                if n = (Stdint.Uint32.of_int 13)
                                then Vale_X64_Machine.R13
                                else
                                  if n = (Stdint.Uint32.of_int 14)
                                  then Vale_X64_Machine.R14
                                  else Vale_X64_Machine.R15
let lower_operand (op : Vale_Compiler_AST.operand) :
  Vale_X64_Semantics.operand=
  match op with
  | Vale_Compiler_AST.OReg r ->
      Vale_X64_Semantics.OReg ((reg_of_operand r), Vale_X64_Machine.R64)
  | Vale_Compiler_AST.OImm i -> Vale_X64_Semantics.OImm i
  | Vale_Compiler_AST.OMemBuffer (uu___, uu___1) ->
      Vale_X64_Semantics.OImm Stdint.Uint64.zero
let lower_ins (op : Vale_Compiler_AST.ins_opcode)
  (args : Vale_Compiler_AST.operand Prims.list) :
  Vale_X64_Semantics.ins FStar_Pervasives_Native.option=
  match (op, args) with
  | (Vale_Compiler_AST.OpMov, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg r ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Mov64
                ((reg_of_operand r), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAdd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Add64
                ((reg_of_operand rd), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpSub, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sub64
                ((reg_of_operand rd), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpXor, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Xor64
                ((reg_of_operand rd), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAnd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.And64
                ((reg_of_operand rd), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let reg_of_name (s : Prims.string) :
  Vale_X64_Machine.reg FStar_Pervasives_Native.option=
  if s = "rax"
  then FStar_Pervasives_Native.Some Vale_X64_Machine.Rax
  else
    if s = "rbx"
    then FStar_Pervasives_Native.Some Vale_X64_Machine.Rbx
    else
      if s = "rcx"
      then FStar_Pervasives_Native.Some Vale_X64_Machine.Rcx
      else
        if s = "rdx"
        then FStar_Pervasives_Native.Some Vale_X64_Machine.Rdx
        else
          if s = "rsi"
          then FStar_Pervasives_Native.Some Vale_X64_Machine.Rsi
          else
            if s = "rdi"
            then FStar_Pervasives_Native.Some Vale_X64_Machine.Rdi
            else
              if s = "rbp"
              then FStar_Pervasives_Native.Some Vale_X64_Machine.Rbp
              else
                if s = "rsp"
                then FStar_Pervasives_Native.Some Vale_X64_Machine.Rsp
                else
                  if s = "r8"
                  then FStar_Pervasives_Native.Some Vale_X64_Machine.R8
                  else
                    if s = "r9"
                    then FStar_Pervasives_Native.Some Vale_X64_Machine.R9
                    else
                      if s = "r10"
                      then FStar_Pervasives_Native.Some Vale_X64_Machine.R10
                      else
                        if s = "r11"
                        then
                          FStar_Pervasives_Native.Some Vale_X64_Machine.R11
                        else
                          if s = "r12"
                          then
                            FStar_Pervasives_Native.Some Vale_X64_Machine.R12
                          else
                            if s = "r13"
                            then
                              FStar_Pervasives_Native.Some
                                Vale_X64_Machine.R13
                            else
                              if s = "r14"
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_X64_Machine.R14
                              else
                                if s = "r15"
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_X64_Machine.R15
                                else FStar_Pervasives_Native.None
let xmm_of_name (s : Prims.string) :
  Vale_X64_Machine.xmm FStar_Pervasives_Native.option=
  if s = "xmm0"
  then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm0
  else
    if s = "xmm1"
    then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm1
    else
      if s = "xmm2"
      then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm2
      else
        if s = "xmm3"
        then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm3
        else
          if s = "xmm4"
          then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm4
          else
            if s = "xmm5"
            then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm5
            else
              if s = "xmm6"
              then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm6
              else
                if s = "xmm7"
                then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm7
                else
                  if s = "xmm8"
                  then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm8
                  else
                    if s = "xmm9"
                    then FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm9
                    else
                      if s = "xmm10"
                      then
                        FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm10
                      else
                        if s = "xmm11"
                        then
                          FStar_Pervasives_Native.Some Vale_X64_Machine.Xmm11
                        else
                          if s = "xmm12"
                          then
                            FStar_Pervasives_Native.Some
                              Vale_X64_Machine.Xmm12
                          else
                            if s = "xmm13"
                            then
                              FStar_Pervasives_Native.Some
                                Vale_X64_Machine.Xmm13
                            else
                              if s = "xmm14"
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_X64_Machine.Xmm14
                              else
                                if s = "xmm15"
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_X64_Machine.Xmm15
                                else FStar_Pervasives_Native.None
let reg_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_X64_Machine.reg FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> reg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let xmm_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_X64_Machine.xmm FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> xmm_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let cond_of_name (s : Prims.string) :
  Vale_X64_Machine.cond_code FStar_Pervasives_Native.option=
  if
    (((((s = "e") || (s = "E")) || (s = "eq")) || (s = "EQ")) || (s = "z"))
      || (s = "Z")
  then FStar_Pervasives_Native.Some Vale_X64_Machine.CondE
  else
    if (((s = "ne") || (s = "NE")) || (s = "nz")) || (s = "NZ")
    then FStar_Pervasives_Native.Some Vale_X64_Machine.CondNE
    else
      if (((s = "b") || (s = "B")) || (s = "c")) || (s = "C")
      then FStar_Pervasives_Native.Some Vale_X64_Machine.CondB
      else
        if (((s = "be") || (s = "BE")) || (s = "na")) || (s = "NA")
        then FStar_Pervasives_Native.Some Vale_X64_Machine.CondBE
        else
          if (((s = "a") || (s = "A")) || (s = "nbe")) || (s = "NBE")
          then FStar_Pervasives_Native.Some Vale_X64_Machine.CondA
          else
            if
              (((((s = "ae") || (s = "AE")) || (s = "nc")) || (s = "NC")) ||
                 (s = "nb"))
                || (s = "NB")
            then FStar_Pervasives_Native.Some Vale_X64_Machine.CondAE
            else
              if (((s = "l") || (s = "L")) || (s = "nge")) || (s = "NGE")
              then FStar_Pervasives_Native.Some Vale_X64_Machine.CondL
              else
                if (((s = "le") || (s = "LE")) || (s = "ng")) || (s = "NG")
                then FStar_Pervasives_Native.Some Vale_X64_Machine.CondLE
                else
                  if (((s = "g") || (s = "G")) || (s = "nle")) || (s = "NLE")
                  then FStar_Pervasives_Native.Some Vale_X64_Machine.CondG
                  else
                    if
                      (((s = "ge") || (s = "GE")) || (s = "nl")) ||
                        (s = "NL")
                    then FStar_Pervasives_Native.Some Vale_X64_Machine.CondGE
                    else FStar_Pervasives_Native.None
let cond_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_X64_Machine.cond_code FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> cond_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let operand_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_X64_Semantics.operand FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s ->
      (match reg_of_name s with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.OReg (r, Vale_X64_Machine.R64))
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.EConst n ->
      if
        (n >= Prims.int_zero) &&
          (n <= (Prims.parse_int "18446744073709551615"))
      then
        FStar_Pervasives_Native.Some
          (Vale_X64_Semantics.OImm (FStar_UInt64.uint_to_t n))
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
let lower_call (target : Vale_Compiler_AST.id)
  (args : Vale_Compiler_AST.expr Prims.list) :
  Vale_X64_Semantics.ins FStar_Pervasives_Native.option=
  match (target, args) with
  | ("Mov64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Mov64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mov", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Mov64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Add64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Add64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Add", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Add64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sub64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sub64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Xor64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Xor64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesenc", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Aesenc (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesdec", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Aesdec (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pxor", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pxor (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Paddd", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Paddd (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pclmulqdq", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pclmulqdq
                  (xd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mov32", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Mov32 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adc64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adc64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adc", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adc64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adcx64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adcx64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adcx", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adcx64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adox64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adox64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adox", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Adox64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbb64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sbb64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbb", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sbb64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Or64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Or64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Not64", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Not64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Not", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Not64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Neg64", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Neg64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Neg", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Neg64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shl64", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Shl64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shl", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Shl64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shr64", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Shr64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shr", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Shr64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sar64", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sar64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sar", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Sar64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol64", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Rol64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Rol64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror64", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Ror64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror", dst::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr count)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Ror64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bswap64", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bswap64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bswap", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bswap64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmp64", s1::s2::[]) ->
      (match ((reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Cmp64 (r1, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmp", s1::s2::[]) ->
      (match ((reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Cmp64 (r1, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Test64", s1::s2::[]) ->
      (match ((reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Test64 (r1, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Test", s1::s2::[]) ->
      (match ((reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Test64 (r1, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lea64", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Lea64
                (rd, (Vale_X64_Machine.MReg (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lea", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Lea64
                (rd, (Vale_X64_Machine.MReg (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Imul64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Imul64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Imul", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Imul64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mul64", src::[]) ->
      (match operand_of_expr src with
       | FStar_Pervasives_Native.Some op ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Mul64 op)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mul", src::[]) ->
      (match operand_of_expr src with
       | FStar_Pervasives_Native.Some op ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Mul64 op)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulx64", hi::lo::src::[]) ->
      (match ((reg_of_expr hi), (reg_of_expr lo), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rhi, FStar_Pervasives_Native.Some rlo,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Mulx64 (rhi, rlo, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulx", hi::lo::src::[]) ->
      (match ((reg_of_expr hi), (reg_of_expr lo), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rhi, FStar_Pervasives_Native.Some rlo,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Mulx64 (rhi, rlo, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Andn64", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Andn64 (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Andn", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Andn64 (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rorx64", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Rorx64
                  (rd, op, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rorx", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Rorx64
                  (rd, op, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sarx64", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sarx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sarx", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sarx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shlx64", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Shlx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shlx", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Shlx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shrx64", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Shrx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Shrx", dst::src::count::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src), (reg_of_expr count))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op,
          FStar_Pervasives_Native.Some rc) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Shrx64 (rd, op, rc))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bsf64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bsf64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bsf", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bsf64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bsr64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bsr64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bsr", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Bsr64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Tzcnt64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Tzcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Tzcnt", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Tzcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lzcnt64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Lzcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lzcnt", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Lzcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Popcnt64", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Popcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Popcnt", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Popcnt64 (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmov64", c::dst::src::[]) ->
      (match ((cond_of_expr c), (reg_of_expr dst), (operand_of_expr src))
       with
       | (FStar_Pervasives_Native.Some cond, FStar_Pervasives_Native.Some rd,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Cmov64 (cond, rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmov", c::dst::src::[]) ->
      (match ((cond_of_expr c), (reg_of_expr dst), (operand_of_expr src))
       with
       | (FStar_Pervasives_Native.Some cond, FStar_Pervasives_Native.Some rd,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Cmov64 (cond, rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Jmp", (Vale_Compiler_AST.EVar target1)::[]) ->
      FStar_Pervasives_Native.Some (Vale_X64_Semantics.Jmp target1)
  | ("Jcc", c::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match cond_of_expr c with
       | FStar_Pervasives_Native.Some cond ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Jcc (cond, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ret", []) -> FStar_Pervasives_Native.Some Vale_X64_Semantics.Ret
  | ("Push64", src::[]) ->
      (match operand_of_expr src with
       | FStar_Pervasives_Native.Some op ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Push64 op)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Push", src::[]) ->
      (match operand_of_expr src with
       | FStar_Pervasives_Native.Some op ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Push64 op)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pop64", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pop64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pop", dst::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pop64 rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesenclast", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Aesenclast (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesdeclast", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Aesdeclast (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesimc", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Aesimc (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aeskeygenassist", dst::src::(Vale_Compiler_AST.EConst rcon)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           if (rcon >= Prims.int_zero) && (rcon <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Aeskeygenassist
                  (xd, xs, (FStar_UInt8.uint_to_t rcon)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vaesenc", dst::s1::s2::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr s1), (xmm_of_expr s2)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some x1,
          FStar_Pervasives_Native.Some x2) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Vaesenc (xd, x1, x2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vaesdec", dst::s1::s2::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr s1), (xmm_of_expr s2)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some x1,
          FStar_Pervasives_Native.Some x2) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Vaesdec (xd, x1, x2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vpclmulqdq", dst::s1::s2::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr s1), (xmm_of_expr s2)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some x1,
          FStar_Pervasives_Native.Some x2) ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Vpclmulqdq
                  (xd, x1, x2, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1rnds4", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Sha1rnds4
                  (xd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1nexte", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha1nexte (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1msg1", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha1msg1 (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1msg2", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha1msg2 (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256rnds2", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha256rnds2 (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256msg1", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha256msg1 (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256msg2", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Sha256msg2 (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movdqu", dst::src::[]) ->
      (match ((xmm_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Movdqu (xd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movdqa", dst::src::[]) ->
      (match ((xmm_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Movdqa (xd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vmovdqu", dst::src::[]) ->
      (match ((xmm_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Vmovdqu (xd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vmovdqa", dst::src::[]) ->
      (match ((xmm_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Vmovdqa (xd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vpxor", dst::s1::s2::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr s1), (xmm_of_expr s2)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some x1,
          FStar_Pervasives_Native.Some x2) ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Vpxor (xd, x1, x2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pand", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pand (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Por", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Por (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psubd", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Psubd (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Paddq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Paddq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psubq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Psubq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Paddb", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Paddb (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psubb", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Psubb (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Paddw", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Paddw (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psubw", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Psubw (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pslld", dst::(Vale_Compiler_AST.EConst count)::[]) ->
      (match xmm_of_expr dst with
       | FStar_Pervasives_Native.Some xd ->
           if (count >= Prims.int_zero) && (count <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pslld (xd, (FStar_UInt8.uint_to_t count)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psrld", dst::(Vale_Compiler_AST.EConst count)::[]) ->
      (match xmm_of_expr dst with
       | FStar_Pervasives_Native.Some xd ->
           if (count >= Prims.int_zero) && (count <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Psrld (xd, (FStar_UInt8.uint_to_t count)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psllq", dst::(Vale_Compiler_AST.EConst count)::[]) ->
      (match xmm_of_expr dst with
       | FStar_Pervasives_Native.Some xd ->
           if (count >= Prims.int_zero) && (count <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Psllq (xd, (FStar_UInt8.uint_to_t count)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Psrlq", dst::(Vale_Compiler_AST.EConst count)::[]) ->
      (match xmm_of_expr dst with
       | FStar_Pervasives_Native.Some xd ->
           if (count >= Prims.int_zero) && (count <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Psrlq (xd, (FStar_UInt8.uint_to_t count)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pshufd", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pshufd
                  (xd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pshufb", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pshufb (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Palignr", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Palignr
                  (xd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpcklbw", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpcklbw (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpckhbw", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpckhbw (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpcklwd", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpcklwd (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpckhwd", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpckhwd (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpckldq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpckldq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpckhdq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpckhdq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpcklqdq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpcklqdq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Punpckhqdq", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Punpckhqdq (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pcmpeqd", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pcmpeqd (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pcmpeqb", dst::src::[]) ->
      (match ((xmm_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some xs)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Pcmpeqb (xd, xs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pextrq", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((reg_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pextrq
                  (rd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pextrd", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((reg_of_expr dst), (xmm_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some xs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pextrd
                  (rd, xs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pinsrd", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some rs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pinsrd
                  (xd, rs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pinsrq", dst::src::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match ((xmm_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some xd, FStar_Pervasives_Native.Some rs)
           ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 255))
           then
             FStar_Pervasives_Native.Some
               (Vale_X64_Semantics.Pinsrq
                  (xd, rs, (FStar_UInt8.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let rec lower_stmt (s : Vale_Compiler_AST.stmt) :
  Vale_X64_Semantics.code FStar_Pervasives_Native.option=
  match s with
  | Vale_Compiler_AST.SIns (op, args) ->
      (match lower_ins op args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Ins i)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.SBlock stmts -> lower_stmts stmts
  | Vale_Compiler_AST.SCall (target, args) ->
      (match lower_call target args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Ins i)
       | FStar_Pervasives_Native.None ->
           FStar_Pervasives_Native.Some
             (Vale_X64_Semantics.Ins (Vale_X64_Semantics.Call target)))
  | Vale_Compiler_AST.SAssert uu___ ->
      FStar_Pervasives_Native.Some (Vale_X64_Semantics.Block [])
  | Vale_Compiler_AST.SAssume uu___ ->
      FStar_Pervasives_Native.Some (Vale_X64_Semantics.Block [])
  | uu___ -> FStar_Pervasives_Native.None
and lower_stmts (stmts : Vale_Compiler_AST.stmt Prims.list) :
  Vale_X64_Semantics.code FStar_Pervasives_Native.option=
  match stmts with
  | [] -> FStar_Pervasives_Native.Some (Vale_X64_Semantics.Block [])
  | s::[] -> lower_stmt s
  | s::rest ->
      (match ((lower_stmt s), (lower_stmts rest)) with
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some
          (Vale_X64_Semantics.Block cs)) ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Block (c1 :: cs))
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some c2)
           ->
           FStar_Pervasives_Native.Some (Vale_X64_Semantics.Block [c1; c2])
       | uu___ -> FStar_Pervasives_Native.None)
