open Prims
let reg_of_operand (r : Vale_Compiler_AST.reg_operand) :
  Vale_Riscv64_Machine.reg=
  match r with
  | Vale_Compiler_AST.RegNumbered n ->
      if n = Stdint.Uint32.zero
      then Vale_Riscv64_Machine.Zero
      else
        if n = Stdint.Uint32.one
        then Vale_Riscv64_Machine.RA
        else
          if n = (Stdint.Uint32.of_int 2)
          then Vale_Riscv64_Machine.SP
          else
            if n = (Stdint.Uint32.of_int 3)
            then Vale_Riscv64_Machine.GP
            else
              if n = (Stdint.Uint32.of_int 4)
              then Vale_Riscv64_Machine.TP
              else
                if n = (Stdint.Uint32.of_int 5)
                then Vale_Riscv64_Machine.T0
                else
                  if n = (Stdint.Uint32.of_int 6)
                  then Vale_Riscv64_Machine.T1
                  else
                    if n = (Stdint.Uint32.of_int 7)
                    then Vale_Riscv64_Machine.T2
                    else
                      if n = (Stdint.Uint32.of_int 8)
                      then Vale_Riscv64_Machine.S0
                      else
                        if n = (Stdint.Uint32.of_int 9)
                        then Vale_Riscv64_Machine.S1
                        else
                          if n = (Stdint.Uint32.of_int 10)
                          then Vale_Riscv64_Machine.A0
                          else
                            if n = (Stdint.Uint32.of_int 11)
                            then Vale_Riscv64_Machine.A1
                            else
                              if n = (Stdint.Uint32.of_int 12)
                              then Vale_Riscv64_Machine.A2
                              else
                                if n = (Stdint.Uint32.of_int 13)
                                then Vale_Riscv64_Machine.A3
                                else
                                  if n = (Stdint.Uint32.of_int 14)
                                  then Vale_Riscv64_Machine.A4
                                  else
                                    if n = (Stdint.Uint32.of_int 15)
                                    then Vale_Riscv64_Machine.A5
                                    else
                                      if n = (Stdint.Uint32.of_int 16)
                                      then Vale_Riscv64_Machine.A6
                                      else
                                        if n = (Stdint.Uint32.of_int 17)
                                        then Vale_Riscv64_Machine.A7
                                        else
                                          if n = (Stdint.Uint32.of_int 18)
                                          then Vale_Riscv64_Machine.S2
                                          else
                                            if n = (Stdint.Uint32.of_int 19)
                                            then Vale_Riscv64_Machine.S3
                                            else
                                              if
                                                n = (Stdint.Uint32.of_int 20)
                                              then Vale_Riscv64_Machine.S4
                                              else
                                                if
                                                  n =
                                                    (Stdint.Uint32.of_int 21)
                                                then Vale_Riscv64_Machine.S5
                                                else
                                                  if
                                                    n =
                                                      (Stdint.Uint32.of_int
                                                         22)
                                                  then
                                                    Vale_Riscv64_Machine.S6
                                                  else
                                                    if
                                                      n =
                                                        (Stdint.Uint32.of_int
                                                           23)
                                                    then
                                                      Vale_Riscv64_Machine.S7
                                                    else
                                                      if
                                                        n =
                                                          (Stdint.Uint32.of_int
                                                             24)
                                                      then
                                                        Vale_Riscv64_Machine.S8
                                                      else
                                                        if
                                                          n =
                                                            (Stdint.Uint32.of_int
                                                               25)
                                                        then
                                                          Vale_Riscv64_Machine.S9
                                                        else
                                                          if
                                                            n =
                                                              (Stdint.Uint32.of_int
                                                                 26)
                                                          then
                                                            Vale_Riscv64_Machine.S10
                                                          else
                                                            if
                                                              n =
                                                                (Stdint.Uint32.of_int
                                                                   27)
                                                            then
                                                              Vale_Riscv64_Machine.S11
                                                            else
                                                              if
                                                                n =
                                                                  (Stdint.Uint32.of_int
                                                                    28)
                                                              then
                                                                Vale_Riscv64_Machine.T3
                                                              else
                                                                if
                                                                  n =
                                                                    (
                                                                    Stdint.Uint32.of_int
                                                                    29)
                                                                then
                                                                  Vale_Riscv64_Machine.T4
                                                                else
                                                                  if
                                                                    n =
                                                                    (Stdint.Uint32.of_int
                                                                    30)
                                                                  then
                                                                    Vale_Riscv64_Machine.T5
                                                                  else
                                                                    Vale_Riscv64_Machine.T6
let lower_operand (op : Vale_Compiler_AST.operand) :
  Vale_Riscv64_Semantics.operand=
  match op with
  | Vale_Compiler_AST.OReg r ->
      Vale_Riscv64_Semantics.OReg (reg_of_operand r)
  | Vale_Compiler_AST.OImm i -> Vale_Riscv64_Semantics.OImm i
  | Vale_Compiler_AST.OMemBuffer (uu___, uu___1) ->
      Vale_Riscv64_Semantics.OImm Stdint.Uint64.zero
let lower_ins (op : Vale_Compiler_AST.ins_opcode)
  (args : Vale_Compiler_AST.operand Prims.list) :
  Vale_Riscv64_Semantics.ins FStar_Pervasives_Native.option=
  match (op, args) with
  | (Vale_Compiler_AST.OpMov, dst::src::[]) ->
      (match (dst, src) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mv
                ((reg_of_operand rd), (reg_of_operand rs)))
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OImm imm) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Li ((reg_of_operand rd), imm))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAdd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs1) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Add
                ((reg_of_operand rd), (reg_of_operand rs1),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAdd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Add
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpSub, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs1) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sub
                ((reg_of_operand rd), (reg_of_operand rs1),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpSub, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sub
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpXor, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs1) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Xor
                ((reg_of_operand rd), (reg_of_operand rs1),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpXor, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Xor
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAnd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs1) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.And
                ((reg_of_operand rd), (reg_of_operand rs1),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAnd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.And
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpOr, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs1) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Or
                ((reg_of_operand rd), (reg_of_operand rs1),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpOr, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Or
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpRev, dst::src::[]) ->
      (match (dst, src) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rs) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Rev8
                ((reg_of_operand rd), (reg_of_operand rs)))
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let reg_of_name (s : Prims.string) :
  Vale_Riscv64_Machine.reg FStar_Pervasives_Native.option=
  if (s = "zero") || (s = "x0")
  then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.Zero
  else
    if (s = "ra") || (s = "x1")
    then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.RA
    else
      if (s = "sp") || (s = "x2")
      then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.SP
      else
        if (s = "gp") || (s = "x3")
        then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.GP
        else
          if (s = "tp") || (s = "x4")
          then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.TP
          else
            if (s = "t0") || (s = "x5")
            then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.T0
            else
              if (s = "t1") || (s = "x6")
              then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.T1
              else
                if (s = "t2") || (s = "x7")
                then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.T2
                else
                  if ((s = "s0") || (s = "fp")) || (s = "x8")
                  then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.S0
                  else
                    if (s = "s1") || (s = "x9")
                    then FStar_Pervasives_Native.Some Vale_Riscv64_Machine.S1
                    else
                      if (s = "a0") || (s = "x10")
                      then
                        FStar_Pervasives_Native.Some Vale_Riscv64_Machine.A0
                      else
                        if (s = "a1") || (s = "x11")
                        then
                          FStar_Pervasives_Native.Some
                            Vale_Riscv64_Machine.A1
                        else
                          if (s = "a2") || (s = "x12")
                          then
                            FStar_Pervasives_Native.Some
                              Vale_Riscv64_Machine.A2
                          else
                            if (s = "a3") || (s = "x13")
                            then
                              FStar_Pervasives_Native.Some
                                Vale_Riscv64_Machine.A3
                            else
                              if (s = "a4") || (s = "x14")
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_Riscv64_Machine.A4
                              else
                                if (s = "a5") || (s = "x15")
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_Riscv64_Machine.A5
                                else
                                  if (s = "a6") || (s = "x16")
                                  then
                                    FStar_Pervasives_Native.Some
                                      Vale_Riscv64_Machine.A6
                                  else
                                    if (s = "a7") || (s = "x17")
                                    then
                                      FStar_Pervasives_Native.Some
                                        Vale_Riscv64_Machine.A7
                                    else
                                      if (s = "s2") || (s = "x18")
                                      then
                                        FStar_Pervasives_Native.Some
                                          Vale_Riscv64_Machine.S2
                                      else
                                        if (s = "s3") || (s = "x19")
                                        then
                                          FStar_Pervasives_Native.Some
                                            Vale_Riscv64_Machine.S3
                                        else
                                          if (s = "s4") || (s = "x20")
                                          then
                                            FStar_Pervasives_Native.Some
                                              Vale_Riscv64_Machine.S4
                                          else
                                            if (s = "s5") || (s = "x21")
                                            then
                                              FStar_Pervasives_Native.Some
                                                Vale_Riscv64_Machine.S5
                                            else
                                              if (s = "s6") || (s = "x22")
                                              then
                                                FStar_Pervasives_Native.Some
                                                  Vale_Riscv64_Machine.S6
                                              else
                                                if (s = "s7") || (s = "x23")
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    Vale_Riscv64_Machine.S7
                                                else
                                                  if
                                                    (s = "s8") || (s = "x24")
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      Vale_Riscv64_Machine.S8
                                                  else
                                                    if
                                                      (s = "s9") ||
                                                        (s = "x25")
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        Vale_Riscv64_Machine.S9
                                                    else
                                                      if
                                                        (s = "s10") ||
                                                          (s = "x26")
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          Vale_Riscv64_Machine.S10
                                                      else
                                                        if
                                                          (s = "s11") ||
                                                            (s = "x27")
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            Vale_Riscv64_Machine.S11
                                                        else
                                                          if
                                                            (s = "t3") ||
                                                              (s = "x28")
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              Vale_Riscv64_Machine.T3
                                                          else
                                                            if
                                                              (s = "t4") ||
                                                                (s = "x29")
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                Vale_Riscv64_Machine.T4
                                                            else
                                                              if
                                                                (s = "t5") ||
                                                                  (s = "x30")
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  Vale_Riscv64_Machine.T5
                                                              else
                                                                if
                                                                  (s = "t6")
                                                                    ||
                                                                    (
                                                                    s = "x31")
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    Vale_Riscv64_Machine.T6
                                                                else
                                                                  FStar_Pervasives_Native.None
let reg_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Riscv64_Machine.reg FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> reg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let operand_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Riscv64_Semantics.operand FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s ->
      (match reg_of_name s with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.OReg r)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.EConst n ->
      if
        (n >= Prims.int_zero) &&
          (n <= (Prims.parse_int "18446744073709551615"))
      then
        FStar_Pervasives_Native.Some
          (Vale_Riscv64_Semantics.OImm (FStar_UInt64.uint_to_t n))
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
let lower_call (target : Vale_Compiler_AST.id)
  (args : Vale_Compiler_AST.expr Prims.list) :
  Vale_Riscv64_Semantics.ins FStar_Pervasives_Native.option=
  match (target, args) with
  | ("Add", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Add (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Addi", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Add (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sub (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Xor (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("And", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.And (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Or (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev8", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Rev8 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aes64esm", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Aes64esm (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aes64dsm", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Aes64dsm (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256sum0", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha256sum0 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256sum1", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha256sum1 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Addw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Addw (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Subw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Subw (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sll", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sll (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sllw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sllw (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Srl", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Srl (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Srlw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Srlw (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sra", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sra (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sraw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sraw (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Slt", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Slt (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sltu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Sltu (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mul", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mul (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mulw (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulh", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mulh (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulhu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mulhu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mulhsu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Mulhsu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Div", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Div (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Divw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Divw (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Divu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Divu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Divuw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Divuw (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rem", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Rem (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Remw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Remw (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Remu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Remu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Remuw", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Remuw (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Andn", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Andn (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orn", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Orn (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xnor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Xnor (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Clz", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Clz (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ctz", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Ctz (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cpop", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Cpop (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Max", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Max (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Maxu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Maxu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Min", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Min (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Minu", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Minu (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Rol (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Riscv64_Semantics.Ror (rd, rs1, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ld", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Ld (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ld", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Ld (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lw", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lw (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lw", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lw (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lwu", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lwu (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lwu", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lwu (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lh", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lh (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lh", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lh (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lhu", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lhu (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lhu", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lhu (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lb", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lb (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lb", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lb (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lbu", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lbu (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lbu", dst::base::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Lbu (rd, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sd", src::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sd (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sd", base::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sd (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sd", src::base::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sd (rs, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sw", src::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sw (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sw", base::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sw (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sw", src::base::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sw (rs, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sh", src::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sh (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sh", base::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sh (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sh", src::base::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sh (rs, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sb", src::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sb (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sb", base::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sb (rs, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sb", src::base::[]) ->
      (match ((reg_of_expr src), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sb (rs, rb, Prims.int_zero))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Beq", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Beq (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bne", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Bne (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Blt", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Blt (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bge", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Bge (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bltu", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Bltu (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bgeu", s1::s2::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Bgeu (rs1, rs2, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Jal", dst::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Jal (rd, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Jalr", dst::base::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr base)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Jalr (rd, rb, offset))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ret", []) -> FStar_Pervasives_Native.Some Vale_Riscv64_Semantics.Ret
  | ("Ecall", []) ->
      FStar_Pervasives_Native.Some Vale_Riscv64_Semantics.Ecall
  | ("Lui", dst::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             (imm >= Prims.int_zero) &&
               (imm <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Riscv64_Semantics.Lui (rd, (FStar_UInt64.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Auipc", dst::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             (imm >= Prims.int_zero) &&
               (imm <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Riscv64_Semantics.Auipc
                  (rd, (FStar_UInt64.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aes64ks1i", dst::src::(Vale_Compiler_AST.EConst rnum)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           if
             (rnum >= Prims.int_zero) &&
               (rnum <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Riscv64_Semantics.Aes64ks1i
                  (rd, rs, (FStar_UInt64.uint_to_t rnum)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aes64ks2", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Aes64ks2 (rd, rs1, rs2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aes64im", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Aes64im (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256sig0", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha256sig0 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256sig1", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha256sig1 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha512sum0", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha512sum0 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha512sum1", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha512sum1 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha512sig0", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha512sig0 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha512sig1", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sha512sig1 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sm4ed", dst::s1::s2::(Vale_Compiler_AST.EConst bs)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           if
             (bs >= Prims.int_zero) &&
               (bs <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Riscv64_Semantics.Sm4ed
                  (rd, rs1, rs2, (FStar_UInt64.uint_to_t bs)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sm4ks", dst::s1::s2::(Vale_Compiler_AST.EConst bs)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs1,
          FStar_Pervasives_Native.Some rs2) ->
           if
             (bs >= Prims.int_zero) &&
               (bs <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Riscv64_Semantics.Sm4ks
                  (rd, rs1, rs2, (FStar_UInt64.uint_to_t bs)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sm3p0", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sm3p0 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sm3p1", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Sm3p1 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let rec lower_stmt (s : Vale_Compiler_AST.stmt) :
  Vale_Riscv64_Semantics.code FStar_Pervasives_Native.option=
  match s with
  | Vale_Compiler_AST.SIns (op, args) ->
      (match lower_ins op args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Ins i)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.SBlock stmts -> lower_stmts stmts
  | Vale_Compiler_AST.SCall (target, args) ->
      (match lower_call target args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Ins i)
       | FStar_Pervasives_Native.None ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Ins (Vale_Riscv64_Semantics.Call target)))
  | Vale_Compiler_AST.SAssert uu___ ->
      FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Block [])
  | Vale_Compiler_AST.SAssume uu___ ->
      FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Block [])
  | uu___ -> FStar_Pervasives_Native.None
and lower_stmts (stmts : Vale_Compiler_AST.stmt Prims.list) :
  Vale_Riscv64_Semantics.code FStar_Pervasives_Native.option=
  match stmts with
  | [] -> FStar_Pervasives_Native.Some (Vale_Riscv64_Semantics.Block [])
  | s::[] -> lower_stmt s
  | s::rest ->
      (match ((lower_stmt s), (lower_stmts rest)) with
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some
          (Vale_Riscv64_Semantics.Block cs)) ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Block (c1 :: cs))
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some c2)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Riscv64_Semantics.Block [c1; c2])
       | uu___ -> FStar_Pervasives_Native.None)
