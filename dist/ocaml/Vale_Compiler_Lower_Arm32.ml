open Prims
let reg_of_operand (r : Vale_Compiler_AST.reg_operand) :
  Vale_Arm32_Machine.reg=
  match r with
  | Vale_Compiler_AST.RegNumbered n ->
      if n = Stdint.Uint32.zero
      then Vale_Arm32_Machine.R0
      else
        if n = Stdint.Uint32.one
        then Vale_Arm32_Machine.R1
        else
          if n = (Stdint.Uint32.of_int 2)
          then Vale_Arm32_Machine.R2
          else
            if n = (Stdint.Uint32.of_int 3)
            then Vale_Arm32_Machine.R3
            else
              if n = (Stdint.Uint32.of_int 4)
              then Vale_Arm32_Machine.R4
              else
                if n = (Stdint.Uint32.of_int 5)
                then Vale_Arm32_Machine.R5
                else
                  if n = (Stdint.Uint32.of_int 6)
                  then Vale_Arm32_Machine.R6
                  else
                    if n = (Stdint.Uint32.of_int 7)
                    then Vale_Arm32_Machine.R7
                    else
                      if n = (Stdint.Uint32.of_int 8)
                      then Vale_Arm32_Machine.R8
                      else
                        if n = (Stdint.Uint32.of_int 9)
                        then Vale_Arm32_Machine.R9
                        else
                          if n = (Stdint.Uint32.of_int 10)
                          then Vale_Arm32_Machine.R10
                          else
                            if n = (Stdint.Uint32.of_int 11)
                            then Vale_Arm32_Machine.R11
                            else
                              if n = (Stdint.Uint32.of_int 12)
                              then Vale_Arm32_Machine.R12
                              else
                                if n = (Stdint.Uint32.of_int 13)
                                then Vale_Arm32_Machine.SP
                                else
                                  if n = (Stdint.Uint32.of_int 14)
                                  then Vale_Arm32_Machine.LR
                                  else Vale_Arm32_Machine.PC
let lower_operand (op : Vale_Compiler_AST.operand) :
  Vale_Arm32_Semantics.operand=
  match op with
  | Vale_Compiler_AST.OReg r -> Vale_Arm32_Semantics.OReg (reg_of_operand r)
  | Vale_Compiler_AST.OImm i ->
      Vale_Arm32_Semantics.OImm (FStar_Int_Cast.uint64_to_uint32 i)
  | Vale_Compiler_AST.OMemBuffer (uu___, uu___1) ->
      Vale_Arm32_Semantics.OImm Stdint.Uint32.zero
let lower_ins (op : Vale_Compiler_AST.ins_opcode)
  (args : Vale_Compiler_AST.operand Prims.list) :
  Vale_Arm32_Semantics.ins FStar_Pervasives_Native.option=
  match (op, args) with
  | (Vale_Compiler_AST.OpMov, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg r ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Mov
                ((reg_of_operand r), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAdd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Add
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAdd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Add
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpSub, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Sub
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpSub, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Sub
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpXor, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Eor
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpXor, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Eor
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAnd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.And
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpAnd, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.And
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpOr, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Orr
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpOr, dst::src::[]) ->
      (match dst with
       | Vale_Compiler_AST.OReg rd ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Orr
                ((reg_of_operand rd), (reg_of_operand rd),
                  (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Vale_Compiler_AST.OpRev, dst::src::[]) ->
      (match (dst, src) with
       | (Vale_Compiler_AST.OReg rd, Vale_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Rev
                ((reg_of_operand rd), (reg_of_operand rn)))
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let reg_of_name (s : Prims.string) :
  Vale_Arm32_Machine.reg FStar_Pervasives_Native.option=
  if ((s = "r0") || (s = "a1")) || (s = "state")
  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R0
  else
    if (s = "r1") || (s = "a2")
    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R1
    else
      if (s = "r2") || (s = "a3")
      then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R2
      else
        if (s = "r3") || (s = "a4")
        then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R3
        else
          if (s = "r4") || (s = "v1")
          then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R4
          else
            if (s = "r5") || (s = "v2")
            then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R5
            else
              if (s = "r6") || (s = "v3")
              then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R6
              else
                if (s = "r7") || (s = "v4")
                then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R7
                else
                  if (s = "r8") || (s = "v5")
                  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R8
                  else
                    if ((s = "r9") || (s = "v6")) || (s = "sb")
                    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.R9
                    else
                      if ((s = "r10") || (s = "v7")) || (s = "sl")
                      then
                        FStar_Pervasives_Native.Some Vale_Arm32_Machine.R10
                      else
                        if ((s = "r11") || (s = "v8")) || (s = "fp")
                        then
                          FStar_Pervasives_Native.Some Vale_Arm32_Machine.R11
                        else
                          if (s = "r12") || (s = "ip")
                          then
                            FStar_Pervasives_Native.Some
                              Vale_Arm32_Machine.R12
                          else
                            if (s = "r13") || (s = "sp")
                            then
                              FStar_Pervasives_Native.Some
                                Vale_Arm32_Machine.SP
                            else
                              if (s = "r14") || (s = "lr")
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_Arm32_Machine.LR
                              else
                                if (s = "r15") || (s = "pc")
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_Arm32_Machine.PC
                                else FStar_Pervasives_Native.None
let reg_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Arm32_Machine.reg FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> reg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let cond_of_name (s : Prims.string) :
  Vale_Arm32_Machine.cond_code FStar_Pervasives_Native.option=
  if (s = "eq") || (s = "EQ")
  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondEQ
  else
    if (s = "ne") || (s = "NE")
    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondNE
    else
      if (((s = "cs") || (s = "CS")) || (s = "hs")) || (s = "HS")
      then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondCS
      else
        if (((s = "cc") || (s = "CC")) || (s = "lo")) || (s = "LO")
        then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondCC
        else
          if (s = "mi") || (s = "MI")
          then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondMI
          else
            if (s = "pl") || (s = "PL")
            then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondPL
            else
              if (s = "vs") || (s = "VS")
              then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondVS
              else
                if (s = "vc") || (s = "VC")
                then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondVC
                else
                  if (s = "hi") || (s = "HI")
                  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondHI
                  else
                    if (s = "ls") || (s = "LS")
                    then
                      FStar_Pervasives_Native.Some Vale_Arm32_Machine.CondLS
                    else
                      if (s = "ge") || (s = "GE")
                      then
                        FStar_Pervasives_Native.Some
                          Vale_Arm32_Machine.CondGE
                      else
                        if (s = "lt") || (s = "LT")
                        then
                          FStar_Pervasives_Native.Some
                            Vale_Arm32_Machine.CondLT
                        else
                          if (s = "gt") || (s = "GT")
                          then
                            FStar_Pervasives_Native.Some
                              Vale_Arm32_Machine.CondGT
                          else
                            if (s = "le") || (s = "LE")
                            then
                              FStar_Pervasives_Native.Some
                                Vale_Arm32_Machine.CondLE
                            else
                              if (s = "al") || (s = "AL")
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_Arm32_Machine.CondAL
                              else FStar_Pervasives_Native.None
let cond_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Arm32_Machine.cond_code FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> cond_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let sreg_of_name (s : Prims.string) :
  Vale_Arm32_Machine.sreg FStar_Pervasives_Native.option=
  if s = "s0"
  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S0
  else
    if s = "s1"
    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S1
    else
      if s = "s2"
      then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S2
      else
        if s = "s3"
        then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S3
        else
          if s = "s4"
          then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S4
          else
            if s = "s5"
            then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S5
            else
              if s = "s6"
              then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S6
              else
                if s = "s7"
                then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S7
                else
                  if s = "s8"
                  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S8
                  else
                    if s = "s9"
                    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.S9
                    else
                      if s = "s10"
                      then
                        FStar_Pervasives_Native.Some Vale_Arm32_Machine.S10
                      else
                        if s = "s11"
                        then
                          FStar_Pervasives_Native.Some Vale_Arm32_Machine.S11
                        else
                          if s = "s12"
                          then
                            FStar_Pervasives_Native.Some
                              Vale_Arm32_Machine.S12
                          else
                            if s = "s13"
                            then
                              FStar_Pervasives_Native.Some
                                Vale_Arm32_Machine.S13
                            else
                              if s = "s14"
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_Arm32_Machine.S14
                              else
                                if s = "s15"
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_Arm32_Machine.S15
                                else
                                  if s = "s16"
                                  then
                                    FStar_Pervasives_Native.Some
                                      Vale_Arm32_Machine.S16
                                  else
                                    if s = "s17"
                                    then
                                      FStar_Pervasives_Native.Some
                                        Vale_Arm32_Machine.S17
                                    else
                                      if s = "s18"
                                      then
                                        FStar_Pervasives_Native.Some
                                          Vale_Arm32_Machine.S18
                                      else
                                        if s = "s19"
                                        then
                                          FStar_Pervasives_Native.Some
                                            Vale_Arm32_Machine.S19
                                        else
                                          if s = "s20"
                                          then
                                            FStar_Pervasives_Native.Some
                                              Vale_Arm32_Machine.S20
                                          else
                                            if s = "s21"
                                            then
                                              FStar_Pervasives_Native.Some
                                                Vale_Arm32_Machine.S21
                                            else
                                              if s = "s22"
                                              then
                                                FStar_Pervasives_Native.Some
                                                  Vale_Arm32_Machine.S22
                                              else
                                                if s = "s23"
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    Vale_Arm32_Machine.S23
                                                else
                                                  if s = "s24"
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      Vale_Arm32_Machine.S24
                                                  else
                                                    if s = "s25"
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        Vale_Arm32_Machine.S25
                                                    else
                                                      if s = "s26"
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          Vale_Arm32_Machine.S26
                                                      else
                                                        if s = "s27"
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            Vale_Arm32_Machine.S27
                                                        else
                                                          if s = "s28"
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              Vale_Arm32_Machine.S28
                                                          else
                                                            if s = "s29"
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                Vale_Arm32_Machine.S29
                                                            else
                                                              if s = "s30"
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  Vale_Arm32_Machine.S30
                                                              else
                                                                if s = "s31"
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    Vale_Arm32_Machine.S31
                                                                else
                                                                  FStar_Pervasives_Native.None
let sreg_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Arm32_Machine.sreg FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> sreg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let dreg_of_name (s : Prims.string) :
  Vale_Arm32_Machine.dreg FStar_Pervasives_Native.option=
  if s = "d0"
  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D0
  else
    if s = "d1"
    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D1
    else
      if s = "d2"
      then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D2
      else
        if s = "d3"
        then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D3
        else
          if s = "d4"
          then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D4
          else
            if s = "d5"
            then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D5
            else
              if s = "d6"
              then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D6
              else
                if s = "d7"
                then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D7
                else
                  if s = "d8"
                  then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D8
                  else
                    if s = "d9"
                    then FStar_Pervasives_Native.Some Vale_Arm32_Machine.D9
                    else
                      if s = "d10"
                      then
                        FStar_Pervasives_Native.Some Vale_Arm32_Machine.D10
                      else
                        if s = "d11"
                        then
                          FStar_Pervasives_Native.Some Vale_Arm32_Machine.D11
                        else
                          if s = "d12"
                          then
                            FStar_Pervasives_Native.Some
                              Vale_Arm32_Machine.D12
                          else
                            if s = "d13"
                            then
                              FStar_Pervasives_Native.Some
                                Vale_Arm32_Machine.D13
                            else
                              if s = "d14"
                              then
                                FStar_Pervasives_Native.Some
                                  Vale_Arm32_Machine.D14
                              else
                                if s = "d15"
                                then
                                  FStar_Pervasives_Native.Some
                                    Vale_Arm32_Machine.D15
                                else
                                  if s = "d16"
                                  then
                                    FStar_Pervasives_Native.Some
                                      Vale_Arm32_Machine.D16
                                  else
                                    if s = "d17"
                                    then
                                      FStar_Pervasives_Native.Some
                                        Vale_Arm32_Machine.D17
                                    else
                                      if s = "d18"
                                      then
                                        FStar_Pervasives_Native.Some
                                          Vale_Arm32_Machine.D18
                                      else
                                        if s = "d19"
                                        then
                                          FStar_Pervasives_Native.Some
                                            Vale_Arm32_Machine.D19
                                        else
                                          if s = "d20"
                                          then
                                            FStar_Pervasives_Native.Some
                                              Vale_Arm32_Machine.D20
                                          else
                                            if s = "d21"
                                            then
                                              FStar_Pervasives_Native.Some
                                                Vale_Arm32_Machine.D21
                                            else
                                              if s = "d22"
                                              then
                                                FStar_Pervasives_Native.Some
                                                  Vale_Arm32_Machine.D22
                                              else
                                                if s = "d23"
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    Vale_Arm32_Machine.D23
                                                else
                                                  if s = "d24"
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      Vale_Arm32_Machine.D24
                                                  else
                                                    if s = "d25"
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        Vale_Arm32_Machine.D25
                                                    else
                                                      if s = "d26"
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          Vale_Arm32_Machine.D26
                                                      else
                                                        if s = "d27"
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            Vale_Arm32_Machine.D27
                                                        else
                                                          if s = "d28"
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              Vale_Arm32_Machine.D28
                                                          else
                                                            if s = "d29"
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                Vale_Arm32_Machine.D29
                                                            else
                                                              if s = "d30"
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  Vale_Arm32_Machine.D30
                                                              else
                                                                if s = "d31"
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    Vale_Arm32_Machine.D31
                                                                else
                                                                  FStar_Pervasives_Native.None
let dreg_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Arm32_Machine.dreg FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s -> dreg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let operand_of_expr (e : Vale_Compiler_AST.expr) :
  Vale_Arm32_Semantics.operand FStar_Pervasives_Native.option=
  match e with
  | Vale_Compiler_AST.EVar s ->
      (match reg_of_name s with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.OReg r)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.EConst n ->
      if (n >= Prims.int_zero) && (n <= (Prims.parse_int "4294967295"))
      then
        FStar_Pervasives_Native.Some
          (Vale_Arm32_Semantics.OImm (FStar_UInt32.uint_to_t n))
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
let lower_call (target : Vale_Compiler_AST.id)
  (args : Vale_Compiler_AST.expr Prims.list) :
  Vale_Arm32_Semantics.ins FStar_Pervasives_Native.option=
  match (target, args) with
  | ("Add", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Add (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Add32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Add (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Sub (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Sub (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Eor (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Eor (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Eor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Eor (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("And", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.And (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("And32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.And (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Orr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Orr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Orr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mov", dst::src::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           (match operand_of_expr src with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Mov (rd, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mov32", dst::src::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           (match operand_of_expr src with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Mov (rd, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovImm", dst::src::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           (match operand_of_expr src with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Mov (rd, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Rev (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev32", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Rev (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Ror (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Vale_Arm32_Semantics.Ror (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol32", dst::s1::(Vale_Compiler_AST.EConst amt)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           let ror_amt =
             (mod) ((Prims.of_int 32) - ((mod) amt (Prims.of_int 32)))
               (Prims.of_int 32) in
           if (ror_amt >= Prims.int_zero) && (ror_amt <= (Prims.of_int 31))
           then
             FStar_Pervasives_Native.Some
               (Vale_Arm32_Semantics.Ror
                  (rd, rn,
                    (Vale_Arm32_Semantics.OImm
                       (FStar_UInt32.uint_to_t ror_amt))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movs", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Movs (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movw", dst::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 65535))
           then
             FStar_Pervasives_Native.Some
               (Vale_Arm32_Semantics.Movw (rd, (FStar_UInt32.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movt", dst::(Vale_Compiler_AST.EConst imm)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 65535))
           then
             FStar_Pervasives_Native.Some
               (Vale_Arm32_Semantics.Movt (rd, (FStar_UInt32.uint_to_t imm)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adds", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Adds (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adc", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Adc (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adcs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Adcs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Subs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Subs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbc", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Sbc (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbcs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Sbcs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rsb", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Rsb (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rsbs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Rsbs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rsc", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Rsc (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rscs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Rscs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mul", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Mul (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mla", dst::s1::s2::ra::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (reg_of_expr ra))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some r_a)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Mla (rd, rn, rm, r_a))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mls", dst::s1::s2::ra::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (reg_of_expr ra))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some r_a)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Mls (rd, rn, rm, r_a))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Smull", rdlo::rdhi::rn::rm::[]) ->
      (match ((reg_of_expr rdlo), (reg_of_expr rdhi), (reg_of_expr rn),
               (reg_of_expr rm))
       with
       | (FStar_Pervasives_Native.Some rlo, FStar_Pervasives_Native.Some rhi,
          FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some r_m)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Smull (rlo, rhi, r_n, r_m))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Umull", rdlo::rdhi::rn::rm::[]) ->
      (match ((reg_of_expr rdlo), (reg_of_expr rdhi), (reg_of_expr rn),
               (reg_of_expr rm))
       with
       | (FStar_Pervasives_Native.Some rlo, FStar_Pervasives_Native.Some rhi,
          FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some r_m)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Umull (rlo, rhi, r_n, r_m))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sdiv", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Sdiv (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Udiv", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Udiv (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ands", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ands (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orrs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Orrs (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Eors", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Eors (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bic", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Bic (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bics", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Bics (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orn", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Orn (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mvn", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Mvn (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mvns", dst::src::[]) ->
      (match ((reg_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Mvns (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lsl", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Lsl (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lsr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Lsr (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Asr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (operand_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some op) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Asr (rd, rn, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Clz", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Clz (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rbit", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Rbit (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev16", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Rev16 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Revsh", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Revsh (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ubfx", dst::rn::(Vale_Compiler_AST.EConst
     lsb)::(Vale_Compiler_AST.EConst width)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr rn)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some r_n)
           ->
           if
             (((lsb >= Prims.int_zero) &&
                 (lsb <= (Prims.parse_int "4294967295")))
                && (width >= Prims.int_zero))
               && (width <= (Prims.parse_int "4294967295"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Arm32_Semantics.Ubfx
                  (rd, r_n, (FStar_UInt32.uint_to_t lsb),
                    (FStar_UInt32.uint_to_t width)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbfx", dst::rn::(Vale_Compiler_AST.EConst
     lsb)::(Vale_Compiler_AST.EConst width)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr rn)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some r_n)
           ->
           if
             (((lsb >= Prims.int_zero) &&
                 (lsb <= (Prims.parse_int "4294967295")))
                && (width >= Prims.int_zero))
               && (width <= (Prims.parse_int "4294967295"))
           then
             FStar_Pervasives_Native.Some
               (Vale_Arm32_Semantics.Sbfx
                  (rd, r_n, (FStar_UInt32.uint_to_t lsb),
                    (FStar_UInt32.uint_to_t width)))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmp", rn::op::[]) ->
      (match ((reg_of_expr rn), (operand_of_expr op)) with
       | (FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some o)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Cmp (r_n, o))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmn", rn::op::[]) ->
      (match ((reg_of_expr rn), (operand_of_expr op)) with
       | (FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some o)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Cmn (r_n, o))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Tst", rn::op::[]) ->
      (match ((reg_of_expr rn), (operand_of_expr op)) with
       | (FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some o)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Tst (r_n, o))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Teq", rn::op::[]) ->
      (match ((reg_of_expr rn), (operand_of_expr op)) with
       | (FStar_Pervasives_Native.Some r_n, FStar_Pervasives_Native.Some o)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Teq (r_n, o))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sxtb", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Sxtb (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sxth", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Sxth (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Uxtb", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Uxtb (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Uxth", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Uxth (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldr", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldr
                (rd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldr", dst::buf::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldr (rd, (Vale_Arm32_Machine.MBase rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrb", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldrb
                (rd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrh", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldrh
                (rd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrsb", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldrsb
                (rd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrsh", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldrsh
                (rd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrd", rt::rt2::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr rt), (reg_of_expr rt2), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some r2,
          FStar_Pervasives_Native.Some rb) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ldrd
                (r1, r2, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", src::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Str
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", buf::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Str
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", src::buf::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Str (rs, (Vale_Arm32_Machine.MBase rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strb", src::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Strb
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strb", buf::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Strb
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strh", src::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Strh
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strh", buf::(Vale_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Strh
                (rs, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strd", rt::rt2::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr rt), (reg_of_expr rt2), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some r1, FStar_Pervasives_Native.Some r2,
          FStar_Pervasives_Native.Some rb) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Strd
                (r1, r2, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Push", r::[]) ->
      (match reg_of_expr r with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Push [rd])
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pop", r::[]) ->
      (match reg_of_expr r with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Pop [rd])
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bc", c::(Vale_Compiler_AST.EVar target1)::[]) ->
      (match cond_of_expr c with
       | FStar_Pervasives_Native.Some cond ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Bc (cond, target1))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bx", target1::[]) ->
      (match reg_of_expr target1 with
       | FStar_Pervasives_Native.Some rt ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Bx rt)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Blx", target1::[]) ->
      (match reg_of_expr target1 with
       | FStar_Pervasives_Native.Some rt ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Blx rt)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vmov", dst::src::[]) ->
      (match ((operand_of_expr dst), (operand_of_expr src)) with
       | (FStar_Pervasives_Native.Some od, FStar_Pervasives_Native.Some os)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Vmov (od, os))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VaddF32", dst::s1::s2::[]) ->
      (match ((sreg_of_expr dst), (sreg_of_expr s1), (sreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some sd, FStar_Pervasives_Native.Some s_1,
          FStar_Pervasives_Native.Some s_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VaddF32 (sd, s_1, s_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VaddF64", dst::s1::s2::[]) ->
      (match ((dreg_of_expr dst), (dreg_of_expr s1), (dreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some dd, FStar_Pervasives_Native.Some d_1,
          FStar_Pervasives_Native.Some d_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VaddF64 (dd, d_1, d_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VsubF32", dst::s1::s2::[]) ->
      (match ((sreg_of_expr dst), (sreg_of_expr s1), (sreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some sd, FStar_Pervasives_Native.Some s_1,
          FStar_Pervasives_Native.Some s_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VsubF32 (sd, s_1, s_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VsubF64", dst::s1::s2::[]) ->
      (match ((dreg_of_expr dst), (dreg_of_expr s1), (dreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some dd, FStar_Pervasives_Native.Some d_1,
          FStar_Pervasives_Native.Some d_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VsubF64 (dd, d_1, d_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VmulF32", dst::s1::s2::[]) ->
      (match ((sreg_of_expr dst), (sreg_of_expr s1), (sreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some sd, FStar_Pervasives_Native.Some s_1,
          FStar_Pervasives_Native.Some s_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VmulF32 (sd, s_1, s_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("VmulF64", dst::s1::s2::[]) ->
      (match ((dreg_of_expr dst), (dreg_of_expr s1), (dreg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some dd, FStar_Pervasives_Native.Some d_1,
          FStar_Pervasives_Native.Some d_2) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.VmulF64 (dd, d_1, d_2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vldr", dst::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((sreg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some sd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Vldr
                (sd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vstr", src::buf::(Vale_Compiler_AST.EConst offset)::[]) ->
      (match ((sreg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some sd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Vstr
                (sd, (Vale_Arm32_Machine.MBaseOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vpush", r::[]) ->
      (match sreg_of_expr r with
       | FStar_Pervasives_Native.Some sd ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Vpush [sd])
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vpop", r::[]) ->
      (match sreg_of_expr r with
       | FStar_Pervasives_Native.Some sd ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Vpop [sd])
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let rec lower_stmt (s : Vale_Compiler_AST.stmt) :
  Vale_Arm32_Semantics.code FStar_Pervasives_Native.option=
  match s with
  | Vale_Compiler_AST.SIns (op, args) ->
      (match lower_ins op args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Ins i)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Vale_Compiler_AST.SBlock stmts -> lower_stmts stmts
  | Vale_Compiler_AST.SCall (target, args) ->
      (match lower_call target args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Ins i)
       | FStar_Pervasives_Native.None ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Ins (Vale_Arm32_Semantics.Bl target)))
  | Vale_Compiler_AST.SAssert uu___ ->
      FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Block [])
  | Vale_Compiler_AST.SAssume uu___ ->
      FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Block [])
  | uu___ -> FStar_Pervasives_Native.None
and lower_stmts (stmts : Vale_Compiler_AST.stmt Prims.list) :
  Vale_Arm32_Semantics.code FStar_Pervasives_Native.option=
  match stmts with
  | [] -> FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Block [])
  | s::[] -> lower_stmt s
  | s::rest ->
      (match ((lower_stmt s), (lower_stmts rest)) with
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some
          (Vale_Arm32_Semantics.Block cs)) ->
           FStar_Pervasives_Native.Some
             (Vale_Arm32_Semantics.Block (c1 :: cs))
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some c2)
           ->
           FStar_Pervasives_Native.Some (Vale_Arm32_Semantics.Block [c1; c2])
       | uu___ -> FStar_Pervasives_Native.None)
