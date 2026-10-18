open Prims
let reg_of_operand (r : Valerie_Compiler_AST.reg_operand) :
  Valerie_AArch64_Machine.reg=
  match r with
  | Valerie_Compiler_AST.RegNumbered n ->
      if n = Stdint.Uint32.zero
      then Valerie_AArch64_Machine.X0
      else
        if n = Stdint.Uint32.one
        then Valerie_AArch64_Machine.X1
        else
          if n = (Stdint.Uint32.of_int 2)
          then Valerie_AArch64_Machine.X2
          else
            if n = (Stdint.Uint32.of_int 3)
            then Valerie_AArch64_Machine.X3
            else
              if n = (Stdint.Uint32.of_int 4)
              then Valerie_AArch64_Machine.X4
              else
                if n = (Stdint.Uint32.of_int 5)
                then Valerie_AArch64_Machine.X5
                else
                  if n = (Stdint.Uint32.of_int 6)
                  then Valerie_AArch64_Machine.X6
                  else
                    if n = (Stdint.Uint32.of_int 7)
                    then Valerie_AArch64_Machine.X7
                    else
                      if n = (Stdint.Uint32.of_int 8)
                      then Valerie_AArch64_Machine.X8
                      else
                        if n = (Stdint.Uint32.of_int 9)
                        then Valerie_AArch64_Machine.X9
                        else
                          if n = (Stdint.Uint32.of_int 10)
                          then Valerie_AArch64_Machine.X10
                          else
                            if n = (Stdint.Uint32.of_int 11)
                            then Valerie_AArch64_Machine.X11
                            else
                              if n = (Stdint.Uint32.of_int 12)
                              then Valerie_AArch64_Machine.X12
                              else
                                if n = (Stdint.Uint32.of_int 13)
                                then Valerie_AArch64_Machine.X13
                                else
                                  if n = (Stdint.Uint32.of_int 14)
                                  then Valerie_AArch64_Machine.X14
                                  else
                                    if n = (Stdint.Uint32.of_int 15)
                                    then Valerie_AArch64_Machine.X15
                                    else
                                      if n = (Stdint.Uint32.of_int 16)
                                      then Valerie_AArch64_Machine.X16
                                      else
                                        if n = (Stdint.Uint32.of_int 17)
                                        then Valerie_AArch64_Machine.X17
                                        else
                                          if n = (Stdint.Uint32.of_int 18)
                                          then Valerie_AArch64_Machine.X18
                                          else
                                            if n = (Stdint.Uint32.of_int 19)
                                            then Valerie_AArch64_Machine.X19
                                            else
                                              if
                                                n = (Stdint.Uint32.of_int 20)
                                              then
                                                Valerie_AArch64_Machine.X20
                                              else
                                                if
                                                  n =
                                                    (Stdint.Uint32.of_int 21)
                                                then
                                                  Valerie_AArch64_Machine.X21
                                                else
                                                  if
                                                    n =
                                                      (Stdint.Uint32.of_int
                                                         22)
                                                  then
                                                    Valerie_AArch64_Machine.X22
                                                  else
                                                    if
                                                      n =
                                                        (Stdint.Uint32.of_int
                                                           23)
                                                    then
                                                      Valerie_AArch64_Machine.X23
                                                    else
                                                      if
                                                        n =
                                                          (Stdint.Uint32.of_int
                                                             24)
                                                      then
                                                        Valerie_AArch64_Machine.X24
                                                      else
                                                        if
                                                          n =
                                                            (Stdint.Uint32.of_int
                                                               25)
                                                        then
                                                          Valerie_AArch64_Machine.X25
                                                        else
                                                          if
                                                            n =
                                                              (Stdint.Uint32.of_int
                                                                 26)
                                                          then
                                                            Valerie_AArch64_Machine.X26
                                                          else
                                                            if
                                                              n =
                                                                (Stdint.Uint32.of_int
                                                                   27)
                                                            then
                                                              Valerie_AArch64_Machine.X27
                                                            else
                                                              if
                                                                n =
                                                                  (Stdint.Uint32.of_int
                                                                    28)
                                                              then
                                                                Valerie_AArch64_Machine.X28
                                                              else
                                                                if
                                                                  n =
                                                                    (
                                                                    Stdint.Uint32.of_int
                                                                    29)
                                                                then
                                                                  Valerie_AArch64_Machine.X29
                                                                else
                                                                  if
                                                                    n =
                                                                    (Stdint.Uint32.of_int
                                                                    30)
                                                                  then
                                                                    Valerie_AArch64_Machine.X30
                                                                  else
                                                                    Valerie_AArch64_Machine.SP
let lower_operand (op : Valerie_Compiler_AST.operand) :
  Valerie_AArch64_Semantics.operand=
  match op with
  | Valerie_Compiler_AST.OReg r ->
      Valerie_AArch64_Semantics.OReg
        ((reg_of_operand r), Valerie_AArch64_Machine.W64)
  | Valerie_Compiler_AST.OImm i -> Valerie_AArch64_Semantics.OImm i
  | Valerie_Compiler_AST.OMemBuffer (r, offset) ->
      Valerie_AArch64_Semantics.OMem
        (Valerie_AArch64_Machine.MOffset
           ((reg_of_operand r), (FStar_UInt32.v offset)))
let lower_ins (op : Valerie_Compiler_AST.ins_opcode)
  (args : Valerie_Compiler_AST.operand Prims.list) :
  Valerie_AArch64_Semantics.ins FStar_Pervasives_Native.option=
  match (op, args) with
  | (Valerie_Compiler_AST.OpMov, dst::src::[]) ->
      (match dst with
       | Valerie_Compiler_AST.OReg r ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Mov
                ((reg_of_operand r), (lower_operand src)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Valerie_Compiler_AST.OpAdd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Valerie_Compiler_AST.OReg rd, Valerie_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Add
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Valerie_Compiler_AST.OpSub, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Valerie_Compiler_AST.OReg rd, Valerie_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sub
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Valerie_Compiler_AST.OpXor, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Valerie_Compiler_AST.OReg rd, Valerie_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Eor
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Valerie_Compiler_AST.OpAnd, dst::src1::src2::[]) ->
      (match (dst, src1) with
       | (Valerie_Compiler_AST.OReg rd, Valerie_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.And
                ((reg_of_operand rd), (reg_of_operand rn),
                  (lower_operand src2)))
       | uu___ -> FStar_Pervasives_Native.None)
  | (Valerie_Compiler_AST.OpRev, dst::src::[]) ->
      (match (dst, src) with
       | (Valerie_Compiler_AST.OReg rd, Valerie_Compiler_AST.OReg rn) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rev64
                ((reg_of_operand rd), (reg_of_operand rn)))
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.None
let reg_of_name (s : Prims.string) :
  Valerie_AArch64_Machine.reg FStar_Pervasives_Native.option=
  if (((s = "r0") || (s = "x0")) || (s = "w0")) || (s = "state")
  then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X0
  else
    if ((s = "r1") || (s = "x1")) || (s = "w1")
    then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X1
    else
      if ((s = "r2") || (s = "x2")) || (s = "w2")
      then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X2
      else
        if ((s = "r3") || (s = "x3")) || (s = "w3")
        then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X3
        else
          if ((s = "r4") || (s = "x4")) || (s = "w4")
          then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X4
          else
            if ((s = "r5") || (s = "x5")) || (s = "w5")
            then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X5
            else
              if ((s = "r6") || (s = "x6")) || (s = "w6")
              then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X6
              else
                if ((s = "r7") || (s = "x7")) || (s = "w7")
                then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X7
                else
                  if ((s = "r8") || (s = "x8")) || (s = "w8")
                  then
                    FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X8
                  else
                    if ((s = "r9") || (s = "x9")) || (s = "w9")
                    then
                      FStar_Pervasives_Native.Some Valerie_AArch64_Machine.X9
                    else
                      if ((s = "r10") || (s = "x10")) || (s = "w10")
                      then
                        FStar_Pervasives_Native.Some
                          Valerie_AArch64_Machine.X10
                      else
                        if ((s = "r11") || (s = "x11")) || (s = "w11")
                        then
                          FStar_Pervasives_Native.Some
                            Valerie_AArch64_Machine.X11
                        else
                          if ((s = "r12") || (s = "x12")) || (s = "w12")
                          then
                            FStar_Pervasives_Native.Some
                              Valerie_AArch64_Machine.X12
                          else
                            if ((s = "r13") || (s = "x13")) || (s = "w13")
                            then
                              FStar_Pervasives_Native.Some
                                Valerie_AArch64_Machine.X13
                            else
                              if ((s = "r14") || (s = "x14")) || (s = "w14")
                              then
                                FStar_Pervasives_Native.Some
                                  Valerie_AArch64_Machine.X14
                              else
                                if
                                  ((s = "r15") || (s = "x15")) || (s = "w15")
                                then
                                  FStar_Pervasives_Native.Some
                                    Valerie_AArch64_Machine.X15
                                else
                                  if
                                    ((s = "r16") || (s = "x16")) ||
                                      (s = "w16")
                                  then
                                    FStar_Pervasives_Native.Some
                                      Valerie_AArch64_Machine.X16
                                  else
                                    if
                                      ((s = "r17") || (s = "x17")) ||
                                        (s = "w17")
                                    then
                                      FStar_Pervasives_Native.Some
                                        Valerie_AArch64_Machine.X17
                                    else
                                      if
                                        ((s = "r18") || (s = "x18")) ||
                                          (s = "w18")
                                      then
                                        FStar_Pervasives_Native.Some
                                          Valerie_AArch64_Machine.X18
                                      else
                                        if
                                          ((s = "r19") || (s = "x19")) ||
                                            (s = "w19")
                                        then
                                          FStar_Pervasives_Native.Some
                                            Valerie_AArch64_Machine.X19
                                        else
                                          if
                                            ((s = "r20") || (s = "x20")) ||
                                              (s = "w20")
                                          then
                                            FStar_Pervasives_Native.Some
                                              Valerie_AArch64_Machine.X20
                                          else
                                            if
                                              ((s = "r21") || (s = "x21")) ||
                                                (s = "w21")
                                            then
                                              FStar_Pervasives_Native.Some
                                                Valerie_AArch64_Machine.X21
                                            else
                                              if
                                                ((s = "r22") || (s = "x22"))
                                                  || (s = "w22")
                                              then
                                                FStar_Pervasives_Native.Some
                                                  Valerie_AArch64_Machine.X22
                                              else
                                                if
                                                  ((s = "r23") || (s = "x23"))
                                                    || (s = "w23")
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    Valerie_AArch64_Machine.X23
                                                else
                                                  if
                                                    ((s = "r24") ||
                                                       (s = "x24"))
                                                      || (s = "w24")
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      Valerie_AArch64_Machine.X24
                                                  else
                                                    if
                                                      ((s = "r25") ||
                                                         (s = "x25"))
                                                        || (s = "w25")
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        Valerie_AArch64_Machine.X25
                                                    else
                                                      if
                                                        ((s = "r26") ||
                                                           (s = "x26"))
                                                          || (s = "w26")
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          Valerie_AArch64_Machine.X26
                                                      else
                                                        if
                                                          ((s = "r27") ||
                                                             (s = "x27"))
                                                            || (s = "w27")
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            Valerie_AArch64_Machine.X27
                                                        else
                                                          if
                                                            ((s = "r28") ||
                                                               (s = "x28"))
                                                              || (s = "w28")
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              Valerie_AArch64_Machine.X28
                                                          else
                                                            if
                                                              ((s = "r29") ||
                                                                 (s = "x29"))
                                                                ||
                                                                (s = "w29")
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                Valerie_AArch64_Machine.X29
                                                            else
                                                              if
                                                                ((s = "r30")
                                                                   ||
                                                                   (s = "x30"))
                                                                  ||
                                                                  (s = "w30")
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  Valerie_AArch64_Machine.X30
                                                              else
                                                                if
                                                                  ((s = "sp")
                                                                    ||
                                                                    (s =
                                                                    "rsp"))
                                                                    ||
                                                                    (
                                                                    s = "wsp")
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    Valerie_AArch64_Machine.SP
                                                                else
                                                                  FStar_Pervasives_Native.None
let reg_of_expr (e : Valerie_Compiler_AST.expr) :
  Valerie_AArch64_Machine.reg FStar_Pervasives_Native.option=
  match e with
  | Valerie_Compiler_AST.EVar s -> reg_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let vec_of_name (s : Prims.string) :
  Valerie_AArch64_Machine.vec FStar_Pervasives_Native.option=
  if ((s = "v0") || (s = "q0")) || (s = "d0")
  then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V0
  else
    if ((s = "v1") || (s = "q1")) || (s = "d1")
    then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V1
    else
      if ((s = "v2") || (s = "q2")) || (s = "d2")
      then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V2
      else
        if ((s = "v3") || (s = "q3")) || (s = "d3")
        then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V3
        else
          if ((s = "v4") || (s = "q4")) || (s = "d4")
          then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V4
          else
            if ((s = "v5") || (s = "q5")) || (s = "d5")
            then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V5
            else
              if ((s = "v6") || (s = "q6")) || (s = "d6")
              then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V6
              else
                if ((s = "v7") || (s = "q7")) || (s = "d7")
                then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V7
                else
                  if ((s = "v8") || (s = "q8")) || (s = "d8")
                  then
                    FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V8
                  else
                    if ((s = "v9") || (s = "q9")) || (s = "d9")
                    then
                      FStar_Pervasives_Native.Some Valerie_AArch64_Machine.V9
                    else
                      if ((s = "v10") || (s = "q10")) || (s = "d10")
                      then
                        FStar_Pervasives_Native.Some
                          Valerie_AArch64_Machine.V10
                      else
                        if ((s = "v11") || (s = "q11")) || (s = "d11")
                        then
                          FStar_Pervasives_Native.Some
                            Valerie_AArch64_Machine.V11
                        else
                          if ((s = "v12") || (s = "q12")) || (s = "d12")
                          then
                            FStar_Pervasives_Native.Some
                              Valerie_AArch64_Machine.V12
                          else
                            if ((s = "v13") || (s = "q13")) || (s = "d13")
                            then
                              FStar_Pervasives_Native.Some
                                Valerie_AArch64_Machine.V13
                            else
                              if ((s = "v14") || (s = "q14")) || (s = "d14")
                              then
                                FStar_Pervasives_Native.Some
                                  Valerie_AArch64_Machine.V14
                              else
                                if
                                  ((s = "v15") || (s = "q15")) || (s = "d15")
                                then
                                  FStar_Pervasives_Native.Some
                                    Valerie_AArch64_Machine.V15
                                else
                                  if
                                    ((s = "v16") || (s = "q16")) ||
                                      (s = "d16")
                                  then
                                    FStar_Pervasives_Native.Some
                                      Valerie_AArch64_Machine.V16
                                  else
                                    if
                                      ((s = "v17") || (s = "q17")) ||
                                        (s = "d17")
                                    then
                                      FStar_Pervasives_Native.Some
                                        Valerie_AArch64_Machine.V17
                                    else
                                      if
                                        ((s = "v18") || (s = "q18")) ||
                                          (s = "d18")
                                      then
                                        FStar_Pervasives_Native.Some
                                          Valerie_AArch64_Machine.V18
                                      else
                                        if
                                          ((s = "v19") || (s = "q19")) ||
                                            (s = "d19")
                                        then
                                          FStar_Pervasives_Native.Some
                                            Valerie_AArch64_Machine.V19
                                        else
                                          if
                                            ((s = "v20") || (s = "q20")) ||
                                              (s = "d20")
                                          then
                                            FStar_Pervasives_Native.Some
                                              Valerie_AArch64_Machine.V20
                                          else
                                            if
                                              ((s = "v21") || (s = "q21")) ||
                                                (s = "d21")
                                            then
                                              FStar_Pervasives_Native.Some
                                                Valerie_AArch64_Machine.V21
                                            else
                                              if
                                                ((s = "v22") || (s = "q22"))
                                                  || (s = "d22")
                                              then
                                                FStar_Pervasives_Native.Some
                                                  Valerie_AArch64_Machine.V22
                                              else
                                                if
                                                  ((s = "v23") || (s = "q23"))
                                                    || (s = "d23")
                                                then
                                                  FStar_Pervasives_Native.Some
                                                    Valerie_AArch64_Machine.V23
                                                else
                                                  if
                                                    ((s = "v24") ||
                                                       (s = "q24"))
                                                      || (s = "d24")
                                                  then
                                                    FStar_Pervasives_Native.Some
                                                      Valerie_AArch64_Machine.V24
                                                  else
                                                    if
                                                      ((s = "v25") ||
                                                         (s = "q25"))
                                                        || (s = "d25")
                                                    then
                                                      FStar_Pervasives_Native.Some
                                                        Valerie_AArch64_Machine.V25
                                                    else
                                                      if
                                                        ((s = "v26") ||
                                                           (s = "q26"))
                                                          || (s = "d26")
                                                      then
                                                        FStar_Pervasives_Native.Some
                                                          Valerie_AArch64_Machine.V26
                                                      else
                                                        if
                                                          ((s = "v27") ||
                                                             (s = "q27"))
                                                            || (s = "d27")
                                                        then
                                                          FStar_Pervasives_Native.Some
                                                            Valerie_AArch64_Machine.V27
                                                        else
                                                          if
                                                            ((s = "v28") ||
                                                               (s = "q28"))
                                                              || (s = "d28")
                                                          then
                                                            FStar_Pervasives_Native.Some
                                                              Valerie_AArch64_Machine.V28
                                                          else
                                                            if
                                                              ((s = "v29") ||
                                                                 (s = "q29"))
                                                                ||
                                                                (s = "d29")
                                                            then
                                                              FStar_Pervasives_Native.Some
                                                                Valerie_AArch64_Machine.V29
                                                            else
                                                              if
                                                                ((s = "v30")
                                                                   ||
                                                                   (s = "q30"))
                                                                  ||
                                                                  (s = "d30")
                                                              then
                                                                FStar_Pervasives_Native.Some
                                                                  Valerie_AArch64_Machine.V30
                                                              else
                                                                if
                                                                  ((s = "v31")
                                                                    ||
                                                                    (s =
                                                                    "q31"))
                                                                    ||
                                                                    (
                                                                    s = "d31")
                                                                then
                                                                  FStar_Pervasives_Native.Some
                                                                    Valerie_AArch64_Machine.V31
                                                                else
                                                                  FStar_Pervasives_Native.None
let vec_of_expr (e : Valerie_Compiler_AST.expr) :
  Valerie_AArch64_Machine.vec FStar_Pervasives_Native.option=
  match e with
  | Valerie_Compiler_AST.EVar s -> vec_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let cond_of_name (s : Prims.string) :
  Valerie_AArch64_Machine.cond_code FStar_Pervasives_Native.option=
  if (s = "eq") || (s = "EQ")
  then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondEQ
  else
    if (s = "ne") || (s = "NE")
    then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondNE
    else
      if (((s = "cs") || (s = "CS")) || (s = "hs")) || (s = "HS")
      then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondCS
      else
        if (((s = "cc") || (s = "CC")) || (s = "lo")) || (s = "LO")
        then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondCC
        else
          if (s = "mi") || (s = "MI")
          then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondMI
          else
            if (s = "pl") || (s = "PL")
            then FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondPL
            else
              if (s = "vs") || (s = "VS")
              then
                FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondVS
              else
                if (s = "vc") || (s = "VC")
                then
                  FStar_Pervasives_Native.Some Valerie_AArch64_Machine.CondVC
                else
                  if (s = "hi") || (s = "HI")
                  then
                    FStar_Pervasives_Native.Some
                      Valerie_AArch64_Machine.CondHI
                  else
                    if (s = "ls") || (s = "LS")
                    then
                      FStar_Pervasives_Native.Some
                        Valerie_AArch64_Machine.CondLS
                    else
                      if (s = "ge") || (s = "GE")
                      then
                        FStar_Pervasives_Native.Some
                          Valerie_AArch64_Machine.CondGE
                      else
                        if (s = "lt") || (s = "LT")
                        then
                          FStar_Pervasives_Native.Some
                            Valerie_AArch64_Machine.CondLT
                        else
                          if (s = "gt") || (s = "GT")
                          then
                            FStar_Pervasives_Native.Some
                              Valerie_AArch64_Machine.CondGT
                          else
                            if (s = "le") || (s = "LE")
                            then
                              FStar_Pervasives_Native.Some
                                Valerie_AArch64_Machine.CondLE
                            else
                              if (s = "al") || (s = "AL")
                              then
                                FStar_Pervasives_Native.Some
                                  Valerie_AArch64_Machine.CondAL
                              else
                                if (s = "nv") || (s = "NV")
                                then
                                  FStar_Pervasives_Native.Some
                                    Valerie_AArch64_Machine.CondNV
                                else FStar_Pervasives_Native.None
let cond_of_expr (e : Valerie_Compiler_AST.expr) :
  Valerie_AArch64_Machine.cond_code FStar_Pervasives_Native.option=
  match e with
  | Valerie_Compiler_AST.EVar s -> cond_of_name s
  | uu___ -> FStar_Pervasives_Native.None
let operand_of_expr (e : Valerie_Compiler_AST.expr)
  (w : Valerie_AArch64_Machine.reg_width) :
  Valerie_AArch64_Semantics.operand FStar_Pervasives_Native.option=
  match e with
  | Valerie_Compiler_AST.EVar s ->
      (match reg_of_name s with
       | FStar_Pervasives_Native.Some r ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.OReg (r, w))
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Valerie_Compiler_AST.EConst n ->
      if
        (n >= Prims.int_zero) &&
          (n <= (Prims.parse_int "18446744073709551615"))
      then
        FStar_Pervasives_Native.Some
          (Valerie_AArch64_Semantics.OImm (FStar_UInt64.uint_to_t n))
      else FStar_Pervasives_Native.None
  | uu___ -> FStar_Pervasives_Native.None
let lower_call (target : Valerie_Compiler_AST.id)
  (args : Valerie_Compiler_AST.expr Prims.list) :
  Valerie_AArch64_Semantics.ins FStar_Pervasives_Native.option=
  match (target, args) with
  | ("Load32", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldr32
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldr32", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldr32
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Store32", buf::(Valerie_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr buf), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rb, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Str32
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str32", buf::(Valerie_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr buf), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rb, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Str32
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Add32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Add32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Add", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Add (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Sub32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sub", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Sub (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Eor32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Eor (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Eor", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Eor (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("And32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.And32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("And", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.And (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Or32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Orr32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orr32", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W32 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Orr32 (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol32", dst::s1::(Valerie_Compiler_AST.EConst amt)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           let ror_amt =
             (mod) ((Prims.of_int 32) - ((mod) amt (Prims.of_int 32)))
               (Prims.of_int 32) in
           if (ror_amt >= Prims.int_zero) && (ror_amt <= (Prims.of_int 31))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Ror32
                  (rd, rn,
                    (Valerie_AArch64_Semantics.OImm
                       (FStar_UInt64.uint_to_t ror_amt))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror32", dst::s1::(Valerie_Compiler_AST.EConst amt)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           let ror_amt = (mod) amt (Prims.of_int 32) in
           if (ror_amt >= Prims.int_zero) && (ror_amt <= (Prims.of_int 31))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Ror32
                  (rd, rn,
                    (Valerie_AArch64_Semantics.OImm
                       (FStar_UInt64.uint_to_t ror_amt))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rol", dst::s1::(Valerie_Compiler_AST.EConst amt)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           let ror_amt =
             (mod) ((Prims.of_int 64) - ((mod) amt (Prims.of_int 64)))
               (Prims.of_int 64) in
           if (ror_amt >= Prims.int_zero) && (ror_amt <= (Prims.of_int 63))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Ror
                  (rd, rn,
                    (Valerie_AArch64_Semantics.OImm
                       (FStar_UInt64.uint_to_t ror_amt))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror", dst::s1::(Valerie_Compiler_AST.EConst amt)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           let ror_amt = (mod) amt (Prims.of_int 64) in
           if (ror_amt >= Prims.int_zero) && (ror_amt <= (Prims.of_int 63))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Ror
                  (rd, rn,
                    (Valerie_AArch64_Semantics.OImm
                       (FStar_UInt64.uint_to_t ror_amt))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ror", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Ror (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("FmovFromGpr", dst::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.FmovGprToVec (vd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("FmovToGpr", dst::src::[]) ->
      (match ((reg_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.FmovVecToGpr (rd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovVecLane", dst::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.InsLane (vd, Prims.int_one, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovVecLane", dst::(Valerie_Compiler_AST.EConst lane)::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           if (lane >= Prims.int_zero) && (lane < (Prims.of_int 2))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.InsLane (vd, lane, rs))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("InsLane", dst::(Valerie_Compiler_AST.EConst lane)::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           if (lane >= Prims.int_zero) && (lane < (Prims.of_int 2))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.InsLane (vd, lane, rs))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovLaneToGpr", dst::src::[]) ->
      (match ((reg_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Umov (rd, vs, Prims.int_one))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovLaneToGpr", dst::src::(Valerie_Compiler_AST.EConst lane)::[]) ->
      (match ((reg_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some vs)
           ->
           if (lane >= Prims.int_zero) && (lane < (Prims.of_int 2))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Umov (rd, vs, lane))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Umov", dst::src::(Valerie_Compiler_AST.EConst lane)::[]) ->
      (match ((reg_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some vs)
           ->
           if (lane >= Prims.int_zero) && (lane < (Prims.of_int 2))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Umov (rd, vs, lane))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MovImm", dst::(Valerie_Compiler_AST.EConst imm)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             (imm >= Prims.int_zero) &&
               (imm <= (Prims.parse_int "18446744073709551615"))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Mov
                  (rd,
                    (Valerie_AArch64_Semantics.OImm
                       (FStar_UInt64.uint_to_t imm))))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mov", dst::src::[]) ->
      (match ((reg_of_expr dst),
               (operand_of_expr src Valerie_AArch64_Machine.W64))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some op)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Mov (rd, op))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rbit", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rbit (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rev64 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev64", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rev64 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev32", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rev32 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rev16", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rev16 (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Clz", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Clz (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("DupVec", dst::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Dup (vd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Dup", dst::src::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Dup (vd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pmull", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Pmull (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Pmull2", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Pmull2 (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Veor", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Veor (vd, vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Veor", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Veor (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vext8", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vext8 (vd, v1, v2, (Prims.of_int 8)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vext8", dst::s1::s2::(Valerie_Compiler_AST.EConst idx)::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           if (idx >= Prims.int_zero) && (idx < (Prims.of_int 16))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Vext8 (vd, v1, v2, idx))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vext", dst::s1::s2::(Valerie_Compiler_AST.EConst idx)::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           if (idx >= Prims.int_zero) && (idx < (Prims.of_int 16))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Vext8 (vd, v1, v2, idx))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aese", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Aese (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesd", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Aesd (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesmc", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Aesmc (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Aesimc", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Aesimc (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1c", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1c (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1p", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1p (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1m", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1m (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1h", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1h (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1su0", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1su0 (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha1su1", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha1su1 (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256h", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha256h (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256h2", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha256h2 (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256su0", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha256su0 (vd, vs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sha256su1", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sha256su1 (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adds", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Adds (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Subs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Subs (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adc", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Adc (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adcs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Adcs (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbc", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sbc (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sbcs", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sbcs (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmp", s1::s2::[]) ->
      (match reg_of_expr s1 with
       | FStar_Pervasives_Native.Some rn ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Cmp (rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cmn", s1::s2::[]) ->
      (match reg_of_expr s1 with
       | FStar_Pervasives_Native.Some rn ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Cmn (rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Tst", s1::s2::[]) ->
      (match reg_of_expr s1 with
       | FStar_Pervasives_Native.Some rn ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Tst (rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Neg", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Neg (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Negs", dst::src::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Negs (rd, rs))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mvn", dst::src::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           (match operand_of_expr src Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Mvn (rd, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Mul", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Mul (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Madd", dst::s1::s2::s3::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (reg_of_expr s3))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some ra)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Madd (rd, rn, rm, ra))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Msub", dst::s1::s2::s3::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (reg_of_expr s3))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some ra)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Msub (rd, rn, rm, ra))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Smulh", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Smulh (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Umulh", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Umulh (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Sdiv", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Sdiv (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Udiv", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Udiv (rd, rn, rm))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ands", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Ands (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Orr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Orn", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Orn (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Eon", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Eon (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bic", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Bic (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Bics", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Bics (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lsl", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Lsl (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Lsr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Lsr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Asr", dst::s1::s2::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn)
           ->
           (match operand_of_expr s2 Valerie_AArch64_Machine.W64 with
            | FStar_Pervasives_Native.Some op ->
                FStar_Pervasives_Native.Some
                  (Valerie_AArch64_Semantics.Asr (rd, rn, op))
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Extr", dst::s1::s2::(Valerie_Compiler_AST.EConst lsb)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm) ->
           if (lsb >= Prims.int_zero) && (lsb <= (Prims.of_int 63))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Extr (rd, rn, rm, lsb))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Csel", dst::s1::s2::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (cond_of_expr c))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Csel (rd, rn, rm, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cset", dst::c::[]) ->
      (match ((reg_of_expr dst), (cond_of_expr c)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cset (rd, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Csetm", dst::c::[]) ->
      (match ((reg_of_expr dst), (cond_of_expr c)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Csetm (rd, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cinc", dst::s1::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (cond_of_expr c)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some cond) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cinc (rd, rn, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cinv", dst::s1::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (cond_of_expr c)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some cond) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cinv (rd, rn, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cneg", dst::s1::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (cond_of_expr c)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some cond) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cneg (rd, rn, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Csinc", dst::s1::s2::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (cond_of_expr c))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Csinc (rd, rn, rm, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Csinv", dst::s1::s2::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (cond_of_expr c))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Csinv (rd, rn, rm, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Csneg", dst::s1::s2::c::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr s1), (reg_of_expr s2),
               (cond_of_expr c))
       with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rn,
          FStar_Pervasives_Native.Some rm, FStar_Pervasives_Native.Some cond)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Csneg (rd, rn, rm, cond))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movz", dst::(Valerie_Compiler_AST.EConst
     imm)::(Valerie_Compiler_AST.EConst shift)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             ((imm >= Prims.int_zero) &&
                (imm <= (Prims.parse_int "18446744073709551615")))
               && (shift >= Prims.int_zero)
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Movz
                  (rd, (FStar_UInt64.uint_to_t imm), shift))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movn", dst::(Valerie_Compiler_AST.EConst
     imm)::(Valerie_Compiler_AST.EConst shift)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             ((imm >= Prims.int_zero) &&
                (imm <= (Prims.parse_int "18446744073709551615")))
               && (shift >= Prims.int_zero)
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Movn
                  (rd, (FStar_UInt64.uint_to_t imm), shift))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Movk", dst::(Valerie_Compiler_AST.EConst
     imm)::(Valerie_Compiler_AST.EConst shift)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           if
             ((imm >= Prims.int_zero) &&
                (imm <= (Prims.parse_int "18446744073709551615")))
               && (shift >= Prims.int_zero)
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Movk
                  (rd, (FStar_UInt64.uint_to_t imm), shift))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Adrp", dst::(Valerie_Compiler_AST.EVar lbl)::[]) ->
      (match reg_of_expr dst with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Adrp (rd, lbl))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldr", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldr
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldr", dst::buf::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldr
                (rd, (Valerie_AArch64_Machine.MReg rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrb", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldrb
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrh", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldrh
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrsb", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldrsb
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrsh", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldrsh
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldrsw", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldrsw
                (rd, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", buf::(Valerie_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Str
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", src::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Str
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Str", src::buf::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Str
                (rs, (Valerie_AArch64_Machine.MReg rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strb", buf::(Valerie_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Strb
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strb", src::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Strb
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strh", buf::(Valerie_Compiler_AST.EConst offset)::src::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Strh
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Strh", src::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Strh
                (rs, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ldp", d1::d2::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr d1), (reg_of_expr d2), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rd1, FStar_Pervasives_Native.Some rd2,
          FStar_Pervasives_Native.Some rb) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ldp
                (rd1, rd2, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Stp", s1::s2::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((reg_of_expr s1), (reg_of_expr s2), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some rs1, FStar_Pervasives_Native.Some rs2,
          FStar_Pervasives_Native.Some rb) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Stp
                (rs1, rs2, (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("B", (Valerie_Compiler_AST.EVar lbl)::[]) ->
      FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.B lbl)
  | ("BCond", c::(Valerie_Compiler_AST.EVar lbl)::[]) ->
      (match cond_of_expr c with
       | FStar_Pervasives_Native.Some cond ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.BCond (cond, lbl))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Blr", r::[]) ->
      (match reg_of_expr r with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Blr rd)
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ret", []) -> FStar_Pervasives_Native.Some Valerie_AArch64_Semantics.Ret
  | ("Cbz", r::(Valerie_Compiler_AST.EVar lbl)::[]) ->
      (match reg_of_expr r with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cbz (rd, lbl))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Cbnz", r::(Valerie_Compiler_AST.EVar lbl)::[]) ->
      (match reg_of_expr r with
       | FStar_Pervasives_Native.Some rd ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Cbnz (rd, lbl))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Fadd", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Fadd (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Fsub", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Fsub (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Fmul", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Fmul (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Fdiv", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Fdiv (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Smov", dst::src::(Valerie_Compiler_AST.EConst lane)::[]) ->
      (match ((reg_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some rd, FStar_Pervasives_Native.Some vs)
           ->
           if (lane >= Prims.int_zero) && (lane < (Prims.of_int 2))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Smov (rd, vs, lane))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vadd", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vadd
                (vd, v1, v2, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vsub", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vsub
                (vd, v1, v2, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vmul", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vmul
                (vd, v1, v2, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vand", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vand (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vorr", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vorr (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vrev64", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vrev64
                (vd, vs, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vrev32", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vrev32
                (vd, vs, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Vrev16", dst::src::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr src)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some vs)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Vrev16
                (vd, vs, Valerie_AArch64_Machine.B16))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Eor3", dst::s1::s2::s3::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2),
               (vec_of_expr s3))
       with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2, FStar_Pervasives_Native.Some v3)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Eor3 (vd, v1, v2, v3))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Rax1", dst::s1::s2::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Rax1 (vd, v1, v2))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Xar", dst::s1::s2::(Valerie_Compiler_AST.EConst imm)::[]) ->
      (match ((vec_of_expr dst), (vec_of_expr s1), (vec_of_expr s2)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some v1,
          FStar_Pervasives_Native.Some v2) ->
           if (imm >= Prims.int_zero) && (imm <= (Prims.of_int 63))
           then
             FStar_Pervasives_Native.Some
               (Valerie_AArch64_Semantics.Xar (vd, v1, v2, imm))
           else FStar_Pervasives_Native.None
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ld1", dst::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ld1
                (vd, Valerie_AArch64_Machine.B16,
                  (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("Ld1", dst::buf::[]) ->
      (match ((vec_of_expr dst), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some vd, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ld1
                (vd, Valerie_AArch64_Machine.B16,
                  (Valerie_AArch64_Machine.MReg rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("St1", src::buf::(Valerie_Compiler_AST.EConst offset)::[]) ->
      (match ((vec_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some vs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.St1
                (vs, Valerie_AArch64_Machine.B16,
                  (Valerie_AArch64_Machine.MOffset (rb, offset))))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("St1", src::buf::[]) ->
      (match ((vec_of_expr src), (reg_of_expr buf)) with
       | (FStar_Pervasives_Native.Some vs, FStar_Pervasives_Native.Some rb)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.St1
                (vs, Valerie_AArch64_Machine.B16,
                  (Valerie_AArch64_Machine.MReg rb)))
       | uu___ -> FStar_Pervasives_Native.None)
  | ("MsrDit", (Valerie_Compiler_AST.EConst n)::[]) ->
      FStar_Pervasives_Native.Some
        (Valerie_AArch64_Semantics.MsrDit (n <> Prims.int_zero))
  | ("MsrDit", (Valerie_Compiler_AST.EVar s)::[]) ->
      FStar_Pervasives_Native.Some
        (Valerie_AArch64_Semantics.MsrDit
           (((s = "1") || (s = "true")) || (s = "True")))
  | uu___ -> FStar_Pervasives_Native.None
let rec lower_stmt (s : Valerie_Compiler_AST.stmt) :
  Valerie_AArch64_Semantics.code FStar_Pervasives_Native.option=
  match s with
  | Valerie_Compiler_AST.SIns (op, args) ->
      (match lower_ins op args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Ins i)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Valerie_Compiler_AST.SBlock stmts -> lower_stmts stmts
  | Valerie_Compiler_AST.SCall (target, args) ->
      (match lower_call target args with
       | FStar_Pervasives_Native.Some i ->
           FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Ins i)
       | FStar_Pervasives_Native.None ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Ins
                (Valerie_AArch64_Semantics.Bl target)))
  | Valerie_Compiler_AST.SAssert uu___ ->
      FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Block [])
  | Valerie_Compiler_AST.SAssume uu___ ->
      FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Block [])
  | uu___ -> FStar_Pervasives_Native.None
and lower_stmts (stmts : Valerie_Compiler_AST.stmt Prims.list) :
  Valerie_AArch64_Semantics.code FStar_Pervasives_Native.option=
  match stmts with
  | [] -> FStar_Pervasives_Native.Some (Valerie_AArch64_Semantics.Block [])
  | s::[] -> lower_stmt s
  | s::rest ->
      (match ((lower_stmt s), (lower_stmts rest)) with
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some
          (Valerie_AArch64_Semantics.Block cs)) ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Block (c1 :: cs))
       | (FStar_Pervasives_Native.Some c1, FStar_Pervasives_Native.Some c2)
           ->
           FStar_Pervasives_Native.Some
             (Valerie_AArch64_Semantics.Block [c1; c2])
       | uu___ -> FStar_Pervasives_Native.None)
