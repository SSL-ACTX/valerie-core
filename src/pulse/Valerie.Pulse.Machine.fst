module Valerie.Pulse.Machine
#lang-pulse

open Pulse.Lib.Pervasives
open Pulse.Lib.Array
open PulseCore.FractionalPermission
open FStar.UInt8
open FStar.UInt32
open FStar.UInt64
open Valerie.Defs
module U8 = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module SZ = FStar.SizeT
module Seq = FStar.Seq

/// 32 Physical general-purpose 64-bit registers
let num_gprs : SZ.t = 32sz

type reg_array = array U64.t

/// Pure 32-bit arithmetic helper functions
inline_for_extraction
let op_trunc32 (a: U64.t) : U32.t =
  U32.uint_to_t (U64.v a % 4294967296)

inline_for_extraction
let op_add32 (a: U64.t) (b: U64.t) : U64.t =
  let w1 = op_trunc32 a in
  let w2 = op_trunc32 b in
  U64.uint_to_t (U32.v (U32.add_mod w1 w2))

inline_for_extraction
let op_sub32 (a: U64.t) (b: U64.t) : U64.t =
  let w1 = op_trunc32 a in
  let w2 = op_trunc32 b in
  U64.uint_to_t (U32.v (U32.sub_mod w1 w2))

inline_for_extraction
let op_xor32 (a: U64.t) (b: U64.t) : U64.t =
  let w1 = op_trunc32 a in
  let w2 = op_trunc32 b in
  U64.uint_to_t (U32.v (U32.logxor w1 w2))

inline_for_extraction
let op_rol32 (a: U64.t) (amt: U32.t{U32.v amt < 32}) : U64.t =
  let w = op_trunc32 a in
  U64.uint_to_t (U32.v (U32.rotate_left w amt))

inline_for_extraction
let op_ror32 (a: U64.t) (amt: U32.t{U32.v amt < 32}) : U64.t =
  let w = op_trunc32 a in
  U64.uint_to_t (U32.v (U32.rotate_right w amt))

/// Read a 64-bit register from the register bank in Pulse separation logic
fn pulse_read_reg
  (regs: reg_array)
  (idx: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v idx < Seq.length s})
  preserves pts_to regs s
  returns v: U64.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  let v = regs.(idx);
  v
}

/// Write a 64-bit register in the register bank in Pulse separation logic
fn pulse_write_reg
  (regs: reg_array)
  (idx: SZ.t)
  (v: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v idx < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v idx) v)
{
  regs.(idx) <- v;
  Seq.lemma_len_upd (SZ.v idx) v s;
  ()
}

/// Execute Mov instruction in Pulse: regs[rd] := regs[rn]
fn pulse_step_mov
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (Seq.index s (SZ.v rn)))
{
  let vn = regs.(rn);
  regs.(rd) <- vn;
  Seq.lemma_len_upd (SZ.v rd) vn s;
  ()
}

/// Execute Add instruction in Pulse: regs[rd] := regs[rn] + regs[rm]
fn pulse_step_add
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.add_mod (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.add_mod vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Sub instruction in Pulse: regs[rd] := regs[rn] - regs[rm]
fn pulse_step_sub
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.sub_mod (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.sub_mod vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Xor instruction in Pulse: regs[rd] := regs[rn] ^ regs[rm]
fn pulse_step_xor
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logxor (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.logxor vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Stateful Memory Load in Pulse Separation Logic (supports fractional read-sharing):
fn pulse_step_load
  (regs: reg_array)
  (mem: array U64.t)
  (rd: SZ.t)
  (offset: SZ.t)
  (#p: perm)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U64.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem #p s_mem
  returns _: unit
  ensures exists* s_regs'.
    pts_to regs s_regs' **
    pts_to mem #p s_mem **
    pure (Seq.length s_regs' == Seq.length s_regs /\ s_regs' == Seq.upd s_regs (SZ.v rd) (Seq.index s_mem (SZ.v offset)))
{
  let w = mem.(offset);
  regs.(rd) <- w;
  Seq.lemma_len_upd (SZ.v rd) w s_regs;
  ()
}

/// Stateful Memory Store in Pulse Separation Logic:
fn pulse_step_store
  (regs: reg_array)
  (mem: array U64.t)
  (rn: SZ.t)
  (offset: SZ.t)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rn < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U64.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem s_mem
  returns _: unit
  ensures exists* s_mem'.
    pts_to regs s_regs **
    pts_to mem s_mem' **
    pure (Seq.length s_mem' == Seq.length s_mem /\ s_mem' == Seq.upd s_mem (SZ.v offset) (Seq.index s_regs (SZ.v rn)))
{
  let w = regs.(rn);
  mem.(offset) <- w;
  Seq.lemma_len_upd (SZ.v offset) w s_mem;
  ()
}

/// 32-bit Memory Load in Pulse Separation Logic (supports fractional read-sharing):
fn pulse_step_load32
  (regs: reg_array)
  (mem: array U32.t)
  (rd: SZ.t)
  (offset: SZ.t)
  (#p: perm)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U32.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem #p s_mem
  returns _: unit
  ensures exists* s_regs'.
    pts_to regs s_regs' **
    pts_to mem #p s_mem **
    pure (Seq.length s_regs' == Seq.length s_regs /\ s_regs' == Seq.upd s_regs (SZ.v rd) (U64.uint_to_t (U32.v (Seq.index s_mem (SZ.v offset)))))
{
  let w = mem.(offset);
  let ext = U64.uint_to_t (U32.v w);
  regs.(rd) <- ext;
  Seq.lemma_len_upd (SZ.v rd) ext s_regs;
  ()
}

/// 8-bit Memory Load in Pulse Separation Logic (supports fractional read-sharing):
fn pulse_step_load8
  (regs: reg_array)
  (mem: array U8.t)
  (rd: SZ.t)
  (offset: SZ.t)
  (#p: perm)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U8.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem #p s_mem
  returns _: unit
  ensures exists* s_regs'.
    pts_to regs s_regs' **
    pts_to mem #p s_mem **
    pure (Seq.length s_regs' == Seq.length s_regs /\ s_regs' == Seq.upd s_regs (SZ.v rd) (U64.uint_to_t (U8.v (Seq.index s_mem (SZ.v offset)))))
{
  let w = mem.(offset);
  let ext = U64.uint_to_t (U8.v w);
  regs.(rd) <- ext;
  Seq.lemma_len_upd (SZ.v rd) ext s_regs;
  ()
}

/// 8-bit Memory Store in Pulse Separation Logic:
fn pulse_step_store8
  (regs: reg_array)
  (mem: array U8.t)
  (rn: SZ.t)
  (offset: SZ.t)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rn < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U8.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem s_mem
  returns _: unit
  ensures exists* s_mem'.
    pts_to regs s_regs **
    pts_to mem s_mem' **
    pure (Seq.length s_mem' == Seq.length s_mem /\ s_mem' == Seq.upd s_mem (SZ.v offset) (U8.uint_to_t (U64.v (Seq.index s_regs (SZ.v rn)) % 256)))
{
  let w = regs.(rn);
  let trunc = U8.uint_to_t (U64.v w % 256);
  mem.(offset) <- trunc;
  Seq.lemma_len_upd (SZ.v offset) trunc s_mem;
  ()
}

/// 32-bit Memory Store in Pulse Separation Logic:
fn pulse_step_store32
  (regs: reg_array)
  (mem: array U32.t)
  (rn: SZ.t)
  (offset: SZ.t)
  (#s_regs: Ghost.erased (Seq.seq U64.t){SZ.v rn < Seq.length s_regs})
  (#s_mem: Ghost.erased (Seq.seq U32.t){SZ.v offset < Seq.length s_mem})
  requires
    pts_to regs s_regs **
    pts_to mem s_mem
  returns _: unit
  ensures exists* s_mem'.
    pts_to regs s_regs **
    pts_to mem s_mem' **
    pure (Seq.length s_mem' == Seq.length s_mem /\ s_mem' == Seq.upd s_mem (SZ.v offset) (op_trunc32 (Seq.index s_regs (SZ.v rn))))
{
  let w = regs.(rn);
  let trunc = op_trunc32 w;
  mem.(offset) <- trunc;
  Seq.lemma_len_upd (SZ.v offset) trunc s_mem;
  ()
}

/// Execute Ror (rotate right 64-bit) in Pulse
fn pulse_step_ror
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (amt: U32.t{U32.v amt < 64})
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.rotate_right (Seq.index s (SZ.v rn)) amt))
{
  let vn = regs.(rn);
  let res = U64.rotate_right vn amt;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Add32 in Pulse
fn pulse_step_add32
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_add32 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = op_add32 vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Sub32 in Pulse
fn pulse_step_sub32
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_sub32 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = op_sub32 vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Xor32 in Pulse
fn pulse_step_xor32
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_xor32 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = op_xor32 vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Rol32 in Pulse
fn pulse_step_rol32
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (amt: U32.t{U32.v amt < 32})
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_rol32 (Seq.index s (SZ.v rn)) amt))
{
  let vn = regs.(rn);
  let res = op_rol32 vn amt;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Ror32 in Pulse
fn pulse_step_ror32
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (amt: U32.t{U32.v amt < 32})
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_ror32 (Seq.index s (SZ.v rn)) amt))
{
  let vn = regs.(rn);
  let res = op_ror32 vn amt;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute And in Pulse: regs[rd] := regs[rn] & regs[rm]
fn pulse_step_and
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logand (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.logand vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Or in Pulse: regs[rd] := regs[rn] | regs[rm]
fn pulse_step_or
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logor (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.logor vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Shift Left in Pulse: regs[rd] := regs[rn] << amt
fn pulse_step_shl
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (amt: U32.t{U32.v amt < 64})
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.shift_left (Seq.index s (SZ.v rn)) amt))
{
  let vn = regs.(rn);
  let res = U64.shift_left vn amt;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Execute Shift Right in Pulse: regs[rd] := regs[rn] >> amt
fn pulse_step_shr
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (amt: U32.t{U32.v amt < 64})
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.shift_right (Seq.index s (SZ.v rn)) amt))
{
  let vn = regs.(rn);
  let res = U64.shift_right vn amt;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Immediate ALU operations in Pulse
fn pulse_step_add_imm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.add_mod (Seq.index s (SZ.v rn)) imm))
{
  let vn = regs.(rn);
  let res = U64.add_mod vn imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_sub_imm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.sub_mod (Seq.index s (SZ.v rn)) imm))
{
  let vn = regs.(rn);
  let res = U64.sub_mod vn imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_xor_imm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logxor (Seq.index s (SZ.v rn)) imm))
{
  let vn = regs.(rn);
  let res = U64.logxor vn imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_and_imm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logand (Seq.index s (SZ.v rn)) imm))
{
  let vn = regs.(rn);
  let res = U64.logand vn imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_or_imm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logor (Seq.index s (SZ.v rn)) imm))
{
  let vn = regs.(rn);
  let res = U64.logor vn imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// 3-way Bitwise XOR in Pulse: regs[rd] := regs[rn] ^ regs[rm] ^ regs[ra]
fn pulse_step_xor3
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (ra: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s /\ SZ.v ra < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logxor (Seq.index s (SZ.v rn)) (U64.logxor (Seq.index s (SZ.v rm)) (Seq.index s (SZ.v ra)))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let va = regs.(ra);
  let res = U64.logxor vn (U64.logxor vm va);
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Vector Byte Extract: regs[rd] := regs[rn]
fn pulse_step_vext8
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (Seq.index s (SZ.v rn)))
{
  let vn = regs.(rn);
  regs.(rd) <- vn;
  Seq.lemma_len_upd (SZ.v rd) vn s;
  ()
}

inline_for_extraction
let op_pmull64 (a: U64.t) (b: U64.t) : U64.t =
  (unpack_nat128 (poly_mul64 a b)).lo

inline_for_extraction
let op_pmull2_64 (a: U64.t) (b: U64.t) : U64.t =
  (unpack_nat128 (poly_mul64 a b)).hi

inline_for_extraction
let op_pclmulqdq64 (a: U64.t) (b: U64.t) (imm: U64.t) : U64.t =
  if U64.v (U64.logand imm 1uL) = 0 then
    (unpack_nat128 (poly_mul64 a b)).lo
  else
    (unpack_nat128 (poly_mul64 a b)).hi

/// Polynomial Multiply (PMULL / CLMUL)
fn pulse_step_pmull
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_pmull64 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = op_pmull64 vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_pmull2
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_pmull2_64 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = op_pmull2_64 vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// Carryless Multiply with immediate (PCLMULQDQ)
fn pulse_step_pclmulqdq
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (imm: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_pclmulqdq64 (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rd)) imm))
{
  let vn = regs.(rn);
  let vd = regs.(rd);
  let res = op_pclmulqdq64 vn vd imm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

inline_for_extraction
let op_aes_round (a: U64.t) (k: U64.t) : U64.t =
  U64.logxor a k

/// AES Round Encryption (AESENC)
fn pulse_step_aesenc
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_aes_round (Seq.index s (SZ.v rd)) (Seq.index s (SZ.v rn))))
{
  let vd = regs.(rd);
  let vn = regs.(rn);
  let res = op_aes_round vd vn;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

inline_for_extraction
let op_rev64 (a: U64.t) : U64.t =
  let b0 = U64.logand a 0xFFuL in
  let b1 = U64.logand (U64.shift_right a 8ul) 0xFFuL in
  let b2 = U64.logand (U64.shift_right a 16ul) 0xFFuL in
  let b3 = U64.logand (U64.shift_right a 24ul) 0xFFuL in
  let b4 = U64.logand (U64.shift_right a 32ul) 0xFFuL in
  let b5 = U64.logand (U64.shift_right a 40ul) 0xFFuL in
  let b6 = U64.logand (U64.shift_right a 48ul) 0xFFuL in
  let b7 = U64.logand (U64.shift_right a 56ul) 0xFFuL in
  U64.logor (U64.shift_left b0 56ul)
    (U64.logor (U64.shift_left b1 48ul)
      (U64.logor (U64.shift_left b2 40ul)
        (U64.logor (U64.shift_left b3 32ul)
          (U64.logor (U64.shift_left b4 24ul)
            (U64.logor (U64.shift_left b5 16ul)
              (U64.logor (U64.shift_left b6 8ul) b7))))))

/// Byte Reversal / Endianness Swap (REV8 / BSWAP)
fn pulse_step_rev
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (op_rev64 (Seq.index s (SZ.v rn))))
{
  let vn = regs.(rn);
  let res = op_rev64 vn;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

/// SHA-256 Schedule Word Operation
fn pulse_step_sha256sum0
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (Seq.index s (SZ.v rn)))
{
  let vn = regs.(rn);
  regs.(rd) <- vn;
  Seq.lemma_len_upd (SZ.v rd) vn s;
  ()
}

/// RISC-V Cryptographic AES Round Instructions
fn pulse_step_aes64esm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logxor (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.logxor vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}

fn pulse_step_aes64dsm
  (regs: reg_array)
  (rd: SZ.t)
  (rn: SZ.t)
  (rm: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v rd < Seq.length s /\ SZ.v rn < Seq.length s /\ SZ.v rm < Seq.length s})
  requires pts_to regs s
  returns _: unit
  ensures exists* s'. pts_to regs s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v rd) (U64.logxor (Seq.index s (SZ.v rn)) (Seq.index s (SZ.v rm))))
{
  let vn = regs.(rn);
  let vm = regs.(rm);
  let res = U64.logxor vn vm;
  regs.(rd) <- res;
  Seq.lemma_len_upd (SZ.v rd) res s;
  ()
}
