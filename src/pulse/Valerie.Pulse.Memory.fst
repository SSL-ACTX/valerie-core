module Valerie.Pulse.Memory
#lang-pulse

open Pulse.Lib.Pervasives
open Pulse.Lib.Array
open Pulse.Lib.Array.PtsToRange
open PulseCore.FractionalPermission
open FStar.UInt8
open FStar.UInt32
open FStar.UInt64
module U8 = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module SZ = FStar.SizeT
module Seq = FStar.Seq

/// Advanced Separation Logic Memory & Buffer System for Modern Vale
///
/// Implements:
/// Fractional Permissions (`#p: perm`): Read-only sharing vs exclusive writes.
/// Range Slicing (`pts_to_range`): Zero-overhead buffer partitioning and re-joining.
/// Stack Frame Linear Lifecycle: Memory safety and non-interference for stack allocations.

type byte_array = array U8.t
type word32_array = array U32.t
type word64_array = array U64.t

/// Read a single byte with fractional permission (allows concurrent shared reads)
fn pulse_read_byte_shared
  (buf: byte_array)
  (idx: SZ.t)
  (#p: perm)
  (#s: Ghost.erased (Seq.seq U8.t){SZ.v idx < Seq.length s})
  preserves pts_to buf #p s
  returns v: U8.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  let v = buf.(idx);
  v
}

/// Read a single byte with full exclusive ownership
fn pulse_read_byte
  (buf: byte_array)
  (idx: SZ.t)
  (#s: Ghost.erased (Seq.seq U8.t){SZ.v idx < Seq.length s})
  preserves pts_to buf s
  returns v: U8.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  pulse_read_byte_shared buf idx
}

/// Write a single byte (requires exclusive permission)
fn pulse_write_byte
  (buf: byte_array)
  (idx: SZ.t)
  (v: U8.t)
  (#s: Ghost.erased (Seq.seq U8.t){SZ.v idx < Seq.length s})
  requires pts_to buf s
  returns _: unit
  ensures exists* s'. pts_to buf s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v idx) v)
{
  buf.(idx) <- v;
  Seq.lemma_len_upd (SZ.v idx) v s;
  ()
}

/// Read a 32-bit word with fractional permission
fn pulse_read_u32_shared
  (buf: word32_array)
  (idx: SZ.t)
  (#p: perm)
  (#s: Ghost.erased (Seq.seq U32.t){SZ.v idx < Seq.length s})
  preserves pts_to buf #p s
  returns v: U32.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  let v = buf.(idx);
  v
}

fn pulse_read_u32
  (buf: word32_array)
  (idx: SZ.t)
  (#s: Ghost.erased (Seq.seq U32.t){SZ.v idx < Seq.length s})
  preserves pts_to buf s
  returns v: U32.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  pulse_read_u32_shared buf idx
}

/// Write a 32-bit word
fn pulse_write_u32
  (buf: word32_array)
  (idx: SZ.t)
  (v: U32.t)
  (#s: Ghost.erased (Seq.seq U32.t){SZ.v idx < Seq.length s})
  requires pts_to buf s
  returns _: unit
  ensures exists* s'. pts_to buf s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v idx) v)
{
  buf.(idx) <- v;
  Seq.lemma_len_upd (SZ.v idx) v s;
  ()
}

/// Read a 64-bit word with fractional permission
fn pulse_read_u64_shared
  (buf: word64_array)
  (idx: SZ.t)
  (#p: perm)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v idx < Seq.length s})
  preserves pts_to buf #p s
  returns v: U64.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  let v = buf.(idx);
  v
}

fn pulse_read_u64
  (buf: word64_array)
  (idx: SZ.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v idx < Seq.length s})
  preserves pts_to buf s
  returns v: U64.t
  ensures pure (v == Seq.index s (SZ.v idx))
{
  pulse_read_u64_shared buf idx
}

/// Write a 64-bit word
fn pulse_write_u64
  (buf: word64_array)
  (idx: SZ.t)
  (v: U64.t)
  (#s: Ghost.erased (Seq.seq U64.t){SZ.v idx < Seq.length s})
  requires pts_to buf s
  returns _: unit
  ensures exists* s'. pts_to buf s' ** pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v idx) v)
{
  buf.(idx) <- v;
  Seq.lemma_len_upd (SZ.v idx) v s;
  ()
}

/// Share an exclusive buffer permission into two equal fractional halves (for read sharing)
ghost fn pulse_share_buffer
  (#a: Type0)
  (buf: array a)
  (#p: perm)
  (#s: Seq.seq a)
  requires pts_to buf #p s
  ensures
    pts_to buf #(p /. 2.0R) s **
    pts_to buf #(p /. 2.0R) s
{
  Pulse.Lib.Array.share buf;
  ()
}

/// Gather two equal fractional permissions back into a single permission
ghost fn pulse_gather_buffer
  (#a: Type0)
  (buf: array a)
  (#p: perm)
  (#s1 #s2: Seq.seq a)
  requires
    pts_to buf #p s1 **
    pts_to buf #p s2
  ensures
    pts_to buf #(p *. 2.0R) s1 **
    pure (s1 == s2)
{
  Pulse.Lib.Array.gather buf;
  ()
}

/// Partition an array into a sub-slice range [i, j)
ghost fn pulse_range_intro
  (#a: Type0)
  (buf: array a)
  (#p: perm)
  (#s: Seq.seq a)
  requires pts_to buf #p s
  ensures  pts_to_range buf 0 (length buf) #p s
{
  pts_to_range_intro buf p s;
  ()
}

/// Recombine a full sub-slice range [0, length buf) back into a standard array permission
ghost fn pulse_range_elim
  (#a: Type0)
  (buf: array a)
  (#p: perm)
  (#s: Seq.seq a)
  requires pts_to_range buf 0 (length buf) #p s
  ensures  pts_to buf #p s
{
  pts_to_range_elim buf p s;
  ()
}

/// Zero-cost buffer split: splits [i, j) at index m without copying memory
ghost fn pulse_range_split
  (#a: Type0)
  (buf: array a)
  (i m j: nat)
  (#p: perm)
  (#s: Seq.seq a)
  requires
    pts_to_range buf i j #p s **
    pure (i <= m /\ m <= j)
  ensures exists* s1 s2.
    pts_to_range buf i m #p s1 **
    pts_to_range buf m j #p s2 **
    pure (s == Seq.append s1 s2)
{
  pts_to_range_split buf i m j;
  ()
}

/// Zero-cost buffer join: joins [i, m) and [m, j) back into [i, j)
ghost fn pulse_range_join
  (#a: Type0)
  (buf: array a)
  (i m j: nat)
  (#p: perm)
  (#s1 #s2: Seq.seq a)
  requires
    pts_to_range buf i m #p s1 **
    pts_to_range buf m j #p s2
  ensures
    pts_to_range buf i j #p (Seq.append s1 s2)
{
  pts_to_range_join buf i m j;
  ()
}

/// Read directly from a sub-slice range
fn pulse_range_read_u64
  (buf: word64_array)
  (idx: SZ.t)
  (#l: Ghost.erased nat{l <= SZ.v idx})
  (#r: Ghost.erased nat{SZ.v idx < r})
  (#p: perm)
  (#s: Ghost.erased (Seq.seq U64.t){Seq.length s == r - l})
  preserves pts_to_range buf l r #p s
  returns v: U64.t
  ensures pure (v == Seq.index s (SZ.v idx - l))
{
  let v = pts_to_range_index buf idx;
  v
}

/// Write directly to a sub-slice range
fn pulse_range_write_u64
  (buf: word64_array)
  (idx: SZ.t)
  (v: U64.t)
  (#l: Ghost.erased nat{l <= SZ.v idx})
  (#r: Ghost.erased nat{SZ.v idx < r})
  (#s: Ghost.erased (Seq.seq U64.t){Seq.length s == r - l})
  requires pts_to_range buf l r s
  returns _: unit
  ensures exists* s'.
    pts_to_range buf l r s' **
    pure (Seq.length s' == Seq.length s /\ s' == Seq.upd s (SZ.v idx - l) v)
{
  pts_to_range_upd buf idx v;
  ()
}

/// Cross-Buffer Transfer with Fractional Input (Read-only source)
fn pulse_disjoint_transfer64
  (src: word64_array)
  (dst: word64_array)
  (src_idx: SZ.t)
  (dst_idx: SZ.t)
  (#p_src: perm)
  (#s_src: Ghost.erased (Seq.seq U64.t){SZ.v src_idx < Seq.length s_src})
  (#s_dst: Ghost.erased (Seq.seq U64.t){SZ.v dst_idx < Seq.length s_dst})
  requires
    pts_to src #p_src s_src **
    pts_to dst s_dst
  returns _: unit
  ensures exists* s_dst'.
    pts_to src #p_src s_src **
    pts_to dst s_dst' **
    pure (Seq.length s_dst' == Seq.length s_dst /\ s_dst' == Seq.upd s_dst (SZ.v dst_idx) (Seq.index s_src (SZ.v src_idx)))
{
  let v = src.(src_idx);
  dst.(dst_idx) <- v;
  Seq.lemma_len_upd (SZ.v dst_idx) v s_dst;
  ()
}

fn pulse_disjoint_transfer32
  (src: word32_array)
  (dst: word32_array)
  (src_idx: SZ.t)
  (dst_idx: SZ.t)
  (#p_src: perm)
  (#s_src: Ghost.erased (Seq.seq U32.t){SZ.v src_idx < Seq.length s_src})
  (#s_dst: Ghost.erased (Seq.seq U32.t){SZ.v dst_idx < Seq.length s_dst})
  requires
    pts_to src #p_src s_src **
    pts_to dst s_dst
  returns _: unit
  ensures exists* s_dst'.
    pts_to src #p_src s_src **
    pts_to dst s_dst' **
    pure (Seq.length s_dst' == Seq.length s_dst /\ s_dst' == Seq.upd s_dst (SZ.v dst_idx) (Seq.index s_src (SZ.v src_idx)))
{
  let v = src.(src_idx);
  dst.(dst_idx) <- v;
  Seq.lemma_len_upd (SZ.v dst_idx) v s_dst;
  ()
}
