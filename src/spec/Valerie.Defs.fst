module Valerie.Defs

open FStar.UInt8
open FStar.UInt16
open FStar.UInt32
open FStar.UInt64
open FStar.UInt128

module U8 = FStar.UInt8
module U16 = FStar.UInt16
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module U128 = FStar.UInt128

/// Fixed-width integer types replacing legacy Vale's unbounded nat definitions
type nat8 = U8.t
type nat16 = U16.t
type nat32 = U32.t
type nat64 = U64.t
type nat128 = U128.t

/// Multi-word compound structures (two 32-bit words, four 32-bit words)
type two (a: Type0) = { lo: a; hi: a }
type four (a: Type0) = { v0: a; v1: a; v2: a; v3: a }

type double32 = two nat32
type quad32 = four nat32
type double64 = two nat64

/// Pure bitwise operations without legacy Vale axioms
let nat32_and (a b: nat32) : nat32 = U32.logand a b
let nat32_or  (a b: nat32) : nat32 = U32.logor a b
let nat32_xor (a b: nat32) : nat32 = U32.logxor a b
let nat32_not (a: nat32)   : nat32 = U32.lognot a

let nat64_and (a b: nat64) : nat64 = U64.logand a b
let nat64_or  (a b: nat64) : nat64 = U64.logor a b
let nat64_xor (a b: nat64) : nat64 = U64.logxor a b
let nat64_not (a: nat64)   : nat64 = U64.lognot a

let nat128_and (a b: nat128) : nat128 = U128.logand a b
let nat128_or  (a b: nat128) : nat128 = U128.logor a b
let nat128_xor (a b: nat128) : nat128 = U128.logxor a b

/// Quadword mapping and bitwise logic
let quad32_xor (x y: quad32) : quad32 =
  { v0 = nat32_xor x.v0 y.v0;
    v1 = nat32_xor x.v1 y.v1;
    v2 = nat32_xor x.v2 y.v2;
    v3 = nat32_xor x.v3 y.v3; }

/// Pack two 64-bit words into a 128-bit word
let pack_nat128 (lo hi: nat64) : nat128 =
  U128.logor (U128.shift_left (U128.uint64_to_uint128 hi) 64ul)
             (U128.uint64_to_uint128 lo)

/// Unpack 128-bit word into two 64-bit halves
let unpack_nat128 (x: nat128) : double64 =
  { lo = U128.uint128_to_uint64 x;
    hi = U128.uint128_to_uint64 (U128.shift_right x 64ul) }

/// Pure 64x64 carryless bitvector multiplication (PMULL / PCLMULQDQ)
let rec clmul64_step (a b: nat64) (i: nat{i <= 64}) (acc: nat128) : Tot nat128 (decreases (64 - i)) =
  if i = 64 then acc
  else
    let bit_i = U64.logand (U64.shift_right a (UInt32.uint_to_t i)) 1uL in
    let term =
      if U64.v bit_i = 1 then
        U128.shift_left (U128.uint64_to_uint128 b) (UInt32.uint_to_t i)
      else
        U128.uint_to_t 0 in
    clmul64_step a b (i + 1) (U128.logxor acc term)

let poly_mul64 (a b: nat64) : nat128 =
  clmul64_step a b 0 (U128.uint_to_t 0)

