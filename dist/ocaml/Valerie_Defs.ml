open Prims
type nat8 = FStar_UInt8.t
type nat16 = FStar_UInt16.t
type nat32 = FStar_UInt32.t
type nat64 = FStar_UInt64.t
type nat128 = FStar_UInt128.t
type 'a two = {
  lo: 'a ;
  hi: 'a }
let __proj__Mktwo__item__lo (projectee : 'a two) : 'a=
  match projectee with | { lo; hi;_} -> lo
let __proj__Mktwo__item__hi (projectee : 'a two) : 'a=
  match projectee with | { lo; hi;_} -> hi
type 'a four = {
  v0: 'a ;
  v1: 'a ;
  v2: 'a ;
  v3: 'a }
let __proj__Mkfour__item__v0 (projectee : 'a four) : 'a=
  match projectee with | { v0; v1; v2; v3;_} -> v0
let __proj__Mkfour__item__v1 (projectee : 'a four) : 'a=
  match projectee with | { v0; v1; v2; v3;_} -> v1
let __proj__Mkfour__item__v2 (projectee : 'a four) : 'a=
  match projectee with | { v0; v1; v2; v3;_} -> v2
let __proj__Mkfour__item__v3 (projectee : 'a four) : 'a=
  match projectee with | { v0; v1; v2; v3;_} -> v3
type double32 = nat32 two
type quad32 = nat32 four
type double64 = nat64 two
let nat32_and (a : nat32) (b : nat32) : nat32= FStar_UInt32.logand a b
let nat32_or (a : nat32) (b : nat32) : nat32= FStar_UInt32.logor a b
let nat32_xor (a : nat32) (b : nat32) : nat32= FStar_UInt32.logxor a b
let nat32_not (a : nat32) : nat32= FStar_UInt32.lognot a
let nat64_and (a : nat64) (b : nat64) : nat64= FStar_UInt64.logand a b
let nat64_or (a : nat64) (b : nat64) : nat64= FStar_UInt64.logor a b
let nat64_xor (a : nat64) (b : nat64) : nat64= FStar_UInt64.logxor a b
let nat64_not (a : nat64) : nat64= FStar_UInt64.lognot a
let nat128_and (a : nat128) (b : nat128) : nat128= FStar_UInt128.logand a b
let nat128_or (a : nat128) (b : nat128) : nat128= FStar_UInt128.logor a b
let nat128_xor (a : nat128) (b : nat128) : nat128= FStar_UInt128.logxor a b
let quad32_xor (x : quad32) (y : quad32) : quad32=
  {
    v0 = (nat32_xor x.v0 y.v0);
    v1 = (nat32_xor x.v1 y.v1);
    v2 = (nat32_xor x.v2 y.v2);
    v3 = (nat32_xor x.v3 y.v3)
  }
let pack_nat128 (lo : nat64) (hi : nat64) : nat128=
  FStar_UInt128.logor
    (FStar_UInt128.shift_left (FStar_UInt128.uint64_to_uint128 hi)
       (Stdint.Uint32.of_int 64)) (FStar_UInt128.uint64_to_uint128 lo)
let unpack_nat128 (x : nat128) : double64=
  {
    lo = (FStar_UInt128.uint128_to_uint64 x);
    hi =
      (FStar_UInt128.uint128_to_uint64
         (FStar_UInt128.shift_right x (Stdint.Uint32.of_int 64)))
  }
let rec clmul64_step (a : nat64) (b : nat64) (i : Prims.nat) (acc : nat128) :
  nat128=
  if i = (Prims.of_int 64)
  then acc
  else
    (let bit_i =
       FStar_UInt64.logand
         (FStar_UInt64.shift_right a (FStar_UInt32.uint_to_t i))
         Stdint.Uint64.one in
     let term =
       if (FStar_UInt64.v bit_i) = Prims.int_one
       then
         FStar_UInt128.shift_left (FStar_UInt128.uint64_to_uint128 b)
           (FStar_UInt32.uint_to_t i)
       else FStar_UInt128.uint_to_t Prims.int_zero in
     clmul64_step a b (i + Prims.int_one) (FStar_UInt128.logxor acc term))
let poly_mul64 (a : nat64) (b : nat64) : nat128=
  clmul64_step a b Prims.int_zero (FStar_UInt128.uint_to_t Prims.int_zero)
