open Prims

type t = {
  low: FStar_UInt64.t;
  high: FStar_UInt64.t;
}

let uint_to_t (x : Prims.int) : t =
  let p64 = Prims.pow2 (Prims.of_int 64) in
  {
    low = FStar_UInt64.uint_to_t (Z.rem x p64);
    high = FStar_UInt64.uint_to_t (Z.div x p64);
  }

let uint64_to_uint128 (a : FStar_UInt64.t) : t =
  { low = a; high = FStar_UInt64.uint_to_t Prims.int_zero }

let uint128_to_uint64 (a : t) : FStar_UInt64.t = a.low

let logand (a : t) (b : t) : t =
  {
    low = FStar_UInt64.logand a.low b.low;
    high = FStar_UInt64.logand a.high b.high;
  }

let logxor (a : t) (b : t) : t =
  {
    low = FStar_UInt64.logxor a.low b.low;
    high = FStar_UInt64.logxor a.high b.high;
  }

let logor (a : t) (b : t) : t =
  {
    low = FStar_UInt64.logor a.low b.low;
    high = FStar_UInt64.logor a.high b.high;
  }

let shift_left (a : t) (s : FStar_UInt32.t) : t =
  let s_int = Stdint.Uint32.to_int s in
  if Stdlib.(=) s_int 0 then a
  else if Stdlib.(<) s_int 64 then
    let s32 = s in
    let diff32 = Stdint.Uint32.of_int (Stdlib.(-) 64 s_int) in
    {
      low = FStar_UInt64.shift_left a.low s32;
      high = FStar_UInt64.logor
               (FStar_UInt64.shift_left a.high s32)
               (FStar_UInt64.shift_right a.low diff32);
    }
  else if Stdlib.(<) s_int 128 then
    let s_sub = Stdint.Uint32.of_int (Stdlib.(-) s_int 64) in
    {
      low = FStar_UInt64.uint_to_t Prims.int_zero;
      high = FStar_UInt64.shift_left a.low s_sub;
    }
  else
    {
      low = FStar_UInt64.uint_to_t Prims.int_zero;
      high = FStar_UInt64.uint_to_t Prims.int_zero;
    }

let shift_right (a : t) (s : FStar_UInt32.t) : t =
  let s_int = Stdint.Uint32.to_int s in
  if Stdlib.(=) s_int 0 then a
  else if Stdlib.(<) s_int 64 then
    let s32 = s in
    let diff32 = Stdint.Uint32.of_int (Stdlib.(-) 64 s_int) in
    {
      low = FStar_UInt64.logor
              (FStar_UInt64.shift_right a.low s32)
              (FStar_UInt64.shift_left a.high diff32);
      high = FStar_UInt64.shift_right a.high s32;
    }
  else if Stdlib.(<) s_int 128 then
    let s_sub = Stdint.Uint32.of_int (Stdlib.(-) s_int 64) in
    {
      low = FStar_UInt64.shift_right a.high s_sub;
      high = FStar_UInt64.uint_to_t Prims.int_zero;
    }
  else
    {
      low = FStar_UInt64.uint_to_t Prims.int_zero;
      high = FStar_UInt64.uint_to_t Prims.int_zero;
    }
