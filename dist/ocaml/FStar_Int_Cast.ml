open Prims

let uint32_to_uint64 (x : FStar_UInt32.t) : FStar_UInt64.t =
  FStar_UInt64.uint_to_t (FStar_UInt32.v x)

let uint64_to_uint32 (x : FStar_UInt64.t) : FStar_UInt32.t =
  FStar_UInt32.uint_to_t (Z.rem (FStar_UInt64.v x) (Prims.pow2 (Prims.of_int 32)))
