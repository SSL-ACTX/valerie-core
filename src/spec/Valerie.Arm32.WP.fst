module Valerie.Arm32.WP

open Valerie.Defs
open Valerie.Arm32.Machine
open Valerie.Arm32.Semantics

type post_t (a: Type0) = state -> a -> prop
type wp_t (a: Type0) = state -> post_t a -> prop

unfold let wp_return (#a: Type0) (v: a) : wp_t a =
  fun s0 k -> k s0 v

unfold let wp_bind (#a #b: Type0) (m: wp_t a) (f: a -> wp_t b) : wp_t b =
  fun s0 k -> m s0 (fun s1 v -> f v s1 k)

unfold let wp_ins (i: ins) : wp_t unit =
  fun s0 k -> k (eval_ins i s0) ()

unfold let wp_seq (m1 m2: wp_t unit) : wp_t unit =
  wp_bind m1 (fun () -> m2)

unfold let wp_if_else (cond: ocmp) (m_true m_false: wp_t unit) : wp_t unit =
  fun s0 k ->
    if eval_ocmp cond s0 then m_true s0 k
    else m_false s0 k

let rec wp_block (l: list ins) : wp_t unit =
  match l with
  | [] -> wp_return ()
  | hd::tl -> wp_seq (wp_ins hd) (wp_block tl)

let lemma_wp_ins_sound (i: ins) (s0: state) (k: post_t unit)
  (h: wp_ins i s0 k)
  : Lemma (ensures (let s1 = eval_ins i s0 in k s1 ())) =
  ()

let lemma_wp_seq_sound (m1 m2: wp_t unit) (s0: state) (k: post_t unit)
  (h: wp_seq m1 m2 s0 k)
  : Lemma (ensures (m1 s0 (fun s1 () -> m2 s1 k))) =
  ()

unfold let wp_while
  (cond: ocmp)
  (inv: state -> prop)
  (body: wp_t unit)
  : wp_t unit =
  fun s0 k ->
    inv s0 /\
    (forall (s: state). inv s /\ eval_ocmp cond s ==> body s (fun s' () -> inv s')) /\
    (forall (s: state). inv s /\ not (eval_ocmp cond s) ==> k s ())

let lemma_wp_while_exit (cond: ocmp) (inv: state -> prop) (body: wp_t unit) (s0: state) (k: post_t unit)
  (h: wp_while cond inv body s0 k)
  : Lemma (requires not (eval_ocmp cond s0))
          (ensures k s0 ()) =
  ()
