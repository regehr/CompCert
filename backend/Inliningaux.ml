(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(* To be considered: heuristics based on size of function? *)

(*
let should_inline (id: AST.ident) (f: RTL.coq_function) =
    C2C.atom_is_inline id
 *)

open AST
open RTL
open Maps

let insn_cost t = match t with
    (_, Inop s) -> 0
  | (_, _) -> 1

let rec list_cost e =
  match e with
    [] -> 0
  | x :: xs -> (insn_cost x) + (list_cost xs)

let should_inline (id: AST.ident) (f: RTL.coq_function) =
  if f.fn_sig.sig_cc.cc_vararg
  then
    false
  else
    let cost = list_cost (PTree.elements f.fn_code) in
    cost < 100

