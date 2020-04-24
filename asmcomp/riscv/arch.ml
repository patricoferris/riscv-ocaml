(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*               Nicolas Ojeda Bar <n.oje.bar@gmail.com>               *)
(*                                                                     *)
(*  Copyright 2016 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Specific operations for the RISC-V processor *)

open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Specific operations *)

type specific_operation =
  | Imultaddf of bool                   (* multiply, optionally negate, and add *)
  | Imultsubf of bool                   (* multiply, optionally negate, and subtract *)
  | Ioceq of int                        (* customisation for checking if something is equal to immediate *)
  | Iocval of int                       (* shift logical left 1 and add immediate (1 for OCaml value) *)
  | Iocadd                              (* OCaml integer adding - automatically subtracts 1 *)
  | Iocsub                              (* OCaml integer subtraction - automatically adds 1 *)
  | Ioclea                              (* A load-effective address for OCaml *)

let spacetime_node_hole_pointer_is_live_before = function
  | Imultaddf _ | Imultsubf _ | Ioceq _ | Iocval _ | Iocadd | Iocsub | Ioclea -> false

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)

let is_immediate n =
  (n <= 2047) && (n >= -2048)

(* Sizes, endianness *)

let big_endian = false

let rv64 =
  match Config.model with "riscv64" -> true | "riscv32" -> false | _ -> assert false

let size_addr = if rv64 then 8 else 4
let size_int = size_addr
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
  | Iindexed _ -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Imultaddf false ->
      fprintf ppf "%a *f %a +f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultaddf true ->
      fprintf ppf "-f (%a *f %a +f %a)"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf false ->
      fprintf ppf "%a *f %a -f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf true ->
      fprintf ppf "-f (%a *f %a -f %a)"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Iocadd -> 
      fprintf ppf "%a + %a" printreg arg.(0) printreg arg.(1)
  | Iocsub -> 
      fprintf ppf "%a - %a" printreg arg.(0) printreg arg.(1)
  | Ioceq n -> fprintf ppf "%a = %i"
        printreg arg.(0) n 
  | Iocval n -> fprintf ppf "%a << 1 + %i" 
        printreg arg.(0) n
  | Ioclea -> fprintf ppf "%a << 2 + %a" printreg arg.(0) printreg arg.(1)
  
(* Configuring Extensions *)
type rvconfig = { iszero : bool; arith : bool; bitmanip : bool; jtbl : bool; shiftadd : bool }
let empty_config = {iszero = false; arith = false; bitmanip = false; jtbl = false; shiftadd = false}
let full_config = {iszero = true; arith = true; bitmanip = true; jtbl = true; shiftadd = true}

let mk_config = function 
  | None -> full_config 
  | Some s -> if String.equal s "none" then empty_config else 
    let matching conf = function 
      | 'z' -> {conf with iszero = true}
      | 'a' -> {conf with arith = true}
      | 'b' -> {conf with bitmanip = true}
      | 'j' -> {conf with jtbl = true}
      | 's' -> {conf with shiftadd = true}
      |  _  -> conf
    in 
      Seq.fold_left matching empty_config (String.to_seq s)
  
