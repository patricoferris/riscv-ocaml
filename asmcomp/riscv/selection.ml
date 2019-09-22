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

(* Instruction selection for the RISC-V processor *)

open Cmm
open Arch
open Mach

exception Use_default

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
    Iintop(Iadd|Isub) ->
      ([|res.(0); arg.(1)|], res)
  | Iintop_imm((Iadd|Isub), _) ->
      ([|res.(0)|], res)
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* Instruction selection *)

class selector = object (self)

inherit Selectgen.selector_generic as super

val mutable fastcode_flag = false

method is_immediate n = is_immediate n

method select_addressing _ = function
  | Cop(Cadda, [arg; Cconst_int (n, _)], _) when self#is_immediate n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int (n, _)], _)], dbg) when self#is_immediate n ->
      (Iindexed n, Cop(Caddi, [arg1; arg2], dbg))
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args dbg =
  match (op, args) with
  (* RISC-V does not support immediate operands for multiply high *)
  | (Cmulhi, _) -> (Iintop Imulh, args)
  (* Recognize (neg-)mult-add and (neg-)mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2], _)]) ->
      (Ispecific (Imultaddf false), [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific (Imultsubf false), [arg1; arg2; arg3])
  | (Cnegf, [Cop(Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3], _)]) ->
      (Ispecific (Imultsubf true), [arg1; arg2; arg3])
  | (Cnegf, [Cop(Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3], _)]) ->
      (Ispecific (Imultaddf true), [arg1; arg2; arg3])
  (* RISC-V does not support immediate operands for comparison operators *)
  | (Ccmpi comp, args) -> (Iintop(Icomp (Isigned comp)), args)
  | (Ccmpa comp, args) -> (Iintop(Icomp (Iunsigned comp)), args)
  | (Cmuli, _) -> (Iintop Imul, args)
  | _ ->
      super#select_operation op args dbg

(* Instruction selection for conditionals *)

method! select_condition = function
    Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int (1, _)], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

method! insert_op_debug env op dbg rs rd =
  if fastcode_flag then super#insert_op_debug env op dbg rs rd
  else begin
    match pseudoregs_for_operation op rs rd with
    | rsrs, rdst ->
        self#insert_moves env rs rsrs;
        self#insert_debug env (Iop op) dbg rsrs rdst;
        self#insert_moves env rdst rd;
        rd
    | exception Use_default ->
        super#insert_op_debug env op dbg rs rd
  end

method! emit_fundecl f =
  fastcode_flag <- not (List.mem Reduce_code_size f.fun_codegen_options);
  super#emit_fundecl f
end

let fundecl f = (new selector)#emit_fundecl f
