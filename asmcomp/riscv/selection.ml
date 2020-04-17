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

(* Instruction selection *)

class selector = object (self)

inherit Selectgen.selector_generic as super

val rvconfig = mk_config !Clflags.riscv_arch 

method is_immediate n = is_immediate n

method select_addressing _ = function
  | Cop(Cadda, [arg; Cconst_int n], _) when self#is_immediate n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n], _)], dbg) when self#is_immediate n ->
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
  (* RISC-V Custom OCaml Extensions *)
  | (Caddi, [Cop(Caddi, [arg1; arg2], _); Const_pointer -1]) when rvconfig.arith -> 
    (Ispecific Iocadd, [arg1; arg2])
  | (Caddi, [Cop(Csubi, [arg1; arg2], _); Const_pointer 1]) when rvconfig.arith -> 
    (Ispecific Iocsub, [arg1; arg2])
  | _ ->
      super#select_operation op args dbg

(* Instruction selection for conditionals *)

method! select_condition = function
  | Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Emitting Custom Instructions *)

method! emit_tail (env:Selectgen.environment) exp = 
  match exp with 
  | (Cifthenelse (Cop(Ccmpi Cne, [Cvar ident; Cconst_int 1], debug), Cconst_pointer 1, Cconst_pointer 3)) -> 
    if rvconfig.iszero then (
      let (_cond, earg) = self#select_condition (Cop(Ccmpi Cne, [Cvar ident; Cconst_int 1], debug)) in
      match self#emit_expr env earg with 
        | None -> () 
        | Some rarg -> 
          let ret = self#regs_for typ_int in 
          let r = self#insert_op (Ispecific(Ioceq)) rarg ret in 
          let r = self#insert_op (Ispecific(Iocval)) r ret in
          let loc = Proc.loc_results r in
            self#insert_moves r loc;
            self#insert Ireturn loc [||]
    ) else (
      super#emit_tail env exp
    )
  | _ -> super#emit_tail env exp
end

let fundecl f = (new selector)#emit_fundecl f
