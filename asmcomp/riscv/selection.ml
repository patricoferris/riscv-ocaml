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
open Clflags

(* Instruction selection *)

class selector = object (self)

inherit Selectgen.selector_generic as super

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

method! emit_expr (env:Selectgen.environment) exp = 
  match exp with 
  | (Cifthenelse (Cop(Ccmpi Cne, [a; Cconst_int 1], debug), Cconst_pointer ifso, Cconst_pointer ifnot)) -> 
    if (ifso = 1) && (ifnot = 3) then (
      let (_, earg) = self#select_condition (Cop(Ccmpi Cne, [a; Cconst_int 1], debug)) in
      begin match self#emit_expr env earg with
          None -> None
        | Some rarg ->
            let rd = self#regs_for typ_int in
            let op = (Ispecific (Icamlisint)) in 
            Some (super#insert_op_debug op debug rarg rd)
        end
    ) else (
      super#emit_expr env exp
    )
  | _ -> super#emit_expr env exp

method! emit_tail (env:Selectgen.environment) exp = 
  match exp with 
  | (Cifthenelse (Cop(Ccmpi Cne, [a; Cconst_int 1], debug), Cconst_pointer ifso, Cconst_pointer ifnot)) -> 
    if (ifso = 1) && (ifnot = 3) && (!riscv_arch <> None) then (
      self#emit_return env (Cifthenelse (Cop(Ccmpi Cne, [a; Cconst_int 1], debug), Cconst_pointer ifso, Cconst_pointer ifnot))    
    ) else (
      super#emit_tail env exp
    )
  | _ -> super#emit_tail env exp

method private emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

method private emit_return (env:Selectgen.environment) exp =
  match self#emit_expr env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results r in
      super#insert_moves r loc;
      super#insert Ireturn loc [||]
end

let fundecl f = (new selector)#emit_fundecl f
