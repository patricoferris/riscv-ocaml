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
    print_endline ("CIF CIF CIF " ^ string_of_int ifso ^ string_of_int ifnot);
    if (ifso = 1) && (ifnot = 3) then (
      let (_, earg) = self#select_condition (Cop(Ccmpi Cne, [a; Cconst_int 1], debug)) in
      begin match self#emit_expr env earg with
          None -> None
        | Some rarg ->
            let _ = self#emit_tuple env ([Ctuple ([Cconst_pointer ifso; Cconst_pointer ifnot])]) in
            let rd = self#regs_for typ_int in
            let op = (Ispecific (Icamlisint)) in 
            print_endline "===CIFTHENELSE===";
            Some (super#insert_op_debug op debug rarg rd)
        end
    ) else (
      super#emit_expr env exp
    )
  | _ -> super#emit_expr env exp

method private emit_tuple_not_flattened env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match self#emit_expr env exp with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem
  in
  emit_list exp_list

method private emit_tuple env exp_list =
  Array.concat (self#emit_tuple_not_flattened env exp_list)

end

let fundecl f = (new selector)#emit_fundecl f
