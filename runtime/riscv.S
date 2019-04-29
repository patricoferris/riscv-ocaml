/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*               Nicolas Ojeda Bar <n.oje.bar@gmail.com>               */
/*                                                                     */
/*  Copyright 2017 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, RISC-V processor, 64-bit mode */
/* Must be preprocessed by cpp */

#define TRAP_PTR	s1
#define ALLOC_PTR	s10
#define ALLOC_LIMIT	s11

#define TMP	t0
#define TMP2	t1
#define ARG	t2

#if defined(MODEL_riscv64)
#define STORE	sd
#define LOAD	ld
#define WSZ	8
#else
#define STORE	sw
#define LOAD	lw
#define WSZ	4
#endif

#if defined(__PIC__)
        .option pic
#define PLT(r) r@plt
#else
        .option nopic
#define PLT(r) r
#endif

        .section        .text
/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

        .align  2
        .globl  caml_call_gc
        .type   caml_call_gc, @function
caml_call_gc:
        /* Record return address */
        STORE   ra, caml_last_return_address, TMP2
        /* Record lowest stack address */
        STORE   sp, caml_bottom_of_stack, TMP2
.Lcaml_call_gc:
        /* Set up stack space, saving return address */
        /* (1 reg for RA, 1 reg for FP, 21 allocatable int regs, 20 caller-save float regs) * 8 */
        /* + 1 for alignment */
        addi    sp, sp, -0x160
        STORE   ra, 0x8(sp)
        STORE   s0, 0x0(sp)
        /* Save allocatable integer registers on the stack,
           in the order given in proc.ml */
        STORE   a0, 0x10(sp)
        STORE   a1, 0x18(sp)
        STORE   a2, 0x20(sp)
        STORE   a3, 0x28(sp)
        STORE   a4, 0x30(sp)
        STORE   a5, 0x38(sp)
        STORE   a6, 0x40(sp)
        STORE   a7, 0x48(sp)
        STORE   s2, 0x50(sp)
        STORE   s3, 0x58(sp)
        STORE   s4, 0x60(sp)
        STORE   s5, 0x68(sp)
        STORE   s6, 0x70(sp)
        STORE   s7, 0x78(sp)
        STORE   s8, 0x80(sp)
        STORE   s9, 0x88(sp)
        STORE   t2, 0x90(sp)
        STORE   t3, 0x98(sp)
        STORE   t4, 0xa0(sp)
        STORE   t5, 0xa8(sp)
        STORE   t6, 0xb0(sp)
        /* Save caller-save floating-point registers on the stack
           (callee-saves are preserved by caml_garbage_collection) */
        fsd     ft0, 0xb8(sp)
        fsd     ft1, 0xc0(sp)
        fsd     ft2, 0xc8(sp)
        fsd     ft3, 0xd0(sp)
        fsd     ft4, 0xd8(sp)
        fsd     ft5, 0xe0(sp)
        fsd     ft6, 0xe8(sp)
        fsd     ft7, 0xf0(sp)
        fsd     fa0, 0xf8(sp)
        fsd     fa1, 0x100(sp)
        fsd     fa2, 0x108(sp)
        fsd     fa3, 0x110(sp)
        fsd     fa4, 0x118(sp)
        fsd     fa5, 0x120(sp)
        fsd     fa6, 0x128(sp)
        fsd     fa7, 0x130(sp)
        fsd     ft8, 0x138(sp)
        fsd     ft9, 0x140(sp)
        fsd     ft9, 0x148(sp)
        fsd     ft10, 0x150(sp)
        fsd     ft11, 0x158(sp)
        /* Store pointer to saved integer registers in caml_gc_regs */
        addi    TMP, sp, 16
        STORE   TMP, caml_gc_regs, TMP2
        /* Save current allocation pointer for debugging purposes */
        STORE   ALLOC_PTR, caml_young_ptr, TMP2
        /* Save trap pointer in case an exception is raised during GC */
        STORE   TRAP_PTR, caml_exception_pointer, TMP2
        /* Call the garbage collector */
        call    PLT(caml_garbage_collection)
        /* Restore registers */
        LOAD    a0, 0x10(sp)
        LOAD    a1, 0x18(sp)
        LOAD    a2, 0x20(sp)
        LOAD    a3, 0x28(sp)
        LOAD    a4, 0x30(sp)
        LOAD    a5, 0x38(sp)
        LOAD    a6, 0x40(sp)
        LOAD    a7, 0x48(sp)
        LOAD    s2, 0x50(sp)
        LOAD    s3, 0x58(sp)
        LOAD    s4, 0x60(sp)
        LOAD    s5, 0x68(sp)
        LOAD    s6, 0x70(sp)
        LOAD    s7, 0x78(sp)
        LOAD    s8, 0x80(sp)
        LOAD    s9, 0x88(sp)
        LOAD    t2, 0x90(sp)
        LOAD    t3, 0x98(sp)
        LOAD    t4, 0xa0(sp)
        LOAD    t5, 0xa8(sp)
        LOAD    t6, 0xb0(sp)
        fld     ft0, 0xb8(sp)
        fld     ft1, 0xc0(sp)
        fld     ft2, 0xc8(sp)
        fld     ft3, 0xd0(sp)
        fld     ft4, 0xd8(sp)
        fld     ft5, 0xe0(sp)
        fld     ft6, 0xe8(sp)
        fld     ft7, 0xf0(sp)
        fld     fa0, 0xf8(sp)
        fld     fa1, 0x100(sp)
        fld     fa2, 0x108(sp)
        fld     fa3, 0x110(sp)
        fld     fa4, 0x118(sp)
        fld     fa5, 0x120(sp)
        fld     fa6, 0x128(sp)
        fld     fa7, 0x130(sp)
        fld     ft8, 0x138(sp)
        fld     ft9, 0x140(sp)
        fld     ft9, 0x148(sp)
        fld     ft10, 0x150(sp)
        fld     ft11, 0x158(sp)
        /* Reload new allocation pointer and allocation limit */
        LOAD    ALLOC_PTR, caml_young_ptr
        LOAD    ALLOC_LIMIT, caml_young_limit
        /* Free stack space and return to caller */
        LOAD    ra, 0x8(sp)
        LOAD    s0, 0x0(sp)
        addi    sp, sp, 0x160
        ret
        .size   caml_call_gc, .-caml_call_gc

/* Call a C function from OCaml */
/* Function to call is in ARG */

        .align  2
        .globl  caml_c_call
        .type   caml_c_call, @function
caml_c_call:
        /* Preserve return address in callee-save register s2 */
        mv      s2, ra
        /* Record lowest stack address and return address */
        STORE   ra, caml_last_return_address, TMP2
        STORE   sp, caml_bottom_of_stack, TMP2
        /* Make the exception handler alloc ptr available to the C code */
        STORE   ALLOC_PTR, caml_young_ptr, TMP2
        STORE   TRAP_PTR, caml_exception_pointer, TMP2
        /* Call the function */
        jalr    ARG
        /* Reload alloc ptr and alloc limit */
        LOAD    ALLOC_PTR, caml_young_ptr
        LOAD    ALLOC_LIMIT, caml_young_limit
        /* Return */
        jr      s2
        .size   caml_c_call, .-caml_c_call

/* Raise an exception from OCaml */
        .align  2
        .globl  caml_raise_exn
        .type   caml_raise_exn, @function
caml_raise_exn:
        /* Test if backtrace is active */
        LOAD    TMP, caml_backtrace_active
        bnez    TMP, 2f
1:      /* Cut stack at current trap handler */
        mv      sp, TRAP_PTR
        /* Pop previous handler and jump to it */
        LOAD    TMP, 8(sp)
        LOAD    TRAP_PTR, 0(sp)
        addi    sp, sp, 16
        jr      TMP
2:      /* Preserve exception bucket in callee-save register s2 */
        mv      s2, a0
        /* Stash the backtrace */
        mv      a1, ra
        mv      a2, sp
        mv      a3, TRAP_PTR
        call    PLT(caml_stash_backtrace)
        /* Restore exception bucket and raise */
        mv      a0, s2
        j       1b
        .size   caml_raise_exn, .-caml_raise_exn

        .globl  caml_reraise_exn
        .type   caml_reraise_exn, @function

/* Raise an exception from C */

        .align  2
        .globl  caml_raise_exception
        .type   caml_raise_exception, @function
caml_raise_exception:
        LOAD    TRAP_PTR, caml_exception_pointer
        LOAD    ALLOC_PTR, caml_young_ptr
        LOAD    ALLOC_LIMIT, caml_young_limit
        LOAD    TMP, caml_backtrace_active
        bnez    TMP, 2f
1:      /* Cut stack at current trap handler */
        mv      sp, TRAP_PTR
        LOAD    TMP, 8(sp)
        LOAD    TRAP_PTR, 0(sp)
        addi    sp, sp, 16
        jr      TMP
2:      /* Preserve exception bucket in callee-save register s2 */
        mv      s2, a0
        LOAD    a1, caml_last_return_address
        LOAD    a2, caml_bottom_of_stack
        mv      a3, TRAP_PTR
        call    PLT(caml_stash_backtrace)
        mv      a0, s2
        j       1b
        .size   caml_raise_exception, .-caml_raise_exception

/* Start the OCaml program */

        .align  2
        .globl  caml_start_program
        .type   caml_start_program, @function
caml_start_program:

        la      ARG, caml_program
        /* Code shared with caml_callback* */
        /* Address of OCaml code to call is in ARG */
        /* Arguments to the OCaml code are in a0 ... a7 */
.Ljump_to_caml:
        /* Set up stack frame and save callee-save registers */
        addi    sp, sp, -0xd0
        STORE   ra, 0xc0(sp)
        STORE   s0, 0x0(sp)
        STORE   s1, 0x8(sp)
        STORE   s2, 0x10(sp)
        STORE   s3, 0x18(sp)
        STORE   s4, 0x20(sp)
        STORE   s5, 0x28(sp)
        STORE   s6, 0x30(sp)
        STORE   s7, 0x38(sp)
        STORE   s8, 0x40(sp)
        STORE   s9, 0x48(sp)
        STORE   s10, 0x50(sp)
        STORE   s11, 0x58(sp)
        fsd     fs0, 0x60(sp)
        fsd     fs1, 0x68(sp)
        fsd     fs2, 0x70(sp)
        fsd     fs3, 0x78(sp)
        fsd     fs4, 0x80(sp)
        fsd     fs5, 0x88(sp)
        fsd     fs6, 0x90(sp)
        fsd     fs7, 0x98(sp)
        fsd     fs8, 0xa0(sp)
        fsd     fs9, 0xa8(sp)
        fsd     fs10, 0xb0(sp)
        fsd     fs11, 0xb8(sp)
        addi    sp, sp, -32
        /* Setup a callback link on the stack */
        LOAD    TMP, caml_bottom_of_stack
        STORE   TMP, 0(sp)
        LOAD    TMP, caml_last_return_address
        STORE   TMP, 8(sp)
        LOAD    TMP, caml_gc_regs
        STORE   TMP, 16(sp)
        /* set up a trap frame */
        addi    sp, sp, -16
        LOAD    TMP, caml_exception_pointer
        STORE   TMP, 0(sp)
        lla     TMP, .Ltrap_handler
        STORE   TMP, 8(sp)
        mv      TRAP_PTR, sp
        LOAD    ALLOC_PTR, caml_young_ptr
        LOAD    ALLOC_LIMIT, caml_young_limit
        STORE   x0, caml_last_return_address, TMP2
        jalr    ARG
.Lcaml_retaddr:         /* pop trap frame, restoring caml_exception_pointer */
        LOAD    TMP, 0(sp)
        STORE   TMP, caml_exception_pointer, TMP2
        addi    sp, sp, 16
.Lreturn_result:        /* pop callback link, restoring global variables */
        LOAD    TMP, 0(sp)
        STORE   TMP, caml_bottom_of_stack, TMP2
        LOAD    TMP, 8(sp)
        STORE   TMP, caml_last_return_address, TMP2
        LOAD    TMP, 16(sp)
        STORE   TMP, caml_gc_regs, TMP2
        addi    sp, sp, 32
        /* Update allocation pointer */
        STORE   ALLOC_PTR, caml_young_ptr, TMP2
        /* reload callee-save registers and return */
        LOAD    ra, 0xc0(sp)
        LOAD    s0, 0x0(sp)
        LOAD    s1, 0x8(sp)
        LOAD    s2, 0x10(sp)
        LOAD    s3, 0x18(sp)
        LOAD    s4, 0x20(sp)
        LOAD    s5, 0x28(sp)
        LOAD    s6, 0x30(sp)
        LOAD    s7, 0x38(sp)
        LOAD    s8, 0x40(sp)
        LOAD    s9, 0x48(sp)
        LOAD    s10, 0x50(sp)
        LOAD    s11, 0x58(sp)
        fld     fs0, 0x60(sp)
        fld     fs1, 0x68(sp)
        fld     fs2, 0x70(sp)
        fld     fs3, 0x78(sp)
        fld     fs4, 0x80(sp)
        fld     fs5, 0x88(sp)
        fld     fs6, 0x90(sp)
        fld     fs7, 0x98(sp)
        fld     fs8, 0xa0(sp)
        fld     fs9, 0xa8(sp)
        fld     fs10, 0xb0(sp)
        fld     fs11, 0xb8(sp)
        addi    sp, sp, 0xd0
        ret
.Ltrap_handler:
        STORE   TRAP_PTR, caml_exception_pointer, TMP2
        ori     a0, a0, 2
        j       .Lreturn_result
        .size   caml_start_program, .-caml_start_program

/* Callback from C to OCaml */

        .align  2
        .globl  caml_callback_exn
        .type   caml_callback_exn, @function
caml_callback_exn:
        /* Initial shuffling of arguments (a0 = closure, a1 = first arg) */
        mv      TMP, a0
        mv      a0, a1          /* a0 = first arg */
        mv      a1, TMP        /* a1 = closure environment */
        LOAD    ARG, 0(TMP)    /* code pointer */
        j       .Ljump_to_caml
        .size   caml_callback_exn, .-caml_callback_exn

        .align  2
        .globl  caml_callback2_exn
        .type   caml_callback2_exn, @function
caml_callback2_exn:
        /* Initial shuffling of arguments (a0 = closure, a1 = arg1, a2 = arg2) */
        mv      TMP, a0
        mv      a0, a1
        mv      a1, a2
        mv      a2, TMP
        la      ARG, caml_apply2
        j       .Ljump_to_caml
        .size   caml_callback2_exn, .-caml_callback2_exn

        .align  2
        .globl  caml_callback3_exn
        .type   caml_callback3_exn, @function
caml_callback3_exn:
        /* Initial shuffling of argumnets */
        /* (a0 = closure, a1 = arg1, a2 = arg2, a3 = arg3) */
        mv      TMP, a0
        mv      a0, a1
        mv      a1, a2
        mv      a2, a3
        mv      a3, TMP
        la      ARG, caml_apply3
        j       .Ljump_to_caml
        .size   caml_callback3_exn, .-caml_callback3_exn

        .align  2
        .globl  caml_ml_array_bound_error
        .type   caml_ml_array_bound_error, @function
caml_ml_array_bound_error:
        /* Load address of [caml_array_bound_error] in ARG */
        la      ARG, caml_array_bound_error
        /* Call that function */
        j       caml_c_call

        .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

        .section .data
        .align  3
        .globl  caml_system__frametable
        .type   caml_system__frametable, @object
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .size   caml_system__frametable, .-caml_system__frametable
