;; 
;; Copyright (c) 2004-2005 The Trustees of Indiana University.
;;                         All rights reserved.
;; Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
;;                         All rights reserved.
;; Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
;;                         University of Stuttgart.  All rights reserved.
;; $COPYRIGHT$
;; 
;; Additional copyrights may follow
;; 
;; $HEADER$
;; 
	.section __TEXT,__text,regular,pure_instructions
	.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32
.section __TEXT,__text,regular,pure_instructions
	.align 2
	.align 2
	.globl _ompi_atomic_mb
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_mb:
	stmw r30,-8(r1)
	stwu r1,-48(r1)
	mr r30,r1
	lwz r1,0(r1)
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_rmb
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_rmb:
	stmw r30,-8(r1)
	stwu r1,-48(r1)
	mr r30,r1
	lwz r1,0(r1)
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_wmb
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_wmb:
	stmw r30,-8(r1)
	stwu r1,-48(r1)
	mr r30,r1
	lwz r1,0(r1)
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_32
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_32:
	stmw r30,-8(r1)
	stwu r1,-64(r1)
	mr r30,r1
	stw r3,88(r30)
	stw r4,92(r30)
	stw r5,96(r30)
	lwz r10,88(r30)
	lwz r11,88(r30)
	lwz r9,92(r30)
	lwz r0,96(r30)
	lwz r2,88(r30)
	1: lwarx   r8, 0, r11  
    cmpw    0, r8, r9  
    bne-    2f         
    stwcx.  r0, 0, r11  
    bne-    1b         
2:
	mr r0,r8
	stw r0,32(r30)
	lwz r2,32(r30)
	lwz r0,92(r30)
	cmpw cr7,r2,r0
	mfcr r0
	rlwinm r0,r0,31,1
	mr r3,r0
	lwz r1,0(r1)
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_acq_32
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_acq_32:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-96(r1)
	mr r30,r1
	stw r3,120(r30)
	stw r4,124(r30)
	stw r5,128(r30)
	lwz r3,120(r30)
	lwz r4,124(r30)
	lwz r5,128(r30)
	bl _ompi_atomic_cmpset_32
	mr r0,r3
	stw r0,64(r30)
	bl _ompi_atomic_rmb
	lwz r0,64(r30)
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_rel_32
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_rel_32:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-80(r1)
	mr r30,r1
	stw r3,104(r30)
	stw r4,108(r30)
	stw r5,112(r30)
	bl _ompi_atomic_wmb
	lwz r3,104(r30)
	lwz r4,108(r30)
	lwz r5,112(r30)
	bl _ompi_atomic_cmpset_32
	mr r0,r3
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_64
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_64:
	stmw r30,-8(r1)
	stwu r1,-96(r1)
	mr r30,r1
	stw r3,120(r30)
	stw r4,32(r30)
	stw r5,36(r30)
	stw r6,40(r30)
	stw r7,44(r30)
	lwz r10,120(r30)
	lwz r0,120(r30)
	lwz r11,32(r30)
	lwz r12,36(r30)
	lwz r2,40(r30)
	lwz r3,44(r30)
	lwz r9,120(r30)
	1: ldarx   r7, 0, r0  
    cmpd    0, r7, r11  
    bne-    2f         
    stdcx.  r2, 0, r0  
    bne-    1b         
2:
	mr r2,r7
	mr r3,r8
	stw r2,64(r30)
	stw r3,68(r30)
	lfd f0,64(r30)
	stfd f0,48(r30)
	li r8,0
	stw r8,56(r30)
	lwz r2,48(r30)
	lwz r0,32(r30)
	cmpw cr7,r2,r0
	bne cr7,L8
	lwz r0,52(r30)
	lwz r2,36(r30)
	cmpw cr7,r0,r2
	bne cr7,L8
	li r0,1
	stw r0,56(r30)
L8:
	lwz r0,56(r30)
	mr r3,r0
	lwz r1,0(r1)
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_acq_64
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_acq_64:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-112(r1)
	mr r30,r1
	stw r3,136(r30)
	stw r4,64(r30)
	stw r5,68(r30)
	stw r6,72(r30)
	stw r7,76(r30)
	lwz r3,136(r30)
	lwz r4,64(r30)
	lwz r5,68(r30)
	lwz r6,72(r30)
	lwz r7,76(r30)
	bl _ompi_atomic_cmpset_64
	mr r0,r3
	stw r0,80(r30)
	bl _ompi_atomic_rmb
	lwz r0,80(r30)
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
	.align 2
	.globl _ompi_atomic_cmpset_rel_64
.section __TEXT,__text,regular,pure_instructions
	.align 2
_ompi_atomic_cmpset_rel_64:
	mflr r0
	stmw r30,-8(r1)
	stw r0,8(r1)
	stwu r1,-96(r1)
	mr r30,r1
	stw r3,120(r30)
	stw r4,64(r30)
	stw r5,68(r30)
	stw r6,72(r30)
	stw r7,76(r30)
	bl _ompi_atomic_wmb
	lwz r3,120(r30)
	lwz r4,64(r30)
	lwz r5,68(r30)
	lwz r6,72(r30)
	lwz r7,76(r30)
	bl _ompi_atomic_cmpset_64
	mr r0,r3
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r30,-8(r1)
	blr
