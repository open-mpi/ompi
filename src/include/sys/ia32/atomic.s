;; 
;; Copyright (c) 2004-2005 The Trustees of Indiana University.
;;                         All rights reserved.
;; Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
;;                         All rights reserved.
;; $COPYRIGHT$
;; 
;; Additional copyrights may follow
;; 
;; $HEADER$
;; 
	.file	"atomic.c"
	.text
.globl ompi_atomic_mb
	.type	ompi_atomic_mb,@function
ompi_atomic_mb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
.Lfe1:
	.size	ompi_atomic_mb,.Lfe1-ompi_atomic_mb
.globl ompi_atomic_rmb
	.type	ompi_atomic_rmb,@function
ompi_atomic_rmb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
.Lfe2:
	.size	ompi_atomic_rmb,.Lfe2-ompi_atomic_rmb
.globl ompi_atomic_wmb
	.type	ompi_atomic_wmb,@function
ompi_atomic_wmb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
.Lfe3:
	.size	ompi_atomic_wmb,.Lfe3-ompi_atomic_wmb
.globl ompi_atomic_cmpset_32
	.type	ompi_atomic_cmpset_32,@function
ompi_atomic_cmpset_32:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	movl	12(%ebp), %eax
	movl	%eax, -8(%ebp)
	movl	-8(%ebp), %edx
	movl	16(%ebp), %ecx
	movl	%edx, %eax
	movl	8(%ebp), %ebx
#APP
	cmpxchgl %ecx,(%ebx)   
      setz     %al    
      movzbl   %al,%eax 

#NO_APP
	movl	%eax, %edx
	movl	%edx, -8(%ebp)
	movl	-8(%ebp), %eax
	cmpl	12(%ebp), %eax
	sete	%al
	movzbl	%al, %eax
	addl	$4, %esp
	popl	%ebx
	leave
	ret
.Lfe4:
	.size	ompi_atomic_cmpset_32,.Lfe4-ompi_atomic_cmpset_32
.globl ompi_atomic_cmpset_acq_32
	.type	ompi_atomic_cmpset_acq_32,@function
ompi_atomic_cmpset_acq_32:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	subl	$4, %esp
	pushl	16(%ebp)
	pushl	12(%ebp)
	pushl	8(%ebp)
	call	ompi_atomic_cmpset_32
	addl	$16, %esp
	leave
	ret
.Lfe5:
	.size	ompi_atomic_cmpset_acq_32,.Lfe5-ompi_atomic_cmpset_acq_32
.globl ompi_atomic_cmpset_rel_32
	.type	ompi_atomic_cmpset_rel_32,@function
ompi_atomic_cmpset_rel_32:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	subl	$4, %esp
	pushl	16(%ebp)
	pushl	12(%ebp)
	pushl	8(%ebp)
	call	ompi_atomic_cmpset_32
	addl	$16, %esp
	leave
	ret
.Lfe6:
	.size	ompi_atomic_cmpset_rel_32,.Lfe6-ompi_atomic_cmpset_rel_32
.globl ompi_atomic_cmpset_64
	.type	ompi_atomic_cmpset_64,@function
ompi_atomic_cmpset_64:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$28, %esp
	movl	12(%ebp), %eax
	movl	16(%ebp), %edx
	movl	%eax, -16(%ebp)
	movl	%edx, -12(%ebp)
	movl	20(%ebp), %eax
	movl	24(%ebp), %edx
	movl	%eax, -24(%ebp)
	movl	%edx, -20(%ebp)
	movl	-16(%ebp), %eax
	movl	-12(%ebp), %edx
	movl	%eax, -32(%ebp)
	movl	%edx, -28(%ebp)
	movl	-16(%ebp), %ecx
	movl	-12(%ebp), %ebx
	movl	-32(%ebp), %eax
	xorl	%ecx, %eax
	movl	-28(%ebp), %edx
	xorl	%ebx, %edx
	orl	%edx, %eax
	testl	%eax, %eax
	sete	%al
	movzbl	%al, %eax
	addl	$28, %esp
	popl	%ebx
	leave
	ret
.Lfe7:
	.size	ompi_atomic_cmpset_64,.Lfe7-ompi_atomic_cmpset_64
.globl ompi_atomic_cmpset_acq_64
	.type	ompi_atomic_cmpset_acq_64,@function
ompi_atomic_cmpset_acq_64:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	12(%ebp), %eax
	movl	16(%ebp), %edx
	movl	%eax, -8(%ebp)
	movl	%edx, -4(%ebp)
	movl	20(%ebp), %eax
	movl	24(%ebp), %edx
	movl	%eax, -16(%ebp)
	movl	%edx, -12(%ebp)
	subl	$12, %esp
	pushl	-12(%ebp)
	pushl	-16(%ebp)
	pushl	-4(%ebp)
	pushl	-8(%ebp)
	pushl	8(%ebp)
	call	ompi_atomic_cmpset_64
	addl	$32, %esp
	leave
	ret
.Lfe8:
	.size	ompi_atomic_cmpset_acq_64,.Lfe8-ompi_atomic_cmpset_acq_64
.globl ompi_atomic_cmpset_rel_64
	.type	ompi_atomic_cmpset_rel_64,@function
ompi_atomic_cmpset_rel_64:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	12(%ebp), %eax
	movl	16(%ebp), %edx
	movl	%eax, -8(%ebp)
	movl	%edx, -4(%ebp)
	movl	20(%ebp), %eax
	movl	24(%ebp), %edx
	movl	%eax, -16(%ebp)
	movl	%edx, -12(%ebp)
	subl	$12, %esp
	pushl	-12(%ebp)
	pushl	-16(%ebp)
	pushl	-4(%ebp)
	pushl	-8(%ebp)
	pushl	8(%ebp)
	call	ompi_atomic_cmpset_64
	addl	$32, %esp
	leave
	ret
.Lfe9:
	.size	ompi_atomic_cmpset_rel_64,.Lfe9-ompi_atomic_cmpset_rel_64
	.ident	"GCC: (GNU) 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"
