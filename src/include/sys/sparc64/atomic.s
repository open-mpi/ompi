;; 
;; $HEADER$
;; 
	.file	"atomic.c"
gcc2_compiled.:
.section	".text"
	.align 4
	.global ompi_atomic_mb
	.type	 ompi_atomic_mb,#function
	.proc	020
ompi_atomic_mb:
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
.LL2:
	ret
	restore
.LLfe1:
	.size	 ompi_atomic_mb,.LLfe1-ompi_atomic_mb
	.align 4
	.global ompi_atomic_rmb
	.type	 ompi_atomic_rmb,#function
	.proc	020
ompi_atomic_rmb:
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
.LL3:
	ret
	restore
.LLfe2:
	.size	 ompi_atomic_rmb,.LLfe2-ompi_atomic_rmb
	.align 4
	.global ompi_atomic_wmb
	.type	 ompi_atomic_wmb,#function
	.proc	020
ompi_atomic_wmb:
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
.LL4:
	ret
	restore
.LLfe3:
	.size	 ompi_atomic_wmb,.LLfe3-ompi_atomic_wmb
	.align 4
	.global ompi_atomic_cmpset_32
	.type	 ompi_atomic_cmpset_32,#function
	.proc	04
ompi_atomic_cmpset_32:
	!#PROLOGUE# 0
	save	%sp, -120, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+72], %o0
	st	%o0, [%fp-20]
	ld	[%fp-20], %o0
	ld	[%fp+68], %o1
	ld	[%fp+76], %o2
	casa [%o1] 0x80, %o2, %o0
	st	%o0, [%fp-20]
	ld	[%fp-20], %o0
	ld	[%fp+72], %o1
	xor	%o0, %o1, %o2
	subcc	%g0, %o2, %g0
	subx	%g0, -1, %o0
	mov	%o0, %i0
	b	.LL5
	 nop
.LL5:
	ret
	restore
.LLfe4:
	.size	 ompi_atomic_cmpset_32,.LLfe4-ompi_atomic_cmpset_32
	.align 4
	.global ompi_atomic_cmpset_acq_32
	.type	 ompi_atomic_cmpset_acq_32,#function
	.proc	04
ompi_atomic_cmpset_acq_32:
	!#PROLOGUE# 0
	save	%sp, -120, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	ld	[%fp+68], %o0
	ld	[%fp+72], %o1
	ld	[%fp+76], %o2
	call	ompi_atomic_cmpset_32, 0
	 nop
	st	%o0, [%fp-20]
	call	ompi_atomic_rmb, 0
	 nop
	ld	[%fp-20], %o0
	mov	%o0, %i0
	b	.LL6
	 nop
.LL6:
	ret
	restore
.LLfe5:
	.size	 ompi_atomic_cmpset_acq_32,.LLfe5-ompi_atomic_cmpset_acq_32
	.align 4
	.global ompi_atomic_cmpset_rel_32
	.type	 ompi_atomic_cmpset_rel_32,#function
	.proc	04
ompi_atomic_cmpset_rel_32:
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp+72]
	st	%i2, [%fp+76]
	call	ompi_atomic_wmb, 0
	 nop
	ld	[%fp+68], %o0
	ld	[%fp+72], %o1
	ld	[%fp+76], %o2
	call	ompi_atomic_cmpset_32, 0
	 nop
	mov	%o0, %i0
	b	.LL7
	 nop
.LL7:
	ret
	restore
.LLfe6:
	.size	 ompi_atomic_cmpset_rel_32,.LLfe6-ompi_atomic_cmpset_rel_32
	.align 4
	.global ompi_atomic_cmpset_64
	.type	 ompi_atomic_cmpset_64,#function
	.proc	04
ompi_atomic_cmpset_64:
	!#PROLOGUE# 0
	save	%sp, -136, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp-24]
	st	%i2, [%fp-20]
	st	%i3, [%fp-32]
	st	%i4, [%fp-28]
	ldd	[%fp-24], %o0
	std	%o0, [%fp-40]
	ldd	[%fp-40], %o0
	ld	[%fp+68], %o2
	ldd	[%fp-32], %o4
	casxa [%o2] 0x80, %o4, %o0
	std	%o0, [%fp-40]
	mov	0, %o0
	ld	[%fp-40], %o1
	ld	[%fp-24], %o2
	cmp	%o1, %o2
	bne	.LL9
	nop
	ld	[%fp-36], %o1
	ld	[%fp-20], %o2
	cmp	%o1, %o2
	bne	.LL9
	nop
	mov	1, %o0
.LL9:
	mov	%o0, %i0
	b	.LL8
	 nop
.LL8:
	ret
	restore
.LLfe7:
	.size	 ompi_atomic_cmpset_64,.LLfe7-ompi_atomic_cmpset_64
	.align 4
	.global ompi_atomic_cmpset_acq_64
	.type	 ompi_atomic_cmpset_acq_64,#function
	.proc	04
ompi_atomic_cmpset_acq_64:
	!#PROLOGUE# 0
	save	%sp, -136, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp-24]
	st	%i2, [%fp-20]
	st	%i3, [%fp-32]
	st	%i4, [%fp-28]
	ld	[%fp+68], %o0
	ld	[%fp-24], %o1
	ld	[%fp-20], %o2
	ld	[%fp-32], %o3
	ld	[%fp-28], %o4
	call	ompi_atomic_cmpset_64, 0
	 nop
	st	%o0, [%fp-36]
	call	ompi_atomic_rmb, 0
	 nop
	ld	[%fp-36], %o0
	mov	%o0, %i0
	b	.LL10
	 nop
.LL10:
	ret
	restore
.LLfe8:
	.size	 ompi_atomic_cmpset_acq_64,.LLfe8-ompi_atomic_cmpset_acq_64
	.align 4
	.global ompi_atomic_cmpset_rel_64
	.type	 ompi_atomic_cmpset_rel_64,#function
	.proc	04
ompi_atomic_cmpset_rel_64:
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	st	%i0, [%fp+68]
	st	%i1, [%fp-24]
	st	%i2, [%fp-20]
	st	%i3, [%fp-32]
	st	%i4, [%fp-28]
	call	ompi_atomic_wmb, 0
	 nop
	ld	[%fp+68], %o0
	ld	[%fp-24], %o1
	ld	[%fp-20], %o2
	ld	[%fp-32], %o3
	ld	[%fp-28], %o4
	call	ompi_atomic_cmpset_64, 0
	 nop
	mov	%o0, %i0
	b	.LL11
	 nop
.LL11:
	ret
	restore
.LLfe9:
	.size	 ompi_atomic_cmpset_rel_64,.LLfe9-ompi_atomic_cmpset_rel_64
	.ident	"GCC: (GNU) 2.95.2 19991024 (release)"
