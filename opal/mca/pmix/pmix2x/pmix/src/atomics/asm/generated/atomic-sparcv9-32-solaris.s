	.text

	.align 4


	.globl pmix_atomic_mb
	.type pmix_atomic_mb, #function
pmix_atomic_mb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad | #LoadStore | #StoreStore | #StoreLoad
	retl
	nop
	.size pmix_atomic_mb, .-pmix_atomic_mb


	.globl pmix_atomic_rmb
	.type pmix_atomic_rmb, #function
pmix_atomic_rmb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad
	retl
	nop
	.size pmix_atomic_rmb, .-pmix_atomic_rmb


	.globl pmix_atomic_wmb
	.type pmix_atomic_wmb, #function
pmix_atomic_wmb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	retl
	nop
	.size pmix_atomic_wmb, .-pmix_atomic_wmb


	.globl pmix_atomic_cmpset_32
	.type pmix_atomic_cmpset_32, #function
pmix_atomic_cmpset_32:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casa [%o0] 0x80, %o1, %o2
	xor	%o2, %o1, %o2
	subcc	%g0, %o2, %g0
	retl
	subx	%g0, -1, %o0
	.size pmix_atomic_cmpset_32, .-pmix_atomic_cmpset_32


	.globl pmix_atomic_cmpset_acq_32
	.type pmix_atomic_cmpset_acq_32, #function
pmix_atomic_cmpset_acq_32:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casa [%o0] 0x80, %o1, %o2
	xor     %o2, %o1, %o2
	subcc   %g0, %o2, %g0
	subx    %g0, -1, %o0
	membar #LoadLoad
	retl
	sra     %o0, 0, %o0
	.size pmix_atomic_cmpset_acq_32, .-pmix_atomic_cmpset_acq_32


	.globl pmix_atomic_cmpset_rel_32
	.type pmix_atomic_cmpset_rel_32, #function
pmix_atomic_cmpset_rel_32:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	casa [%o0] 0x80, %o1, %o2
	xor     %o2, %o1, %o2
	subcc   %g0, %o2, %g0
	retl
	subx    %g0, -1, %o0
	.size pmix_atomic_cmpset_rel_32, .-pmix_atomic_cmpset_rel_32


	.globl pmix_atomic_cmpset_64
	.type pmix_atomic_cmpset_64, #function
pmix_atomic_cmpset_64:
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i3, %o4
	mov	%i4, %o5
	st	%i1, [%fp-32]
	st	%i2, [%fp-28]
	std	%o4, [%fp-24]
	ldx [%fp-24], %g1
	ldx [%fp-32], %g2
	casxa [%i0] 0x80, %g2, %g1
	stx %g1, [%fp-24]

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	.L12
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	.L12
	mov	1, %i0
.L12:
	ret
	restore
	.size pmix_atomic_cmpset_64, .-pmix_atomic_cmpset_64


	.globl pmix_atomic_cmpset_acq_64
	.type pmix_atomic_cmpset_acq_64, #function
pmix_atomic_cmpset_acq_64:
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i1, %o4
	mov	%i2, %o5
	mov	%i3, %o2
	mov	%i4, %o3
	std	%o4, [%fp-32]
	std	%o2, [%fp-24]
	ldx [%fp-24], %g1
	ldx [%fp-32], %g2
	casxa [%i0] 0x80, %g2, %g1
	stx %g1, [%fp-24]

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	.L16
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	.L16
	mov	1, %i0
.L16:
	membar #LoadLoad
	ret
	restore
	.size pmix_atomic_cmpset_acq_64, .-pmix_atomic_cmpset_acq_64


	.globl pmix_atomic_cmpset_rel_64
	.type pmix_atomic_cmpset_rel_64, #function
pmix_atomic_cmpset_rel_64:
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i1, %o4
	mov	%i2, %o5
	mov	%i3, %o2
	mov	%i4, %o3
	membar #StoreStore
	std	%o4, [%fp-32]
	std	%o2, [%fp-24]
	ldx [%fp-24], %g1
	ldx [%fp-32], %g2
	casxa [%i0] 0x80, %g2, %g1
	stx %g1, [%fp-24]

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	.L21
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	.L21
	mov	1, %i0
.L21:
	ret
	restore
	.size pmix_atomic_cmpset_rel_64, .-pmix_atomic_cmpset_rel_64


	.globl pmix_sys_timer_get_cycles
	.type pmix_sys_timer_get_cycles, #function
pmix_sys_timer_get_cycles:
        save    %sp,-96,%sp
        rd      %tick,%o0
        srlx    %o0,32,%o1
        or      %g0,%o1,%i0
        ret     ! Result =  %i0
        restore %o0,0,%o1
	.size pmix_sys_timer_get_cycles, .-pmix_sys_timer_get_cycles
