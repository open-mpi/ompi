START_FILE
	TEXT

	ALIGN(4)

	
START_FUNC(ompi_atomic_mb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad | #LoadStore | #StoreStore | #StoreLoad
	retl
	nop
END_FUNC(ompi_atomic_mb)


START_FUNC(ompi_atomic_rmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad
	retl
	nop
END_FUNC(ompi_atomic_rmb)


START_FUNC(ompi_atomic_wmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	retl
	nop
END_FUNC(ompi_atomic_wmb)


START_FUNC(ompi_atomic_cmpset_32)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	mov	%o2, %g1
	casa [%o0] 0x80, %o1, %g1
	xor	%g1, %o1, %g1
	subcc	%g0, %g1, %g0
	retl
	subx	%g0, -1, %o0
END_FUNC(ompi_atomic_cmpset_32)


START_FUNC(ompi_atomic_cmpset_acq_32)
	!#PROLOGUE# 0
	save	%sp, -192, %sp
	!#PROLOGUE# 1
	mov	%i0, %o0
	mov	%i1, %o1
	call	ompi_atomic_cmpset_32, 0
	mov	%i2, %o2
	call	ompi_atomic_rmb, 0
	mov	%o0, %i0
	return	%i7+8
	sra	%o0, 0, %o0
END_FUNC(ompi_atomic_cmpset_acq_32)


START_FUNC(ompi_atomic_cmpset_rel_32)
	!#PROLOGUE# 0
	save	%sp, -192, %sp
	!#PROLOGUE# 1
	call	ompi_atomic_wmb, 0
	sra	%i1, 0, %i1
	sra	%i2, 0, %i2
	mov	%i0, %o0
	mov	%i1, %o1
	call	ompi_atomic_cmpset_32, 0
	mov	%i2, %o2
	ret
	restore %g0, %o0, %o0
END_FUNC(ompi_atomic_cmpset_rel_32)


START_FUNC(ompi_atomic_cmpset_64)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	mov	%o2, %g1
	casxa [%o0] 0x80, %o1, %g1
	xor	%g1, %o1, %g1
	mov	0, %o0
	retl
	movre	%g1, 1, %o0
END_FUNC(ompi_atomic_cmpset_64)


START_FUNC(ompi_atomic_cmpset_acq_64)
	!#PROLOGUE# 0
	save	%sp, -192, %sp
	!#PROLOGUE# 1
	mov	%i0, %o0
	mov	%i1, %o1
	call	ompi_atomic_cmpset_64, 0
	mov	%i2, %o2
	call	ompi_atomic_rmb, 0
	mov	%o0, %i0
	return	%i7+8
	sra	%o0, 0, %o0
END_FUNC(ompi_atomic_cmpset_acq_64)


START_FUNC(ompi_atomic_cmpset_rel_64)
	!#PROLOGUE# 0
	save	%sp, -192, %sp
	!#PROLOGUE# 1
	call	ompi_atomic_wmb, 0
	 nop
	mov	%i0, %o0
	mov	%i1, %o1
	call	ompi_atomic_cmpset_64, 0
	mov	%i2, %o2
	ret
	restore %g0, %o0, %o0
END_FUNC(ompi_atomic_cmpset_rel_64)
