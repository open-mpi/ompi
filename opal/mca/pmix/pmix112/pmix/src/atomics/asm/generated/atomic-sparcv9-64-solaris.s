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
	!#PROLOGUE# 1
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	retl
	movre   %o2, 1, %o0
	.size pmix_atomic_cmpset_64, .-pmix_atomic_cmpset_64


	.globl pmix_atomic_cmpset_acq_64
	.type pmix_atomic_cmpset_acq_64, #function
pmix_atomic_cmpset_acq_64:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	movre   %o2, 1, %o0
	membar #LoadLoad
	retl
	sra     %o0, 0, %o0
	.size pmix_atomic_cmpset_acq_64, .-pmix_atomic_cmpset_acq_64


	.globl pmix_atomic_cmpset_rel_64
	.type pmix_atomic_cmpset_rel_64, #function
pmix_atomic_cmpset_rel_64:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	retl
	movre   %o2, 1, %o0
	.size pmix_atomic_cmpset_rel_64, .-pmix_atomic_cmpset_rel_64


	.globl pmix_sys_timer_get_cycles
	.type pmix_sys_timer_get_cycles, #function
pmix_sys_timer_get_cycles:
        save    %sp,-176,%sp
        rd      %tick,%o0
        ret     ! Result =  %i0
        restore %o0,0,%o0
	.size pmix_sys_timer_get_cycles, .-pmix_sys_timer_get_cycles
