	.text

	.align 2
	.globl pmix_atomic_mb
	.type pmix_atomic_mb, @function
pmix_atomic_mb:
	sync
	blr
	.size pmix_atomic_mb, .-pmix_atomic_mb


	.globl pmix_atomic_rmb
	.type pmix_atomic_rmb, @function
pmix_atomic_rmb:
	lwsync
	blr
	.size pmix_atomic_rmb, .-pmix_atomic_rmb


	.globl pmix_atomic_wmb
	.type pmix_atomic_wmb, @function
pmix_atomic_wmb:
	eieio
	blr
	.size pmix_atomic_wmb, .-pmix_atomic_wmb


	.globl pmix_atomic_cmpset_32
	.type pmix_atomic_cmpset_32, @function
pmix_atomic_cmpset_32:
	.L1: lwarx   0, 0, 3
	   cmpw    0, 0, 4
	   bne-    .L2
	   stwcx.  5, 0, 3
	   bne-    .L1
	.L2:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr
	.size pmix_atomic_cmpset_32, .-pmix_atomic_cmpset_32


	.globl pmix_atomic_cmpset_acq_32
	.type pmix_atomic_cmpset_acq_32, @function
pmix_atomic_cmpset_acq_32:
	.L3: lwarx   0, 0, 3
	   cmpw    0, 0, 4
	   bne-    .L4
	   stwcx.  5, 0, 3
	   bne-    .L3
	sync
	.L4:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	lwsync
	blr
	.size pmix_atomic_cmpset_acq_32, .-pmix_atomic_cmpset_acq_32


	.globl pmix_atomic_cmpset_rel_32
	.type pmix_atomic_cmpset_rel_32, @function
pmix_atomic_cmpset_rel_32:
	eieio
	.L5: lwarx   0, 0, 3
	   cmpw    0, 0, 4
	   bne-    .L6
	   stwcx.  5, 0, 3
	   bne-    .L5
	sync
	.L6:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr
	.size pmix_atomic_cmpset_rel_32, .-pmix_atomic_cmpset_rel_32



	.globl pmix_atomic_add_32
	.type pmix_atomic_add_32, @function
pmix_atomic_add_32:
	.L13:   lwarx 0, 0, 3
	     add  0, 4, 0
	     stwcx.   0, 0, 3
	     bne-  .L13
	mr	3,0
	blr
	.size pmix_atomic_add_32, .-pmix_atomic_add_32


	.globl pmix_atomic_sub_32
	.type pmix_atomic_sub_32, @function
pmix_atomic_sub_32:
	.L14:   lwarx 0,0,3
	     subf  0,4,0
	     stwcx.   0,0,3
	     bne-  .L14
	mr	3,0
	blr
	.size pmix_atomic_sub_32, .-pmix_atomic_sub_32

	.globl pmix_sys_timer_get_cycles
	.type pmix_sys_timer_get_cycles, @function
pmix_sys_timer_get_cycles:
	.L15:
	mftbu 0
	mftb 11
	mftbu 2
	cmpw 7,2,0
	bne+ 7,.L15
	li 4,0
	li 9,0
	or 3,2,9
	or 4,4,11
	blr
	.size pmix_sys_timer_get_cycles, .-pmix_sys_timer_get_cycles
