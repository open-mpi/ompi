	TEXT

	ALIGN(4)
START_FUNC(ompi_atomic_mb)
	sync
	blr
END_FUNC(ompi_atomic_mb)


START_FUNC(ompi_atomic_rmb)
	lwsync
	blr
END_FUNC(ompi_atomic_rmb)


START_FUNC(ompi_atomic_wmb)
	eieio
	blr
END_FUNC(ompi_atomic_wmb)


START_FUNC(ompi_atomic_cmpset_32)
	1: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    2f         
	   stwcx.  r5, 0, r3  
	   bne-    1b         
	sync 
	2:
	cmpw cr7,r0,r4
	mfcr r3
	rlwinm r3,r3,31,1
	blr
END_FUNC(ompi_atomic_cmpset_32)
	

START_FUNC(ompi_atomic_cmpset_acq_32)
	mflr r0
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-144(r1)
	bl _ompi_atomic_cmpset_32
	mr r29,r3
	bl _ompi_atomic_rmb
	mr r3,r29
	addi r1,r1,144
	ld r0,16(r1)
	mtlr r0
	ld r29,-24(r1)
	blr
END_FUNC(ompi_atomic_cmpset_acq_32)


START_FUNC(ompi_atomic_cmpset_rel_32)
	mflr r0
	std r27,-40(r1)
	std r28,-32(r1)
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-160(r1)
	mr r29,r3
	mr r28,r4
	mr r27,r5
	bl _ompi_atomic_wmb
	mr r3,r29
	mr r4,r28
	mr r5,r27
	bl _ompi_atomic_cmpset_32
	addi r1,r1,160
	ld r0,16(r1)
	mtlr r0
	ld r27,-40(r1)
	ld r28,-32(r1)
	ld r29,-24(r1)
	blr
END_FUNC(ompi_atomic_cmpset_rel_32)


START_FUNC(ompi_atomic_cmpset_64)
	1: ldarx   r0, 0, r3  
	   cmpd    0, r0, r4  
	   bne-    2f         
	   stdcx.  r5, 0, r3  
	   bne-    1b         
	2:
	xor r3,r4,r0
	subfic r2,r3,0
	adde r3,r2,r3
	blr
END_FUNC(ompi_atomic_cmpset_64)


START_FUNC(ompi_atomic_cmpset_acq_64)
	mflr r0
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-144(r1)
	bl _ompi_atomic_cmpset_64
	mr r29,r3
	bl _ompi_atomic_rmb
	mr r3,r29
	addi r1,r1,144
	ld r0,16(r1)
	mtlr r0
	ld r29,-24(r1)
	blr
END_FUNC(ompi_atomic_cmpset_acq_64)


START_FUNC(ompi_atomic_cmpset_rel_64)
	mflr r0
	std r27,-40(r1)
	std r28,-32(r1)
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-160(r1)
	mr r29,r3
	mr r28,r4
	mr r27,r5
	bl _ompi_atomic_wmb
	mr r3,r29
	mr r4,r28
	mr r5,r27
	bl _ompi_atomic_cmpset_64
	addi r1,r1,160
	ld r0,16(r1)
	mtlr r0
	ld r27,-40(r1)
	ld r28,-32(r1)
	ld r29,-24(r1)
	blr
END_FUNC(ompi_atomic_cmpset_rel_64)


START_FUNC(ompi_atomic_add_32)
	1:   lwarx r0, 0, r3 
	     add  r0, r4, r0                
	     stwcx.   r0, 0, r3              
	     bne-  1b                      
	
	lwz r3,0(r3)
	extsw r3,r3
	blr
END_FUNC(ompi_atomic_add_32)


START_FUNC(ompi_atomic_sub_32)
	1:   lwarx r0,0,r3
	     subf  r0,r4,r0                
	     stwcx.   r0,0,r3              
	     bne-  1b                      
	
	lwz r3,0(r3)
	extsw r3,r3
	blr
END_FUNC(ompi_atomic_sub_32)
