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
	xor r3,r0,r4
	subfic r2,r3,0
	adde r3,r2,r3
	blr
END_FUNC(ompi_atomic_cmpset_32)


START_FUNC(ompi_atomic_cmpset_acq_32)
	1: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    2f         
	   stwcx.  r5, 0, r3  
	   bne-    1b         
	sync 
	2:
	xor r3,r0,r4
	subfic r2,r3,0
	adde r3,r2,r3
	lwsync
	blr
END_FUNC(ompi_atomic_cmpset_acq_32)


START_FUNC(ompi_atomic_cmpset_rel_32)
	eieio
	1: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    2f         
	   stwcx.  r5, 0, r3  
	   bne-    1b         
	sync 
	2:
	xor r3,r0,r4
	subfic r2,r3,0
	adde r3,r2,r3
	blr
END_FUNC(ompi_atomic_cmpset_rel_32)

#START_64BIT
START_FUNC(ompi_atomic_cmpset_64)
	1: ldarx   r9, 0, r3  
	   cmpd    0, r9, r4  
	   bne-    2f         
	   stdcx.  r6, 0, r3  
	   bne-    1b         
	2:
	li r3,0
	cmpw cr7,r9,r4
	bnelr+ cr7
	cmpw cr7,r10,r5
	bnelr+ cr7
	li r3,1
	blr
END_FUNC(ompi_atomic_cmpset_64)


START_FUNC(ompi_atomic_cmpset_acq_64)
        1: ldarx   r9, 0, r3  
           cmpd    0, r9, r4  
           bne-    2f         
           stdcx.  r6, 0, r3  
           bne-    1b         
        2:
        cmpw cr0,r9,r4
        li r3,0
        bne+ cr0,L15
        cmpw cr0,r10,r5
        bne+ cr0,L15
        li r3,1
L15:
        lwsync
        blr
END_FUNC(ompi_atomic_cmpset_acq_64)


START_FUNC(ompi_atomic_cmpset_rel_64)
        eieio
        1: ldarx   r9, 0, r3  
           cmpd    0, r9, r4  
           bne-    2f         
           stdcx.  r6, 0, r3  
           bne-    1b         
        2:
        cmpw cr0,r9,r4
        li r3,0
        bnelr+ cr0
        cmpw cr0,r10,r5
        bnelr+ cr0
        li r3,1
        blr
END_FUNC(ompi_atomic_cmpset_rel_64)
#END_64BIT


START_FUNC(ompi_atomic_add_32)
	1:   lwarx r0, 0, r3 
	     add  r0, r4, r0                
	     stwcx.   r0, 0, r3              
	     bne-  1b                      
	
	lwz r3,0(r3)
	blr
END_FUNC(ompi_atomic_add_32)


START_FUNC(ompi_atomic_sub_32)
	1:   lwarx r0,0,r3
	     subf  r0,r4,r0                
	     stwcx.   r0,0,r3              
	     bne-  1b                      
	
	lwz r3,0(r3)
	blr
END_FUNC(ompi_atomic_sub_32)
