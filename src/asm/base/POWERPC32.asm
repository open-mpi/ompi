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
	stw r4,-32(r1)
	stw r5,-28(r1)
	stw r6,-24(r1)
	stw r7,-20(r1)
	ld r5,-32(r1)
	ld r7,-24(r1)
	1: ldarx   r9, 0, r3  
	   cmpd    0, r9, r5  
	   bne-    2f         
	   stdcx.  r7, 0, r3
	   bne-    1b
	2:
	xor r3,r5,r9
	subfic r2,r3,0
	adde r3,r2,r3
	blr
END_FUNC(ompi_atomic_cmpset_64)


START_FUNC(ompi_atomic_cmpset_acq_64)
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        1: ldarx   r9, 0, r3  
           cmpd    0, r9, r5
           bne-    2f         
           stdcx.  r7, 0, r3  
           bne-    1b         
        2:
        xor r3,r5,r9
        subfic r2,r3,0
        adde r3,r2,r3
        blr
        lwsync
        blr
END_FUNC(ompi_atomic_cmpset_acq_64)


START_FUNC(ompi_atomic_cmpset_rel_64)
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        eieio
        1: ldarx   r9, 0, r3  
           cmpd    0, r9, r5  
           bne-    2f         
           stdcx.  r7, 0, r3  
           bne-    1b         
        2:
        xor r3,r5,r9
        subfic r2,r3,0
        adde r3,r2,r3
        blr
        lwsync
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
