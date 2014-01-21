START_FILE

#ifdef __linux__
#include <sys/asm.h>
#else
#include <asm.h>
#endif
#include <regdef.h>
	
	TEXT

	ALIGN(8)
LEAF(opal_atomic_mb)
#ifdef __linux__
	.set mips2
#endif
	sync
#ifdef __linux__
	.set mips0
#endif	
	j	ra
END(opal_atomic_mb)

	
	ALIGN(8)
LEAF(opal_atomic_rmb)
#ifdef __linux__
	.set mips2
#endif
	sync
#ifdef __linux__
	.set mips0
#endif
	j	ra
END(opal_atomic_rmb)
	
	
LEAF(opal_atomic_wmb)
#ifdef __linux__
	.set mips2
#endif
	sync
#ifdef __linux__
	.set mips0
#endif
	j	ra
END(opal_atomic_wmb)


LEAF(opal_atomic_cmpset_32)
	.set noreorder        
retry1:                
#ifdef __linux__
	.set mips2
#endif
	ll     $3, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	bne    $3, $5, done1   
	or     $2, $6, 0      
#ifdef __linux__
	.set mips2
#endif
	sc     $2, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	beqz   $2, retry1
done1:                 
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_32)


LEAF(opal_atomic_cmpset_acq_32)
	.set noreorder        
retry2:                
#ifdef __linux__
	.set mips2
#endif
	ll     $3, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	bne    $3, $5, done2   
	or     $2, $6, 0      
#ifdef __linux__
	.set mips2
#endif
	sc     $2, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	beqz   $2, retry2   
done2:                 
#ifdef __linux__
	.set mips2
#endif
	sync
#ifdef __linux__
	.set mips0
#endif
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_acq_32)

	
LEAF(opal_atomic_cmpset_rel_32)
	.set noreorder        
#ifdef __linux__
	.set mips2
#endif
	sync
#ifdef __linux__
	.set mips0
#endif
retry3:                
#ifdef __linux__
	.set mips2
#endif
	ll     $3, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	bne    $3, $5, done3   
	or     $2, $6, 0      
#ifdef __linux__
	.set mips2
#endif
	sc     $2, 0($4)         
#ifdef __linux__
	.set mips0
#endif
	beqz   $2, retry3   
done3:                 
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_rel_32)
	
#ifdef __mips64	
LEAF(opal_atomic_cmpset_64)
	.set noreorder        
retry4:                
	lld    $3, 0($4)         
	bne    $3, $5, done4   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry4   
done4:                 
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_64)


LEAF(opal_atomic_cmpset_acq_64)
	.set noreorder        
retry5:                
	lld    $3, 0($4)         
	bne    $3, $5, done5   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry5   
done5:                 
	sync
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_acq_64)


LEAF(opal_atomic_cmpset_rel_64)
	.set noreorder        
	sync
retry6:                
	lld    $3, 0($4)         
	bne    $3, $5, done6   
	or     $2, $6, 0      
	scd    $2, 0($4)         
	beqz   $2, retry6   
done6:                 
	xor	$3,$3,$5
	j	ra
	sltu	$2,$3,1
	.set reorder          
END(opal_atomic_cmpset_rel_64)
#endif /* __mips64 */
