/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* Inline C implementation of the functions defined in atomic.h */


/**********************************************************************
 *
 * Atomic math operations
 *
 * All the architectures provide a compare_and_set atomic operations. If
 * they dont provide atomic additions and/or substractions then we can
 * define these operations using the atomic compare_and_set.
 *
 * Some architectures does not provide support for the 64 bits
 * atomic operations. Until we find a better solution let's just
 * undefine all those functions if there is no 64 bit cmpset
 *
 *********************************************************************/
#if OMPI_HAVE_ATOMIC_CMPSET_32

#if !defined(OMPI_HAVE_ATOMIC_ADD_32)
#define OMPI_HAVE_ATOMIC_ADD_32 1
static inline int32_t
ompi_atomic_add_32(volatile int32_t *addr, int delta)
{
   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */


#if !defined(OMPI_HAVE_ATOMIC_SUB_32)
#define OMPI_HAVE_ATOMIC_SUB_32 1
static inline int32_t
ompi_atomic_sub_32(volatile int32_t *addr, int delta)
{
   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval - delta));
   return (oldval - delta);
}
#endif  /* OMPI_HAVE_ATOMIC_SUB_32 */

#endif /* OMPI_HAVE_ATOMIC_CMPSET_32 */


#if OMPI_HAVE_ATOMIC_CMPSET_64

#if !defined(OMPI_HAVE_ATOMIC_ADD_64)
#define OMPI_HAVE_ATOMIC_ADD_64 1
static inline int64_t
ompi_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
   int64_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OMPI_HAVE_ATOMIC_ADD_64 */


#if !defined(OMPI_HAVE_ATOMIC_SUB_64)
#define OMPI_HAVE_ATOMIC_SUB_64 1
static inline int64_t
ompi_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
    int64_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval - delta));
    return (oldval - delta);
}
#endif  /* OMPI_HAVE_ATOMIC_SUB_64 */

#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */


#if (OMPI_HAVE_ATOMIC_CMPSET_32 || OMPI_HAVE_ATOMIC_CMPSET_64)

static inline int
ompi_atomic_cmpset_xx(volatile void* addr, int64_t oldval,
                      int64_t newval, size_t length)
{
   switch( length ) {
#if OMPI_HAVE_ATOMIC_CMPSET_32
   case 4:
      return ompi_atomic_cmpset_32( (volatile int32_t*)addr,
                                    (int32_t)oldval, (int32_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */

#if OMPI_HAVE_ATOMIC_CMPSET_64
   case 8:
      return ompi_atomic_cmpset_64( (volatile int64_t*)addr,
                                    (int64_t)oldval, (int64_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */
   default:
      /* This should never happen, so deliberately cause a seg fault
         for corefile analysis */
      *(int*)(0) = 0;
   }
   return 0;  /* always fail */
}


static inline int
ompi_atomic_cmpset_acq_xx(volatile void* addr, int64_t oldval,
                          int64_t newval, size_t length)
{
   switch( length ) {
#if OMPI_HAVE_ATOMIC_CMPSET_32
   case 4:
      return ompi_atomic_cmpset_acq_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */

#if OMPI_HAVE_ATOMIC_CMPSET_64
   case 8:
      return ompi_atomic_cmpset_acq_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */
   default:
      /* This should never happen, so deliberately cause a seg fault
         for corefile analysis */
      *(int*)(0) = 0;
   }
   return 0;  /* always fail */
}


static inline int
ompi_atomic_cmpset_rel_xx(volatile void* addr, int64_t oldval,
                          int64_t newval, size_t length)
{
   switch( length ) {
#if OMPI_HAVE_ATOMIC_CMPSET_32
   case 4:
      return ompi_atomic_cmpset_rel_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */

#if OMPI_HAVE_ATOMIC_CMPSET_64
   case 8:
      return ompi_atomic_cmpset_rel_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */
   default:
      /* This should never happen, so deliberately cause a seg fault
         for corefile analysis */
      *(int*)(0) = 0;
   }
   return 0;  /* always fail */
}


static inline void
ompi_atomic_add_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OMPI_HAVE_ATOMIC_CMPSET_32
   case 4:
      ompi_atomic_add_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */

#if OMPI_HAVE_ATOMIC_CMPSET_64
   case 8:
      ompi_atomic_add_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */
   default:
      /* This should never happen, so deliberately cause a seg fault
         for corefile analysis */
      *(int*)(0) = 0;
   }
}


static inline void
ompi_atomic_sub_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OMPI_HAVE_ATOMIC_CMPSET_32
   case 4:
      ompi_atomic_sub_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_32 */

#if OMPI_HAVE_ATOMIC_CMPSET_64 
   case 8:
      ompi_atomic_sub_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OMPI_HAVE_ATOMIC_CMPSET_64 */
   default:
      /* This should never happen, so deliberately cause a seg fault
         for corefile analysis */
      *(int*)(0) = 0;
   }
}

#endif /* (OMPI_HAVE_ATOMIC_CMPSET_32 || OMPI_HAVE_ATOMIC_CMPSET_64) */


/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/
#if OMPI_HAVE_ATOMIC_SPINLOCKS
/* 
 * Lock initialization function. It set the lock to UNLOCKED.
 */
static inline void
ompi_atomic_init( ompi_lock_t* lock, int value )
{
   lock->u.lock = value;
}


static inline int
ompi_atomic_trylock(ompi_lock_t *lock)
{
   return ompi_atomic_cmpset_acq( &(lock->u.lock),
                                  OMPI_ATOMIC_UNLOCKED, OMPI_ATOMIC_LOCKED);
}


static inline void
ompi_atomic_lock(ompi_lock_t *lock)
{
   while( !ompi_atomic_cmpset_acq( &(lock->u.lock),
                                  OMPI_ATOMIC_UNLOCKED, OMPI_ATOMIC_LOCKED) ) {
      while (lock->u.lock == OMPI_ATOMIC_LOCKED) {
         /* spin */ ;
      }
   }
}


static inline void
ompi_atomic_unlock(ompi_lock_t *lock)
{
    /*
   ompi_atomic_cmpset_rel( &(lock->u.lock),
                           OMPI_ATOMIC_LOCKED, OMPI_ATOMIC_UNLOCKED);
                           */
   lock->u.lock=OMPI_ATOMIC_UNLOCKED;
}

#endif /* OMPI_HAVE_ATOMIC_SPINLOCKS */
