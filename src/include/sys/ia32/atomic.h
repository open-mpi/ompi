/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On ia32, we use cmpxchg.
 */


#ifdef HAVE_SMP
#define SMPLOCK "lock; "
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define SMPLOCK
#define MB()
#endif


static inline void ompi_atomic_mb(void)
{
    MB();
}


static inline void ompi_atomic_rmb(void)
{
    MB();
}


static inline void ompi_atomic_wmb(void)
{
    MB();
}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_32
static inline int ompi_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval,
                                        int32_t newval)
{
   unsigned char ret;
   __asm__ __volatile (
                       SMPLOCK "cmpxchgl %1,%2   \n\t"
                               "sete     %0      \n\t"
                       : "=qm" (ret)
                       : "q"(newval), "m"(*((volatile long*)addr)), "a"(oldval)
                       : "memory");
   
   return (int)ret;
}

#define ompi_atomic_cmpset_acq_32 ompi_atomic_cmpset_32
#define ompi_atomic_cmpset_rel_32 ompi_atomic_cmpset_32

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_64
typedef struct {
   uint32_t lo;
   uint32_t hi;
} lwords_t;

/* On Linux the EBX register is used by the shared libraries
 * to keep the global offset. In same time this register is 
 * required by the cmpxchg8b instruction (as an input parameter).
 * This conflict orce us to save the EBX before the cmpxchg8b 
 * and to restore it afterward.
 */
static inline int ompi_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval,
                                        int64_t newval)
{
   /* 
    * Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into
    * m64. Else, clear ZF and load m64 into EDX:EAX.
    */
   lwords_t *pnew = (lwords_t*)&newval;

#if 1
   int64_t prev;

   __asm__ __volatile__(
                        "push %%ebx           \n\t"
                        "movl %3, %%ebx       \n\t"
                        SMPLOCK "cmpxchg8b %4 \n\t"
                        "pop %%ebx            \n\t"
                        : "=A" (prev)
                        : "0" (oldval), "c" ((unsigned long)pnew->lo),
                          "r" ((unsigned long)pnew->hi), "m" (addr)
                        : "cc", "memory");
   return (prev == oldval);
#else
   unsigned char realized;

   lwords_t *pold = (lwords_t*)&oldval;

   __asm__ __volatile(
                      "push %%ebx            \n\t"
                      "movl %4, %%ebx        \n\t"
                      SMPLOCK "cmpxchg8b %1  \n\t"
                      "sete      %0          \n\t"
                      "pop %%ebx             \n\t"
                      : "=qm" (realized)
                      : "m"(*((volatile long*)addr)), "a"(pold->hi), "d"(pold->lo),
                        "r"(pnew->hi), "c"(pnew->lo)
                      : "cc", "memory" );
   return realized;
#endif
}

#define ompi_atomic_cmpset_acq_64 ompi_atomic_cmpset_64
#define ompi_atomic_cmpset_rel_64 ompi_atomic_cmpset_64

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_32
/**
 * atomic_add - add integer to atomic variable
 * @i: integer value to add
 * @v: pointer of type int
 *
 * Atomically adds @i to @v.
 */
static inline int ompi_atomic_add_32(volatile int32_t* v, int i)
{
   __asm__ __volatile__(
                        SMPLOCK "addl %1,%0"
                        :"=m" (*v)
                        :"ir" (i), "m" (*v));
   return (*v);  /* should be an atomic operation */
}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_32
/**
 * atomic_sub - subtract the atomic variable
 * @i: integer value to subtract
 * @v: pointer of type int
 *
 * Atomically subtracts @i from @v.
 */
static inline int ompi_atomic_sub_32(volatile int32_t* v, int i)
{
   __asm__ __volatile__(
                        SMPLOCK "subl %1,%0"
                        :"=m" (*v)
                        :"ir" (i), "m" (*v));
   return (*v);  /* should be an atomic operation */
}


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
