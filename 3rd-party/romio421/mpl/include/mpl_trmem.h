/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_TRMEM_H_INCLUDED
#define MPL_TRMEM_H_INCLUDED

#include <stdlib.h>

/* Sometime we have memory allocated from external library but requires
 * us to free. Use MPL_external_free for these cases.
*/
MPL_STATIC_INLINE_PREFIX void MPL_external_free(void *buf)
{
    free(buf);
}

#if defined MPL_NEEDS_STRDUP_DECL && !defined strdup
extern char *strdup(const char *);
#endif /* MPL_NEEDS_STRDUP_DECL */

#if defined(MPL_USE_MEMORY_TRACING)
#define MPL_strdup(a)    MPL_trstrdup(a,__LINE__,__FILE__)
#elif defined(MPL_HAVE_STRDUP)
#define MPL_strdup strdup
#else
char *MPL_strdup(const char *str);
#endif /* defined(MPL_USE_MEMORY_TRACING) || defined(MPL_HAVE_STRDUP) */

#if defined MPL_NEEDS_ALIGNED_ALLOC_DECL
extern void *aligned_alloc(size_t alignment, size_t size);
#endif

/* This list should match the array in mpl_trmem.c */
typedef enum {
    MPL_MEM_ADDRESS,            /* Address information */
    MPL_MEM_OBJECT,             /* General MPI Objects */
    MPL_MEM_COMM,               /* Communicators */
    MPL_MEM_GROUP,              /* Groups */
    MPL_MEM_STRINGS,            /* String buffers */
    MPL_MEM_RMA,                /* RMA data transfers, windows, etc. */
    MPL_MEM_BUFFER,             /* Internal buffers for data transfers */
    MPL_MEM_SHM,                /* Shared memory windows, buffers, etc. */
    MPL_MEM_THREAD,             /* Threading information, locks, etc. */
    MPL_MEM_DYNAMIC,            /* Dynamic process related information */
    MPL_MEM_IO,                 /* MPI I/O related objects */
    MPL_MEM_GREQ,               /* Generalized requests */
    MPL_MEM_DATATYPE,           /* MPI datatypes and related structures */
    MPL_MEM_MPIT,               /* MPI_T structures */
    MPL_MEM_DEBUG,              /* Data for the debugging information */
    MPL_MEM_PM,                 /* Data for process managers */
    MPL_MEM_COLL,               /* Memory related to collective operations */
    MPL_MEM_USER,               /* User memory allocations */
    MPL_MEM_OTHER,              /* Other small memory allocations */
    MPL_MAX_MEMORY_CLASS
} MPL_memory_class;

typedef struct {
    long max_allocated_mem;     /* The maximum amount of memory allocated at one time */
    long curr_allocated_mem;    /* The current amount of memory allocated at one time */
    long total_allocated_mem;   /* The total amount of memory allocated */
    long num_allocations;       /* The total number of alloations */
} MPL_memory_allocation_t;

/*M
  MPL_malloc - Allocate memory

  Synopsis:
.vb
  void *MPL_malloc(size_t len, MPL_memory_allocation_t type)
.ve

  Input Parameter:
. len - Length of memory to allocate in bytes
. type - The category of memory being allocated

  Return Value:
  Pointer to allocated memory, or null if memory could not be allocated.

  Notes:
  This routine will often be implemented as the simple macro
.vb
  #define MPL_malloc(n) malloc(n)
.ve
  However, it can also be defined as
.vb
  #define MPL_malloc(n) MPL_trmalloc(n,__LINE__,__FILE__)
.ve
  where 'MPL_trmalloc' is a tracing version of 'malloc' that is included with
  MPICH.

  Module:
  Utility
  M*/

/*M
  MPL_calloc - Allocate memory that is initialized to zero.

  Synopsis:
.vb
    void *MPL_calloc(size_t nelm, size_t elsize)
.ve

  Input Parameters:
+ nelm - Number of elements to allocate
- elsize - Size of each element.
. type - The category of memory being allocated

  Notes:
  Like 'MPL_malloc' and 'MPL_free', this will often be implemented as a
  macro but may use 'MPL_trcalloc' to provide a tracing version.

  Module:
  Utility
  M*/

/*M
  MPL_free - Free memory

  Synopsis:
.vb
   void MPL_free(void *ptr)
.ve

  Input Parameter:
. ptr - Pointer to memory to be freed.  This memory must have been allocated
  with 'MPL_malloc'.

  Notes:
  This routine will often be implemented as the simple macro
.vb
  #define MPL_free(n) free(n)
.ve
  However, it can also be defined as
.vb
  #define MPL_free(n) MPL_trfree(n,__LINE__,__FILE__)
.ve
  where 'MPL_trfree' is a tracing version of 'free' that is included with
  MPICH.

  Module:
  Utility
  M*/

/*M
  MPL_mmap - Map memory

  Synopsis:
.vb
  void *MPL_mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
.ve

  Input Parameters:
. addr - Starting address for the new mapping
. length - Length of the mapping
. prot - Desired memory protection of the mapping
. flags - Determines whether updates to the mapping are visible to other
  processes mapping the same region, and whether updates are carried
  through to the underlying file.
. fd - The file backing the memory region
. offset - Offset into the file
. type - The category of memory being allocated

  Notes:
  This routine will often be implemented as the simple macro
.vb
  #define MPL_mmap(a,b,c,d,e,f) mmap(a,b,c,d,e,f)
.ve
  However, it can also be defined as
.vb
  #define MPL_mmap(a,b,c,d,e,f) MPL_trmmap(a,b,c,d,e,f,__LINE__,__FILE__)
.ve
  where 'MPL_trmmap' is a tracing version of 'mmap' that is included with
  MPICH.

  Module:
  Utility
  M*/

/*M
  MPL_munmap - Unmapmemory

  Synopsis:
.vb
  void *MPL_munmap(void *addr, size_t length)
.ve

  Input Parameters:
. addr - Starting address for the new mapping
. length - Length of the mapping

  Notes:
  This routine will often be implemented as the simple macro
.vb
  #define MPL_munmap(a,b) munmap(a,b)
.ve
  However, it can also be defined as
.vb
  #define MPL_munmap(a,b) MPL_trmunmap(a,b,__LINE__,__FILE__)
.ve
  where 'MPL_trmunmap' is a tracing version of 'munmap' that is included with
  MPICH.

  Module:
  Utility
  M*/

/*M
  MPL_aligned_alloc - Allocate aligned memory

  Synopsis:
.vb
  void *MPL_aligned_alloc(size_t alignment, size_t size)
.ve

  Input Parameters:
. alignment - Returned address will be multiple of this value.
              It must be power of two and multiple of sizeof(void *).
. length - Length of the memory to allocate

  Notes:
  This routine will often be implemented as a call to posix_memalign(3),
  However, it can also be defined as
.vb
  #define MPL_aligned_alloc(a,b,c) MPL_traligned_alloc(a,b,c,__LINE__,__FILE__)
.ve
  where 'MPL_traligned_alloc' is a tracing version of 'aligned_alloc' that is included with
  MPICH.

  Module:
  Utility
  M*/

#include "string.h"

#ifdef HAVE_MM256_STREAM_SI256
#include <immintrin.h>

static inline void MPL_Memcpy_stream(void *dest, const void *src, size_t n)
{
    /* Anything less than 256 bytes is not worth optimizing */
    if (n <= 256) {
        memcpy(dest, src, n);
        return;
    }

    char *d = (char *) dest;
    const char *s = (const char *) src;

    /* Copy the first 63 bytes or less (if the address isn't 64-byte aligned) using a regular memcpy
     * to make the rest faster */
    if (((uintptr_t) d) & 63) {
        const uintptr_t t = 64 - (((uintptr_t) d) & 63);
        memcpy(d, s, t);
        d += t;
        s += t;
        n -= t;
    }

    /* Copy 256 bytes at a time by unrolling a series of 32-byte streaming copies. */
    while (n >= 256) {
        __m256i ymm0 = _mm256_loadu_si256((__m256i const *) (s + (32 * 0)));
        __m256i ymm1 = _mm256_loadu_si256((__m256i const *) (s + (32 * 1)));
        __m256i ymm2 = _mm256_loadu_si256((__m256i const *) (s + (32 * 2)));
        __m256i ymm3 = _mm256_loadu_si256((__m256i const *) (s + (32 * 3)));
        __m256i ymm4 = _mm256_loadu_si256((__m256i const *) (s + (32 * 4)));
        __m256i ymm5 = _mm256_loadu_si256((__m256i const *) (s + (32 * 5)));
        __m256i ymm6 = _mm256_loadu_si256((__m256i const *) (s + (32 * 6)));
        __m256i ymm7 = _mm256_loadu_si256((__m256i const *) (s + (32 * 7)));
        _mm256_stream_si256((__m256i *) (d + (32 * 0)), ymm0);
        _mm256_stream_si256((__m256i *) (d + (32 * 1)), ymm1);
        _mm256_stream_si256((__m256i *) (d + (32 * 2)), ymm2);
        _mm256_stream_si256((__m256i *) (d + (32 * 3)), ymm3);
        _mm256_stream_si256((__m256i *) (d + (32 * 4)), ymm4);
        _mm256_stream_si256((__m256i *) (d + (32 * 5)), ymm5);
        _mm256_stream_si256((__m256i *) (d + (32 * 6)), ymm6);
        _mm256_stream_si256((__m256i *) (d + (32 * 7)), ymm7);
        d += 256;
        s += 256;
        n -= 256;
    }

    /* Once there are fewer than 256 bytes left to be copied, copy a chunk of 128 and 64 bytes (if
     * applicable). */
    if (n >= 128) {
        __m256i ymm0 = _mm256_loadu_si256((__m256i const *) (s + (32 * 0)));
        __m256i ymm1 = _mm256_loadu_si256((__m256i const *) (s + (32 * 1)));
        __m256i ymm2 = _mm256_loadu_si256((__m256i const *) (s + (32 * 2)));
        __m256i ymm3 = _mm256_loadu_si256((__m256i const *) (s + (32 * 3)));
        _mm256_stream_si256((__m256i *) (d + (32 * 0)), ymm0);
        _mm256_stream_si256((__m256i *) (d + (32 * 1)), ymm1);
        _mm256_stream_si256((__m256i *) (d + (32 * 2)), ymm2);
        _mm256_stream_si256((__m256i *) (d + (32 * 3)), ymm3);
        d += 128;
        s += 128;
        n -= 128;
    }

    if (n >= 64) {
        __m256i ymm0 = _mm256_loadu_si256((__m256i const *) (s + (32 * 0)));
        __m256i ymm1 = _mm256_loadu_si256((__m256i const *) (s + (32 * 1)));
        _mm256_stream_si256((__m256i *) (d + (32 * 0)), ymm0);
        _mm256_stream_si256((__m256i *) (d + (32 * 1)), ymm1);
        d += 64;
        s += 64;
        n -= 64;
    }

    /* If there is any data left, copy it using a regular memcpy */
    if (n > 0) {
        memcpy(d, s, (n & 63));
    }

    /* A memory fence is required after the streaming stores above. */
    _mm_sfence();
}

#else

static inline void MPL_Memcpy_stream(void *dest, const void *src, size_t n)
{
    memcpy(dest, src, n);
}

#endif

#ifdef MPL_USE_MEMORY_TRACING

/* Define these as invalid C to catch their use in the code.
 * The ::: should cause the compiler to choke; the string will give the explanation
 */
#define malloc(a)         'Error use MPL_malloc' :::
#define calloc(a,b)       'Error use MPL_calloc' :::
#define free(a)           'Error use MPL_free'   :::
#define realloc(a)        'Error use MPL_realloc' :::
/* These two functions can't be guarded because we use #include <sys/mman.h>
 * throughout the code to be able to use other symbols in that header file.
 * Because we include that header directly, we bypass this guard and cause
 * compile problems.
 * #define mmap(a,b,c,d,e,f) 'Error use MPL_mmap'   :::
 * #define munmap(a,b)       'Error use MPL_munmap' :::
 */
#undef strdup   /* in case strdup is a macro */
#define strdup(a)         'Error use MPL_strdup' :::

#define MPL_malloc(a,b)    MPL_trmalloc((a),(b),__LINE__,__FILE__)
#define MPL_calloc(a,b,c)    MPL_trcalloc((a),(b),(c),__LINE__,__FILE__)
#define MPL_free(a)      MPL_trfree(a,__LINE__,__FILE__)
#define MPL_realloc(a,b,c)    MPL_trrealloc((a),(b),(c),__LINE__,__FILE__)
#define MPL_mmap(a,b,c,d,e,f,g) MPL_trmmap((a),(b),(c),(d),(e),(f),(g),__LINE__,__FILE__)
#define MPL_munmap(a,b,c) MPL_trmunmap((a),(b),(c),__LINE__,__FILE__)

/* Directly call malloc/calloc/realloc, so those memory segments are not
 * checked in MPI_Finalize() */
void *MPL_direct_malloc(size_t size);
void *MPL_direct_calloc(size_t nmemb, size_t size);
void *MPL_direct_realloc(void *ptr, size_t size);
char *MPL_direct_strdup(const char *s);
void MPL_direct_free(void *ptr);

#ifdef MPL_DEFINE_ALIGNED_ALLOC
#define MPL_aligned_alloc(a,b,c) MPL_traligned_alloc((a),(b),(c),__LINE__,__FILE__)
#endif /* #ifdef MPL_DEFINE_ALIGNED_ALLOC */

#else /* MPL_USE_MEMORY_TRACING */
/* No memory tracing; just use native functions */
/* size_t allows for larger values than PTRDIFF_MAX.  GCC throws a
 * warning if we pass a signed integer to MPL_malloc and friends,
 * which when typecast to size_t becomes a very large number, saying
 * that the max size exceeds that of PTRDIFF_MAX. */
static inline void *MPL_malloc(size_t size, MPL_memory_class memclass)
{
    if (size <= PTRDIFF_MAX) {
        return malloc(size);
    } else {
        return NULL;
    }
}

static inline void *MPL_calloc(size_t nmemb, size_t size, MPL_memory_class memclass)
{
    if (size <= PTRDIFF_MAX) {
        return calloc(nmemb, size);
    } else {
        return NULL;
    }
}

static inline void *MPL_realloc(void *ptr, size_t size, MPL_memory_class memclass)
{
    if (size <= PTRDIFF_MAX) {
        return realloc(ptr, size);
    } else {
        return NULL;
    }
}

#define MPL_free(a)      free((void *)(a))
#define MPL_mmap(a,b,c,d,e,f,g) mmap((void *)(a),(size_t)(b),(int)(c),(int)(d),(int)(e),(off_t)(f))
#define MPL_munmap(a,b,c)  munmap((void *)(a),(size_t)(b))

#define MPL_direct_malloc   malloc
#define MPL_direct_calloc   calloc
#define MPL_direct_realloc  realloc
#define MPL_direct_strdup   strdup
#define MPL_direct_free     free

#ifdef MPL_DEFINE_ALIGNED_ALLOC
MPL_STATIC_INLINE_PREFIX void *MPL_aligned_alloc(size_t alignment, size_t size,
                                                 MPL_memory_class class)
{
#if defined (MPL_HAVE_ALIGNED_ALLOC)
    return aligned_alloc(alignment, size);
#elif defined (MPL_HAVE_POSIX_MEMALIGN)
    void *ptr;
    int ret;

    ret = posix_memalign(&ptr, alignment, size);
    if (ret != 0)
        return NULL;
    return ptr;
#else
#error "MPL_DEFINE_ALIGNED_ALLOC defined but no underlying support function found - should not reach here."
#endif
}
#endif /* #ifdef MPL_DEFINE_ALIGNED_ALLOC */

#endif /* MPL_USE_MEMORY_TRACING */


/* --------------------------------------------------------------------
 * MPL memory alignment union
 * The MPL_mem_alignment_t union is used to help internal structures or buffers
 * follow the alignment rules for all predefined datatypes. */

#if 0   /* sample usage : wrap up sample_t structure. */
typedef union {
    sample_t var;
    MPL_mem_alignment_t alignment;
} aligned_sample_t;
#endif

/* Union including all C types that possibly require the largest alignment bytes.
 * Note that we do not need other Fortran/CXX predefined types because all of them
 * are internally translated to appropriate C types. */
typedef union {
    /* Integer types.
     * We only list signed types here, because an unsigned type always require
     * the same alignment as its signed version. Fix me if this theory is wrong.*/
    long long_align;
#ifdef HAVE_LONG_LONG
    long long ll_align;
#endif
#ifdef HAVE_INT32_T
    int32_t int32_t_align;
#endif
#ifdef HAVE_INT64_T
    int64_t int64_t_align;
#endif

    /* Logical type */
#ifdef HAVE__BOOL
    _Bool bool_align;
#endif

    /* Floating-point types */
    double double_align;
#ifdef HAVE_LONG_DOUBLE
    long double ld_align;
#endif

    /* Complex types */
#ifdef HAVE_DOUBLE__COMPLEX
    double _Complex d_complex_align;
#endif
#ifdef HAVE_LONG_DOUBLE__COMPLEX
    long double _Complex ld_complex_align;
#endif
    /* MPICH handles Fortran/CXX complex types as structure (see src/include/oputil.h).
     * Because some platform may have special alignment rule for structures,
     * we include them as well. */
    struct {
        double re;
        double im;
    } mpl_d_complex_align;
#ifdef HAVE_LONG_DOUBLE
    struct {
        long double re;
        long double im;
    } mpl_ld_complex_align;
#endif
} MPL_mem_alignment_t;

/* END of MPL memory alignment union
 * -------------------------------------------------------------------- */

/* FIXME: Consider an option of specifying __attribute__((malloc)) for
   gcc - this lets gcc-style compilers know that the returned pointer
   does not alias any pointer prior to the call.
 */
void MPL_trinit(void);
void MPL_trconfig(int, int *);
void *MPL_trmalloc(size_t, MPL_memory_class, int, const char[]);
void MPL_trfree(void *, int, const char[]);
int MPL_trvalid(const char[]);
int MPL_trvalid2(const char[], int, const char[]);
void *MPL_trcalloc(size_t, size_t, MPL_memory_class, int, const char[]);
#include <sys/types.h>
void *MPL_trmmap(void *, size_t, int, int, int, off_t, MPL_memory_class, int, const char[]);
void MPL_trmunmap(void *, size_t, MPL_memory_class, int, const char[]);
void *MPL_trrealloc(void *, size_t, MPL_memory_class, int, const char[]);
void *MPL_trstrdup(const char *, int, const char[]);
void *MPL_traligned_alloc(size_t alignment, size_t length, MPL_memory_class, int, const char[]);

/* Make sure that FILE is defined */
#include <stdio.h>
void MPL_trdump(FILE *, int);
void MPL_trcategorydump(FILE * fp);

char *MPL_strdup_no_spaces(const char *str);

#endif /* MPL_TRMEM_H_INCLUDED */
