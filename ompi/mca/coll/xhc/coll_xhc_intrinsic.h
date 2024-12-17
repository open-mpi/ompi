/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_XHC_ATOMIC_EXPORT_H
#define MCA_COLL_XHC_ATOMIC_EXPORT_H

#include <stdint.h>
#include "opal/sys/atomic.h"

// ----------------------------------------

#define IS_SIG_ATOMIC_X_BITS(x) \
    (SIG_ATOMIC_MAX == INT ## x ## _MAX) || (SIG_ATOMIC_MAX == UINT ## x ## _MAX)

// If xf_sig_t is ever re-defined to be signed,
 // CHECK_FLAGS()'s comparisons must be adjusted
#if IS_SIG_ATOMIC_X_BITS(64)
    typedef uint64_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(32)
    typedef uint32_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(16)
    typedef uint16_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(8)
    typedef uint8_t xf_sig_t;
#endif

typedef int __attribute__((aligned(SIZEOF_INT))) xf_int_t;
typedef size_t __attribute__((aligned(SIZEOF_SIZE_T))) xf_size_t;

// ----------------------------------------

#define xhc_atomic_rmb opal_atomic_rmb
#define xhc_atomic_wmb opal_atomic_wmb
#define xhc_atomic_fmb opal_atomic_mb

// https://github.com/open-mpi/ompi/issues/9722

#if OPAL_USE_GCC_BUILTIN_ATOMICS || OPAL_USE_C11_ATOMICS
    #define xhc_atomic_load_int(addr) __atomic_load_n(addr, __ATOMIC_RELAXED)
    #define xhc_atomic_store_int(addr, val) __atomic_store_n(addr, val, __ATOMIC_RELAXED)

    #define xhc_atomic_load_size_t(addr) __atomic_load_n(addr, __ATOMIC_RELAXED)
    #define xhc_atomic_store_size_t(addr, val) __atomic_store_n(addr, val, __ATOMIC_RELAXED)
#else
    #define xhc_atomic_load_int(addr) (*(addr))
    #define xhc_atomic_store_int(addr, val) (*(addr) = (val))

    #define xhc_atomic_load_size_t(addr) (*(addr))
    #define xhc_atomic_store_size_t(addr, val) (*(addr) = (val))

    #warning "GCC or the C11 atomics backend was not found. XHC might not function correctly"
/* #else
    #error "XHC atomics do not yet work without the GCC or the C11 backend" */
#endif


// If/when opal atomic load/store size_t is added

/* #define xhc_atomic_load_size_t(addr) \
    opal_atomic_load_size_t ((opal_atomic_size_t *) addr)
#define xhc_atomic_store_size_t(addr, val) \
    opal_atomic_store_size_t ((opal_atomic_size_t *) addr, val) */


// If/when opal atomic load/store is added, and if opal atomic load/store int is not

/* #if 4 == SIZEOF_INT
    #define xhc_atomic_load_int(addr) opal_atomic_load_32 ((opal_atomic_int32_t *) addr)
    #define xhc_atomic_store_int(addr, val) opal_atomic_store_32 ((opal_atomic_int32_t *) addr, val)
#elif 8 == SIZEOF_INT
    #define xhc_atomic_load_int(addr) opal_atomic_load_64 ((opal_atomic_int64_t *) addr)
    #define xhc_atomic_store_int(addr, val) opal_atomic_store_64 ((opal_atomic_int64_t *) addr, val)
#else
    #error "Unsupported int size"
#endif */


// If/when opal atomic load/store is added, and if opal atomic load/store size_t is not

/* #if 4 == SIZEOF_SIZE_T
    #define xhc_atomic_load_size_t(addr) opal_atomic_load_32 ((opal_atomic_int32_t *) addr)
    #define xhc_atomic_store_size_t(addr, val) opal_atomic_store_32 ((opal_atomic_int32_t *) addr, val)
#elif 8 == SIZEOF_SIZE_T
    #define xhc_atomic_load_size_t(addr) opal_atomic_load_64 ((opal_atomic_int64_t *) addr)
    #define xhc_atomic_store_size_t(addr, val) opal_atomic_store_64 ((opal_atomic_int64_t *) addr, val)
#else
    #error "Unsupported size_t size"
#endif */

static inline bool xhc_atomic_cmpxchg_strong_relaxed(volatile xf_sig_t *addr,
        xf_sig_t *oldval, xf_sig_t newval) {

    #if OPAL_USE_GCC_BUILTIN_ATOMICS || OPAL_USE_C11_ATOMICS
        return __atomic_compare_exchange_n(addr, oldval, newval,
            false, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    #else
        #if IS_SIG_ATOMIC_X_BITS(32)
            return opal_atomic_compare_exchange_strong_32(addr, oldval, newval);
        #elif IS_SIG_ATOMIC_X_BITS(64)
            return opal_atomic_compare_exchange_strong_64(addr, oldval, newval);
        #else
            #error "Unsupported sig_atomic_t size"
        #endif
    #endif
}

// ----------------------------------------

static inline void xhc_prefetchw(void *addr, size_t len, int target) {
    for(char *p = (char *) ((uintptr_t) addr & ~63);
            p < (char *) addr + len; p += 64) {

        switch(target) {
            case 0:
            case 1: // L1 cache
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                __asm__ __volatile__("prefetchw (%1)" : : "i" (0), "r" (p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl1keep, %a0\n" : : "p" (p));
            #endif
                break;

            case 2: // L2 cache
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                __asm__ __volatile__("prefetchwt1 (%1)" : : "i" (0), "r" (p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl2keep, %a0\n" : : "p" (p));
            #endif
                break;

            case 3: // L3 cache
            default:
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                // no such thing as a 'prefetchwt2'
                __asm__ __volatile__("prefetchwt1 (%1)" : : "i" (0), "r" (p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl3keep, %a0\n" : : "p" (p));
            #endif
                break;
        }
    }
}

#endif
