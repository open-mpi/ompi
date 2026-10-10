/*
 * Copyright (c) 2021-2026 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_XHC_ATOMIC_EXPORT_H
#define MCA_COLL_XHC_ATOMIC_EXPORT_H

#include <stdint.h>

#if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
    #include <cpuid.h>
#endif

#include "opal/sys/atomic.h"

// ----------------------------------------

#define IS_SIG_ATOMIC_X_BITS(x) \
    (SIG_ATOMIC_MAX == INT ## x ## _MAX) || (SIG_ATOMIC_MAX == UINT ## x ## _MAX)

/* If xf_sig_t is ever changed to be signed, make sure
 * to also adjust the comparisons in CHECK_FLAG(). */
#if IS_SIG_ATOMIC_X_BITS(64)
    typedef uint64_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(32)
    typedef uint32_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(16)
    typedef uint16_t xf_sig_t;
#elif IS_SIG_ATOMIC_X_BITS(8)
    typedef uint8_t xf_sig_t;
#endif

typedef size_t __attribute__((aligned(SIZEOF_SIZE_T))) xf_size_t;

// ----------------------------------------

#define xhc_atomic_rmb opal_atomic_rmb
#define xhc_atomic_wmb opal_atomic_wmb
#define xhc_atomic_fmb opal_atomic_mb

// https://github.com/open-mpi/ompi/issues/9722

#if OPAL_USE_GCC_BUILTIN_ATOMICS || OPAL_USE_C11_ATOMICS
    #define xhc_atomic_load_size_t(addr) __atomic_load_n(addr, __ATOMIC_RELAXED)
    #define xhc_atomic_store_size_t(addr, val) __atomic_store_n(addr, val, __ATOMIC_RELAXED)
#else
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

// Prefetchw's effect appears to be 'stronger' on Intel
static inline bool xhc_has_prefetchw_strong(void) {
    static bool strong = false;

    #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
        #define INTEL_EBX ('G' | ('e'<<8) | ('n'<<16) | ('u'<<24))
        #define INTEL_EDX ('i' | ('n'<<8) | ('e'<<16) | ('I'<<24))
        #define INTEL_ECX ('n' | ('t'<<8) | ('e'<<16) | ('l'<<24))

        unsigned int eax, ebx, ecx, edx;

        if(__get_cpuid(0, &eax, &ebx, &ecx, &edx)
            && INTEL_EBX == ebx && INTEL_ECX == ecx && INTEL_EDX == edx)
        {
            strong = true;
        }
    #endif

    return strong;
}

static inline void xhc_prefetchw(void *addr, size_t len, int target) {
    for(char *p = (char *) ((uintptr_t) addr & ~63);
            p < (char *) addr + len; p += 64) {

        switch(target) {
            case 0:
            case 1: // L1 cache
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                __asm__ __volatile__("prefetchw %0" :: "m" (*p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl1keep, %a0" :: "p" (p));
            #endif
                break;

            case 2: // L2 cache
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                __asm__ __volatile__("prefetchwt1 %0" :: "m" (*p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl2keep, %a0" :: "p" (p));
            #endif
                break;

            case 3: // L3 cache
            default:
            #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
                // no such thing as a 'prefetchwt2'
                __asm__ __volatile__("prefetchwt1 %0" :: "m" (*p));
            #elif defined(PLATFORM_ARCH_AARCH64)
                __asm__ __volatile__("prfm pstl3keep, %a0" :: "p" (p));
            #endif
                break;
        }
    }
}

static inline bool xhc_has_cldemote(void) {
    static bool supported = false;

    #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
        static bool init = false;

        if(!init) {
            unsigned int eax, ebx, ecx, edx;

            if(__get_cpuid_count(0x07, 0, &eax, &ebx, &ecx, &edx)) {
                supported = (ecx & (1 << 25));
            }

            init = true;
        }
    #endif

    return supported;
}

static inline void xhc_cldemote(void *addr, size_t len) {
    #if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
        if(xhc_has_cldemote()) {
            for(char *p = (char *) ((uintptr_t) addr & ~63);
                p < (char *) addr + len; p += 64)
            {
                __asm__ __volatile__ ("cldemote %0" :: "m" (*p) : "memory");
            }
        }
    #endif
}

#endif
