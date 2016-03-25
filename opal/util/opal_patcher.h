/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * ******** ADD IBM COPYRIGHT HERE ******
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_PATCHER_H)
#define OPAL_PATCHER_H

#include "opal_config.h"

// Any function being patched in must use SYMBOLPATCH_BEGIN at the top,
// and SYMBOLPATCH_END before it returns (this is just for PPC).

#if (defined(__PPC64__) || defined(__powerpc64__) || defined(__PPC__)) && defined(OPAL_GCC_INLINE_ASSEMBLY)

// special processing for ppc64 to save and restore TOC (r2)
// Reference: "64-bit PowerPC ELF Application Binary Interface Supplement 1.9"
#define OPAL_PATCHER_BEGIN \
    unsigned long toc_save; \
    asm volatile ("std 2, %0" : "=m" (toc_save)); \
    asm volatile ("nop; nop; nop; nop; nop");
#define OPAL_PATCHER_END \
    asm volatile ("ld  2, %0" : : "m" (toc_save));

#else // !__PPC64__

#define OPAL_PATCHER_BEGIN
#define OPAL_PATCHER_END

#endif


/**
 * Patch all instances of calls to a function
 *
 * @param[in] func_symbol_name  Name of symbol to patch
 * @param[in] func_new_addr     Pointer to new function to call
 *
 * @returns OPAL_SUCCESS on success
 * @returns OPAL_ERR_NOT_AVAILABLE if symbol patching is not supported on this
 *          platform.
 *
 * This function patches any calls to the named symbol with a new function
 * address. Any function that is passed into func_new_addr MUST begin with
 * OPAL_PATCHER_BEGIN and end with OPAL_PATCHER_END.
 */
int opal_patch_symbol (const char *func_symbol_name, uintptr_t func_new_addr);

/**
 * Check if symbol patching is available
 */
bool opal_patch_supported (void);

#endif /* !defined(OPAL_PATCHER_H) */
