/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <stdlib.h>

#include "src/runtime/prte_globals.h"
#include "src/util/malloc.h"
#include "src/util/pmix_output.h"

/*
 * Undefine "malloc" and "free"
 */

#if defined(malloc)
#    undef malloc
#endif
#if defined(calloc)
#    undef calloc
#endif
#if defined(free)
#    undef free
#endif
#if defined(realloc)
#    undef realloc
#endif

/*
 * Public variables
 */
int prte_malloc_debug_level = PRTE_MALLOC_DEBUG_LEVEL;
int prte_malloc_output = -1;

/*
 * Private variables
 */
#if PRTE_ENABLE_DEBUG
static pmix_output_stream_t malloc_stream;
#endif

#if PRTE_ENABLE_DEBUG
/*
 * Finalize the malloc debug interface
 */
void prte_malloc_finalize(void)
{
    if (-1 != prte_malloc_output) {
        pmix_output_close(prte_malloc_output);
        prte_malloc_output = -1;
        PMIX_DESTRUCT(&malloc_stream);
    }
}

/*
 * Initialize the malloc debug interface
 */
void prte_malloc_init(void)
{
    PMIX_CONSTRUCT(&malloc_stream, pmix_output_stream_t);
    malloc_stream.lds_is_debugging = true;
    malloc_stream.lds_verbose_level = 5;
    malloc_stream.lds_prefix = "malloc debug: ";
    malloc_stream.lds_want_stderr = true;
    prte_malloc_output = pmix_output_open(&malloc_stream);
}
#else
void prte_malloc_init(void)
{
}
void prte_malloc_finalize(void)
{
}
#endif /* PRTE_ENABLE_DEBUG */

/*
 * Debug version of malloc
 */
void *prte_malloc(size_t size, const char *file, int line)
{
    void *addr;
#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 1) {
        if (size <= 0) {
            pmix_output(prte_malloc_output, "Request for %ld bytes (%s, %d)", (long) size, file,
                        line);
        }
    }
#endif /* PRTE_ENABLE_DEBUG */

    addr = malloc(size);

#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 0) {
        if (NULL == addr) {
            pmix_output(prte_malloc_output, "Request for %ld bytes failed (%s, %d)", (long) size,
                        file, line);
        }
    }
#endif /* PRTE_ENABLE_DEBUG */
    return addr;
}

/*
 * Debug version of calloc
 */
void *prte_calloc(size_t nmembers, size_t size, const char *file, int line)
{
    void *addr;
#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 1) {
        if (size <= 0) {
            pmix_output(prte_malloc_output, "Request for %ld zeroed elements of size %ld (%s, %d)",
                        (long) nmembers, (long) size, file, line);
        }
    }
#endif /* PRTE_ENABLE_DEBUG */
    addr = calloc(nmembers, size);
#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 0) {
        if (NULL == addr) {
            pmix_output(prte_malloc_output,
                        "Request for %ld zeroed elements of size %ld failed (%s, %d)",
                        (long) nmembers, (long) size, file, line);
        }
    }
#endif /* PRTE_ENABLE_DEBUG */
    return addr;
}

/*
 * Debug version of realloc
 */
void *prte_realloc(void *ptr, size_t size, const char *file, int line)
{
    void *addr;
#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 1) {
        if (size <= 0) {
            if (NULL == ptr) {
                pmix_output(prte_malloc_output, "Realloc NULL for %ld bytes (%s, %d)", (long) size,
                            file, line);
            } else {
                pmix_output(prte_malloc_output, "Realloc %p for %ld bytes (%s, %d)", ptr,
                            (long) size, file, line);
            }
        }
    }
#endif /* PRTE_ENABLE_DEBUG */
    addr = realloc(ptr, size);
#if PRTE_ENABLE_DEBUG
    if (prte_malloc_debug_level > 0) {
        if (NULL == addr) {
            pmix_output(prte_malloc_output, "Realloc %p for %ld bytes failed (%s, %d)", ptr,
                        (long) size, file, line);
        }
    }
#endif /* PRTE_ENABLE_DEBUG */
    return addr;
}

/*
 * Debug version of free
 */
void prte_free(void *addr, const char *file, int line)
{
    PRTE_HIDE_UNUSED_PARAMS(file, line);
    free(addr);
}

void prte_malloc_debug(int level)
{
#if PRTE_ENABLE_DEBUG
    prte_malloc_debug_level = level;
#endif /* PRTE_ENABLE_DEBUG */
}
