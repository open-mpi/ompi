/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for the opal_malloc / opal_calloc / opal_realloc / opal_free
 * family and opal_malloc_debug().
 *
 * NOTE: malloc.h explicitly states these are back-end functions intended to
 * be called with __FILE__ and __LINE__ by a front-end macro.  The tests call
 * them directly with those literal arguments, which is the documented
 * interface.
 *
 * NOTE: opal_malloc_debug() only changes observable state when
 * OPAL_ENABLE_DEBUG is defined; in non-debug builds the call is a no-op.
 * We therefore only test that allocations continue to succeed after calling
 * it, not that it alters any internal diagnostic output.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/malloc.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <string.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/* Canary bytes we write into allocated memory to verify access. */
#define CANARY_BYTE 0xAB
#define ALLOC_SIZE  128

static void test_malloc_basic(void)
{
    void *p = opal_malloc(ALLOC_SIZE, __FILE__, __LINE__);
    test_verify("opal_malloc returns non-NULL", NULL != p);
    if (NULL != p) {
        /* write and read back to prove the memory is usable */
        memset(p, CANARY_BYTE, ALLOC_SIZE);
        test_verify("opal_malloc memory is writable/readable",
                    (unsigned char) CANARY_BYTE == ((unsigned char *)p)[0]);
        test_verify("opal_malloc memory last byte ok",
                    (unsigned char) CANARY_BYTE == ((unsigned char *)p)[ALLOC_SIZE - 1]);
        opal_free(p, __FILE__, __LINE__);
    }
}

static void test_calloc_zeroes(void)
{
    size_t n = 16;
    size_t sz = sizeof(int);
    int i;
    int all_zero;

    void *p = opal_calloc(n, sz, __FILE__, __LINE__);
    test_verify("opal_calloc returns non-NULL", NULL != p);
    if (NULL != p) {
        all_zero = 1;
        for (i = 0; i < (int)(n * sz); ++i) {
            if (0 != ((unsigned char *)p)[i]) {
                all_zero = 0;
                break;
            }
        }
        test_verify("opal_calloc zeroes all bytes", all_zero);
        opal_free(p, __FILE__, __LINE__);
    }
}

static void test_realloc_grow(void)
{
    /* Allocate a small block, write a pattern, then grow it.
     * The leading bytes must be preserved by realloc. */
    size_t small = 8;
    size_t large = 256;
    void *p;
    void *q;
    int i;
    int prefix_ok;

    p = opal_malloc(small, __FILE__, __LINE__);
    test_verify("realloc_grow: initial malloc non-NULL", NULL != p);
    if (NULL == p) {
        return;
    }
    for (i = 0; i < (int)small; ++i) {
        ((unsigned char *)p)[i] = (unsigned char)(i + 1);
    }

    q = opal_realloc(p, large, __FILE__, __LINE__);
    test_verify("realloc_grow: returns non-NULL", NULL != q);
    if (NULL != q) {
        prefix_ok = 1;
        for (i = 0; i < (int)small; ++i) {
            if ((unsigned char)(i + 1) != ((unsigned char *)q)[i]) {
                prefix_ok = 0;
                break;
            }
        }
        test_verify("realloc_grow: old prefix bytes preserved", prefix_ok);
        opal_free(q, __FILE__, __LINE__);
    } else {
        /* p is still valid when realloc fails; free it */
        opal_free(p, __FILE__, __LINE__);
    }
}

static void test_realloc_shrink(void)
{
    size_t large = 512;
    size_t small = 4;
    void *p;
    void *q;
    int i;
    int prefix_ok;

    p = opal_malloc(large, __FILE__, __LINE__);
    test_verify("realloc_shrink: initial malloc non-NULL", NULL != p);
    if (NULL == p) {
        return;
    }
    for (i = 0; i < (int)large; ++i) {
        ((unsigned char *)p)[i] = 0xFF;
    }

    q = opal_realloc(p, small, __FILE__, __LINE__);
    test_verify("realloc_shrink: returns non-NULL", NULL != q);
    if (NULL != q) {
        /* first 'small' bytes must still be 0xFF */
        prefix_ok = 1;
        for (i = 0; i < (int)small; ++i) {
            if (0xFF != ((unsigned char *)q)[i]) {
                prefix_ok = 0;
                break;
            }
        }
        test_verify("realloc_shrink: retained bytes intact", prefix_ok);
        opal_free(q, __FILE__, __LINE__);
    } else {
        opal_free(p, __FILE__, __LINE__);
    }
}

/*
 * test_malloc_debug_does_not_break_alloc
 *
 * opal_malloc_debug() is a no-op in non-debug builds; in debug builds
 * it adjusts an internal diagnostic level.  In either case allocations
 * must succeed afterwards.
 */
static void test_malloc_debug_does_not_break_alloc(void)
{
    void *p;

    opal_malloc_debug(0);
    p = opal_malloc(64, __FILE__, __LINE__);
    test_verify("after opal_malloc_debug(0): malloc succeeds", NULL != p);
    if (NULL != p) {
        opal_free(p, __FILE__, __LINE__);
    }

    opal_malloc_debug(1);
    p = opal_malloc(64, __FILE__, __LINE__);
    test_verify("after opal_malloc_debug(1): malloc succeeds", NULL != p);
    if (NULL != p) {
        opal_free(p, __FILE__, __LINE__);
    }

    opal_malloc_debug(2);
    p = opal_malloc(64, __FILE__, __LINE__);
    test_verify("after opal_malloc_debug(2): malloc succeeds", NULL != p);
    if (NULL != p) {
        opal_free(p, __FILE__, __LINE__);
    }
}

static void test_free_null_is_safe(void)
{
    void *p;

    /* opal_free maps directly to free(); passing NULL is defined in C
     * to be a no-op.  Rather than assert a constant, verify the wrapper
     * adds no unexpected behavior by confirming the allocator is still
     * functional after the NULL free -- an observable condition. */
    opal_free(NULL, __FILE__, __LINE__);

    p = opal_malloc(32, __FILE__, __LINE__);
    test_verify("opal_malloc still works after opal_free(NULL)", NULL != p);
    if (NULL != p) {
        opal_free(p, __FILE__, __LINE__);
    }
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_malloc");

    opal_init_util(&argc, &argv);

    test_malloc_basic();
    test_calloc_zeroes();
    test_realloc_grow();
    test_realloc_shrink();
    test_malloc_debug_does_not_break_alloc();
    test_free_null_is_safe();

    r = test_finalize();
    opal_finalize_util();
    return r;
}
