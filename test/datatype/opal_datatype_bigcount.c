/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * OPAL-layer "big count" datatype tests.
 *
 * This is a singleton test (no MPI_Init, OPAL library only) that
 * exercises the parts of the big-count datatype support that live in
 * OPAL:
 *
 *   1. The count/displacement type-punning helpers in
 *      opal/util/count_disp_array.h.  An opal_count_array_t aliases
 *      either an int[] (32 bits per element) or a size_t[] (64 bits
 *      per element); the disp variant aliases int[] or ptrdiff_t[].
 *      These tests verify that values larger than INT_MAX survive a
 *      store/read-back through the 64-bit-backed flavor, and that the
 *      32-bit-backed flavor is reported correctly.
 *
 *   2. The OPAL datatype engine's 64-bit size/extent accounting: a
 *      contiguous type whose element count exceeds INT_MAX must report
 *      the correct (64-bit) size and extent rather than a truncated
 *      32-bit value.
 *
 * These paths are not otherwise covered by the test/datatype suite.
 */

#include "opal_config.h"

#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "opal/datatype/opal_datatype.h"
#include "opal/runtime/opal.h"
#include "opal/util/count_disp_array.h"

static int failures = 0;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__,       \
                    #cond);                                               \
            ++failures;                                                   \
        }                                                                 \
    } while (0)

/* True only where size_t is wide enough for the 64-bit count path to be
 * meaningful (and therefore where opal_count_array_is_64bit() can be
 * true).  On a 32-bit size_t platform big count is not supported, so the
 * 64-bit-specific assertions are skipped. */
static bool have_64bit_counts(void)
{
    return 8 == sizeof(size_t);
}

static void test_count_array_punning(void)
{
    /* 32-bit-backed: int[] with values up to INT_MAX. */
    int idata[3] = {0, 7, INT_MAX};
    opal_count_array_t ia = OPAL_COUNT_ARRAY_CREATE(idata);

    CHECK(!opal_count_array_is_64bit(ia));
    CHECK(sizeof(int) == opal_count_array_sizeof(ia));
    CHECK((size_t) 0 == opal_count_array_get(ia, 0));
    CHECK((size_t) 7 == opal_count_array_get(ia, 1));
    CHECK((size_t) INT_MAX == opal_count_array_get(ia, 2));

    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipping 64-bit count assertions)\n");
        return;
    }

    /* 64-bit-backed: size_t[] holding values above INT_MAX.  This is the
     * heart of big count: the value must round-trip undamaged. */
    size_t big1 = (size_t) INT_MAX + 1;
    size_t big2 = (size_t) INT_MAX * 4 + 123;
    size_t sdata[3] = {1, big1, big2};
    opal_count_array_t sa = OPAL_COUNT_ARRAY_CREATE(sdata);

    CHECK(opal_count_array_is_64bit(sa));
    CHECK(sizeof(size_t) == opal_count_array_sizeof(sa));
    CHECK((size_t) 1 == opal_count_array_get(sa, 0));
    CHECK(big1 == opal_count_array_get(sa, 1));
    CHECK(big2 == opal_count_array_get(sa, 2));

    /* Write-back of an above-INT_MAX value. */
    size_t big3 = (size_t) INT_MAX * 7 + 5;
    opal_count_array_set(sa, 0, big3);
    CHECK(big3 == opal_count_array_get(sa, 0));
    /* Untouched neighbors stay intact. */
    CHECK(big1 == opal_count_array_get(sa, 1));
    CHECK(big2 == opal_count_array_get(sa, 2));
}

static void test_disp_array_punning(void)
{
    /* 32-bit-backed: int[] displacements. */
    int idata[2] = {-3, INT_MAX};
    opal_disp_array_t ia = OPAL_DISP_ARRAY_CREATE(idata);

    CHECK(!opal_disp_array_is_64bit(ia));
    CHECK(sizeof(int) == opal_disp_array_sizeof(ia));
    CHECK((ptrdiff_t) -3 == opal_disp_array_get(ia, 0));
    CHECK((ptrdiff_t) INT_MAX == opal_disp_array_get(ia, 1));

    if (!have_64bit_counts()) {
        return;
    }

    /* 64-bit-backed: ptrdiff_t[] displacements above INT_MAX, including a
     * large negative one. */
    ptrdiff_t big_pos = (ptrdiff_t) INT_MAX * 5 + 9;
    ptrdiff_t big_neg = -((ptrdiff_t) INT_MAX * 5 + 9);
    ptrdiff_t pdata[2] = {big_pos, big_neg};
    opal_disp_array_t pa = OPAL_DISP_ARRAY_CREATE(pdata);

    CHECK(opal_disp_array_is_64bit(pa));
    CHECK(sizeof(ptrdiff_t) == opal_disp_array_sizeof(pa));
    CHECK(big_pos == opal_disp_array_get(pa, 0));
    CHECK(big_neg == opal_disp_array_get(pa, 1));

    /* Write-back of a large negative displacement; neighbor stays intact. */
    ptrdiff_t big3 = -((ptrdiff_t) INT_MAX * 7 + 5);
    opal_disp_array_set(pa, 0, big3);
    CHECK(big3 == opal_disp_array_get(pa, 0));
    CHECK(big_neg == opal_disp_array_get(pa, 1));
}

static void test_engine_large_contiguous(void)
{
    /* The whole function builds types whose size/extent exceed INT_MAX, so it
     * is only meaningful (and only well-defined) where size_t/ptrdiff_t are
     * 64-bit.  Guard up front -- otherwise the very first contiguous below
     * overflows ptrdiff_t on a 32-bit platform. */
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }

    /* A contiguous run of (INT_MAX + 1000) single bytes: an int would
     * overflow, so size/extent must be computed in 64 bits.  Note this
     * only allocates O(1) datatype *metadata* -- no large buffer. */
    size_t count = (size_t) INT_MAX + 1000;
    opal_datatype_t *pdt = opal_datatype_create(2);
    CHECK(NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    int rc = opal_datatype_add(pdt, &opal_datatype_uint1, count, 0, 1);
    CHECK(0 == rc);
    opal_datatype_commit(pdt);

    size_t size = 0;
    opal_datatype_type_size(pdt, &size);
    CHECK(count == size);

    ptrdiff_t extent = 0;
    opal_datatype_type_extent(pdt, &extent);
    CHECK((ptrdiff_t) count == extent);

    printf("  contiguous(%" PRIuPTR " x uint1): size=%" PRIuPTR
           " extent=%td\n",
           (uintptr_t) count, (uintptr_t) size, extent);

    opal_datatype_destroy(&pdt);

    /* Push past 4 GB to make truncation unmistakable. */
    size_t bigger = (size_t) INT_MAX * 3; /* ~6.4 GB */
    opal_datatype_t *pdt2 = opal_datatype_create(2);
    CHECK(NULL != pdt2);
    if (NULL == pdt2) {
        return;
    }
    rc = opal_datatype_add(pdt2, &opal_datatype_uint1, bigger, 0, 1);
    CHECK(0 == rc);
    opal_datatype_commit(pdt2);
    size = 0;
    opal_datatype_type_size(pdt2, &size);
    CHECK(bigger == size);
    extent = 0;
    opal_datatype_type_extent(pdt2, &extent);
    CHECK((ptrdiff_t) bigger == extent);
    opal_datatype_destroy(&pdt2);
}

int main(int argc, char *argv[])
{
    opal_init(&argc, &argv);

    printf("--- opal_count_array punning ---\n");
    test_count_array_punning();
    printf("--- opal_disp_array punning ---\n");
    test_disp_array_punning();
    printf("--- opal datatype engine (size/extent > INT_MAX) ---\n");
    test_engine_large_contiguous();

    opal_finalize();

    if (0 == failures) {
        printf("SUCCESS: all OPAL big-count datatype checks passed\n");
        return 0;
    }
    fprintf(stderr, "FAILURE: %d OPAL big-count datatype check(s) failed\n",
            failures);
    return 1;
}
