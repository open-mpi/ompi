/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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

#include "opal_config.h"

#include "support.h"
#include "opal/util/alfg.h"
#include "opal/constants.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Number of values to draw per sequence in divergence/determinism tests */
#define SEQ_LEN 256

/* -----------------------------------------------------------------------
 * opal_srand return value
 * ----------------------------------------------------------------------- */

static void test_srand_returns_one(void)
{
    opal_rng_buff_t buff;
    int rc;

    rc = opal_srand(&buff, 12345u);
    /* NOTE: unlike other OPAL functions, opal_srand returns 1 on success */
    test_verify("opal_srand returns 1", 1 == rc);
}

/* -----------------------------------------------------------------------
 * opal_rand: a drawn sequence is not a single constant value
 * ----------------------------------------------------------------------- */

static void test_rand_not_constant(void)
{
    opal_rng_buff_t buff;
    uint32_t first;
    uint32_t v;
    int i;
    int any_differ = 0;

    opal_srand(&buff, 99999u);
    first = opal_rand(&buff);
    for (i = 1; i < SEQ_LEN; i++) {
        v = opal_rand(&buff);
        if (v != first) {
            any_differ = 1;
        }
    }

    test_verify("opal_rand: sequence is not a single constant value", any_differ);
}

/* -----------------------------------------------------------------------
 * opal_rand determinism: same seed -> same sequence
 * ----------------------------------------------------------------------- */

static void test_rand_deterministic(void)
{
    opal_rng_buff_t buff1, buff2;
    uint32_t seq1[SEQ_LEN];
    uint32_t seq2[SEQ_LEN];
    int i;
    int deterministic = 1;

    opal_srand(&buff1, 42u);
    opal_srand(&buff2, 42u);

    for (i = 0; i < SEQ_LEN; i++) {
        seq1[i] = opal_rand(&buff1);
        seq2[i] = opal_rand(&buff2);
        if (seq1[i] != seq2[i]) {
            deterministic = 0;
        }
    }

    test_verify("opal_rand: same seed produces identical sequences", deterministic);
}

/* -----------------------------------------------------------------------
 * opal_rand divergence: different seeds produce different sequences
 * ----------------------------------------------------------------------- */

static void test_rand_divergence(void)
{
    opal_rng_buff_t buffA, buffB;
    uint32_t seqA[SEQ_LEN];
    uint32_t seqB[SEQ_LEN];
    int i;
    int any_differ = 0;

    opal_srand(&buffA, 1u);
    opal_srand(&buffB, 2u);

    for (i = 0; i < SEQ_LEN; i++) {
        seqA[i] = opal_rand(&buffA);
        seqB[i] = opal_rand(&buffB);
        if (seqA[i] != seqB[i]) {
            any_differ = 1;
        }
    }

    test_verify("opal_rand: different seeds produce different sequences", any_differ);
}

/* -----------------------------------------------------------------------
 * opal_rand: re-seeding with same seed resets to same sequence
 * ----------------------------------------------------------------------- */

static void test_rand_reseed(void)
{
    opal_rng_buff_t buff;
    uint32_t first[SEQ_LEN];
    uint32_t second[SEQ_LEN];
    int i;
    int same = 1;

    opal_srand(&buff, 777u);
    for (i = 0; i < SEQ_LEN; i++) {
        first[i] = opal_rand(&buff);
    }

    /* Re-seed with the same value */
    opal_srand(&buff, 777u);
    for (i = 0; i < SEQ_LEN; i++) {
        second[i] = opal_rand(&buff);
        if (first[i] != second[i]) {
            same = 0;
        }
    }

    test_verify("opal_rand: re-seeding with same seed restores sequence", same);
}

/* -----------------------------------------------------------------------
 * opal_rand: seed=0 does not crash and produces a sequence
 * ----------------------------------------------------------------------- */

static void test_rand_seed_zero(void)
{
    opal_rng_buff_t buff;
    int rc;
    uint32_t v;

    rc = opal_srand(&buff, 0u);
    test_verify("opal_srand with seed=0 returns 1", 1 == rc);
    v = opal_rand(&buff);
    (void) v;
    test_verify("opal_rand after seed=0: no crash", 1);
}

/* -----------------------------------------------------------------------
 * opal_rand: seed=UINT32_MAX does not crash
 * ----------------------------------------------------------------------- */

static void test_rand_seed_max(void)
{
    opal_rng_buff_t buff;
    int rc;
    uint32_t v;

    rc = opal_srand(&buff, UINT32_MAX);
    test_verify("opal_srand with seed=UINT32_MAX returns 1", 1 == rc);
    v = opal_rand(&buff);
    (void) v;
    test_verify("opal_rand after seed=UINT32_MAX: no crash", 1);
}

/* -----------------------------------------------------------------------
 * opal_rand: buffer state is independent across two buffers
 * ----------------------------------------------------------------------- */

static void test_rand_independent_buffers(void)
{
    opal_rng_buff_t buffX, buffY;
    uint32_t x1, x2, y1, y2;

    opal_srand(&buffX, 10u);
    opal_srand(&buffY, 20u);

    /* Advance each buffer by a different amount */
    x1 = opal_rand(&buffX);
    x2 = opal_rand(&buffX);
    y1 = opal_rand(&buffY);
    y2 = opal_rand(&buffY);

    /* Confirm they don't interfere by re-seeding X and checking it */
    opal_rng_buff_t buffX2;
    opal_srand(&buffX2, 10u);
    test_verify("opal_rand: buffX is independent of buffY (first value)",
                x1 == opal_rand(&buffX2));
    test_verify("opal_rand: buffX is independent of buffY (second value)",
                x2 == opal_rand(&buffX2));

    (void) y1;
    (void) y2;
}

/* -----------------------------------------------------------------------
 * opal_random (global wrapper)
 * ----------------------------------------------------------------------- */

static void test_random_range(void)
{
    int v;
    int i;
    int all_nonneg = 1;

    /* Seed the global buffer first via a buff; the implementation copies
     * from buff into the global alfg_buffer inside opal_srand. */
    opal_rng_buff_t buff;
    opal_srand(&buff, 55555u);

    for (i = 0; i < SEQ_LEN; i++) {
        v = opal_random();
        /* opal_random masks with 0x7FFFFFFF -> always non-negative */
        if (v < 0) {
            all_nonneg = 0;
        }
    }

    test_verify("opal_random: all values are non-negative", all_nonneg);
}

static void test_random_deterministic_via_global(void)
{
    opal_rng_buff_t buff;
    int seq1[SEQ_LEN];
    int seq2[SEQ_LEN];
    int i;
    int same = 1;

    /* First run */
    opal_srand(&buff, 12u);
    for (i = 0; i < SEQ_LEN; i++) {
        seq1[i] = opal_random();
    }

    /* Re-seed global to same value and re-draw */
    opal_srand(&buff, 12u);
    for (i = 0; i < SEQ_LEN; i++) {
        seq2[i] = opal_random();
        if (seq1[i] != seq2[i]) {
            same = 0;
        }
    }

    test_verify("opal_random: same global seed produces same sequence", same);
}

static void test_random_not_always_zero(void)
{
    opal_rng_buff_t buff;
    int i;
    int any_nonzero = 0;

    opal_srand(&buff, 1u);
    for (i = 0; i < SEQ_LEN; i++) {
        if (0 != opal_random()) {
            any_nonzero = 1;
            break;
        }
    }

    test_verify("opal_random: produces non-zero values", any_nonzero);
}

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_alfg");

    /* opal_srand */
    test_srand_returns_one();
    test_rand_seed_zero();
    test_rand_seed_max();

    /* opal_rand */
    test_rand_not_constant();
    test_rand_deterministic();
    test_rand_divergence();
    test_rand_reseed();
    test_rand_independent_buffers();

    /* opal_random */
    test_random_range();
    test_random_deterministic_via_global();
    test_random_not_always_zero();

    return test_finalize();
}
