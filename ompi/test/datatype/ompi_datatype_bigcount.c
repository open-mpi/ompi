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
 * OMPI-layer "big count" datatype tests, exercised through the internal
 * ompi_datatype_* routines.
 *
 * This is a singleton test that initializes only the OMPI datatype
 * engine (opal_init + ompi_datatype_init) -- it does NOT call MPI_Init,
 * matching every other auto-run test in test/datatype.  The public MPI
 * C-binding wrappers (MPI_Type_*_c, MPI_Type_get_contents[_c]) are
 * exercised separately by mpi_datatype_bigcount.c, which does call
 * MPI_Init.
 *
 * Coverage:
 *   1. ompi_datatype_create_contiguous() with a count above INT_MAX:
 *      the resulting size/extent must be computed in 64 bits.
 *   2. ompi_datatype_set_args()/get_args() round-trip with an
 *      above-INT_MAX count, modeled exactly on the big-count branch of
 *      the generated type_contiguous binding (counts stored in the
 *      "large" array; ci=0, cl=1).  Verifies the int-vs-MPI_Count
 *      type-punning preserves the value.
 *   3. The get_args() invariant that the #14055 get_contents fix relies
 *      on: when handed an oversized output array, get_args fills only
 *      the real number of constituent entries and never touches the
 *      caller's surplus slots.
 *   4. ompi_datatype_match_size() (#14057): a (typeclass, size) request
 *      returns only a basic scalar predefined type, and fails cleanly
 *      otherwise.
 *   5. Pack/unpack of the datatype description for above-INT_MAX types:
 *      ompi_datatype_get_pack_description() followed by
 *      ompi_datatype_create_from_packed_description() must rebuild an
 *      equivalent type.  This is the only path that exercises the
 *      bigcount ("l != NULL") branches of __ompi_datatype_create_from_args
 *      and the size_t count array in the packed description -- the code
 *      that one-sided, MPI-IO, and heterogeneous transfers rely on.
 */

#include "ompi_config.h"

#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mpi.h>

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/util/count_disp_array.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/runtime/opal.h"

static int failures = 0;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__,       \
                    #cond);                                               \
            ++failures;                                                   \
        }                                                                 \
    } while (0)

/* Recognizable non-NULL garbage handle used to poison surplus output
 * slots (named to match POISON in mpi_datatype_bigcount.c). */
static ompi_datatype_t *const POISON = (ompi_datatype_t *) (intptr_t) 0xdeadbeef;

static bool have_64bit_counts(void)
{
    return 8 == sizeof(size_t);
}

/* (1) Engine size/extent for a contiguous type whose count exceeds
 * INT_MAX, built through the OMPI wrapper. */
static void test_create_contiguous_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }

    size_t count = (size_t) INT_MAX + 4096;
    ompi_datatype_t *type = NULL;
    int rc = ompi_datatype_create_contiguous(count, &ompi_mpi_byte.dt, &type);
    CHECK(OMPI_SUCCESS == rc);
    CHECK(NULL != type);
    if (NULL == type) {
        return;
    }
    ompi_datatype_commit(&type);

    size_t size = 0;
    ompi_datatype_type_size(type, &size);
    CHECK(count == size); /* MPI_BYTE has size 1 */

    ptrdiff_t extent = 0;
    ompi_datatype_type_extent(type, &extent);
    CHECK((ptrdiff_t) count == extent);

    printf("  contiguous(%" PRIuPTR " x MPI_BYTE): size=%" PRIuPTR "\n",
           (uintptr_t) count, (uintptr_t) size);

    ompi_datatype_destroy(&type);
}

/* (2) + (3) set_args/get_args type-punning round-trip and the
 * oversized-output-array invariant.  We store the args exactly as the
 * big-count type_contiguous binding does. */
static void test_set_get_args_bigcount(void)
{
    if (!have_64bit_counts()) {
        return;
    }

    size_t count = (size_t) INT_MAX + 12345;
    ompi_datatype_t *type = NULL;
    int rc = ompi_datatype_create_contiguous(count, &ompi_mpi_byte.dt, &type);
    CHECK(OMPI_SUCCESS == rc);
    if (NULL == type) {
        return;
    }

    /* Mirror type_contiguous.c.in's OMPI_BIGCOUNT_SRC branch: a single
     * MPI_Count count (8 bytes => 64-bit count array) goes into the
     * "large" slot, so ci=0, cl=1. */
    MPI_Count cnt = (MPI_Count) count;
    ompi_count_array_t a_i[1] = {OMPI_COUNT_ARRAY_CREATE(&cnt)};
    ompi_datatype_t *oldtype = &ompi_mpi_byte.dt;
    CHECK(ompi_count_array_is_64bit(a_i[0]));

    rc = ompi_datatype_set_args(type, 0, 1, a_i, 0, OMPI_DISP_ARRAY_NULL, 1,
                                &oldtype, MPI_COMBINER_CONTIGUOUS);
    CHECK(OMPI_SUCCESS == rc);

    /* which=0: retrieve the real counts and combiner. */
    size_t ci = 0, cl = 0, ca = 0, cd = 0;
    int32_t combiner = -1;
    rc = ompi_datatype_get_args(type, 0, &ci, NULL, &cl, NULL, &ca, NULL, &cd,
                                NULL, &combiner);
    CHECK(OMPI_SUCCESS == rc);
    CHECK(MPI_COMBINER_CONTIGUOUS == combiner);
    CHECK(0 == ci);
    CHECK(1 == cl);
    CHECK(0 == ca);
    CHECK(1 == cd);

    /* which=1: retrieve the values; the above-INT_MAX count must come
     * back undamaged through the large (MPI_Count) array. */
    MPI_Count lout[1] = {0};
    ompi_datatype_t *dout[1] = {NULL};
    rc = ompi_datatype_get_args(type, 1, &ci, NULL, &cl, lout, &ca, NULL, &cd,
                                dout, NULL);
    CHECK(OMPI_SUCCESS == rc);
    CHECK((MPI_Count) count == lout[0]);
    CHECK(&ompi_mpi_byte.dt == dout[0]);

    /* (3) Oversized output array: ask get_args to fill a datatype array
     * far larger than the single real constituent.  This is the legal
     * usage (max_datatypes discovered via get_envelope may exceed the
     * real count) that crashed MPI_Type_get_contents before #14055.  At
     * this layer the invariant is: only the first cd entries are
     * written; the surplus poisoned slots are left untouched. */
    enum { SLACK = 8 };
    enum { LPOISON = -424242 }; /* sentinel for surplus large-count slots */
    ompi_datatype_t *big_d[1 + SLACK];
    MPI_Count lbig[1 + SLACK];
    for (int i = 0; i < 1 + SLACK; ++i) {
        big_d[i] = POISON;
        lbig[i] = LPOISON;
    }
    size_t cap_ci = 0, cap_cl = (size_t) 1 + SLACK, cap_ca = 0,
           cap_cd = (size_t) 1 + SLACK;
    rc = ompi_datatype_get_args(type, 1, &cap_ci, NULL, &cap_cl, lbig, &cap_ca,
                                NULL, &cap_cd, big_d, NULL);
    CHECK(OMPI_SUCCESS == rc);
    CHECK(&ompi_mpi_byte.dt == big_d[0]);
    CHECK((MPI_Count) count == lbig[0]);
    /* Both the datatype and large-count surplus slots must be untouched. */
    for (int i = 1; i < 1 + SLACK; ++i) {
        CHECK(POISON == big_d[i]);
        CHECK((MPI_Count) LPOISON == lbig[i]);
    }

    ompi_datatype_destroy(&type);
}

/* (4) match_size (#14057): only basic scalar predefined types may be
 * returned; size 0 (and any request with no scalar match) must fail. */
static void check_match(uint32_t datakind, size_t size, bool expect_match)
{
    /* On no match, ompi_datatype_match_size() returns the sentinel
     * &ompi_mpi_datatype_null.dt (which MPI_Type_match_size turns into
     * MPI_ERR_ARG) -- it does not return NULL. */
    const ompi_datatype_t *none = &ompi_mpi_datatype_null.dt;
    const ompi_datatype_t *dt =
        ompi_datatype_match_size(size, datakind, OMPI_DATATYPE_FLAG_DATA_C);
    if (!expect_match) {
        CHECK(none == dt);
        return;
    }
    CHECK(NULL != dt && none != dt);
    if (NULL == dt || none == dt) {
        return;
    }
    /* Must be a basic scalar type of exactly the requested size and the
     * requested language/kind -- never a composite (e.g. a paired type)
     * and never an unavailable size-0 type. */
    CHECK(OPAL_DATATYPE_FLAG_BASIC
          == (dt->super.flags & OPAL_DATATYPE_FLAG_BASIC));
    CHECK(size == dt->super.size);
    CHECK(OMPI_DATATYPE_FLAG_DATA_C
          == (dt->super.flags & OMPI_DATATYPE_FLAG_DATA_LANGUAGE));
    CHECK(datakind == (dt->super.flags & OMPI_DATATYPE_FLAG_DATA_TYPE));
}

static void test_match_size_basic_only(void)
{
    /* C float scalars always exist regardless of Fortran availability. */
    check_match(OMPI_DATATYPE_FLAG_DATA_FLOAT, sizeof(float), true);
    check_match(OMPI_DATATYPE_FLAG_DATA_FLOAT, sizeof(double), true);
    check_match(OMPI_DATATYPE_FLAG_DATA_INT, sizeof(int), true);

    /* A size-0 request must never succeed (it previously could return an
     * unavailable predefined type). */
    check_match(OMPI_DATATYPE_FLAG_DATA_FLOAT, 0, false);
    check_match(OMPI_DATATYPE_FLAG_DATA_INT, 0, false);

    /* A wildly unlikely size has no basic scalar match and must fail
     * rather than fall through to a composite. */
    check_match(OMPI_DATATYPE_FLAG_DATA_FLOAT, 7, false);
}

/* (5) Pack the description of a committed bigcount type and rebuild it
 * from the packed bytes, then verify the rebuilt type is equivalent.
 * Rebuilding runs __ompi_datatype_create_from_args, whose bigcount
 * branches (l != NULL) are otherwise untested. */
static void pack_unpack_check(const char *label, ompi_datatype_t *type)
{
    /* A 0 length is the documented upstream failure return; treat it as the
     * real failure rather than letting malloc(0) muddy the diagnostic. */
    size_t plen = ompi_datatype_pack_description_length(type);
    CHECK(plen > 0);
    if (0 == plen) {
        return;
    }
    void *buf = malloc(plen);
    CHECK(NULL != buf);
    const void *packed = NULL;
    int rc = ompi_datatype_get_pack_description(type, &packed);
    CHECK(OMPI_SUCCESS == rc);
    if (OMPI_SUCCESS != rc || NULL == buf) {
        free(buf);
        return;
    }
    memcpy(buf, packed, plen);

    /* create_from_packed_description advances the cursor, so keep buf to
     * free.  ompi_proc_local() works because main() points
     * ompi_proc_local_proc at a dummy proc (same trick as ddt_pack.c);
     * remote==local makes the heterogeneous byte-swap path a no-op.
     *
     * COVERAGE NOTE: this therefore does NOT exercise the big-endian
     * byte-swap branch of __ompi_datatype_create_from_packed_description
     * (the opal_swap_bytes8 loops over the size_t count and ptrdiff_t disp
     * arrays).  Driving that path would require pre-swapping the packed
     * buffer to mimic a different-endian sender, i.e. re-implementing the
     * wire format here; it is better covered by a real heterogeneous run. */
    void *cursor = buf;
    ompi_datatype_t *rebuilt =
        ompi_datatype_create_from_packed_description(&cursor, ompi_proc_local());
    free(buf);
    CHECK(NULL != rebuilt);
    if (NULL == rebuilt) {
        return;
    }

    /* Size, extent, and true extent must survive unchanged. */
    size_t s0 = 0, s1 = 0;
    ompi_datatype_type_size(type, &s0);
    ompi_datatype_type_size(rebuilt, &s1);
    CHECK(s0 == s1);

    ptrdiff_t lb0, ext0, lb1, ext1, tlb0, text0, tlb1, text1;
    ompi_datatype_get_extent(type, &lb0, &ext0);
    ompi_datatype_get_extent(rebuilt, &lb1, &ext1);
    CHECK(lb0 == lb1);
    CHECK(ext0 == ext1);
    ompi_datatype_get_true_extent(type, &tlb0, &text0);
    ompi_datatype_get_true_extent(rebuilt, &tlb1, &text1);
    CHECK(tlb0 == tlb1);
    CHECK(text0 == text1);

    /* Envelope must match: same combiner and the same number of each kind
     * of argument.  In particular cl (large counts) must be preserved --
     * if a bigcount were truncated into the int array on the way through,
     * cl would shrink here. */
    size_t ci0, cl0, ca0, cd0, ci1, cl1, ca1, cd1;
    int32_t comb0 = -1, comb1 = -2;
    CHECK(OMPI_SUCCESS
          == ompi_datatype_get_args(type, 0, &ci0, NULL, &cl0, NULL, &ca0, NULL,
                                    &cd0, NULL, &comb0));
    CHECK(OMPI_SUCCESS
          == ompi_datatype_get_args(rebuilt, 0, &ci1, NULL, &cl1, NULL, &ca1,
                                    NULL, &cd1, NULL, &comb1));
    CHECK(comb0 == comb1);
    CHECK(ci0 == ci1);
    CHECK(cl0 == cl1);
    CHECK(ca0 == ca1);
    CHECK(cd0 == cd1);

    /* Every stored argument value -- ints, large counts, displacements, and
     * constituent datatypes -- must survive the round-trip, not just the
     * counts.  Compare the rebuilt type's args against the original's (the
     * original's values are checked against the constructor inputs by the
     * MPI-level constructor tests). */
    if (ci0 == ci1 && cl0 == cl1 && ca0 == ca1 && cd0 == cd1) {
        int *i0 = malloc((ci0 + 1) * sizeof(int));
        int *i1 = malloc((ci1 + 1) * sizeof(int));
        MPI_Count *l0 = malloc((cl0 + 1) * sizeof(MPI_Count));
        MPI_Count *l1 = malloc((cl1 + 1) * sizeof(MPI_Count));
        ptrdiff_t *a0 = malloc((ca0 + 1) * sizeof(ptrdiff_t));
        ptrdiff_t *a1 = malloc((ca1 + 1) * sizeof(ptrdiff_t));
        ompi_datatype_t **d0 = malloc((cd0 + 1) * sizeof(ompi_datatype_t *));
        ompi_datatype_t **d1 = malloc((cd1 + 1) * sizeof(ompi_datatype_t *));

        if (NULL == i0 || NULL == i1 || NULL == l0 || NULL == l1
            || NULL == a0 || NULL == a1 || NULL == d0 || NULL == d1) {
            CHECK(false); /* allocation failure */
        } else {
            size_t qi = ci0, ql = cl0, qa = ca0, qd = cd0;
            CHECK(OMPI_SUCCESS
                  == ompi_datatype_get_args(type, 1, &qi, i0, &ql, l0, &qa, a0,
                                            &qd, d0, NULL));
            qi = ci1; ql = cl1; qa = ca1; qd = cd1;
            CHECK(OMPI_SUCCESS
                  == ompi_datatype_get_args(rebuilt, 1, &qi, i1, &ql, l1, &qa,
                                            a1, &qd, d1, NULL));

            for (size_t k = 0; k < ci0; ++k) {
                CHECK(i0[k] == i1[k]);
            }
            for (size_t k = 0; k < cl0; ++k) {
                CHECK(l0[k] == l1[k]);
            }
            for (size_t k = 0; k < ca0; ++k) {
                CHECK(a0[k] == a1[k]);
            }
            /* Constituent datatypes must be *equivalent*, but not necessarily
             * the same handle: reconstruction canonicalizes predefined types
             * by id (e.g. a packed MPI_INT rebuilds as ompi_mpi_int32_t), so
             * check predefined-ness and size rather than pointer identity. */
            for (size_t k = 0; k < cd0; ++k) {
                size_t z0 = 0, z1 = 0;
                ompi_datatype_type_size(d0[k], &z0);
                ompi_datatype_type_size(d1[k], &z1);
                CHECK(z0 == z1);
                CHECK(ompi_datatype_is_predefined(d0[k])
                      == ompi_datatype_is_predefined(d1[k]));
            }
        }

        free(i0); free(i1); free(l0); free(l1);
        free(a0); free(a1); free(d0); free(d1);
    }

    printf("  %-11s size=%" PRIuPTR " extent=%td combiner=%d cl=%zu  ok\n",
           label, (uintptr_t) s0, ext0, (int) comb0, cl0);

    if (!ompi_datatype_is_predefined(rebuilt)) {
        ompi_datatype_destroy(&rebuilt);
    }
}

static void test_pack_unpack_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }

    ompi_datatype_t *old = &ompi_mpi_byte.dt;

    /* contiguous: a single above-INT_MAX count (cl=1). */
    {
        MPI_Count count = (MPI_Count) INT_MAX + 7;
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS == ompi_datatype_create_contiguous(count, old, &t));
        ompi_count_array_t a_i[1] = {OMPI_COUNT_ARRAY_CREATE(&count)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 1, a_i, 0, OMPI_DISP_ARRAY_NULL,
                                        1, &old, MPI_COMBINER_CONTIGUOUS));
        ompi_datatype_commit(&t);
        pack_unpack_check("contiguous", t);
        ompi_datatype_destroy(&t);
    }

    /* vector: above-INT_MAX count, blocklength, and (element) stride. */
    {
        MPI_Count count = (MPI_Count) INT_MAX + 5;
        MPI_Count blen = (MPI_Count) INT_MAX + 6;
        MPI_Count stride = (MPI_Count) INT_MAX + 7;
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_vector(count, blen, stride, old, &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(&blen),
                                     OMPI_COUNT_ARRAY_CREATE(&stride)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 3, a_i, 0, OMPI_DISP_ARRAY_NULL,
                                        1, &old, MPI_COMBINER_VECTOR));
        ompi_datatype_commit(&t);
        pack_unpack_check("vector", t);
        ompi_datatype_destroy(&t);
    }

    /* indexed: 2 blocks, with an above-INT_MAX blocklength and (element)
     * displacement. */
    {
        MPI_Count count = 2;
        MPI_Count bl[2] = {(MPI_Count) INT_MAX + 5, 3};
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_indexed(count, OMPI_COUNT_ARRAY_CREATE(bl),
                                              OMPI_DISP_ARRAY_CREATE(disp), old,
                                              &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(bl),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 * count + 1, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_INDEXED));
        ompi_datatype_commit(&t);
        pack_unpack_check("indexed", t);
        ompi_datatype_destroy(&t);
    }

    /* hindexed: 2 blocks, above-INT_MAX blocklength and (byte)
     * displacement. */
    {
        MPI_Count count = 2;
        MPI_Count bl[2] = {(MPI_Count) INT_MAX + 5, 3};
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_hindexed(count,
                                               OMPI_COUNT_ARRAY_CREATE(bl),
                                               OMPI_DISP_ARRAY_CREATE(disp), old,
                                               &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(bl),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 * count + 1, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_HINDEXED));
        ompi_datatype_commit(&t);
        pack_unpack_check("hindexed", t);
        ompi_datatype_destroy(&t);
    }

    /* struct: 2 heterogeneous blocks, above-INT_MAX blocklength and (byte)
     * displacement. */
    {
        MPI_Count count = 2;
        MPI_Count bl[2] = {(MPI_Count) INT_MAX + 5, 3};
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *types[2] = {&ompi_mpi_byte.dt, &ompi_mpi_int.dt};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_struct(count, OMPI_COUNT_ARRAY_CREATE(bl),
                                             OMPI_DISP_ARRAY_CREATE(disp), types,
                                             &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(bl),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 * count + 1, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, count, types,
                                        MPI_COMBINER_STRUCT));
        ompi_datatype_commit(&t);
        pack_unpack_check("struct", t);
        ompi_datatype_destroy(&t);
    }

    /* struct with a *derived* (non-predefined) constituent: exercises the
     * recursive pack/reconstruct path (OBJ_RETAIN + nested pack) that a
     * predefined-only struct does not. */
    {
        ompi_datatype_t *inner = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_contiguous(2, &ompi_mpi_int.dt, &inner));
        /* A derived type used as a struct member must carry its own args (the
         * binding layer normally does this); set them so the recursive pack
         * of the constituent has a description to walk. */
        {
            int two = 2;
            ompi_count_array_t inner_a[1] = {OMPI_COUNT_ARRAY_CREATE(&two)};
            ompi_datatype_t *inner_old = &ompi_mpi_int.dt;
            CHECK(OMPI_SUCCESS
                  == ompi_datatype_set_args(inner, 1, 0, inner_a, 0,
                                            OMPI_DISP_ARRAY_NULL, 1, &inner_old,
                                            MPI_COMBINER_CONTIGUOUS));
        }
        ompi_datatype_commit(&inner);

        MPI_Count count = 2;
        MPI_Count bl[2] = {(MPI_Count) INT_MAX + 5, 1};
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *types[2] = {&ompi_mpi_byte.dt, inner};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_struct(count, OMPI_COUNT_ARRAY_CREATE(bl),
                                             OMPI_DISP_ARRAY_CREATE(disp), types,
                                             &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(bl),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 * count + 1, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, count, types,
                                        MPI_COMBINER_STRUCT));
        ompi_datatype_commit(&t);
        pack_unpack_check("struct+derived", t);
        ompi_datatype_destroy(&t);
        ompi_datatype_destroy(&inner);
    }

    /* hvector: above-INT_MAX count, blocklength, and (byte) stride. */
    {
        MPI_Count count = (MPI_Count) INT_MAX + 5;
        MPI_Count blen = (MPI_Count) INT_MAX + 6;
        MPI_Count stride = (MPI_Count) INT_MAX + 7; /* bytes */
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_hvector(count, blen, (ptrdiff_t) stride,
                                              old, &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(&blen),
                                     OMPI_COUNT_ARRAY_CREATE(&stride)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 3, a_i, 0, OMPI_DISP_ARRAY_NULL,
                                        1, &old, MPI_COMBINER_HVECTOR));
        ompi_datatype_commit(&t);
        pack_unpack_check("hvector", t);
        ompi_datatype_destroy(&t);
    }

    /* indexed_block: above-INT_MAX blocklength and (element) displacement. */
    {
        MPI_Count count = 2;
        MPI_Count blen = (MPI_Count) INT_MAX + 5;
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_indexed_block(count, blen,
                                                    OMPI_DISP_ARRAY_CREATE(disp),
                                                    old, &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(&blen),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 + count, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_INDEXED_BLOCK));
        ompi_datatype_commit(&t);
        pack_unpack_check("indexed_block", t);
        ompi_datatype_destroy(&t);
    }

    /* hindexed_block: above-INT_MAX blocklength and (byte) displacement. */
    {
        MPI_Count count = 2;
        MPI_Count blen = (MPI_Count) INT_MAX + 5;
        MPI_Count disp[2] = {0, (MPI_Count) INT_MAX + 6};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_hindexed_block(count, blen,
                                                     OMPI_DISP_ARRAY_CREATE(disp),
                                                     old, &t));
        ompi_count_array_t a_i[3] = {OMPI_COUNT_ARRAY_CREATE(&count),
                                     OMPI_COUNT_ARRAY_CREATE(&blen),
                                     OMPI_COUNT_ARRAY_CREATE(disp)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2 + count, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_HINDEXED_BLOCK));
        ompi_datatype_commit(&t);
        pack_unpack_check("hindexed_block", t);
        ompi_datatype_destroy(&t);
    }

    /* subarray: above-INT_MAX size, subsize, and start in dimension 0. */
    {
        int ndims = 2, order = MPI_ORDER_C;
        MPI_Count sizes[2] = {2 * (MPI_Count) INT_MAX + 100, 6};
        MPI_Count subsizes[2] = {(MPI_Count) INT_MAX + 5, 2};
        MPI_Count starts[2] = {(MPI_Count) INT_MAX + 3, 1};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_subarray(ndims,
                                               OMPI_COUNT_ARRAY_CREATE(sizes),
                                               OMPI_COUNT_ARRAY_CREATE(subsizes),
                                               OMPI_COUNT_ARRAY_CREATE(starts),
                                               order, old, &t));
        ompi_count_array_t a_i[5] = {OMPI_COUNT_ARRAY_CREATE(&ndims),
                                     OMPI_COUNT_ARRAY_CREATE(sizes),
                                     OMPI_COUNT_ARRAY_CREATE(subsizes),
                                     OMPI_COUNT_ARRAY_CREATE(starts),
                                     OMPI_COUNT_ARRAY_CREATE(&order)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 2, 3 * ndims, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_SUBARRAY));
        ompi_datatype_commit(&t);
        pack_unpack_check("subarray", t);
        ompi_datatype_destroy(&t);
    }

    /* darray: 2-D, single process, above-INT_MAX gsizes. */
    {
        int size = 1, rank = 0, ndims = 2, order = MPI_ORDER_C;
        MPI_Count gsizes[2] = {(MPI_Count) INT_MAX + 5,
                               (MPI_Count) INT_MAX + 6};
        int distribs[2] = {MPI_DISTRIBUTE_BLOCK, MPI_DISTRIBUTE_BLOCK};
        int dargs[2] = {MPI_DISTRIBUTE_DFLT_DARG, MPI_DISTRIBUTE_DFLT_DARG};
        int psizes[2] = {1, 1};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_darray(size, rank, ndims,
                                             OMPI_COUNT_ARRAY_CREATE(gsizes),
                                             distribs, dargs, psizes, order, old,
                                             &t));
        ompi_count_array_t a_i[8] = {OMPI_COUNT_ARRAY_CREATE(&size),
                                     OMPI_COUNT_ARRAY_CREATE(&rank),
                                     OMPI_COUNT_ARRAY_CREATE(&ndims),
                                     OMPI_COUNT_ARRAY_CREATE(gsizes),
                                     OMPI_COUNT_ARRAY_CREATE(distribs),
                                     OMPI_COUNT_ARRAY_CREATE(dargs),
                                     OMPI_COUNT_ARRAY_CREATE(psizes),
                                     OMPI_COUNT_ARRAY_CREATE(&order)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 3 * ndims + 4, ndims, a_i, 0,
                                        OMPI_DISP_ARRAY_NULL, 1, &old,
                                        MPI_COMBINER_DARRAY));
        ompi_datatype_commit(&t);
        pack_unpack_check("darray", t);
        ompi_datatype_destroy(&t);
    }

    /* resized: above-INT_MAX extent and a *negative* lb, exercising the
     * large-count reconstruction branch of create_from_args and the
     * signed round-trip of lb through the (unsigned) size_t count array. */
    {
        MPI_Count lb = -((MPI_Count) INT_MAX + 5);
        MPI_Count extent = (MPI_Count) INT_MAX + 6;
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_resized(old, (ptrdiff_t) lb,
                                              (ptrdiff_t) extent, &t));
        MPI_Count a_l[2] = {lb, extent};
        ompi_count_array_t a_i[1] = {OMPI_COUNT_ARRAY_CREATE(a_l)};
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 2, a_i, 0, OMPI_DISP_ARRAY_NULL,
                                        1, &old, MPI_COMBINER_RESIZED));
        ompi_datatype_commit(&t);
        pack_unpack_check("resized", t);
        ompi_datatype_destroy(&t);
    }

    /* resized (classic form): lb/extent stored as MPI_Aint displacements
     * (ca=2, cl=0), exercising the NULL == l reconstruction branch of
     * create_from_args that the large-count resized case above does not. */
    {
        ptrdiff_t disp[2] = {-3, 4096};
        ompi_datatype_t *t = NULL;
        CHECK(OMPI_SUCCESS
              == ompi_datatype_create_resized(old, disp[0], disp[1], &t));
        CHECK(OMPI_SUCCESS
              == ompi_datatype_set_args(t, 0, 0, NULL, 2,
                                        OMPI_DISP_ARRAY_CREATE(disp), 1, &old,
                                        MPI_COMBINER_RESIZED));
        ompi_datatype_commit(&t);
        pack_unpack_check("resized_aint", t);
        ompi_datatype_destroy(&t);
    }
}

int main(int argc, char *argv[])
{
    /* Make ompi_proc_local() usable without MPI_Init (same approach as
     * ddt_pack.c): the dummy proc is only compared against itself in the
     * pack/unpack path, so its (uninitialized) fields never matter. */
    /* Zero-init so that, even if a future pack/unpack change reads a remote
     * proc field other than proc_arch, the test reads defined memory rather
     * than uninitialized stack. */
    struct ompi_proc_t dummy_proc;
    memset(&dummy_proc, 0, sizeof(dummy_proc));
    ompi_proc_local_proc = &dummy_proc;

    opal_init(&argc, &argv);
    ompi_datatype_init();

    printf("--- ompi_datatype_create_contiguous (count > INT_MAX) ---\n");
    test_create_contiguous_bigcount();
    printf("--- ompi_datatype_set_args/get_args punning + oversized array ---\n");
    test_set_get_args_bigcount();
    printf("--- ompi_datatype_match_size (basic scalar only) ---\n");
    test_match_size_basic_only();
    printf("--- pack/unpack datatype description (count > INT_MAX) ---\n");
    test_pack_unpack_bigcount();

    opal_finalize();

    if (0 == failures) {
        printf("SUCCESS: all OMPI big-count datatype checks passed\n");
        return 0;
    }
    fprintf(stderr, "FAILURE: %d OMPI big-count datatype check(s) failed\n",
            failures);
    return 1;
}
