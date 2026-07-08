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
 * "Big count" datatype tests exercised through the public MPI C API.
 *
 * Unlike the other test/datatype programs, this one calls MPI_Init /
 * MPI_Finalize so that it drives the actual generated C bindings --
 * including the large-count (_c) entry points and the
 * MPI_Type_get_contents[_c] wrappers.  It runs as a singleton (one
 * process, no mpirun), so it is suitable for "make check".
 *
 * Coverage:
 *   A. MPI_Type_contiguous_c() with a count above INT_MAX: MPI_Type_size_c,
 *      MPI_Type_get_envelope_c and MPI_Type_get_contents_c must round-trip
 *      the large count through the MPI_Count ("large count") array.
 *   B. Regression test for the #14055 over-read on the large-count path:
 *      MPI_Type_get_contents_c() called with output arrays *larger* than
 *      the type's real argument counts (legal per the standard, since the
 *      required sizes are discovered via get_envelope) must not walk past
 *      the real constituent datatypes into uninitialized handles.
 *   C. The same regression on the classic (non-_c) MPI_Type_get_contents()
 *      path, which had the identical bug.
 */

#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mpi.h>

static int failures = 0;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__,       \
                    #cond);                                               \
            ++failures;                                                   \
        }                                                                 \
    } while (0)

/* Big count is only supported where size_t is 64-bit (matches the helper in
 * the sibling opal_/ompi_datatype_bigcount.c tests). */
static bool have_64bit_counts(void)
{
    return 8 == sizeof(size_t);
}

/* A recognizable non-NULL garbage handle used to poison surplus output
 * slots.  If get_contents walks into these, it dereferences garbage --
 * which is exactly the #14055 crash. */
static MPI_Datatype const POISON = (MPI_Datatype) (intptr_t) 0xdeadbeef;

/* (A) Large-count contiguous type via the _c API. */
static void test_contiguous_c_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }

    MPI_Count count = (MPI_Count) INT_MAX + 4096;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_contiguous_c(count, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    MPI_Count size = 0;
    CHECK(MPI_SUCCESS == MPI_Type_size_c(t, &size));
    CHECK(count == size); /* MPI_BYTE has size 1 */

    MPI_Count lb = 1, extent = -1;
    CHECK(MPI_SUCCESS == MPI_Type_get_extent_c(t, &lb, &extent));
    CHECK(0 == lb);
    CHECK(count == extent);

    MPI_Count ni = -1, na = -1, nlc = -1, nd = -1;
    int combiner = -1;
    CHECK(MPI_SUCCESS
          == MPI_Type_get_envelope_c(t, &ni, &na, &nlc, &nd, &combiner));
    CHECK(MPI_COMBINER_CONTIGUOUS == combiner);
    CHECK(1 == nd);
    /* The _c constructor stored the count as a large count, so it must be
     * reported in num_large_counts, not num_integers. */
    CHECK(1 == nlc);
    CHECK(0 == ni);
    CHECK(0 == na);

    int aint_ints[1];
    MPI_Aint addrs[1];
    MPI_Count larges[1] = {0};
    MPI_Datatype types[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, ni, na, nlc, nd, aint_ints, addrs,
                                     larges, types));
    CHECK(count == larges[0]);
    CHECK(MPI_BYTE == types[0]);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

/* (B) #14055 regression on the large-count path: oversized output arrays. */
static void test_get_contents_c_oversized(void)
{
    enum { SLACK = 16 };
    MPI_Count bl[2] = {1, 1};
    /* array_of_displacements[] is const MPI_Count[] for
     * MPI_Type_create_struct_c -- this is correct per the MPI standard:
     * the large-count variants of MPI_Type_create_hindexed,
     * MPI_Type_create_hindexed_block, and MPI_Type_create_struct take
     * MPI_Count (not MPI_Aint) byte displacements (MPI 5.0 sec 19.2). */
    MPI_Count disp[2] = {0, (MPI_Count) sizeof(int)};
    MPI_Datatype intypes[2] = {MPI_INT, MPI_DOUBLE};
    MPI_Datatype st = MPI_DATATYPE_NULL;

    CHECK(MPI_SUCCESS
          == MPI_Type_create_struct_c(2, bl, disp, intypes, &st));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&st));

    MPI_Count ni = -1, na = -1, nlc = -1, nd = -1;
    int combiner = -1;
    CHECK(MPI_SUCCESS
          == MPI_Type_get_envelope_c(st, &ni, &na, &nlc, &nd, &combiner));
    CHECK(MPI_COMBINER_STRUCT == combiner);
    /* _c struct: count + 2 blocklengths + 2 displacements are all large
     * counts; no integers or addresses; two constituent datatypes. */
    CHECK(0 == ni);
    CHECK(0 == na);
    CHECK(5 == nlc);
    CHECK(2 == nd);
    if (ni < 0 || na < 0 || nlc < 0 || nd < 0) {
        return; /* envelope failed (already recorded); counts are bogus */
    }

    /* Allocate every output array oversized and poison the datatype tail. */
    int *ai = calloc((size_t) ni + SLACK, sizeof(int));
    MPI_Aint *aa = calloc((size_t) na + SLACK, sizeof(MPI_Aint));
    MPI_Count *alc = calloc((size_t) nlc + SLACK, sizeof(MPI_Count));
    MPI_Datatype *ad = malloc(((size_t) nd + SLACK) * sizeof(MPI_Datatype));
    CHECK(NULL != ai && NULL != aa && NULL != alc && NULL != ad);
    if (NULL == ai || NULL == aa || NULL == alc || NULL == ad) {
        free(ai); free(aa); free(alc); free(ad);
        return;
    }
    for (size_t i = 0; i < (size_t) nd + SLACK; ++i) {
        ad[i] = POISON;
    }

    /* Pass capacities LARGER than the real counts -- the legal usage that
     * crashed before #14055. */
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(st, ni + SLACK, na + SLACK, nlc + SLACK,
                                     nd + SLACK, ai, aa, alc, ad));

    /* Every returned value is checked: count, both blocklengths, both
     * displacements, and both constituent datatypes. */
    CHECK(2 == alc[0]);                     /* count */
    CHECK(bl[0] == alc[1]);                 /* blocklengths */
    CHECK(bl[1] == alc[2]);
    CHECK(disp[0] == alc[3]);               /* displacements */
    CHECK(disp[1] == alc[4]);
    CHECK(MPI_INT == ad[0]);
    CHECK(MPI_DOUBLE == ad[1]);
    /* Surplus datatype slots untouched (no over-read -- the #14055 bug). */
    for (size_t i = (size_t) nd; i < (size_t) nd + SLACK; ++i) {
        CHECK(POISON == ad[i]);
    }

    free(ai);
    free(aa);
    free(alc);
    free(ad);
    CHECK(MPI_SUCCESS == MPI_Type_free(&st));
}

/* (C) Same #14055 regression on the classic (non-_c) path. */
static void test_get_contents_oversized(void)
{
    enum { SLACK = 16 };
    int bl[2] = {1, 1};
    MPI_Aint disp[2] = {0, (MPI_Aint) sizeof(int)};
    MPI_Datatype intypes[2] = {MPI_INT, MPI_DOUBLE};
    MPI_Datatype st = MPI_DATATYPE_NULL;

    CHECK(MPI_SUCCESS == MPI_Type_create_struct(2, bl, disp, intypes, &st));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&st));

    int ni = -1, na = -1, nd = -1, combiner = -1;
    CHECK(MPI_SUCCESS
          == MPI_Type_get_envelope(st, &ni, &na, &nd, &combiner));
    CHECK(MPI_COMBINER_STRUCT == combiner);
    /* classic struct: count + 2 blocklengths are integers; the 2 byte
     * displacements are addresses; two constituent datatypes. */
    CHECK(3 == ni);
    CHECK(2 == na);
    CHECK(2 == nd);
    if (ni < 0 || na < 0 || nd < 0) {
        return; /* envelope failed (already recorded); counts are bogus */
    }

    int *ai = calloc((size_t) ni + SLACK, sizeof(int));
    MPI_Aint *aa = calloc((size_t) na + SLACK, sizeof(MPI_Aint));
    MPI_Datatype *ad = malloc(((size_t) nd + SLACK) * sizeof(MPI_Datatype));
    CHECK(NULL != ai && NULL != aa && NULL != ad);
    if (NULL == ai || NULL == aa || NULL == ad) {
        free(ai); free(aa); free(ad);
        return;
    }
    for (size_t i = 0; i < (size_t) nd + SLACK; ++i) {
        ad[i] = POISON;
    }

    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents(st, ni + SLACK, na + SLACK, nd + SLACK, ai,
                                   aa, ad));

    /* Every returned value is checked. */
    CHECK(2 == ai[0]);          /* count */
    CHECK(bl[0] == ai[1]);      /* blocklengths */
    CHECK(bl[1] == ai[2]);
    CHECK(disp[0] == aa[0]);    /* displacements */
    CHECK(disp[1] == aa[1]);
    CHECK(MPI_INT == ad[0]);
    CHECK(MPI_DOUBLE == ad[1]);
    for (size_t i = (size_t) nd; i < (size_t) nd + SLACK; ++i) {
        CHECK(POISON == ad[i]);
    }

    free(ai);
    free(aa);
    free(ad);
    CHECK(MPI_SUCCESS == MPI_Type_free(&st));
}

/* ===================================================================
 * (D) Large-count round-trip through every derived-type constructor.
 *
 * PR #13460 added MPI_Count type-punning to *all* the datatype
 * constructors, but only contiguous was previously tested with a count
 * above INT_MAX.  For each combiner we build a type with at least one
 * argument above INT_MAX (keeping the number of blocks/dims tiny so only
 * O(1) datatype metadata is allocated -- no large buffers), then verify
 * the MPI 5.0 decode:
 *
 *   - MPI_Type_get_envelope_c reports the standard (ni, na, nlc, nd).
 *   - MPI_Type_get_contents_c returns every above-INT_MAX argument
 *     undamaged, in the array the standard requires.
 *   - MPI_Type_size_c reflects the 64-bit size.
 *
 * Per MPI 5.0 sec 19.2, the "_c" datatype constructors type *all* count
 * and byte-displacement arguments as MPI_Count -- there are no MPI_Aint
 * arguments left in any "_c" datatype constructor.  Therefore byte
 * displacements/strides decode into array_of_large_counts and
 * num_addresses is 0 for every combiner below (including RESIZED, whose
 * lb/extent are MPI_Count in the _c interface).
 * =================================================================== */

/* INT_MAX as an MPI_Count; "BIG + k" is safely above INT_MAX. */
#define BIG ((MPI_Count) INT_MAX)

static void check_env_c(const char *label, MPI_Datatype t,
                        int expect_combiner, MPI_Count eni, MPI_Count ena,
                        MPI_Count enlc, MPI_Count end)
{
    MPI_Count ni = -1, na = -1, nlc = -1, nd = -1;
    int combiner = -1;
    CHECK(MPI_SUCCESS
          == MPI_Type_get_envelope_c(t, &ni, &na, &nlc, &nd, &combiner));
    printf("  %-15s combiner=%2d ni=%lld na=%lld nlc=%lld nd=%lld\n", label,
           combiner, (long long) ni, (long long) na, (long long) nlc,
           (long long) nd);
    fflush(stdout);
    CHECK(expect_combiner == combiner);
    CHECK(eni == ni);
    CHECK(ena == na);
    CHECK(enlc == nlc);
    CHECK(end == nd);
}

/* Verify a type's full footprint: size, lower bound, and extent. */
static void check_size_extent_c(MPI_Datatype t, MPI_Count esize, MPI_Count elb,
                                MPI_Count eextent)
{
    MPI_Count size = -1, lb = 1, extent = -1;
    CHECK(MPI_SUCCESS == MPI_Type_size_c(t, &size));
    CHECK(esize == size);
    CHECK(MPI_SUCCESS == MPI_Type_get_extent_c(t, &lb, &extent));
    CHECK(elb == lb);
    CHECK(eextent == extent);
}

static void test_vector_c_bigcount(void)
{
    MPI_Count count = BIG + 5, blen = BIG + 6, stride = BIG + 7;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_vector_c(count, blen, stride, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("vector_c", t, MPI_COMBINER_VECTOR, 0, 0, 3, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[3] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 3, 1, ai, aa, alc, ad));
    CHECK(count == alc[0]);
    CHECK(blen == alc[1]);
    CHECK(stride == alc[2]);
    CHECK(MPI_BYTE == ad[0]);

    /* size = count*blen; extent spans the last block: (count-1)*stride+blen
     * (oldextent 1).  ~4.6e18, fits in MPI_Count. */
    check_size_extent_c(t, count * blen, 0, (count - 1) * stride + blen);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hvector_c_bigcount(void)
{
    MPI_Count count = BIG + 5, blen = BIG + 6, stride = BIG + 7; /* bytes */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_hvector_c(count, blen, stride, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("hvector_c", t, MPI_COMBINER_HVECTOR, 0, 0, 3, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[3] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 3, 1, ai, aa, alc, ad));
    CHECK(count == alc[0]);
    CHECK(blen == alc[1]);
    CHECK(stride == alc[2]); /* byte stride survives as a large count */
    CHECK(MPI_BYTE == ad[0]);

    check_size_extent_c(t, count * blen, 0, (count - 1) * stride + blen);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_indexed_c_bigcount(void)
{
    MPI_Count bl[2] = {BIG + 5, 3};
    MPI_Count disp[2] = {0, BIG + 6}; /* element units */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_indexed_c(2, bl, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("indexed_c", t, MPI_COMBINER_INDEXED, 0, 0, 5, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[5] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 5, 1, ai, aa, alc, ad));
    CHECK(2 == alc[0]);        /* count */
    CHECK(bl[0] == alc[1]);    /* blocklengths */
    CHECK(bl[1] == alc[2]);
    CHECK(disp[0] == alc[3]);  /* displacements */
    CHECK(disp[1] == alc[4]);
    CHECK(MPI_BYTE == ad[0]);

    /* size = sum of blocklengths; extent spans from the lowest displacement
     * to the highest displacement+blocklength (element units, oldextent 1). */
    MPI_Count ub = disp[0] + bl[0];
    if (disp[1] + bl[1] > ub) { ub = disp[1] + bl[1]; }
    check_size_extent_c(t, bl[0] + bl[1], 0, ub);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hindexed_c_bigcount(void)
{
    MPI_Count bl[2] = {BIG + 5, 3};
    MPI_Count disp[2] = {0, BIG + 6}; /* bytes */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_hindexed_c(2, bl, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("hindexed_c", t, MPI_COMBINER_HINDEXED, 0, 0, 5, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[5] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 5, 1, ai, aa, alc, ad));
    CHECK(2 == alc[0]);
    CHECK(bl[0] == alc[1]);
    CHECK(bl[1] == alc[2]);
    CHECK(disp[0] == alc[3]);
    CHECK(disp[1] == alc[4]);
    CHECK(MPI_BYTE == ad[0]);

    /* Byte displacements, oldextent 1: same footprint formula as indexed. */
    MPI_Count ub = disp[0] + bl[0];
    if (disp[1] + bl[1] > ub) { ub = disp[1] + bl[1]; }
    check_size_extent_c(t, bl[0] + bl[1], 0, ub);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_indexed_block_c_bigcount(void)
{
    MPI_Count blen = BIG + 5;
    MPI_Count disp[2] = {0, BIG + 6}; /* element units */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_indexed_block_c(2, blen, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("indexed_block_c", t, MPI_COMBINER_INDEXED_BLOCK, 0, 0, 4, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[4] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 4, 1, ai, aa, alc, ad));
    CHECK(2 == alc[0]);        /* count */
    CHECK(blen == alc[1]);     /* blocklength */
    CHECK(disp[0] == alc[2]);  /* displacements */
    CHECK(disp[1] == alc[3]);
    CHECK(MPI_BYTE == ad[0]);

    /* size = count*blocklength; extent spans to the farthest block end. */
    MPI_Count ub = disp[0] + blen;
    if (disp[1] + blen > ub) { ub = disp[1] + blen; }
    check_size_extent_c(t, 2 * blen, 0, ub);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hindexed_block_c_bigcount(void)
{
    MPI_Count blen = BIG + 5;
    MPI_Count disp[2] = {0, BIG + 6}; /* bytes */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_hindexed_block_c(2, blen, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("hindexed_block_c", t, MPI_COMBINER_HINDEXED_BLOCK, 0, 0, 4, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[4] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 4, 1, ai, aa, alc, ad));
    CHECK(2 == alc[0]);
    CHECK(blen == alc[1]);
    CHECK(disp[0] == alc[2]);
    CHECK(disp[1] == alc[3]);
    CHECK(MPI_BYTE == ad[0]);

    /* Byte displacements, oldextent 1: same footprint as indexed_block. */
    MPI_Count ub = disp[0] + blen;
    if (disp[1] + blen > ub) { ub = disp[1] + blen; }
    check_size_extent_c(t, 2 * blen, 0, ub);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_struct_c_bigcount(void)
{
    MPI_Count bl[2] = {BIG + 5, 3};
    MPI_Count disp[2] = {0, BIG + 6}; /* bytes */
    MPI_Datatype types[2] = {MPI_BYTE, MPI_INT};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_struct_c(2, bl, disp, types, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("struct_c", t, MPI_COMBINER_STRUCT, 0, 0, 5, 2);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[5] = {0};
    MPI_Datatype ad[2] = {MPI_DATATYPE_NULL, MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 5, 2, ai, aa, alc, ad));
    CHECK(2 == alc[0]);
    CHECK(bl[0] == alc[1]);
    CHECK(bl[1] == alc[2]);
    CHECK(disp[0] == alc[3]);
    CHECK(disp[1] == alc[4]);
    CHECK(MPI_BYTE == ad[0]);
    CHECK(MPI_INT == ad[1]);

    /* size = bl0 bytes + bl1 ints.  extent = upper bound rounded up to the
     * max member alignment (MPI 5.0 eq. 5.1); here that is alignof(int). */
    MPI_Count esize = bl[0] * 1 + bl[1] * (MPI_Count) sizeof(int);
    MPI_Count ub = disp[1] + bl[1] * (MPI_Count) sizeof(int); /* > disp0+bl0 */
    MPI_Count align = (MPI_Count) _Alignof(int);
    check_size_extent_c(t, esize, 0, ((ub + align - 1) / align) * align);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_subarray_c_bigcount(void)
{
    /* 2-D with size, subsize, AND start all above INT_MAX in the first
     * dimension (start + subsize <= size keeps it legal), so the punning
     * of the subsize and start arrays is exercised too -- not just sizes. */
    MPI_Count sizes[2] = {2 * BIG + 100, 6};
    MPI_Count subsizes[2] = {BIG + 5, 2};
    MPI_Count starts[2] = {BIG + 3, 1};
    /* Fail loudly if a future constant edit makes the subarray illegal. */
    CHECK(starts[0] + subsizes[0] <= sizes[0]);
    CHECK(starts[1] + subsizes[1] <= sizes[1]);
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_subarray_c(2, sizes, subsizes, starts,
                                        MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    /* ndims and order are int -> integers; sizes/subsizes/starts are
     * MPI_Count -> large counts. */
    check_env_c("subarray_c", t, MPI_COMBINER_SUBARRAY, 2, 0, 6, 1);

    int ai[2] = {0};
    MPI_Aint aa[1];
    MPI_Count alc[6] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 2, 0, 6, 1, ai, aa, alc, ad));
    CHECK(2 == ai[0]);             /* ndims */
    CHECK(MPI_ORDER_C == ai[1]);   /* order */
    CHECK(sizes[0] == alc[0]);
    CHECK(sizes[1] == alc[1]);
    CHECK(subsizes[0] == alc[2]);
    CHECK(subsizes[1] == alc[3]);
    CHECK(starts[0] == alc[4]);
    CHECK(starts[1] == alc[5]);
    CHECK(MPI_BYTE == ad[0]);

    /* size = product of subsizes (the local block); extent = product of the
     * full sizes (lb 0).  This is what caught the darray int-truncation bug,
     * so subarray is checked the same way. */
    check_size_extent_c(t, subsizes[0] * subsizes[1], 0, sizes[0] * sizes[1]);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_darray_c_bigcount(void)
{
    /* Single process, two dimensions, block distribution: rank 0 owns the
     * entire (huge) global array.  Two gsizes exercise a multi-entry
     * large-count array. */
    MPI_Count gsizes[2] = {BIG + 5, BIG + 6};
    int distribs[2] = {MPI_DISTRIBUTE_BLOCK, MPI_DISTRIBUTE_BLOCK};
    int dargs[2] = {MPI_DISTRIBUTE_DFLT_DARG, MPI_DISTRIBUTE_DFLT_DARG};
    int psizes[2] = {1, 1};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_darray_c(1, 0, 2, gsizes, distribs, dargs,
                                      psizes, MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    /* All ints (size, rank, ndims, distribs[2], dargs[2], psizes[2], order)
     * give ni = 4 + 3*ndims = 10; the gsizes are the only large counts
     * (nlc = ndims = 2). */
    check_env_c("darray_c", t, MPI_COMBINER_DARRAY, 10, 0, 2, 1);

    int ai[10] = {0};
    MPI_Aint aa[1];
    MPI_Count alc[2] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 10, 0, 2, 1, ai, aa, alc, ad));
    CHECK(1 == ai[0]);                          /* size */
    CHECK(0 == ai[1]);                          /* rank */
    CHECK(2 == ai[2]);                          /* ndims */
    CHECK(MPI_DISTRIBUTE_BLOCK == ai[3]);       /* distribs[0] */
    CHECK(MPI_DISTRIBUTE_BLOCK == ai[4]);       /* distribs[1] */
    CHECK(MPI_DISTRIBUTE_DFLT_DARG == ai[5]);   /* dargs[0] */
    CHECK(MPI_DISTRIBUTE_DFLT_DARG == ai[6]);   /* dargs[1] */
    CHECK(1 == ai[7]);                          /* psizes[0] */
    CHECK(1 == ai[8]);                          /* psizes[1] */
    CHECK(MPI_ORDER_C == ai[9]);                /* order */
    CHECK(gsizes[0] == alc[0]);
    CHECK(gsizes[1] == alc[1]);
    CHECK(MPI_BYTE == ad[0]);

    /* One process owns the whole array, so both the size (local bytes) and
     * the extent equal the product of the gsizes.  Asserting size here is
     * what catches the block()/cyclic() int-truncation bug. */
    check_size_extent_c(t, gsizes[0] * gsizes[1], 0, gsizes[0] * gsizes[1]);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_darray_cyclic_c_bigcount(void)
{
    /* CYCLIC distribution drives the cyclic() helper -- a different local-size
     * computation from block() -- which also widened to 64-bit.  One process
     * owns the whole (huge) global array. */
    MPI_Count gsizes[1] = {BIG + 5};
    int distribs[1] = {MPI_DISTRIBUTE_CYCLIC};
    int dargs[1] = {MPI_DISTRIBUTE_DFLT_DARG};
    int psizes[1] = {1};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_darray_c(1, 0, 1, gsizes, distribs, dargs, psizes,
                                      MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("darray_cyclic_c", t, MPI_COMBINER_DARRAY, 7, 0, 1, 1);

    /* Local size == whole array (one process); gsize survives cyclic(). */
    check_size_extent_c(t, gsizes[0], 0, gsizes[0]);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_darray_multiproc_c_bigcount(void)
{
    /* Two processes, query rank 1: blksize = ceil(gsize/2) and the rank*blksize
     * start offset both exceed INT_MAX, exercising the 64-bit products in
     * block() that the single-process darray tests (rank 0, nprocs 1) never
     * reach -- with int arithmetic those products would overflow. */
    MPI_Count gsizes[1] = {4 * BIG};
    int distribs[1] = {MPI_DISTRIBUTE_BLOCK};
    int dargs[1] = {MPI_DISTRIBUTE_DFLT_DARG};
    int psizes[1] = {2};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_darray_c(2, 1, 1, gsizes, distribs, dargs, psizes,
                                      MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("darray_2proc_c", t, MPI_COMBINER_DARRAY, 7, 0, 1, 1);

    /* BLOCK over 2 ranks: blksize = 2*BIG; rank 1 owns the upper 2*BIG
     * elements; the type extent spans the full global array (4*BIG). */
    check_size_extent_c(t, 2 * BIG, 0, 4 * BIG);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_darray_blockcyclic_c_bigcount(void)
{
    /* CYCLIC with an explicit block size > 1 and a gsize not divisible by
     * nprocs*blksize forces rem != 0, exercising cyclic()'s trailing-struct
     * branch (blklens[1] = rem, disps[1] = count*stride) with count*stride
     * above INT_MAX -- untested by the DFLT_DARG (blksize 1) cyclic case. */
    MPI_Count gsizes[1] = {BIG + 7}; /* not a multiple of 3 */
    int distribs[1] = {MPI_DISTRIBUTE_CYCLIC};
    int dargs[1] = {3};
    int psizes[1] = {1};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_darray_c(1, 0, 1, gsizes, distribs, dargs, psizes,
                                      MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    check_env_c("darray_blkcyc_c", t, MPI_COMBINER_DARRAY, 7, 0, 1, 1);

    /* Single process owns everything: size == extent == gsize. */
    check_size_extent_c(t, gsizes[0], 0, gsizes[0]);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_resized_c_bigcount(void)
{
    MPI_Count lb = BIG + 5, extent = BIG + 6;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_resized_c(MPI_BYTE, lb, extent, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    /* MPI 5.0 sec 19.2: MPI_Type_create_resized_c types lb and extent as
     * MPI_Count (not MPI_Aint), so -- exactly as for every other "_c"
     * constructor above -- MPI_Type_get_contents_c must return them in
     * array_of_large_counts with num_addresses == 0. */
    check_env_c("resized_c", t, MPI_COMBINER_RESIZED, 0, 0, 2, 1);

    int ai[1];
    MPI_Aint aa[1];
    MPI_Count alc[2] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, 0, 0, 2, 1, ai, aa, alc, ad));
    CHECK(lb == alc[0]);
    CHECK(extent == alc[1]);
    CHECK(MPI_BYTE == ad[0]);

    /* Resizing does not change the data size (one MPI_BYTE); the requested
     * lb and extent are reported back by get_extent_c. */
    check_size_extent_c(t, 1, lb, extent);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));

    /* A negative lb above INT_MAX in magnitude must also round-trip through
     * the large-count array on the public _c path (signed value preserved). */
    MPI_Count nlb = -(BIG + 5), next = BIG + 6;
    MPI_Datatype t2 = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_resized_c(MPI_BYTE, nlb, next, &t2));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t2));
    check_env_c("resized_c(neg)", t2, MPI_COMBINER_RESIZED, 0, 0, 2, 1);
    MPI_Count alc2[2] = {0};
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t2, 0, 0, 2, 1, ai, aa, alc2, ad));
    CHECK(nlb == alc2[0]);
    CHECK(next == alc2[1]);
    check_size_extent_c(t2, 1, nlb, next);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t2));
}

/* ===================================================================
 * (E) Classic (small-count, int) decode coverage.
 *
 * The (D) tests drive only the "_c" interface, which stores every count
 * and byte displacement as a large count (na == 0).  The classic
 * interface is a *separate* code path (the "#else" branch of each
 * binding): block lengths and element counts land in array_of_integers,
 * while byte displacements/strides (MPI_Aint) land in array_of_addresses.
 * Nothing else in test/datatype exercises get_envelope/get_contents, so
 * these build each constructor with the classic int API and small values
 * and assert the full MPI 5.0 sec 5.1.13 classic decode -- combiner,
 * (ni, na, nd), and the value in every slot.
 * =================================================================== */

static void check_env_classic(const char *label, MPI_Datatype t,
                              int expect_combiner, int eni, int ena, int end)
{
    int ni = -1, na = -1, nd = -1, combiner = -1;
    CHECK(MPI_SUCCESS == MPI_Type_get_envelope(t, &ni, &na, &nd, &combiner));
    printf("  %-15s combiner=%2d ni=%d na=%d nd=%d\n", label, combiner, ni, na,
           nd);
    fflush(stdout);
    CHECK(expect_combiner == combiner);
    CHECK(eni == ni);
    CHECK(ena == na);
    CHECK(end == nd);
}

/* Classic-API footprint check: size, lower bound, and extent. */
static void check_size_extent_classic(MPI_Datatype t, int esize, MPI_Aint elb,
                                      MPI_Aint eextent)
{
    int size = -1;
    CHECK(MPI_SUCCESS == MPI_Type_size(t, &size));
    CHECK(esize == size);
    MPI_Aint lb = 1, extent = -1;
    CHECK(MPI_SUCCESS == MPI_Type_get_extent(t, &lb, &extent));
    CHECK(elb == lb);
    CHECK(eextent == extent);
}

static void test_contiguous_classic(void)
{
    int count = 7;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_contiguous(count, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("contiguous", t, MPI_COMBINER_CONTIGUOUS, 1, 0, 1);

    int ai[1] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 1, 0, 1, ai, aa, ad));
    CHECK(count == ai[0]);
    CHECK(MPI_BYTE == ad[0]);
    check_size_extent_classic(t, count, 0, count);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_vector_classic(void)
{
    int count = 4, blen = 3, stride = 5;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_vector(count, blen, stride, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("vector", t, MPI_COMBINER_VECTOR, 3, 0, 1);

    int ai[3] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 3, 0, 1, ai, aa, ad));
    CHECK(count == ai[0]);
    CHECK(blen == ai[1]);
    CHECK(stride == ai[2]);
    CHECK(MPI_BYTE == ad[0]);
    check_size_extent_classic(t, count * blen, 0, (count - 1) * stride + blen);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hvector_classic(void)
{
    int count = 4, blen = 3;
    MPI_Aint stride = 64; /* bytes -> array_of_addresses */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_hvector(count, blen, stride, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("hvector", t, MPI_COMBINER_HVECTOR, 2, 1, 1);

    int ai[2] = {0};
    MPI_Aint aa[1] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 2, 1, 1, ai, aa, ad));
    CHECK(count == ai[0]);
    CHECK(blen == ai[1]);
    CHECK(stride == aa[0]);
    CHECK(MPI_BYTE == ad[0]);
    check_size_extent_classic(t, count * blen, 0, (count - 1) * stride + blen);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_indexed_classic(void)
{
    int bl[2] = {5, 3};
    int disp[2] = {0, 8}; /* element units */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_indexed(2, bl, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("indexed", t, MPI_COMBINER_INDEXED, 5, 0, 1);

    int ai[5] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 5, 0, 1, ai, aa, ad));
    CHECK(2 == ai[0]);        /* count */
    CHECK(bl[0] == ai[1]);
    CHECK(bl[1] == ai[2]);
    CHECK(disp[0] == ai[3]);  /* element displacements stay in integers */
    CHECK(disp[1] == ai[4]);
    CHECK(MPI_BYTE == ad[0]);
    {
        MPI_Aint ub = disp[0] + bl[0];
        if (disp[1] + bl[1] > ub) { ub = disp[1] + bl[1]; }
        check_size_extent_classic(t, bl[0] + bl[1], 0, ub);
    }
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hindexed_classic(void)
{
    int bl[2] = {5, 3};
    MPI_Aint disp[2] = {0, 64}; /* bytes -> array_of_addresses */
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_hindexed(2, bl, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("hindexed", t, MPI_COMBINER_HINDEXED, 3, 2, 1);

    int ai[3] = {0};
    MPI_Aint aa[2] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 3, 2, 1, ai, aa, ad));
    CHECK(2 == ai[0]);
    CHECK(bl[0] == ai[1]);
    CHECK(bl[1] == ai[2]);
    CHECK(disp[0] == aa[0]);
    CHECK(disp[1] == aa[1]);
    CHECK(MPI_BYTE == ad[0]);
    {
        MPI_Aint ub = disp[0] + bl[0];
        if (disp[1] + bl[1] > ub) { ub = disp[1] + bl[1]; }
        check_size_extent_classic(t, bl[0] + bl[1], 0, ub);
    }
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_indexed_block_classic(void)
{
    int blen = 5;
    int disp[2] = {0, 8};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_indexed_block(2, blen, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("indexed_block", t, MPI_COMBINER_INDEXED_BLOCK, 4, 0, 1);

    int ai[4] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 4, 0, 1, ai, aa, ad));
    CHECK(2 == ai[0]);        /* count */
    CHECK(blen == ai[1]);     /* blocklength */
    CHECK(disp[0] == ai[2]);
    CHECK(disp[1] == ai[3]);
    CHECK(MPI_BYTE == ad[0]);
    {
        MPI_Aint ub = disp[0] + blen;
        if (disp[1] + blen > ub) { ub = disp[1] + blen; }
        check_size_extent_classic(t, 2 * blen, 0, ub);
    }
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_hindexed_block_classic(void)
{
    int blen = 5;
    MPI_Aint disp[2] = {0, 64};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_hindexed_block(2, blen, disp, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("hindexed_block", t, MPI_COMBINER_HINDEXED_BLOCK, 2, 2, 1);

    int ai[2] = {0};
    MPI_Aint aa[2] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 2, 2, 1, ai, aa, ad));
    CHECK(2 == ai[0]);
    CHECK(blen == ai[1]);
    CHECK(disp[0] == aa[0]);
    CHECK(disp[1] == aa[1]);
    CHECK(MPI_BYTE == ad[0]);
    {
        MPI_Aint ub = disp[0] + blen;
        if (disp[1] + blen > ub) { ub = disp[1] + blen; }
        check_size_extent_classic(t, 2 * blen, 0, ub);
    }
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_struct_classic(void)
{
    int bl[2] = {5, 3};
    MPI_Aint disp[2] = {0, 64};
    MPI_Datatype types[2] = {MPI_BYTE, MPI_INT};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_struct(2, bl, disp, types, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("struct", t, MPI_COMBINER_STRUCT, 3, 2, 2);

    int ai[3] = {0};
    MPI_Aint aa[2] = {0};
    MPI_Datatype ad[2] = {MPI_DATATYPE_NULL, MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 3, 2, 2, ai, aa, ad));
    CHECK(2 == ai[0]);
    CHECK(bl[0] == ai[1]);
    CHECK(bl[1] == ai[2]);
    CHECK(disp[0] == aa[0]);
    CHECK(disp[1] == aa[1]);
    CHECK(MPI_BYTE == ad[0]);
    CHECK(MPI_INT == ad[1]);
    {
        MPI_Aint ub = disp[1] + bl[1] * (MPI_Aint) sizeof(int); /* > disp0+bl0 */
        MPI_Aint align = (MPI_Aint) _Alignof(int);
        check_size_extent_classic(t, bl[0] * 1 + bl[1] * (int) sizeof(int), 0,
                                  ((ub + align - 1) / align) * align);
    }
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_subarray_classic(void)
{
    int sizes[2] = {6, 4};
    int subsizes[2] = {2, 2};
    int starts[2] = {1, 1};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_subarray(2, sizes, subsizes, starts, MPI_ORDER_C,
                                      MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("subarray", t, MPI_COMBINER_SUBARRAY, 8, 0, 1);

    int ai[8] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 8, 0, 1, ai, aa, ad));
    CHECK(2 == ai[0]); /* ndims */
    CHECK(sizes[0] == ai[1]);
    CHECK(sizes[1] == ai[2]);
    CHECK(subsizes[0] == ai[3]);
    CHECK(subsizes[1] == ai[4]);
    CHECK(starts[0] == ai[5]);
    CHECK(starts[1] == ai[6]);
    CHECK(MPI_ORDER_C == ai[7]); /* order */
    CHECK(MPI_BYTE == ad[0]);
    check_size_extent_classic(t, subsizes[0] * subsizes[1], 0,
                              sizes[0] * sizes[1]);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_darray_classic(void)
{
    int gsizes[1] = {12};
    int distribs[1] = {MPI_DISTRIBUTE_BLOCK};
    int dargs[1] = {MPI_DISTRIBUTE_DFLT_DARG};
    int psizes[1] = {1};
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_darray(1, 0, 1, gsizes, distribs, dargs, psizes,
                                    MPI_ORDER_C, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));
    check_env_classic("darray", t, MPI_COMBINER_DARRAY, 8, 0, 1);

    int ai[8] = {0};
    MPI_Aint aa[1];
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 8, 0, 1, ai, aa, ad));
    CHECK(1 == ai[0]);                        /* size */
    CHECK(0 == ai[1]);                        /* rank */
    CHECK(1 == ai[2]);                        /* ndims */
    CHECK(gsizes[0] == ai[3]);
    CHECK(MPI_DISTRIBUTE_BLOCK == ai[4]);
    CHECK(MPI_DISTRIBUTE_DFLT_DARG == ai[5]);
    CHECK(1 == ai[6]);                        /* psizes */
    CHECK(MPI_ORDER_C == ai[7]);              /* order */
    CHECK(MPI_BYTE == ad[0]);
    check_size_extent_classic(t, gsizes[0], 0, gsizes[0]);
    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

/* Regression guard for the classic (non-_c) resized path, which shares the
 * binding fixed alongside the _c path: lb/extent are MPI_Aint and must still
 * decode into array_of_addresses (na=2, no large counts). */
static void test_resized_classic(void)
{
    MPI_Aint lb = -3, extent = 4096;
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS == MPI_Type_create_resized(MPI_BYTE, lb, extent, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    int ni = -1, na = -1, nd = -1, combiner = -1;
    CHECK(MPI_SUCCESS == MPI_Type_get_envelope(t, &ni, &na, &nd, &combiner));
    CHECK(MPI_COMBINER_RESIZED == combiner);
    CHECK(0 == ni);
    CHECK(2 == na);
    CHECK(1 == nd);

    int ai[1];
    MPI_Aint aa[2] = {0};
    MPI_Datatype ad[1] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS == MPI_Type_get_contents(t, 0, 2, 1, ai, aa, ad));
    CHECK(lb == aa[0]);
    CHECK(extent == aa[1]);
    CHECK(MPI_BYTE == ad[0]);

    /* Resizing keeps the data size (one MPI_BYTE) but applies lb/extent. */
    check_size_extent_classic(t, 1, lb, extent);

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

static void test_constructors_classic(void)
{
    test_contiguous_classic();
    test_vector_classic();
    test_hvector_classic();
    test_indexed_classic();
    test_hindexed_classic();
    test_indexed_block_classic();
    test_hindexed_block_classic();
    test_struct_classic();
    test_subarray_classic();
    test_darray_classic();
    test_resized_classic();
}

static void test_constructors_c_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped big-count constructors)\n");
        return;
    }
    test_vector_c_bigcount();
    test_hvector_c_bigcount();
    test_indexed_c_bigcount();
    test_hindexed_c_bigcount();
    test_indexed_block_c_bigcount();
    test_hindexed_block_c_bigcount();
    test_struct_c_bigcount();
    test_subarray_c_bigcount();
    test_darray_c_bigcount();
    test_darray_cyclic_c_bigcount();
    test_darray_multiproc_c_bigcount();
    test_darray_blockcyclic_c_bigcount();
    test_resized_c_bigcount();
}

/* (F) get_elements with an element count above INT_MAX.  We seed the status
 * with the public MPI_Status_set_elements_c (no multi-GB transfer needed)
 * so the byte->element division is genuinely exercised by the binding. */
static void test_get_elements_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }

    MPI_Status status;
    memset(&status, 0, sizeof(status)); /* defined fields for memchecker builds */

    /* MPI_BYTE: element count == byte count. */
    MPI_Count nbytes = (MPI_Count) INT_MAX + 100;
    CHECK(MPI_SUCCESS == MPI_Status_set_elements_c(&status, MPI_BYTE, nbytes));

    MPI_Count ec = -1;
    CHECK(MPI_SUCCESS == MPI_Get_elements_c(&status, MPI_BYTE, &ec));
    CHECK(nbytes == ec);

    /* The classic int interface cannot represent > INT_MAX elements, so it
     * must return MPI_UNDEFINED (MPI 3.0). */
    int eci = 0;
    CHECK(MPI_SUCCESS == MPI_Get_elements(&status, MPI_BYTE, &eci));
    CHECK(MPI_UNDEFINED == eci);

    /* MPI_INT: seed the element count and let the binding divide the byte
     * count back out -- a real round-trip, with a result above INT_MAX. */
    MPI_Count nints = (MPI_Count) INT_MAX + 99;
    CHECK(MPI_SUCCESS == MPI_Status_set_elements_c(&status, MPI_INT, nints));
    ec = -1;
    CHECK(MPI_SUCCESS == MPI_Get_elements_c(&status, MPI_INT, &ec));
    CHECK(nints == ec);

    /* A byte count that is NOT an integral number of MPI_INT elements must
     * decode to MPI_UNDEFINED (MPI 5.0 sec 5.1.11).  There is no public way
     * to seed a partial byte count, so set the internal _ucount field
     * directly for just this case. */
    status._ucount = (size_t) nints * sizeof(int) + 1;
    ec = 0;
    CHECK(MPI_SUCCESS == MPI_Get_elements_c(&status, MPI_INT, &ec));
    CHECK(MPI_UNDEFINED == ec);

    printf("  get_elements_c: MPI_BYTE/%lld -> %lld, MPI_INT -> %lld; "
           "classic int & partial -> MPI_UNDEFINED\n",
           (long long) nbytes, (long long) nbytes, (long long) nints);
    fflush(stdout);
}

/* #14055 over-read check, factored so it can run against more than the
 * struct combiner in test_get_contents_c_oversized: with output arrays
 * larger than the envelope, get_contents_c must fill only the real
 * constituent datatypes and leave the surplus slots untouched. */
static void check_no_overread_c(const char *label, MPI_Datatype t)
{
    enum { SLACK = 8 };
    MPI_Count ni = -1, na = -1, nlc = -1, nd = -1;
    int combiner = -1;
    CHECK(MPI_SUCCESS
          == MPI_Type_get_envelope_c(t, &ni, &na, &nlc, &nd, &combiner));
    if (ni < 0 || na < 0 || nlc < 0 || nd < 0) {
        CHECK(false);
        return;
    }
    int *ai = calloc((size_t) ni + SLACK, sizeof(int));
    MPI_Aint *aa = calloc((size_t) na + SLACK, sizeof(MPI_Aint));
    MPI_Count *alc = calloc((size_t) nlc + SLACK, sizeof(MPI_Count));
    MPI_Datatype *ad = malloc(((size_t) nd + SLACK) * sizeof(MPI_Datatype));
    CHECK(NULL != ai && NULL != aa && NULL != alc && NULL != ad);
    if (NULL == ai || NULL == aa || NULL == alc || NULL == ad) {
        free(ai); free(aa); free(alc); free(ad);
        return;
    }
    for (size_t i = 0; i < (size_t) nd + SLACK; ++i) {
        ad[i] = POISON;
    }
    CHECK(MPI_SUCCESS
          == MPI_Type_get_contents_c(t, ni + SLACK, na + SLACK, nlc + SLACK,
                                     nd + SLACK, ai, aa, alc, ad));
    for (size_t i = 0; i < (size_t) nd; ++i) {
        CHECK(POISON != ad[i]); /* real constituents filled in */
    }
    for (size_t i = (size_t) nd; i < (size_t) nd + SLACK; ++i) {
        CHECK(POISON == ad[i]); /* surplus slots untouched */
    }
    printf("  no-overread: %-16s nd=%lld\n", label, (long long) nd);
    fflush(stdout);
    free(ai); free(aa); free(alc); free(ad);
}

static void test_oversized_more_combiners(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }
    /* hindexed_block and resized are the combiners whose large-count decode
     * this work changed; verify their get_contents_c has no over-read too. */
    MPI_Count blen = BIG + 5;
    MPI_Count disp[2] = {0, BIG + 6};
    MPI_Datatype hib = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_hindexed_block_c(2, blen, disp, MPI_BYTE, &hib));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&hib));
    check_no_overread_c("hindexed_block_c", hib);
    CHECK(MPI_SUCCESS == MPI_Type_free(&hib));

    MPI_Datatype rsz = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_create_resized_c(MPI_BYTE, BIG + 5, BIG + 6, &rsz));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&rsz));
    check_no_overread_c("resized_c", rsz);
    CHECK(MPI_SUCCESS == MPI_Type_free(&rsz));
}

/* A datatype built with a large count cannot be decoded through the classic
 * (non-_c) envelope/contents calls: MPI 5.0 sec 5.1.13 requires MPI_ERR_TYPE
 * rather than silent truncation.  This is the exact mismatch a user hits when
 * they build with MPI_Type_*_c and query with the classic API. */
static void test_classic_rejects_bigcount(void)
{
    if (!have_64bit_counts()) {
        printf("  (32-bit size_t: skipped)\n");
        return;
    }
    MPI_Datatype t = MPI_DATATYPE_NULL;
    CHECK(MPI_SUCCESS
          == MPI_Type_contiguous_c((MPI_Count) INT_MAX + 7, MPI_BYTE, &t));
    CHECK(MPI_SUCCESS == MPI_Type_commit(&t));

    /* Both classic decode entry points must fail (num_large_counts > 0). */
    int ni = -1, na = -1, nd = -1, combiner = -1;
    CHECK(MPI_SUCCESS != MPI_Type_get_envelope(t, &ni, &na, &nd, &combiner));

    int ai[4] = {0};
    MPI_Aint aa[4] = {0};
    MPI_Datatype ad[4] = {MPI_DATATYPE_NULL};
    CHECK(MPI_SUCCESS != MPI_Type_get_contents(t, 4, 4, 4, ai, aa, ad));

    CHECK(MPI_SUCCESS == MPI_Type_free(&t));
}

int main(int argc, char *argv[])
{
    int rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        fprintf(stderr, "MPI_Init failed (rc=%d)\n", rc);
        return 1;
    }
    /* Report MPI errors as return codes so a single failing call records a
     * CHECK failure and the test keeps running, instead of aborting the
     * whole program on the first problem. */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    printf("--- MPI_Type_contiguous_c (count > INT_MAX) ---\n");
    test_contiguous_c_bigcount();
    printf("--- MPI_Type_get_contents_c oversized arrays (#14055, _c) ---\n");
    test_get_contents_c_oversized();
    printf("--- MPI_Type_get_contents oversized arrays (#14055, classic) ---\n");
    test_get_contents_oversized();
    printf("--- get_contents_c oversized arrays, more combiners (#14055) ---\n");
    test_oversized_more_combiners();
    printf("--- classic get_envelope/get_contents reject a _c big-count type ---\n");
    test_classic_rejects_bigcount();
    printf("--- derived-type constructors (classic) with small counts ---\n");
    test_constructors_classic();
    printf("--- derived-type constructors (_c) with args > INT_MAX ---\n");
    test_constructors_c_bigcount();
    printf("--- MPI_Get_elements[_c] (elements > INT_MAX) ---\n");
    test_get_elements_bigcount();

    MPI_Finalize();

    if (0 == failures) {
        printf("SUCCESS: all MPI big-count datatype checks passed\n");
        return 0;
    }
    fprintf(stderr, "FAILURE: %d MPI big-count datatype check(s) failed\n",
            failures);
    return 1;
}
