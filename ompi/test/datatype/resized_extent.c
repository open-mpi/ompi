/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Regression test for the MPI bound-marker (MPI_Type_create_resized) extent
 * model.  MPI_Type_create_resized installs explicit lower/upper bound markers
 * on a type; per the MPI standard those markers define the extent, propagate
 * into an enclosing derived type, and -- crucially -- suppress the alignment
 * "epsilon" padding that is otherwise applied to a composite's upper bound.
 *
 * The interesting case is a resized extent that is *not* a multiple of the
 * type's alignment (e.g. an int resized to extent 6, alignment 4).  Without
 * the marker semantics the composite's upper bound would be rounded up to the
 * next alignment boundary (6 -> 8), silently changing the extent of any type
 * built on top of it.  The all-multiple-of-alignment resized types exercised
 * by to_self never trip this path, so it is guarded here explicitly.
 */

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures = 0;

static void check_extent(const char *label, ompi_datatype_t *type, ptrdiff_t exp_lb,
                         ptrdiff_t exp_extent, ptrdiff_t exp_true_lb, ptrdiff_t exp_true_extent)
{
    ptrdiff_t lb, extent, true_lb, true_extent;

    ompi_datatype_get_extent(type, &lb, &extent);
    ompi_datatype_get_true_extent(type, &true_lb, &true_extent);

    if (lb == exp_lb && extent == exp_extent && true_lb == exp_true_lb
        && true_extent == exp_true_extent) {
        printf("PASS %-32s lb=%td extent=%td true_lb=%td true_extent=%td\n", label, lb, extent,
               true_lb, true_extent);
    } else {
        printf("FAIL %-32s got lb=%td extent=%td true_lb=%td true_extent=%td; expected lb=%td "
               "extent=%td true_lb=%td true_extent=%td\n",
               label, lb, extent, true_lb, true_extent, exp_lb, exp_extent, exp_true_lb,
               exp_true_extent);
        failures++;
    }
}

/*
 * Pack then unpack a couple of instances of contiguous(3, resized(int, 0, 6))
 * and confirm the engine strides by the marker-defined extent (18), not an
 * epsilon-padded 20.  With extent 18 the six ints live at byte offsets
 * {0, 6, 12, 18, 24, 30}; an epsilon-padded extent of 20 would move the second
 * instance to {20, 26, 32} and corrupt the round trip.
 */
static void check_roundtrip(ompi_datatype_t *type)
{
    const int count = 2;
    const ptrdiff_t positions[6] = {0, 6, 12, 18, 24, 30};
    const int values[6] = {100, 101, 102, 103, 104, 105};
    unsigned char src[64], dst[64], packed[64], expected[64];
    opal_convertor_t *conv;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data;
    int done;

    /* Distinct filler so a stray stride would surface as garbage in the packed
     * stream rather than accidentally matching. */
    memset(src, 0xAA, sizeof(src));
    for (int i = 0; i < 6; i++) {
        int32_t v = (int32_t) values[i];
        memcpy(src + positions[i], &v, sizeof(v));
    }
    for (int i = 0; i < 6; i++) {
        int32_t v = (int32_t) values[i];
        memcpy(expected + i * sizeof(int32_t), &v, sizeof(v));
    }

    conv = opal_convertor_create(opal_local_arch, 0);
    if (OPAL_SUCCESS != opal_convertor_prepare_for_send(conv, &type->super, count, src)) {
        printf("FAIL roundtrip: could not prepare send convertor\n");
        failures++;
        OBJ_RELEASE(conv);
        return;
    }
    iov.iov_base = packed;
    iov.iov_len = sizeof(packed);
    iov_count = 1;
    max_data = sizeof(packed);
    done = opal_convertor_pack(conv, &iov, &iov_count, &max_data);
    OBJ_RELEASE(conv);

    if (1 != done || (6 * sizeof(int32_t)) != max_data) {
        printf("FAIL roundtrip pack: done=%d packed=%zu (expected done=1 packed=%zu)\n", done,
               max_data, 6 * sizeof(int32_t));
        failures++;
        return;
    }
    if (0 != memcmp(packed, expected, 6 * sizeof(int32_t))) {
        printf("FAIL roundtrip pack: packed stream does not match the extent-18 layout\n");
        failures++;
        return;
    }

    memset(dst, 0, sizeof(dst));
    conv = opal_convertor_create(opal_local_arch, 0);
    if (OPAL_SUCCESS != opal_convertor_prepare_for_recv(conv, &type->super, count, dst)) {
        printf("FAIL roundtrip: could not prepare recv convertor\n");
        failures++;
        OBJ_RELEASE(conv);
        return;
    }
    iov.iov_base = packed;
    iov.iov_len = max_data;
    iov_count = 1;
    done = opal_convertor_unpack(conv, &iov, &iov_count, &max_data);
    OBJ_RELEASE(conv);

    for (int i = 0; i < 6; i++) {
        int32_t v;
        memcpy(&v, dst + positions[i], sizeof(v));
        if (v != (int32_t) values[i]) {
            printf("FAIL roundtrip unpack: int at offset %td is %d (expected %d)\n", positions[i],
                   v, values[i]);
            failures++;
            return;
        }
    }
    printf("PASS roundtrip contiguous(3, resized(int, 0, 6)) count=2\n");
}

int main(int argc, char *argv[])
{
    ompi_datatype_t *r6 = NULL, *c3 = NULL;

    (void) argc;
    (void) argv;

    /* make ompi_proc_local() work for the datatype engine */
    struct ompi_proc_t dummy_proc;
    ompi_proc_local_proc = &dummy_proc;

    opal_init(NULL, NULL);
    ompi_datatype_init();

    /* A bare resize simply sets lb/extent; true bounds are unchanged. */
    ompi_datatype_create_resized(&ompi_mpi_int.dt, 0, 6, &r6);
    ompi_datatype_commit(&r6);
    check_extent("resized(int, 0, 6)", r6, 0, 6, 0, 4);

    /* contiguous(3) over the resized int must keep the marker-defined extent
     * (3 * 6 = 18); it must NOT be rounded up to the alignment boundary (20). */
    ompi_datatype_create_contiguous(3, r6, &c3);
    ompi_datatype_commit(&c3);
    check_extent("contiguous(3, resized(int, 0, 6))", c3, 0, 18, 0, 16);

    check_roundtrip(c3);

    ompi_datatype_destroy(&c3);
    ompi_datatype_destroy(&r6);

    opal_finalize();

    if (0 == failures) {
        printf("All resized-extent checks passed\n");
        return 0;
    }
    printf("%d resized-extent check(s) failed\n", failures);
    return 1;
}
