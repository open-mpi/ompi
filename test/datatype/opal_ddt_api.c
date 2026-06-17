/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
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
 * Unit tests for OPAL datatype API functions with little or no existing
 * coverage:
 *
 *   opal_datatype_is_monotonic          (opal_datatype_monotonic.c)
 *   opal_datatype_get_element_count     (opal_datatype_get_count.c)
 *   opal_datatype_set_element_count     (opal_datatype_get_count.c)
 *   opal_datatype_compute_ptypes        (opal_datatype_get_count.c)
 *   opal_datatype_copy_content_same_ddt (opal_datatype_copy.c)
 *   opal_datatype_clone                 (opal_datatype_clone.c)
 *   opal_datatype_resize                (opal_datatype_resize.c)
 *
 * Plus helpers exercising opal_datatype_create / opal_datatype_add /
 * opal_datatype_commit used to build the non-trivial types above.
 *
 * NOTE: The library is built with -DNDEBUG, so assert() is a no-op.
 * All verification MUST go through test_verify() / test_failure().
 *
 * NOTE ON INIT: The task specification suggests opal_init_util(), but
 * opal_datatype_init() (and the convertor / accelerator framework that
 * opal_datatype_is_monotonic() requires) is only bootstrapped by the
 * full opal_init() path.  Every sibling datatype test (ddt_pack.c,
 * checksum.c, opal_datatype_test.c, …) confirms this.  We therefore
 * call opal_init(&argc, &argv) / opal_finalize() following the
 * established codebase pattern.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* Helper: build a vector of (count) blocks of (blocklen) elements of
 * base_type, with inter-block stride (stride * sizeof(base_type)).
 * The caller is responsible for OBJ_RELEASE on the returned pointer.
 */
static opal_datatype_t *make_vector(const opal_datatype_t *base_type,
                                    int count, int blocklen, int stride)
{
    opal_datatype_t *pdt;
    ptrdiff_t extent;

    opal_datatype_type_extent(base_type, &extent);
    pdt = opal_datatype_create(base_type->desc.used + 2);
    if (NULL == pdt) {
        return NULL;
    }

    if ((blocklen == stride) || (1 == count)) {
        /* contiguous */
        opal_datatype_add(pdt, base_type, (size_t)(count * blocklen), 0, extent);
    } else if (1 == blocklen) {
        opal_datatype_add(pdt, base_type, (size_t) count, 0, extent * stride);
    } else {
        opal_datatype_t *inner;
        inner = opal_datatype_create(base_type->desc.used + 2);
        if (NULL == inner) {
            OBJ_RELEASE(pdt);
            return NULL;
        }
        opal_datatype_add(inner, base_type, (size_t) blocklen, 0, extent);
        opal_datatype_add(pdt, inner, (size_t) count, 0, extent * stride);
        OBJ_RELEASE(inner);
    }

    opal_datatype_commit(pdt);
    return pdt;
}

/* ------------------------------------------------------------------ */
/* test_is_monotonic
 *
 * Definitions:
 *   A contiguous array of int4 is trivially monotonic.
 *   A vector with positive stride (stride > blocklen) is monotonic:
 *     consecutive iov bases advance forward in memory.
 *   A type whose iov stream walks backward (decreasing base addresses)
 *   is NOT monotonic.
 *
 * Primary non-monotonic construction — negative-stride vector:
 *   3 single int4 elements, stride = -2 (i.e. each element is placed
 *   2*sizeof(int4) = 8 bytes BEFORE the previous one).
 *   opal_datatype_add(pdt, &int4, count=3, disp=0, extent=-8)
 *   Layout:  elem[0] @ 0, elem[1] @ -8, elem[2] @ -16
 *   The convertor produces iov_base = 0, then -8, then -16 — strictly
 *   decreasing → not monotonic.
 *   This uses the blocklen==1 path in make_vector, which issues a
 *   single opal_datatype_add with extent*stride.  A single loop cannot
 *   be reordered by the commit-time optimizer, so the backward traversal
 *   order is guaranteed by construction.
 *
 * Secondary non-monotonic construction (redundant cross-check) —
 * reversed-displacement struct:
 *   add int4 @ disp=4 first, then add int4 @ disp=0.
 *   MPI struct semantics require descriptors to be traversed in the
 *   order they were added, so the convertor visits offset 4 before
 *   offset 0 — backward.  We verify true_ub==8 before trusting the
 *   monotonicity result to confirm commit did not reorder descriptors.
 */
static void test_is_monotonic(void)
{
    opal_datatype_t *pdt_contig;
    opal_datatype_t *pdt_pos_vector;
    opal_datatype_t *pdt_neg_stride;
    opal_datatype_t *pdt_backward_struct;
    int32_t result;

    /* --- contiguous int4 array: monotonic --- */
    pdt_contig = opal_datatype_create(opal_datatype_int4.desc.used + 2);
    test_verify("create contiguous type", NULL != pdt_contig);
    if (NULL != pdt_contig) {
        opal_datatype_add(pdt_contig, &opal_datatype_int4,
                          4, 0, (ptrdiff_t) sizeof(int32_t));
        opal_datatype_commit(pdt_contig);

        result = opal_datatype_is_monotonic(pdt_contig);
        test_verify("contiguous int4 array is monotonic (result == 1)",
                    1 == result);
        OBJ_RELEASE(pdt_contig);
    }

    /* --- vector with positive stride: monotonic --- */
    /* 3 blocks of 2 int4, stride 5 (gap between blocks). */
    pdt_pos_vector = make_vector(&opal_datatype_int4, 3, 2, 5);
    test_verify("create positive-stride vector", NULL != pdt_pos_vector);
    if (NULL != pdt_pos_vector) {
        result = opal_datatype_is_monotonic(pdt_pos_vector);
        test_verify("positive-stride vector is monotonic (result == 1)",
                    1 == result);
        OBJ_RELEASE(pdt_pos_vector);
    }

    /* --- Primary non-monotonic: negative-stride vector --- */
    /* 3 single int4 elements, extent per step = -2 * sizeof(int4) = -8.
     * Layout: elem[0] @ 0, elem[1] @ -8, elem[2] @ -16.
     * A single opal_datatype_add loop cannot be reordered by commit,
     * so the backward traversal is guaranteed.
     * Sanity: true_lb == -16, true_ub == 4 (first element's ub).
     */
    {
        ptrdiff_t e = (ptrdiff_t) sizeof(int32_t); /* 4 */
        pdt_neg_stride = opal_datatype_create(opal_datatype_int4.desc.used + 2);
        test_verify("create negative-stride vector", NULL != pdt_neg_stride);
        if (NULL != pdt_neg_stride) {
            /* blocklen=1, stride=-2: extent_per_step = e * -2 = -8 */
            opal_datatype_add(pdt_neg_stride, &opal_datatype_int4,
                              3, 0, e * (-2));
            opal_datatype_commit(pdt_neg_stride);

            /* Confirm construction: true_lb=-16, true_ub=4 */
            test_verify("neg-stride: true_lb == -16",
                        -16 == (int) pdt_neg_stride->true_lb);
            test_verify("neg-stride: true_ub == 4",
                        4   == (int) pdt_neg_stride->true_ub);

            result = opal_datatype_is_monotonic(pdt_neg_stride);
            test_verify("negative-stride vector is NOT monotonic (result == 0)",
                        0 == result);
            OBJ_RELEASE(pdt_neg_stride);
        }
    }

    /* --- Secondary non-monotonic: reversed-displacement struct --- */
    /* add int4 @ disp=4 first, then add int4 @ disp=0.
     * Verify construction (true_ub==8) before trusting the result.
     */
    {
        ptrdiff_t e = (ptrdiff_t) sizeof(int32_t);
        pdt_backward_struct = opal_datatype_create(4);
        test_verify("create backward struct", NULL != pdt_backward_struct);
        if (NULL != pdt_backward_struct) {
            /* first descriptor: int4 at byte 4 */
            opal_datatype_add(pdt_backward_struct, &opal_datatype_int4, 1, e, e);
            /* second descriptor: int4 at byte 0  (lower address) */
            opal_datatype_add(pdt_backward_struct, &opal_datatype_int4, 1, 0, e);
            opal_datatype_commit(pdt_backward_struct);

            /* Sanity: true extent covers bytes 0..7 */
            test_verify("backward struct: true_lb == 0",
                        0 == (int) pdt_backward_struct->true_lb);
            test_verify("backward struct: true_ub == 8",
                        8 == (int) pdt_backward_struct->true_ub);

            result = opal_datatype_is_monotonic(pdt_backward_struct);
            test_verify("backward struct is NOT monotonic (result == 0)",
                        0 == result);
            OBJ_RELEASE(pdt_backward_struct);
        }
    }
}

/* ------------------------------------------------------------------ */
/* test_get_element_count
 *
 * Type: vector of 3 blocks x 4 elements of int4, stride = 6.
 *   Basic element size = 4 bytes.
 *   Total basic elements = 3 * 4 = 12.
 *   type->size = 12 * 4 = 48 bytes.
 *
 * opal_datatype_get_element_count(t, t->size) must return 12.
 * opal_datatype_get_element_count(t, 4)       must return 1.
 * opal_datatype_get_element_count(t, 16)      must return 4.
 */
static void test_get_element_count(void)
{
    opal_datatype_t *pdt;
    ssize_t count;
    size_t sz;

    /* 3 blocks * 4 int4 elements, stride 6 */
    pdt = make_vector(&opal_datatype_int4, 3, 4, 6);
    test_verify("create vector for get_element_count", NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    /* Hand-computed: 3*4 = 12 elements, size = 12*4 = 48 */
    opal_datatype_type_size(pdt, &sz);
    test_verify("vector size == 48", 48 == (int) sz);

    count = opal_datatype_get_element_count(pdt, sz);
    test_verify("get_element_count(full size) == 12", 12 == (int) count);

    count = opal_datatype_get_element_count(pdt, 4);
    test_verify("get_element_count(4 bytes) == 1", 1 == (int) count);

    count = opal_datatype_get_element_count(pdt, 16);
    test_verify("get_element_count(16 bytes) == 4", 4 == (int) count);

    OBJ_RELEASE(pdt);
}

/* ------------------------------------------------------------------ */
/* test_set_element_count
 *
 * Using the same vector (3 blocks x 4 int4, stride 6):
 *   set_element_count(0, &len)  => len = 0
 *   set_element_count(1, &len)  => len = 4   (1 * 4 bytes)
 *   set_element_count(4, &len)  => len = 16  (4 * 4 bytes)
 *   set_element_count(12, &len) => len = 48  (12 * 4 bytes = full type)
 */
static void test_set_element_count(void)
{
    opal_datatype_t *pdt;
    size_t len;
    int32_t rc;

    pdt = make_vector(&opal_datatype_int4, 3, 4, 6);
    test_verify("create vector for set_element_count", NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    len = (size_t) -1;
    rc = opal_datatype_set_element_count(pdt, 0, &len);
    test_verify("set_element_count(0) returns 0", 0 == rc);
    test_verify("set_element_count(0) => len == 0", 0 == (int) len);

    len = (size_t) -1;
    rc = opal_datatype_set_element_count(pdt, 1, &len);
    test_verify("set_element_count(1) returns 0", 0 == rc);
    test_verify("set_element_count(1) => len == 4", 4 == (int) len);

    len = (size_t) -1;
    rc = opal_datatype_set_element_count(pdt, 4, &len);
    test_verify("set_element_count(4) returns 0", 0 == rc);
    test_verify("set_element_count(4) => len == 16", 16 == (int) len);

    len = (size_t) -1;
    rc = opal_datatype_set_element_count(pdt, 12, &len);
    test_verify("set_element_count(12) returns 0", 0 == rc);
    test_verify("set_element_count(12) => len == 48", 48 == (int) len);

    OBJ_RELEASE(pdt);
}

/* ------------------------------------------------------------------ */
/* test_compute_ptypes
 *
 * Type: struct of 1 int1 + 1 float8.
 *   After commit and opal_datatype_compute_ptypes():
 *     ptypes[OPAL_DATATYPE_INT1]   == 1
 *     ptypes[OPAL_DATATYPE_FLOAT8] == 1
 *   All other ptypes entries == 0.
 *   type->size == sizeof(int8) + sizeof(float8) == 1 + 8 == 9 bytes.
 *   type->nbElems == 2 (one int1 + one float8 == 2 basic elements).
 */
static void test_compute_ptypes(void)
{
    opal_datatype_t *pdt;
    size_t sz;
    int rc;

    /* Build { int1 @ disp=0, float8 @ disp=1 } */
    pdt = opal_datatype_create((ssize_t)(opal_datatype_int1.desc.used
                                        + opal_datatype_float8.desc.used + 4));
    test_verify("create struct type for compute_ptypes", NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    opal_datatype_add(pdt, &opal_datatype_int1, 1, 0,
                      (ptrdiff_t) sizeof(int8_t));
    opal_datatype_add(pdt, &opal_datatype_float8, 1, 1,
                      (ptrdiff_t) sizeof(double));
    opal_datatype_commit(pdt);

    opal_datatype_type_size(pdt, &sz);
    /* size = 1 (int1) + 8 (float8) = 9 */
    test_verify("struct int1+float8 size == 9", 9 == (int) sz);
    test_verify("struct int1+float8 nbElems == 2", 2 == (int) pdt->nbElems);

    rc = opal_datatype_compute_ptypes(pdt);
    test_verify("compute_ptypes returns 0", 0 == rc);
    test_verify("ptypes array allocated", NULL != pdt->ptypes);

    if (NULL != pdt->ptypes) {
        test_verify("ptypes[OPAL_DATATYPE_INT1] == 1",
                    1 == (int) pdt->ptypes[OPAL_DATATYPE_INT1]);
        test_verify("ptypes[OPAL_DATATYPE_FLOAT8] == 1",
                    1 == (int) pdt->ptypes[OPAL_DATATYPE_FLOAT8]);
        /* A type containing only int1+float8 must NOT count int4 */
        test_verify("ptypes[OPAL_DATATYPE_INT4] == 0",
                    0 == (int) pdt->ptypes[OPAL_DATATYPE_INT4]);
        /* second call must be a no-op (array already allocated) */
        rc = opal_datatype_compute_ptypes(pdt);
        test_verify("second compute_ptypes call returns 0 (no-op)", 0 == rc);
    }

    OBJ_RELEASE(pdt);
}

/* ------------------------------------------------------------------ */
/* test_copy_content_same_ddt
 *
 * Type: vector of 3 blocks x 2 int4 elements, stride = 4.
 *   extent    = stride * sizeof(int4) = 4 * 4 = 16 bytes
 *   size      = 3 * 2 * 4 = 24 bytes (only data bytes, no gaps)
 *   true_ub   = 3*stride*4 - gap_at_end = (3-1)*16 + 2*4 = 40 bytes
 *   Memory span for 1 repetition = 40 bytes (true_extent).
 *
 * Layout (byte offsets, stride=4 int4 = 16 bytes per block):
 *   block 0: bytes  0.. 7  (2 x int4)
 *   block 1: bytes 16..23
 *   block 2: bytes 32..39
 *   gap bytes: 8..15, 24..31  (each 8 bytes)
 *
 * We allocate two 40-byte buffers, fill src with ascending bytes,
 * fill dst with sentinel (0xCC), then call copy_content_same_ddt.
 * Verify:
 *   (a) data bytes (blocks) in dst match src.
 *   (b) gap bytes in dst still hold the sentinel (gaps not touched).
 */
static void test_copy_content_same_ddt(void)
{
    opal_datatype_t *pdt;
    ptrdiff_t lb, extent;
    size_t sz;
    int rc;

    /* 3 blocks x 2 int4, stride 4 int4 = 4 * sizeof(int4) */
    pdt = make_vector(&opal_datatype_int4, 3, 2, 4);
    test_verify("create vector for copy_content", NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    opal_datatype_get_extent(pdt, &lb, &extent);
    opal_datatype_type_size(pdt, &sz);

    /* extent = 3 blocks * 4 stride * 4 bytes = wait, let's think again.
     *
     * stride = 4 elements of int4 = stride in units of basic type.
     * make_vector uses: opal_datatype_add(inner, base, count=2, 0, e)
     *                   opal_datatype_add(outer, inner, count=3, 0, e*stride=4*4=16)
     * So outer extent = 3 * 16 = 48 bytes.  But wait: outer ub = 0 + 2*16 + 2*4 = 40.
     * Actually: extent = ub - lb.
     *
     * After commit:
     *   lb = 0
     *   ub = (count-1)*stride*e + blocklen*e = 2*16 + 2*4 = 40
     *   extent = 40
     *   size = 3*2*4 = 24
     *
     * Buffer size needed for 1 repetition = true_extent = ub - lb = 40.
     */
    {
        ptrdiff_t true_lb, true_extent;
        size_t buf_size;
        unsigned char *src_buf, *dst_buf;
        unsigned int i;

        opal_datatype_get_true_extent(pdt, &true_lb, &true_extent);
        buf_size = (size_t)(true_lb + true_extent);
        if (0 == buf_size) {
            test_failure("copy_content: unexpected zero buffer size");
            OBJ_RELEASE(pdt);
            return;
        }

        src_buf = (unsigned char *) malloc(buf_size);
        dst_buf = (unsigned char *) malloc(buf_size);
        test_verify("alloc src_buf for copy_content", NULL != src_buf);
        test_verify("alloc dst_buf for copy_content", NULL != dst_buf);
        if (NULL == src_buf || NULL == dst_buf) {
            free(src_buf);
            free(dst_buf);
            OBJ_RELEASE(pdt);
            return;
        }

        /* Fill src with ascending pattern, dst with sentinel */
        for (i = 0; i < (unsigned int) buf_size; i++) {
            src_buf[i] = (unsigned char)(i & 0xFF);
        }
        memset(dst_buf, 0xCC, buf_size);

        /* pdst / psrc adjusted by lb (== 0 here, so same as buf ptr) */
        rc = opal_datatype_copy_content_same_ddt(pdt, 1,
                                                 (char *)(dst_buf - lb),
                                                 (char *)(src_buf - lb));
        test_verify("copy_content_same_ddt returns OPAL_SUCCESS",
                    OPAL_SUCCESS == rc);

        /*
         * Verify data blocks were copied correctly.
         * Block layout (stride=4 int4=16, blocklen=2 int4=8):
         *   block 0: bytes  0.. 7
         *   block 1: bytes 16..23
         *   block 2: bytes 32..39
         */
        {
            int all_data_ok = 1;
            int all_gaps_ok = 1;
            int b, byte_off;
            int block_size_bytes = 2 * (int) sizeof(int32_t);  /* 8 */
            int stride_bytes     = 4 * (int) sizeof(int32_t);  /* 16 */

            for (b = 0; b < 3; b++) {
                int block_start = b * stride_bytes;

                /* data bytes inside the block */
                for (i = (unsigned int) block_start;
                     i < (unsigned int)(block_start + block_size_bytes);
                     i++) {
                    if (dst_buf[i] != src_buf[i]) {
                        all_data_ok = 0;
                    }
                }
                /* gap bytes after this block (skip for last block) */
                if (b < 2) {
                    int gap_start = block_start + block_size_bytes;
                    int gap_end   = block_start + stride_bytes;
                    for (byte_off = gap_start; byte_off < gap_end; byte_off++) {
                        if (0xCC != dst_buf[byte_off]) {
                            all_gaps_ok = 0;
                        }
                    }
                }
            }

            test_verify("copy_content: data bytes match src", all_data_ok);
            test_verify("copy_content: gap bytes retain sentinel (0xCC)",
                        all_gaps_ok);
        }

        free(src_buf);
        free(dst_buf);
    }

    /* Suppress unused-variable warnings for lb/extent/sz. */
    (void) lb;
    (void) extent;
    (void) sz;

    OBJ_RELEASE(pdt);
}

/* ------------------------------------------------------------------ */
/* test_clone
 *
 * Build a committed vector, clone it into a freshly created
 * destination, then verify:
 *   (a) size matches
 *   (b) extent matches
 *   (c) nbElems matches
 *   (d) flags (committed bit) match
 *
 * The destination datatype must be allocated with enough descriptor
 * space before clone is called -- mirroring ompi_datatype_duplicate().
 */
static void test_clone(void)
{
    opal_datatype_t *orig;
    opal_datatype_t *cloned;
    size_t orig_sz, clone_sz;
    ptrdiff_t orig_ext, clone_ext;

    /* original: 4 blocks x 3 float8, stride 5 */
    orig = make_vector(&opal_datatype_float8, 4, 3, 5);
    test_verify("create vector for clone test", NULL != orig);
    if (NULL == orig) {
        return;
    }

    /* Allocate clone destination with enough descriptor room */
    cloned = opal_datatype_create((ssize_t)(orig->desc.used + 2));
    test_verify("create clone destination", NULL != cloned);
    if (NULL == cloned) {
        OBJ_RELEASE(orig);
        return;
    }

    int32_t rc = opal_datatype_clone(orig, cloned);
    test_verify("opal_datatype_clone returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    opal_datatype_type_size(orig,   &orig_sz);
    opal_datatype_type_size(cloned, &clone_sz);
    test_verify("clone size == original size", orig_sz == clone_sz);

    opal_datatype_type_extent(orig,   &orig_ext);
    opal_datatype_type_extent(cloned, &clone_ext);
    test_verify("clone extent == original extent", orig_ext == clone_ext);

    test_verify("clone nbElems == original nbElems",
                orig->nbElems == cloned->nbElems);
    test_verify("clone has COMMITTED flag",
                opal_datatype_is_committed(cloned));

    /* Hand-computed values for 4 blocks x 3 float8, stride 5:
     *   size    = 4 * 3 * 8 = 96 bytes
     *   extent  = (4-1)*5*8 + 3*8 = 120 + 24 = 144 bytes
     *   nbElems = 4 * 3 = 12
     */
    test_verify("clone size == 96",    96  == (int) clone_sz);
    test_verify("clone extent == 144", 144 == (int) clone_ext);
    test_verify("clone nbElems == 12", 12  == (int) cloned->nbElems);

    OBJ_RELEASE(cloned);
    OBJ_RELEASE(orig);
}

/* ------------------------------------------------------------------ */
/* test_resize
 *
 * Build a committed vector of 2 blocks x 2 int4, stride 3.
 *   natural size   = 2 * 2 * 4 = 16 bytes
 *   natural extent = (2-1)*3*4 + 2*4 = 12 + 8 = 20 bytes
 *   natural lb     = 0
 *
 * After resize(lb=0, extent=100):
 *   lb     == 0
 *   ub     == 100
 *   extent == 100
 *   size unchanged (resize only changes lb/ub, not actual data size)
 *
 * After resize(lb=-4, extent=100):
 *   lb     == -4
 *   ub     == -4 + 100 == 96
 *   extent == 100
 */
static void test_resize(void)
{
    opal_datatype_t *pdt;
    ptrdiff_t lb, ext;
    size_t sz;
    int32_t rc;

    /* 2 blocks x 2 int4, stride 3 */
    pdt = make_vector(&opal_datatype_int4, 2, 2, 3);
    test_verify("create vector for resize test", NULL != pdt);
    if (NULL == pdt) {
        return;
    }

    /* Verify natural state before resize */
    opal_datatype_type_size(pdt, &sz);
    test_verify("resize: natural size == 16", 16 == (int) sz);

    opal_datatype_get_extent(pdt, &lb, &ext);
    test_verify("resize: natural lb == 0", 0 == (int) lb);
    test_verify("resize: natural extent == 20", 20 == (int) ext);

    /* resize to extent=100, lb=0 */
    rc = opal_datatype_resize(pdt, 0, 100);
    test_verify("opal_datatype_resize(lb=0, ext=100) returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    opal_datatype_get_extent(pdt, &lb, &ext);
    test_verify("after resize(0,100): lb == 0",   0   == (int) lb);
    test_verify("after resize(0,100): ext == 100", 100 == (int) ext);

    /* size must be unchanged by resize */
    opal_datatype_type_size(pdt, &sz);
    test_verify("resize does not change size (still 16)", 16 == (int) sz);

    /* resize with negative lb */
    rc = opal_datatype_resize(pdt, -4, 100);
    test_verify("opal_datatype_resize(lb=-4, ext=100) returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    opal_datatype_get_extent(pdt, &lb, &ext);
    test_verify("after resize(-4,100): lb == -4",  -4  == (int) lb);
    test_verify("after resize(-4,100): ext == 100", 100 == (int) ext);

    opal_datatype_type_lb(pdt, &lb);
    test_verify("type_lb after resize(-4,100) == -4", -4 == (int) lb);

    opal_datatype_type_ub(pdt, &ext);
    test_verify("type_ub after resize(-4,100) == 96", 96 == (int) ext);

    OBJ_RELEASE(pdt);
}

/* ------------------------------------------------------------------ */
int main(int argc, char *argv[])
{
    int r;

    test_init("opal_ddt_api");

    /* Full opal_init required: opal_datatype_init() and the accelerator
     * framework (needed by opal_datatype_is_monotonic()) are bootstrapped
     * only by opal_init(), not by the lighter opal_init_util().  All
     * sibling tests in test/datatype/ follow the same pattern.        */
    opal_init(&argc, &argv);

    test_is_monotonic();
    test_get_element_count();
    test_set_element_count();
    test_compute_ptypes();
    test_copy_content_same_ddt();
    test_clone();
    test_resize();

    r = test_finalize();
    opal_finalize();
    return r;
}
