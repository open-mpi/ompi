/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Self-checking unit tests for OPAL heterogeneous datatype conversion.
 *
 * Covered source files:
 *   opal/datatype/opal_copy_functions_heterogeneous.c  -- byte-swap copy functions
 *   opal/datatype/opal_datatype_fake_stack.c           -- position-restore stack
 *   opal/datatype/opal_datatype_get_count.c            -- compute_remote_size / compute_ptypes
 *
 * Strategy: construct a "remote" architecture whose endianness is the
 * opposite of the local one (remote_arch = opal_local_arch ^ OPAL_ARCH_ISBIGENDIAN).
 * Build wire buffers by byte-reversing known host values independently of
 * the implementation, then unpack via a heterogeneous convertor and assert
 * that the recovered host values match.
 *
 * NOTE: The library is compiled with -DNDEBUG so assert() is a no-op.
 * All verification MUST go through test_verify() so that failures are real.
 *
 * Initialization note: this file uses opal_init()/opal_finalize() rather
 * than the opal_init_util()+opal_datatype_init() pattern used by some OPAL
 * class tests.  The reason is that opal_convertor_prepare_for_recv()
 * unconditionally invokes opal_convertor_accelerator_init(), which calls into
 * the accelerator framework.  That framework is only initialized by the full
 * opal_init() path (see the explicit ordering note in opal_init.c: "datatype
 * convertor code has a dependency on the accelerator framework being
 * initialized").  This matches unpack_hetero.c and every other in-tree
 * datatype test that links against only libopal.la.
 */

/*
 * STATUS: this test is intentionally NOT wired into test/datatype/Makefile.am
 * (not built or run by "make check") because two of its assertions currently
 * fail and document open items awaiting a maintainer's decision:
 *
 *   1. test_fake_stack_direct() (the "fake_stack: bConverted advanced to
 *      seek_to" assertion) exercises an internal helper
 *      (opal_convertor_create_stack_with_pos_general) that has zero production
 *      callers; the asserted postcondition is undocumented and does not hold
 *      for the predefined-contiguous input used here.  Likely a test bug.
 *
 *   2. test_set_position_and_resume() (the "set_pos: elem[2] correct after
 *      resume" assertion) reveals a real disp-composition asymmetry between the
 *      homogeneous and heterogeneous unpack paths after a mid-stream
 *      opal_convertor_set_position() on a FLAG_CONTIGUOUS recv convertor.
 *      Whether that is a live bug depends on whether production ever drives
 *      that exact combination -- a question for a datatype maintainer.
 *
 * The remaining ~60 assertions pass on both Linux and macOS.  See the inline
 * NOTE/OPEN QUESTION comments at each failing assertion for the full analysis.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "opal/util/arch.h"

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* -----------------------------------------------------------------------
 * Helper: portable byte-reversal.
 * Produces the byte-swapped representation of 'in' in 'out'.
 * 'size' is the width in bytes (1, 2, 4, 8, ...).
 * This is derived from first principles (mirror the byte array), NOT from
 * opal_dt_swap_bytes, so that the test is independent of the implementation.
 * ----------------------------------------------------------------------- */
static void swap_bytes(void *out, const void *in, size_t size)
{
    const unsigned char *src = (const unsigned char *) in;
    unsigned char *dst = (unsigned char *) out;
    size_t i;

    for (i = 0; i < size; i++) {
        dst[i] = src[size - 1 - i];
    }
}

/* -----------------------------------------------------------------------
 * Helper: unpack one or more elements of a predefined type from a
 * wire buffer via a heterogeneous convertor and return the convertor.
 * The caller must OBJ_RELEASE the returned convertor.
 * Returns OPAL_SUCCESS on success, otherwise the error code.
 * ----------------------------------------------------------------------- */
static int hetero_unpack(uint32_t remote_arch,
                         const opal_datatype_t *dtype,
                         size_t count,
                         void *recv_buf,
                         void *wire_buf,
                         size_t wire_len)
{
    opal_convertor_t *conv;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data;
    int rc;

    conv = opal_convertor_create(remote_arch, 0);
    if (NULL == conv) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    rc = opal_convertor_prepare_for_recv(conv, dtype, count, recv_buf);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(conv);
        return rc;
    }

    iov.iov_base = wire_buf;
    iov.iov_len  = wire_len;
    iov_count    = 1;
    max_data     = wire_len;
    opal_convertor_unpack(conv, &iov, &iov_count, &max_data);

    OBJ_RELEASE(conv);
    return OPAL_SUCCESS;
}

/* -----------------------------------------------------------------------
 * Test sections
 * ----------------------------------------------------------------------- */

/*
 * test_int2: copy_int2_heterogeneous
 *
 * Single int16_t value.  The wire buffer contains the byte-swapped
 * representation; unpacking through a hetero convertor must restore
 * the original host value.
 */
static void test_int2(uint32_t remote_arch)
{
    int16_t host_val = 0x1234;
    int16_t wire_val;
    int16_t recv_val = 0;
    int rc;

    /* Build wire buffer: byte-reversed host value */
    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_int2, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("int2: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("int2: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_int4: copy_int4_heterogeneous
 *
 * Single int32_t.  This is the same path exercised by unpack_hetero.c
 * but with a known value and a real assertion.
 */
static void test_int4(uint32_t remote_arch)
{
    int32_t host_val = 0x01020304;
    int32_t wire_val;
    int32_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_int4, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("int4: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("int4: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_int8: copy_int8_heterogeneous
 */
static void test_int8(uint32_t remote_arch)
{
    int64_t host_val = (int64_t) 0x0102030405060708LL;
    int64_t wire_val;
    int64_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_int8, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("int8: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("int8: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_uint2: copy_int2_heterogeneous (reused for UINT2)
 */
static void test_uint2(uint32_t remote_arch)
{
    uint16_t host_val = 0xABCD;
    uint16_t wire_val;
    uint16_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_uint2, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("uint2: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("uint2: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_uint4: copy_int4_heterogeneous (reused for UINT4)
 */
static void test_uint4(uint32_t remote_arch)
{
    uint32_t host_val = 0xDEADBEEFU;
    uint32_t wire_val;
    uint32_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_uint4, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("uint4: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("uint4: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_uint8: copy_int8_heterogeneous (reused for UINT8)
 */
static void test_uint8(uint32_t remote_arch)
{
    uint64_t host_val = 0xCAFEBABEDEADF00DULL;
    uint64_t wire_val;
    uint64_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_uint8, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("uint8: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("uint8: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_float4: copy_float4_heterogeneous
 *
 * IEEE-754 single precision.  Use the bit-pattern of a known float.
 * 1.0f in IEEE-754 LE = 0x3F800000.
 * Byte-reversed: 0x0000803F.
 * After unpacking via hetero convertor we should recover 1.0f.
 */
static void test_float4(uint32_t remote_arch)
{
    float host_val = 1.0f;
    float wire_val_f;
    float recv_val = 0.0f;
    int rc;

    swap_bytes(&wire_val_f, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_float4, 1, &recv_val,
                       &wire_val_f, sizeof(wire_val_f));
    test_verify("float4: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("float4: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_float8: copy_float8_heterogeneous
 *
 * IEEE-754 double precision.  Use 1.0 = 0x3FF0000000000000 in LE.
 */
static void test_float8(uint32_t remote_arch)
{
    double host_val = 1.0;
    double wire_val_d;
    double recv_val = 0.0;
    int rc;

    swap_bytes(&wire_val_d, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_float8, 1, &recv_val,
                       &wire_val_d, sizeof(wire_val_d));
    test_verify("float8: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("float8: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_int4_contiguous_array
 *
 * Unpack 4 contiguous int32_t values.  This exercises the
 * multi-element contiguous path in copy_int4_heterogeneous (the
 * "countperblock = count, nblocksleft = 1" branch).
 */
static void test_int4_contiguous_array(uint32_t remote_arch)
{
    static const int32_t host_vals[4] = {0x10203040, 0x50607080,
                                         (int32_t) 0x90A0B0C0, (int32_t) 0xD0E0F001};
    int32_t wire_buf[4];
    int32_t recv_buf[4];
    size_t i;
    int rc;

    memset(recv_buf, 0, sizeof(recv_buf));

    for (i = 0; i < 4; i++) {
        swap_bytes(&wire_buf[i], &host_vals[i], sizeof(int32_t));
    }

    rc = hetero_unpack(remote_arch, &opal_datatype_int4, 4, recv_buf,
                       wire_buf, sizeof(wire_buf));
    test_verify("int4[4]: unpack succeeds", OPAL_SUCCESS == rc);
    for (i = 0; i < 4; i++) {
        test_verify("int4[4]: each element byte-swapped correctly",
                    host_vals[i] == recv_buf[i]);
    }
}

/*
 * test_int4_negative: single negative int32_t
 *
 * Verify sign extension is preserved across the byte swap.
 */
static void test_int4_negative(uint32_t remote_arch)
{
    int32_t host_val = -1;          /* 0xFFFFFFFF */
    int32_t wire_val;
    int32_t recv_val = 0;
    int rc;

    swap_bytes(&wire_val, &host_val, sizeof(host_val));

    rc = hetero_unpack(remote_arch, &opal_datatype_int4, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("int4 negative: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("int4 negative: -1 round-trips correctly", -1 == recv_val);
}

/*
 * test_wchar: copy_wchar_heterogeneous
 *
 * wchar_t width is platform-dependent; we only test if sizeof(wchar_t) > 1
 * so there is something to swap.
 */
static void test_wchar(uint32_t remote_arch)
{
    wchar_t host_val;
    wchar_t wire_val;
    wchar_t recv_val;
    int rc;

    if (sizeof(wchar_t) <= 1) {
        /* Nothing interesting to byte-swap; skip but count as a pass. */
        test_verify("wchar: skip (sizeof == 1)", 1);
        return;
    }

    host_val = (wchar_t) 0x0410;       /* some non-ASCII codepoint, fits in 2+ byte wchar_t */
    swap_bytes(&wire_val, &host_val, sizeof(host_val));
    recv_val = 0;

    rc = hetero_unpack(remote_arch, &opal_datatype_wchar, 1, &recv_val,
                       &wire_val, sizeof(wire_val));
    test_verify("wchar: unpack succeeds", OPAL_SUCCESS == rc);
    test_verify("wchar: byte-swap round-trip", host_val == recv_val);
}

/*
 * test_remote_size_predefined
 *
 * opal_convertor_compute_remote_size on a predefined type should return
 * count * sizeof(type).
 *
 * Covers: opal_datatype_compute_remote_size (predefined short-circuit path)
 */
static void test_remote_size_predefined(uint32_t remote_arch)
{
    opal_convertor_t *conv;
    size_t rsize;
    const size_t count = 3;
    int64_t dummy_buf[3] = {0, 0, 0};
    int rc;

    conv = opal_convertor_create(remote_arch, 0);
    test_verify("remote_size pred: convertor created", NULL != conv);
    if (NULL == conv) {
        return;
    }

    rc = opal_convertor_prepare_for_recv(conv, &opal_datatype_int8, count, dummy_buf);
    test_verify("remote_size pred: prepare ok", OPAL_SUCCESS == rc);

    rsize = opal_convertor_compute_remote_size(conv);
    test_verify("remote_size pred: returns count * sizeof(int8)",
                (count * sizeof(int64_t)) == rsize);

    OBJ_RELEASE(conv);
}

/*
 * test_remote_size_derived
 *
 * opal_convertor_compute_remote_size on a derived type must call
 * opal_datatype_compute_ptypes (get_count.c).
 *
 * We build a struct-like derived type: 3 x int4 followed by 2 x int8.
 * Local and remote sizes of each predefined type are the same in this
 * test (we only flip endianness, not type sizes), so remote_size should
 * equal local_size = count * (3*4 + 2*8) = 1 * 20 = 20 bytes.
 *
 * Covers: opal_datatype_compute_ptypes, opal_datatype_compute_remote_size
 *         (derived type path)
 */
static void test_remote_size_derived(uint32_t remote_arch)
{
    opal_datatype_t *derived = NULL;
    opal_convertor_t *conv   = NULL;
    static char dummy_recv[64];
    size_t rsize;
    int rc;
    /* 3 x int4 (12 bytes) + 2 x int8 (16 bytes) = 28 bytes total */
    const size_t expected_local = 3 * sizeof(int32_t) + 2 * sizeof(int64_t);

    rc = opal_datatype_create_contiguous(0, &opal_datatype_empty, &derived);
    test_verify("remote_size derived: create empty ok", OPAL_SUCCESS == rc);
    if (OPAL_SUCCESS != rc || NULL == derived) {
        return;
    }

    rc = opal_datatype_add(derived, &opal_datatype_int4, 3, 0, sizeof(int32_t));
    test_verify("remote_size derived: add int4 ok", OPAL_SUCCESS == rc);

    rc = opal_datatype_add(derived, &opal_datatype_int8, 2,
                           3 * (ptrdiff_t) sizeof(int32_t), sizeof(int64_t));
    test_verify("remote_size derived: add int8 ok", OPAL_SUCCESS == rc);

    rc = opal_datatype_commit(derived);
    test_verify("remote_size derived: commit ok", OPAL_SUCCESS == rc);

    conv = opal_convertor_create(remote_arch, 0);
    test_verify("remote_size derived: convertor created", NULL != conv);
    if (NULL == conv) {
        opal_datatype_destroy(&derived);
        return;
    }

    /* Use the dummy recv buffer (not accessed; we only test size computation). */
    rc = opal_convertor_prepare_for_recv(conv, derived, 1, dummy_recv);
    test_verify("remote_size derived: prepare ok", OPAL_SUCCESS == rc);

    rsize = opal_convertor_compute_remote_size(conv);
    /*
     * Since we only flip endianness (not type sizes), the remote size equals
     * the local size for this derived type.
     */
    test_verify("remote_size derived: equals expected local size",
                expected_local == rsize);

    OBJ_RELEASE(conv);
    opal_datatype_destroy(&derived);
}

/*
 * test_strided_int4_unpack
 *
 * Build a derived type with two non-contiguous int32_t fields (a "struct"
 * with a gap).  This exercises copy_int4_heterogeneous on a non-contiguous
 * layout (the "countperblock=1, nblocksleft=count" branch).
 *
 * Layout per instance (bytes):
 *   [0..3]  = field A (int32_t)
 *   [4..7]  = gap (unwritten)
 *   [8..11] = field B (int32_t)
 *   extent  = 12 bytes
 *   size    = 8 bytes
 *
 * The wire buffer for N instances is N*8 bytes of packed (gapless) data.
 * The recv buffer must be at least N*12 bytes.
 */
static void test_strided_int4_unpack(uint32_t remote_arch)
{
    opal_datatype_t *gapped = NULL;
    opal_convertor_t *conv  = NULL;
    const int NINSTANCES = 3;
    /*
     * Two int32_t fields per instance, separated by a 4-byte gap.
     * field A at offset 0, field B at offset 8.
     * extent = 12 bytes.
     */
    const ptrdiff_t disp_A  = 0;
    const ptrdiff_t disp_B  = 8;
    const ptrdiff_t ext_instance = 12; /* extent of one instance */

    /* Host values: 3 instances, 2 fields each */
    static const int32_t host_A[3] = {0x11223344, 0x55667788, (int32_t) 0x99AABBCC};
    static const int32_t host_B[3] = {0x0A0B0C0D, 0x0E0F1011, 0x12131415};

    /* Wire buffer: packed, 2 int32 per instance, no gaps */
    int32_t wire_buf[6];
    /* Recv buffer: 3 instances each of extent 12 bytes = 9 int32 slots */
    int32_t recv_buf[9];
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data;
    int rc;
    int i;

    memset(recv_buf, 0xAB, sizeof(recv_buf)); /* fill with sentinel */

    /* Build wire buffer: byte-swapped A then byte-swapped B, per instance */
    for (i = 0; i < NINSTANCES; i++) {
        swap_bytes(&wire_buf[i * 2 + 0], &host_A[i], sizeof(int32_t));
        swap_bytes(&wire_buf[i * 2 + 1], &host_B[i], sizeof(int32_t));
    }

    /* Create derived type: empty base, then add field A and field B */
    rc = opal_datatype_create_contiguous(0, &opal_datatype_empty, &gapped);
    test_verify("gapped int4: create empty ok", OPAL_SUCCESS == rc);
    if (OPAL_SUCCESS != rc || NULL == gapped) {
        return;
    }

    /* field A: 1 int4 at offset 0, extent = disp_B (= 8) */
    rc = opal_datatype_add(gapped, &opal_datatype_int4, 1, disp_A, disp_B);
    test_verify("gapped int4: add field A ok", OPAL_SUCCESS == rc);

    /* field B: 1 int4 at offset 8, extent = ext_instance (= 12) */
    rc = opal_datatype_add(gapped, &opal_datatype_int4, 1, disp_B, ext_instance);
    test_verify("gapped int4: add field B ok", OPAL_SUCCESS == rc);

    rc = opal_datatype_commit(gapped);
    test_verify("gapped int4: commit ok", OPAL_SUCCESS == rc);

    test_verify("gapped int4: type size == 8",
                (size_t) 8 == gapped->size);
    test_verify("gapped int4: type is non-contiguous",
                0 == (gapped->flags & OPAL_DATATYPE_FLAG_NO_GAPS));

    conv = opal_convertor_create(remote_arch, 0);
    test_verify("gapped int4: convertor created", NULL != conv);
    if (NULL == conv) {
        opal_datatype_destroy(&gapped);
        return;
    }

    rc = opal_convertor_prepare_for_recv(conv, gapped, NINSTANCES, recv_buf);
    test_verify("gapped int4: prepare ok", OPAL_SUCCESS == rc);

    iov.iov_base = wire_buf;
    iov.iov_len  = sizeof(wire_buf);    /* 6 * 4 = 24 bytes */
    iov_count    = 1;
    max_data     = sizeof(wire_buf);
    opal_convertor_unpack(conv, &iov, &iov_count, &max_data);

    /*
     * Each instance of gapped occupies ext_instance=12 bytes in recv_buf
     * (i.e. 3 int32 slots).  Field A is at offset 0, field B at offset 8
     * within each instance.
     *
     *  instance i recv_buf layout (int32 indices):
     *    [i*3 + 0] = field A    (offset 0 / ext=12 => byte 0 in instance)
     *    [i*3 + 1] = gap        (bytes 4-7, never written)
     *    [i*3 + 2] = field B    (offset 8 / ext=12 => byte 8 in instance)
     */
    for (i = 0; i < NINSTANCES; i++) {
        test_verify("gapped int4: field A round-trips",
                    host_A[i] == recv_buf[i * 3 + 0]);
        test_verify("gapped int4: field B round-trips",
                    host_B[i] == recv_buf[i * 3 + 2]);
    }

    OBJ_RELEASE(conv);
    opal_datatype_destroy(&gapped);
}

/*
 * test_fake_stack_direct
 *
 * Directly call opal_convertor_create_stack_with_pos_general to exercise
 * the heterogeneous position-restore stack (opal_datatype_fake_stack.c).
 *
 * The function signature is:
 *   int opal_convertor_create_stack_with_pos_general(
 *           opal_convertor_t *pConvertor,
 *           size_t starting_point,
 *           const size_t *sizes);
 *
 * Preconditions from the source:
 *   - starting_point != 0
 *   - pConvertor->bConverted != starting_point  (i.e. not already there)
 *   - starting_point <= pConvertor->count * pData->size
 *   - !(pConvertor->flags & CONVERTOR_SEND)
 *
 * We set up a heterogeneous recv convertor over an array of int4 values,
 * partially unpack the first element (so bConverted = sizeof(int32_t)),
 * then call the function to seek to the second element.  After the call
 * we verify that bConverted has advanced to the requested position.
 *
 * Note: opal_convertor_create_stack_with_pos_general is declared in
 * opal_datatype_fake_stack.c with an extern prototype inside the same
 * file.  We declare it here to be able to call it.
 */
extern int opal_convertor_create_stack_with_pos_general(opal_convertor_t *pConvertor,
                                                        size_t starting_point,
                                                        const size_t *sizes);

static void test_fake_stack_direct(uint32_t remote_arch)
{
    /*
     * 4 int32_t values; after seeking past 2 elements, bConverted == 8.
     */
    static const int32_t host_vals[4] = {10, 20, 30, 40};
    int32_t wire_buf[4];
    int32_t recv_buf[4];
    opal_convertor_t *conv = NULL;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data;
    size_t seek_to;
    int rc;
    int i;

    memset(recv_buf, 0, sizeof(recv_buf));

    for (i = 0; i < 4; i++) {
        swap_bytes(&wire_buf[i], &host_vals[i], sizeof(int32_t));
    }

    conv = opal_convertor_create(remote_arch, 0);
    test_verify("fake_stack: convertor created", NULL != conv);
    if (NULL == conv) {
        return;
    }

    rc = opal_convertor_prepare_for_recv(conv, &opal_datatype_int4, 4, recv_buf);
    test_verify("fake_stack: prepare ok", OPAL_SUCCESS == rc);

    /* Unpack the first element so that bConverted = sizeof(int32_t) */
    iov.iov_base = wire_buf;
    iov.iov_len  = sizeof(int32_t);
    iov_count    = 1;
    max_data     = sizeof(int32_t);
    opal_convertor_unpack(conv, &iov, &iov_count, &max_data);

    test_verify("fake_stack: first element unpacked",
                host_vals[0] == recv_buf[0]);
    test_verify("fake_stack: bConverted == sizeof(int32_t) after first unpack",
                sizeof(int32_t) == conv->bConverted);

    /*
     * Now seek to just past the second element: starting_point = 2*4 = 8.
     * Preconditions satisfied:
     *   starting_point (8) != 0
     *   bConverted (4) != starting_point (8)
     *   starting_point (8) <= count*size (4*4 = 16)
     *   !(flags & CONVERTOR_SEND)
     */
    seek_to = 2 * sizeof(int32_t);
    rc = opal_convertor_create_stack_with_pos_general(conv, seek_to,
                                                      opal_datatype_local_sizes);
    test_verify("fake_stack: create_stack_with_pos_general returns ok",
                OPAL_SUCCESS == rc);
    /*
     * NOTE: this assertion currently FAILS, and the assertion -- not the
     * library -- is the suspect.  opal_convertor_create_stack_with_pos_general()
     * is an internal helper with ZERO production callers (grep the tree): for a
     * predefined CONTIGUOUS datatype like int4, production always routes through
     * opal_convertor_create_stack_with_pos_contig() instead, so this path is
     * never exercised in practice.  Called directly here in the heterogeneous +
     * predefined-contiguous regime, the general walker consumes the single DATA
     * descriptor element and falls through to bConverted = local_size (16), not
     * the asserted seek_to (8).  There is no documented postcondition that
     * bConverted == starting_point for this helper.  Left in place (failing) for
     * a maintainer to confirm/restrict.  (This test is intentionally NOT wired
     * into "make check"; see the file-level note.)
     */
    test_verify("fake_stack: bConverted advanced to seek_to",
                seek_to == conv->bConverted);

    OBJ_RELEASE(conv);
}

/*
 * test_set_position_and_resume
 *
 * Exercises the convertor set-position path (which internally calls
 * opal_convertor_set_position_nocheck) then resumes unpacking.
 *
 * We send 4 int32_t values one at a time, verifying that we can seek
 * to an arbitrary byte offset mid-array and continue unpacking from
 * that point.
 *
 * This path also exercises the fake_stack indirectly via
 * opal_convertor_set_position -> set_position_nocheck ->
 * (for contiguous types) create_stack_with_pos_contig.
 */
static void test_set_position_and_resume(uint32_t remote_arch)
{
    static const int32_t host_vals[4] = {100, 200, 300, 400};
    int32_t wire_buf[4];
    int32_t recv_buf[4];
    opal_convertor_t *conv = NULL;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data;
    size_t pos;
    int rc;
    int i;

    memset(recv_buf, 0, sizeof(recv_buf));
    for (i = 0; i < 4; i++) {
        swap_bytes(&wire_buf[i], &host_vals[i], sizeof(int32_t));
    }

    conv = opal_convertor_create(remote_arch, 0);
    test_verify("set_pos: convertor created", NULL != conv);
    if (NULL == conv) {
        return;
    }

    rc = opal_convertor_prepare_for_recv(conv, &opal_datatype_int4, 4, recv_buf);
    test_verify("set_pos: prepare ok", OPAL_SUCCESS == rc);

    /* Unpack element 0 only */
    iov.iov_base = &wire_buf[0];
    iov.iov_len  = sizeof(int32_t);
    iov_count    = 1;
    max_data     = sizeof(int32_t);
    opal_convertor_unpack(conv, &iov, &iov_count, &max_data);
    test_verify("set_pos: elem[0] correct", host_vals[0] == recv_buf[0]);

    /* Seek past element 1 (skip it) to byte offset 8 */
    pos = 2 * sizeof(int32_t);
    rc = opal_convertor_set_position(conv, &pos);
    test_verify("set_pos: set_position ok", OPAL_SUCCESS == rc);
    test_verify("set_pos: position landed at 8", (size_t) 8 == conv->bConverted);

    /* Unpack elements 2 and 3 from their respective wire positions */
    iov.iov_base = &wire_buf[2];
    iov.iov_len  = 2 * sizeof(int32_t);
    iov_count    = 1;
    max_data     = 2 * sizeof(int32_t);
    opal_convertor_unpack(conv, &iov, &iov_count, &max_data);
    /*
     * OPEN QUESTION (this assertion currently FAILS): after set_position(8) on a
     * HETEROGENEOUS, FLAG_CONTIGUOUS recv convertor, elem[2] is never written
     * (stays 0) while elem[3] resumes correctly.  Traced cause: a disp-
     * composition mismatch -- opal_convertor_create_stack_with_pos_contig()
     * encodes the element skip in stack[0].disp, but the heterogeneous unpack
     * path (opal_unpack_general) initializes conv_ptr from stack[1].disp (== 0).
     * The identical sequence PASSES homogeneous (the contig unpack composes both
     * disps) and FAILS heterogeneous.  Whether this is a live OPAL bug or merely
     * an unreachable path depends on whether production ever calls
     * opal_convertor_set_position() mid-stream on a heterogeneous +
     * FLAG_CONTIGUOUS recv convertor -- a question for a maintainer.  Left in
     * place (failing).  (This test is intentionally NOT wired into "make check";
     * see the file-level note.)
     */
    test_verify("set_pos: elem[2] correct after resume", host_vals[2] == recv_buf[2]);
    test_verify("set_pos: elem[3] correct after resume", host_vals[3] == recv_buf[3]);

    OBJ_RELEASE(conv);
}

/* -----------------------------------------------------------------------
 * main
 * ----------------------------------------------------------------------- */
int main(int argc, char *argv[])
{
    uint32_t remote_arch;
    int ret;

    test_init("opal_ddt_hetero");

    /*
     * opal_init initialises the arch detection (opal_local_arch) and
     * the datatype engine.  All existing OPAL-only datatype tests follow
     * this pattern.
     */
    opal_init(NULL, NULL);

    /*
     * Simulate a peer whose endianness is the opposite of ours.
     * This forces every conversion function to byte-swap.
     */
    remote_arch = opal_local_arch ^ OPAL_ARCH_ISBIGENDIAN;

    /* --- copy_functions_heterogeneous.c --- */
    test_int2(remote_arch);
    test_int4(remote_arch);
    test_int8(remote_arch);
    test_uint2(remote_arch);
    test_uint4(remote_arch);
    test_uint8(remote_arch);
    test_float4(remote_arch);
    test_float8(remote_arch);
    test_wchar(remote_arch);

    /* Contiguous multi-element: exercises "countperblock = count" branch */
    test_int4_contiguous_array(remote_arch);
    test_int4_negative(remote_arch);

    /* --- opal_datatype_get_count.c (compute_ptypes, compute_remote_size) --- */
    test_remote_size_predefined(remote_arch);
    test_remote_size_derived(remote_arch);

    /* --- non-contiguous strided type --- */
    test_strided_int4_unpack(remote_arch);

    /* --- opal_datatype_fake_stack.c --- */
    test_fake_stack_direct(remote_arch);

    /* --- set_position path (also drives fake_stack indirectly) --- */
    test_set_position_and_resume(remote_arch);

    ret = test_finalize();
    opal_finalize();
    return ret;
}
