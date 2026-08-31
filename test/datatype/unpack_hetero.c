/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <stdio.h>
#include <string.h>

/* Compile with:
gcc -DHAVE_CONFIG_H -I. -I../../include -I../.. -I../../include -I../../../ompi-trunk/opal
-I../../../ompi-trunk/orte -g opal_datatype_test.c -o opal_datatype_test
*/

uint32_t remote_arch = 0xffffffff;

/**
 * Main function. Call several tests and print-out the results. It try to stress the convertor
 * using difficult data-type constructions as well as strange segment sizes for the conversion.
 * Usually, it is able to detect most of the data-type and convertor problems. Any modifications
 * on the data-type engine should first pass all the tests from this file, before going into other
 * tests.
 */
int main(int argc, char *argv[])
{
    opal_init(NULL, NULL);

    /**
     * By default simulate homogeneous architectures.
     */
    remote_arch = opal_local_arch ^ OPAL_ARCH_ISBIGENDIAN;

    opal_convertor_t *pConv;
    int32_t sbuf[2], rbuf[2];
    unsigned char packed[sizeof(sbuf)], expected[sizeof(sbuf)];
    size_t max_data;
    struct iovec a;
    uint32_t iov_count;

    sbuf[0] = 0x01000000;
    sbuf[1] = 0x02000000;
    for (size_t i = 0; i < sizeof(sbuf); ++i) {
        expected[i] = ((unsigned char *) sbuf)[sizeof(sbuf[0]) * (i / sizeof(sbuf[0]))
                                                + sizeof(sbuf[0]) - 1 - (i % sizeof(sbuf[0]))];
    }

    printf("\n\n#\n * TEST PACKING WITHOUT SPLITTING AN INT\n#\n\n");
    pConv = opal_convertor_create(remote_arch, 0);
    pConv->flags |= CONVERTOR_SEND_CONVERSION;
    if (OPAL_SUCCESS != opal_convertor_prepare_for_send(pConv, &opal_datatype_int4, 2, sbuf)) {
        printf("Cannot attach the datatype to a convertor\n");
        return OPAL_ERROR;
    }

    /* The pack/unpack calls perform the actual conversion and advance the
     * convertor, so they must run in every build.  Keep them out of assert()
     * (which expands to nothing under -DNDEBUG) and validate the outcome with
     * explicit checks instead. */
    a.iov_base = packed;
    a.iov_len = sizeof(sbuf[0]) - 1;
    iov_count = 1;
    max_data = sizeof(sbuf[0]) - 1;
    if (0 != opal_pack_general(pConv, &a, &iov_count, &max_data) || 0 != max_data) {
        printf("packing a partial element must not make progress\n");
        return OPAL_ERROR;
    }

    for (size_t i = 0; i < 2; ++i) {
        a.iov_base = packed + i * sizeof(sbuf[0]);
        a.iov_len = sizeof(sbuf[0]);
        iov_count = 1;
        max_data = sizeof(sbuf[0]);
        /* The convertor reports completion (1) only on the final element. */
        if ((int) i != opal_pack_general(pConv, &a, &iov_count, &max_data)
            || sizeof(sbuf[0]) != max_data) {
            printf("packing element %" PRIsize_t " produced unexpected state\n", i);
            return OPAL_ERROR;
        }
    }
    if (0 != memcmp(packed, expected, sizeof(packed))) {
        printf("packed bytes do not match the expected byte-swapped layout\n");
        return OPAL_ERROR;
    }
    OBJ_RELEASE(pConv);

    printf("\n\n#\n * TEST UNPACKING 1 int out of 1\n#\n\n");

    pConv = opal_convertor_create(remote_arch, 0);
    rbuf[0] = -1;
    rbuf[1] = -1;
    if (OPAL_SUCCESS != opal_convertor_prepare_for_recv(pConv, &opal_datatype_int4, 1, rbuf)) {
        printf("Cannot attach the datatype to a convertor\n");
        return OPAL_ERROR;
    }

    a.iov_base = sbuf;
    a.iov_len = 4;
    iov_count = 1;
    max_data = 4;
    opal_unpack_general(pConv, &a, &iov_count, &max_data);

    if (1 != rbuf[0] || -1 != rbuf[1]) {
        printf("unpacking 1 int out of 1 produced unexpected result\n");
        return OPAL_ERROR;
    }
    OBJ_RELEASE(pConv);

    printf("\n\n#\n * TEST UNPACKING 2 ints ONE BYTE AT A TIME\n#\n\n");
    pConv = opal_convertor_create(remote_arch, 0);
    rbuf[0] = -1;
    rbuf[1] = -1;
    if (OPAL_SUCCESS != opal_convertor_prepare_for_recv(pConv, &opal_datatype_int4, 2, rbuf)) {
        printf("Cannot attach the datatype to a convertor\n");
        return OPAL_ERROR;
    }

    for (size_t i = 0; i < sizeof(sbuf); ++i) {
        a.iov_base = (unsigned char *) sbuf + i;
        a.iov_len = 1;
        iov_count = 1;
        max_data = 1;
        opal_unpack_general(pConv, &a, &iov_count, &max_data);
        if (1 != max_data) {
            printf("unpacking byte %" PRIsize_t " did not consume exactly one byte\n", i);
            return OPAL_ERROR;
        }
    }

    if (1 != rbuf[0] || 2 != rbuf[1]) {
        printf("unpacking 2 ints one byte at a time produced unexpected result\n");
        return OPAL_ERROR;
    }
    OBJ_RELEASE(pConv);

    printf("\n\n#\n * TEST UNPACKING 1 int out of 2\n#\n\n");
    pConv = opal_convertor_create(remote_arch, 0);
    rbuf[0] = -1;
    rbuf[1] = -1;
    if (OPAL_SUCCESS != opal_convertor_prepare_for_recv(pConv, &opal_datatype_int4, 2, rbuf)) {
        printf("Cannot attach the datatype to a convertor\n");
        return OPAL_ERROR;
    }

    a.iov_base = sbuf;
    a.iov_len = 4;
    iov_count = 1;
    max_data = 4;
    opal_unpack_general(pConv, &a, &iov_count, &max_data);

    if (1 != rbuf[0] || -1 != rbuf[1]) {
        printf("unpacking 1 int out of 2 produced unexpected result\n");
        return OPAL_ERROR;
    }
    OBJ_RELEASE(pConv);

    /*
     * TEST: unpack a vector (blocklen=2) one element at a time.
     *
     * The datatype is a vector of 2 blocks of 2 int4s with stride 3 (non-contiguous,
     * so the descriptor has a LOOP with a DATA entry whose blocklen=2).  Unpacking
     * one int4 (4 bytes) per call stops mid-block after the first element, which
     * exercises unpack_partial_blocklen_heterogeneous() -- the path that is never
     * reached by any existing tests because they all use blocklen=1 types.
     *
     * Pack copies the sender's bytes onto the wire without conversion; unpack
     * converts from the remote representation to local.  vpacked holds the wire
     * bytes as a peer with the opposite endianness would have sent them for the
     * integer values 1, 2, 3, 4: on a little-endian machine 0x0N000000 in memory
     * is [00,00,00,0N], which is exactly how a big-endian peer encodes the integer
     * N.  opal_unpack_general interprets those bytes in remote (opposite-endian)
     * format and stores the resulting local values 1, 2, 3, 4 in vdst.
     */
    printf("\n\n#\n * TEST UNPACKING A VECTOR (blocklen=2) ONE ELEMENT AT A TIME\n#\n\n");
    {
        /* Wire bytes sent by a peer with the opposite endianness for integers 1..4.
         * 0x01000000 stored on a LE machine is [00,00,00,01] = BE representation
         * of the integer 1; the same value on a BE machine is [01,00,00,00] = LE
         * representation of 1.  Either way the memory layout matches what the
         * opposite-endian peer would have placed on the wire. */
        int32_t vpacked[4] = {0x01000000, 0x02000000, 0x03000000, 0x04000000};
        int32_t vdst[5] = {-1, -1, -1, -1, -1};
        opal_datatype_t *inner, *vtype;
        ptrdiff_t extent = sizeof(int32_t);

        /* inner = 2 contiguous int4s (blocklen=2 in the descriptor) */
        inner = opal_datatype_create((int32_t) opal_datatype_int4.desc.used + 2);
        opal_datatype_add(inner, &opal_datatype_int4, 2, 0, extent);
        /* vtype = 2 repetitions of inner, stride 3 int4s */
        vtype = opal_datatype_create((int32_t) inner->desc.used + 2 + 2);
        opal_datatype_add(vtype, inner, 2, 0, 3 * extent);
        OBJ_RELEASE(inner);
        opal_datatype_commit(vtype);

        /* Unpack one int4 (4 bytes) at a time.  The first call processes one
         * element and leaves COUNT=3 (3 % blocklen(2) == 1 != 0), so the second
         * call enters unpack_partial_blocklen_heterogeneous() to finish the block
         * before the main mover can resume on a block boundary. */
        pConv = opal_convertor_create(remote_arch, 0);
        if (OPAL_SUCCESS != opal_convertor_prepare_for_recv(pConv, vtype, 1, vdst)) {
            printf("Cannot attach vector datatype to recv convertor\n");
            OBJ_RELEASE(pConv);
            OBJ_RELEASE(vtype);
            return OPAL_ERROR;
        }
        for (size_t i = 0; i < 4; ++i) {
            a.iov_base = (unsigned char *) vpacked + i * sizeof(int32_t);
            a.iov_len = sizeof(int32_t);
            iov_count = 1;
            max_data = sizeof(int32_t);
            opal_unpack_general(pConv, &a, &iov_count, &max_data);
            if (sizeof(int32_t) != max_data) {
                printf("unpacking element %" PRIsize_t " did not consume exactly one int4\n", i);
                OBJ_RELEASE(pConv);
                OBJ_RELEASE(vtype);
                return OPAL_ERROR;
            }
        }
        OBJ_RELEASE(pConv);
        OBJ_RELEASE(vtype);

        /* opal_unpack_general always byte-swaps when remote_arch differs, regardless of
         * OPAL_ENABLE_HETEROGENEOUS_SUPPORT (which only controls whether the convertor
         * selects opal_unpack_general automatically; calling it directly always converts). */
        if (1 != vdst[0] || 2 != vdst[1] || -1 != vdst[2] || 3 != vdst[3] || 4 != vdst[4]) {
            printf("unpacking vector one element at a time produced unexpected result\n");
            printf("  got: %d %d %d %d %d\n", vdst[0], vdst[1], vdst[2], vdst[3], vdst[4]);
            printf("  expected: 1 2 -1 3 4\n");
            return OPAL_ERROR;
        }
    }

    /*
     * TEST: pack a vector (blocklen=2) one element at a time.
     *
     * Symmetric twin of the unpack test above, exercising
     * pack_partial_blocklen_heterogeneous().  The same datatype (2 blocks of 2
     * int4s, stride 3) is packed one int4 per output fragment.  After the first
     * call COUNT=3 (3 % blocklen(2) == 1 != 0), so the second call enters
     * pack_partial_blocklen_heterogeneous() to drain the partial block before
     * the main mover can resume on a block boundary.  CONVERTOR_SEND_CONVERSION
     * is set explicitly so opal_pack_general byte-swaps into the remote
     * representation, matching what the unpack test above received.
     */
    printf("\n\n#\n * TEST PACKING A VECTOR (blocklen=2) ONE ELEMENT AT A TIME\n#\n\n");
    {
        int32_t vsrc[5] = {1, 2, -1, 3, 4};   /* local values; slot [2] is the stride gap */
        int32_t vpacked_out[4] = {0, 0, 0, 0};
        int32_t expected_wire[4] = {0x01000000, 0x02000000, 0x03000000, 0x04000000};
        opal_datatype_t *inner, *vtype;
        ptrdiff_t extent = sizeof(int32_t);

        inner = opal_datatype_create((int32_t) opal_datatype_int4.desc.used + 2);
        opal_datatype_add(inner, &opal_datatype_int4, 2, 0, extent);
        vtype = opal_datatype_create((int32_t) inner->desc.used + 2 + 2);
        opal_datatype_add(vtype, inner, 2, 0, 3 * extent);
        OBJ_RELEASE(inner);
        opal_datatype_commit(vtype);

        pConv = opal_convertor_create(remote_arch, 0);
        pConv->flags |= CONVERTOR_SEND_CONVERSION;
        if (OPAL_SUCCESS != opal_convertor_prepare_for_send(pConv, vtype, 1, vsrc)) {
            printf("Cannot attach vector datatype to send convertor\n");
            OBJ_RELEASE(pConv);
            OBJ_RELEASE(vtype);
            return OPAL_ERROR;
        }
        for (size_t i = 0; i < 4; ++i) {
            a.iov_base = (unsigned char *) vpacked_out + i * sizeof(int32_t);
            a.iov_len = sizeof(int32_t);
            iov_count = 1;
            max_data = sizeof(int32_t);
            opal_pack_general(pConv, &a, &iov_count, &max_data);
            if (sizeof(int32_t) != max_data) {
                printf("packing element %" PRIsize_t " did not produce exactly one int4\n", i);
                OBJ_RELEASE(pConv);
                OBJ_RELEASE(vtype);
                return OPAL_ERROR;
            }
        }
        OBJ_RELEASE(pConv);
        OBJ_RELEASE(vtype);

        if (0 != memcmp(vpacked_out, expected_wire, sizeof(expected_wire))) {
            printf("packing vector one element at a time produced unexpected wire bytes\n");
            printf("  got:      %08x %08x %08x %08x\n",
                   vpacked_out[0], vpacked_out[1], vpacked_out[2], vpacked_out[3]);
            printf("  expected: %08x %08x %08x %08x\n",
                   expected_wire[0], expected_wire[1], expected_wire[2], expected_wire[3]);
            return OPAL_ERROR;
        }
    }

    /* clean-ups all data allocations */
    opal_finalize();

    return OPAL_SUCCESS;
}
