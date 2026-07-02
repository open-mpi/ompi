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

    a.iov_base = packed;
    a.iov_len = sizeof(sbuf[0]) - 1;
    iov_count = 1;
    max_data = sizeof(sbuf[0]) - 1;
    assert(0 == opal_pack_general(pConv, &a, &iov_count, &max_data));
    assert(0 == max_data);

    for (size_t i = 0; i < 2; ++i) {
        a.iov_base = packed + i * sizeof(sbuf[0]);
        a.iov_len = sizeof(sbuf[0]);
        iov_count = 1;
        max_data = sizeof(sbuf[0]);
        assert((int) i == opal_pack_general(pConv, &a, &iov_count, &max_data));
        assert(sizeof(sbuf[0]) == max_data);
    }
    assert(0 == memcmp(packed, expected, sizeof(packed)));
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

    assert(1 == rbuf[0]);
    assert(-1 == rbuf[1]);
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
        assert(1 == max_data);
    }

    assert(1 == rbuf[0]);
    assert(2 == rbuf[1]);
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

    assert(1 == rbuf[0]);
    assert(-1 == rbuf[1]);
    OBJ_RELEASE(pConv);

    /* clean-ups all data allocations */
    opal_finalize();

    return OPAL_SUCCESS;
}
