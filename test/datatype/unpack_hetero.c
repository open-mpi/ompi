/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/runtime/opal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/util/arch.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>
#include <string.h>

/* Compile with:
gcc -DHAVE_CONFIG_H -I. -I../../include -I../.. -I../../include -I../../../ompi-trunk/opal -I../../../ompi-trunk/orte -g opal_datatype_test.c -o opal_datatype_test
*/

uint32_t remote_arch = 0xffffffff;

/**
 * Main function. Call several tests and print-out the results. It try to stress the convertor
 * using difficult data-type constructions as well as strange segment sizes for the conversion.
 * Usually, it is able to detect most of the data-type and convertor problems. Any modifications
 * on the data-type engine should first pass all the tests from this file, before going into other
 * tests.
 */
int main( int argc, char* argv[] )
{
    opal_datatype_init();

    /**
     * By default simulate homogeneous architectures.
     */
    remote_arch = opal_local_arch ^ OPAL_ARCH_ISBIGENDIAN;

    opal_convertor_t * pConv;
    int sbuf[2], rbuf[2];
    size_t max_data;
    struct iovec a;
    uint32_t iov_count;

    sbuf[0] = 0x01000000; sbuf[1] = 0x02000000;

    printf( "\n\n#\n * TEST UNPACKING 1 int out of 1\n#\n\n" );

    pConv = opal_convertor_create( remote_arch, 0 );
    rbuf[0] = -1; rbuf[1] = -1;
    if( OPAL_SUCCESS != opal_convertor_prepare_for_recv( pConv, &opal_datatype_int4, 1, rbuf ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OPAL_ERROR;
    }
    
    a.iov_base = sbuf;
    a.iov_len = 4;
    iov_count = 1;
    max_data = 4;
    opal_unpack_general( pConv, &a, &iov_count, &max_data );

    assert(1 == rbuf[0]);
    assert(-1 == rbuf[1]);
    OBJ_RELEASE(pConv);

    printf( "\n\n#\n * TEST UNPACKING 1 int out of 2\n#\n\n" );
    pConv = opal_convertor_create( remote_arch, 0 );
    rbuf[0] = -1; rbuf[1] = -1;
    if( OPAL_SUCCESS != opal_convertor_prepare_for_recv( pConv, &opal_datatype_int4, 2, rbuf ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OPAL_ERROR;
    }
    

    a.iov_base = sbuf;
    a.iov_len = 4;
    iov_count = 1;
    max_data = 4;
    opal_unpack_general( pConv, &a, &iov_count, &max_data );

    assert(1 == rbuf[0]);
    assert(-1 == rbuf[1]);
    OBJ_RELEASE(pConv);

    /* clean-ups all data allocations */
    opal_datatype_finalize();
    opal_finalize();
    return OPAL_SUCCESS;
}
