/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2018      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This test check the correct OMPI datatype description for
 * extremely large types (over 4GB).
 */

#include <mpi.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/runtime/opal.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"

#define MAX_IOVEC 10
#define MAX_CHUNK (1024*1024*1024)  /* 1GB */

static int verbose = 0;

static size_t
count_length_via_convertor_raw(char* msg,
                               MPI_Datatype dtype, int count)
{
    opal_convertor_t* pconv;
    struct iovec iov[MAX_IOVEC];
    uint32_t iov_count = MAX_IOVEC, i;
    size_t length = MAX_CHUNK, packed_iovec = 0, packed = 0;

    pconv = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_send(pconv, (const struct opal_datatype_t *)dtype, 1, NULL);
    while( 0 == opal_convertor_raw(pconv, iov, &iov_count, &length) ) {
        if( verbose ) {
            printf("iov_count = %d packed_iovec = %"PRIsize_t" length = %"PRIsize_t"\n",
                   iov_count, packed_iovec, length);
        }
        packed += length;
        for( i = 0; i < iov_count; i++ ) {
            packed_iovec += iov[i].iov_len;
            if( verbose ) {
                printf("[%s] add %"PRIsize_t" bytes -> so far %"PRIsize_t" bytes\n",
                       msg, iov[i].iov_len, packed_iovec);
            }
        }
        if( packed != packed_iovec ) {
            printf( "[%s] Raw data amount diverges %"PRIsize_t" != %"PRIsize_t"\n",
                    msg, packed, packed_iovec);
            exit(-1);
        }
        iov_count = MAX_IOVEC;  /* number of available iov */
        length = MAX_CHUNK;
    }
    if( verbose ) {
        printf("iov_count = %d packed_iovec = %"PRIsize_t" length = %"PRIsize_t"\n",
               iov_count, packed_iovec, length);
    }
    packed += length;
    for( i = 0; i < iov_count; i++ ) {
        packed_iovec += iov[i].iov_len;
        if( verbose ) {
            printf("[%s] add %"PRIsize_t" bytes -> so far %"PRIsize_t" bytes\n",
                   msg, iov[i].iov_len, packed_iovec);
        }
    }
    if( packed != packed_iovec ) {
        printf( "[%s] Raw data amount diverges %"PRIsize_t" != %"PRIsize_t"\n",
                msg, packed, packed_iovec);
        exit(-1);
    }
    return packed_iovec;
}

int main(int argc, char * argv[])
{

    int const per_process = 192;
    int const per_type = 20000000;
    int blocklen, stride, count;

    int scounts[2] = {per_process, per_process};
    int sdispls[2] = {3*per_process, 0*per_process};
    int rcounts[2] = {per_process, per_process};
    int rdispls[2] = {1*per_process, 2*per_process};

    MPI_Datatype ddt, stype, rtype;
    size_t length, packed;

    opal_init_util(&argc, &argv);
    ompi_datatype_init();

    ompi_datatype_create_contiguous( per_type, MPI_FLOAT, &ddt);

    /*
     * Large sparse datatype: indexed contiguous
     */
    ompi_datatype_create_indexed(2, scounts, sdispls, ddt, &stype);
    ompi_datatype_commit(&stype);

    packed = count_length_via_convertor_raw("1. INDEX", stype, 1);
    opal_datatype_type_size(&stype->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }
    ompi_datatype_destroy(&stype);

    /*
     * Large contiguous datatype: indexed contiguous
     */
    ompi_datatype_create_indexed(2, rcounts, rdispls, ddt, &rtype);
    ompi_datatype_commit(&rtype);

    packed = count_length_via_convertor_raw("2. INDEX", rtype, 1);
    opal_datatype_type_size(&rtype->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }
    ompi_datatype_destroy(&rtype);
    ompi_datatype_destroy(&ddt);

    /*
     * Large sparse datatype: vector
     */
    count = INT_MAX / 2;
    blocklen = stride = 4;
    ompi_datatype_create_vector(count, blocklen, stride, MPI_FLOAT, &ddt);
    ompi_datatype_commit(&ddt);

    packed = count_length_via_convertor_raw("3. VECTOR", ddt, 1);
    opal_datatype_type_size(&ddt->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }
    ompi_datatype_destroy(&ddt);

    /*
     * Large sparse datatype: contiguous
     */
    MPI_Datatype tmp;
    ompi_datatype_create_contiguous(stride, MPI_FLOAT, &tmp);
    ompi_datatype_create_contiguous(count, tmp, &ddt);
    ompi_datatype_commit(&ddt);

    packed = count_length_via_convertor_raw("4. CONTIG", ddt, 1);
    opal_datatype_type_size(&ddt->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }
    ompi_datatype_destroy(&ddt);
    ompi_datatype_destroy(&tmp);

    return 0;
}
