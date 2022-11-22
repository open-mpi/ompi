/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TYPE_COUNT    3
#define TYPE_BLEN     2
#define TYPE_STRIDE   4

#define CONT_COUNT    2

#define COUNT         3

#define CHUNK   ((TYPE_BLEN*8)*2-4)

/**
 * Print how many elements on both sides of ptr.
 */
static void show_neighborhood(double* ptr, int how_many, bool show_hex)
{
    int i;

    printf("%12p: ", (void*)ptr);
    for( i = -how_many; i < how_many;  i++ ) {
        if( 0 == i ) {
            printf(" <%g> ", ptr[i]);
        } else {
            printf("  %g  ", ptr[i]);
        }
    }
    if( show_hex ) {
        char* cptr = (char*)ptr;
        printf("\n            : ");
        for( i = -how_many; i < how_many;  i++ ) {
            if( 0 == i ) printf(" <");
            for( size_t j = 0; j < sizeof(double); j++ ) {
                printf("%02x", cptr[i * sizeof(double)+j]);
            }
            if( 0 == i ) printf("> ");
            else printf(" ");
        }
    }
    printf("\n\n");
}

/**
 * -------G---[---][---]    OPAL_LOOP_S 19 times the next 2 elements extent 18432
 * -cC---P-DB-[---][---]    OPAL_FLOAT8 count 72 disp 0x80 (128) blen 16 extent 256 (size 9216)
 * -------G---[---][---]    OPAL_LOOP_E prev 2 elements first elem displacement 128 size of data 9216
 * -------G---[---][---]    OPAL_LOOP_E prev 3 elements first elem displacement 128 size of data 175104
 */

int main( int argc, char* argv[] )
{
    ompi_datatype_t* vector;
    ompi_datatype_t* base;
    uint32_t iov_count;
    size_t max_data, size, length;
    struct iovec iov[2];
    opal_convertor_t* convertor;
    ptrdiff_t extent, base_extent;
    double *array, *packed;
    char* bpacked;
    int i, j;

    opal_init(NULL, NULL);
    ompi_datatype_init();

    ompi_datatype_create_vector(TYPE_COUNT, TYPE_BLEN, TYPE_STRIDE, MPI_DOUBLE, &base);
    ompi_datatype_create_contiguous(CONT_COUNT, base, &vector);

    opal_datatype_commit(&vector->super);

    ompi_datatype_dump(vector);

    opal_datatype_type_size(&vector->super, &size);
    opal_datatype_type_extent(&vector->super, &extent);
    opal_datatype_type_extent(&base->super, &base_extent);

    array = (double*)malloc( extent * COUNT );
    packed = (double*)malloc( size * COUNT );
    bpacked = (char*)packed;

    /**
     * Initialize the sparse data using the index.
     */
    for( i = 0; i < (TYPE_BLEN * TYPE_COUNT * CONT_COUNT * COUNT); i++ ) {
        packed[i] = (double)(i % TYPE_BLEN);
    }
    memset(array, extent * COUNT, TYPE_BLEN + 1);

    /**
     * Pack the sparse data into the packed array. This simulate the first step
     * of the buffered operation.
     */
    convertor = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_recv(convertor, &vector->super, COUNT, array);

    for( length = 0; length < (size * COUNT); ) {
        iov[0].iov_base = bpacked + length;
        iov[0].iov_len = CHUNK;
        max_data = iov[0].iov_len;

        iov_count = 1;
        opal_convertor_unpack( convertor, iov, &iov_count, &max_data );
        length += max_data;

        int idx = 0;
        size_t checked = 0;
        for( int m = 0; m < COUNT; m++ ) {
            char* mptr = (char*)array + m * extent;
            for( int k = 0; k < CONT_COUNT; k++ ) {
                char* kptr = mptr + k * base_extent;
                for( j = 0; j < TYPE_COUNT; j++ ) {
                    double* jarray = (double*)kptr + j * TYPE_STRIDE;
                    for( i = 0; i < TYPE_BLEN; i++ ) {
                        checked += sizeof(double);
                        if( checked > length )
                            goto next_iteration;
                        if( jarray[i] != (double)(idx % TYPE_BLEN) ) {
                            fprintf(stderr, "\n\n\nError during check for the %d element, length %" PRIsize_t " (chunk %d)\n",
                                    idx, length, CHUNK);
                            fprintf(stderr, "Error at position %d [%d:%d:%d:%d] found %g expected %g\n\n\n",
                                    idx, m, k, j, i, jarray[i], (double)(idx % TYPE_BLEN));
                            show_neighborhood(jarray + i, 4, true);
                            exit(-1);
                        }
                        idx++;
                    }
                }
            }
        }
next_iteration:
        /* nothing special to do here, just move to the next conversion */
        continue;
    }

    OBJ_RELEASE(convertor);

    /**
     * The datatype is not useful anymore
     */
    OBJ_RELEASE(vector);

    free(array);
    free(packed);

    /* clean-ups all data allocations */
    opal_finalize_util ();

    return 0;
}
