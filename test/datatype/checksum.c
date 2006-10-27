/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_checksum.h"

#include <stdlib.h>
#include <time.h>

#define SIZE 1024

typedef struct {
    int useful;
    int useless;
} my_data_t;

int main( int argc, char* argv[] )
{
    MPI_Datatype sparse;
    int *array, *packed;
    my_data_t* sparse_array;
    int i;
    uint32_t iov_count;
    size_t max_data;
    uint32_t pack_checksum, contiguous_checksum, sparse_checksum, manual_checksum;
    struct iovec iov[2];
    ompi_convertor_t* convertor;

    ompi_ddt_init();
    srandom( (int)time(NULL) );
    /*srandomdev();*/

    ompi_ddt_create_vector( SIZE, 1, 2, MPI_INT, &sparse );
    ompi_ddt_commit( &sparse );

    sparse_array = (my_data_t*)malloc( sizeof(my_data_t) * SIZE );
    array = (int*)malloc( sizeof(int) * SIZE );
    packed = (int*)malloc( sizeof(int) * SIZE );
    
    /**
     * Initialize the sparse data using the index.
     */
    for( i = 0; i < SIZE; i++ ) {
        sparse_array[i].useful = random();
        sparse_array[i].useless = 0;
    }

    /**
     * Pack the sparse data into the packed array. This simulate the first step
     * of the buffered operation.
     */
    convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, NULL );
    ompi_convertor_prepare_for_send( convertor, sparse, SIZE, sparse_array );

    iov[0].iov_base = packed;
    iov[0].iov_len = sizeof(int) * SIZE;
    max_data = iov[0].iov_len;

    iov_count = 1;
    ompi_convertor_pack( convertor, iov, &iov_count, &max_data );
    pack_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    /**
     * Now move the data from the packed array into the fragment to
     * be sent over the network (still simulation).
     */
    convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, NULL );
    ompi_convertor_prepare_for_send( convertor, MPI_INT, SIZE, packed );

    iov[0].iov_base = array;
    iov[0].iov_len = sizeof(int) * SIZE;
    max_data = iov[0].iov_len;

    iov_count = 1;
    ompi_convertor_pack( convertor, iov, &iov_count, &max_data );
    contiguous_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    /**
     * And now we're on the receiver side. We just get one fragment from
     * the network and now we unpack it in the user memory using 2
     * separate iovec.
     */
    convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, NULL );
    ompi_convertor_prepare_for_recv( convertor, sparse, SIZE, sparse_array );

    max_data = sizeof(int) * SIZE;
    iov[0].iov_base = array;
    iov[0].iov_len = max_data / 2;
    iov[1].iov_base = (char*)array + iov[0].iov_len;
    iov[1].iov_len = max_data - iov[0].iov_len;

    iov_count = 2;
    ompi_convertor_unpack( convertor, iov, &iov_count, &max_data );
    sparse_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    /**
     * The datatype is not usefull anymore
     */
    OBJ_RELEASE(sparse);

    /**
     * The 3 checksum have to match.
     */
    printf( "contiguous checksum %x\n", contiguous_checksum );
    printf( "packed checksum     %x\n", pack_checksum );
    printf( "sparse checksum     %x\n", sparse_checksum );
    if( (sparse_checksum != contiguous_checksum) ||
        (pack_checksum != sparse_checksum) ) {
        printf( "ERROR!!! the checksum algorithm does not work as expected\n" );
        return 1;
    }
    printf( "COOL the 3 checksum match\n" );

    /**
     * Now that the packed buffer contain the data we want, let's try to call
     * the checksum directly to see if there is any difference.
     */
    {
        uint32_t ui1 = 0;
        size_t ui2 = 0;
        manual_checksum = OPAL_CSUM_PARTIAL( packed, sizeof(int) * SIZE, &ui1, &ui2 );
    }
    printf( "manual checksum     %x\n", manual_checksum );
    return 0;
}
