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

#include <stdlib.h>

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
    size_t max_data, position;
    int32_t free_after;
    uint32_t pack_checksum, contiguous_checksum, sparse_checksum;
    struct iovec iov;
    ompi_convertor_t* convertor;

    ompi_ddt_init();
    srandomdev();

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
    convertor = ompi_convertor_create( ompi_mpi_local_arch, CONVERTOR_WITH_CHECKSUM );
    position = 0;
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, &position, NULL, NULL );
    ompi_convertor_prepare_for_send( convertor, sparse, SIZE, sparse_array );

    iov.iov_base = packed;
    iov.iov_len = sizeof(int) * SIZE;
    max_data = iov.iov_len;

    iov_count = 1;
    ompi_convertor_pack( convertor, &iov, &iov_count, &max_data, &free_after );
    pack_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    /**
     * Now move the data from the packed array into the fragment to
     * be sent over the network (still simulation).
     */
    convertor = ompi_convertor_create( ompi_mpi_local_arch, CONVERTOR_WITH_CHECKSUM );
    position = 0;
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, &position, NULL, NULL );
    ompi_convertor_prepare_for_send( convertor, MPI_INT, SIZE, packed );

    iov.iov_base = array;
    iov.iov_len = sizeof(int) * SIZE;
    max_data = iov.iov_len;

    iov_count = 1;
    ompi_convertor_pack( convertor, &iov, &iov_count, &max_data, &free_after );
    contiguous_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    /**
     * And now we're on the receiver side. We just get one fragment from
     * the network and now we unpack it in the user memory.
     */
    convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );
    position = 0;
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, &position, NULL, NULL );
    ompi_convertor_prepare_for_recv( convertor, sparse, SIZE, sparse_array );

    iov.iov_base = array;
    iov.iov_len = sizeof(int) * SIZE;
    max_data = iov.iov_len;

    iov_count = 1;
    ompi_convertor_unpack( convertor, &iov, &iov_count, &max_data, &free_after );
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
    if( (sparse_checksum != contiguous_checksum) &&
        (pack_checksum != sparse_checksum) ) {
        printf( "ERROR!!! the checksum algorithm does not work as expected\n" );
        return 1;
    }
    printf( "COOL the 3 checksum match\n" );

    return 0;
}
