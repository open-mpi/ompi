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

#define SIZE 1024

int main( int argc, char* argv[] )
{
    MPI_Datatype sparse;
    int *array, *sparse_array, *packed;
    int i;
    uint32_t iov_count;
    size_t max_data, position;
    int32_t free_after;
    uint32_t contiguous_checksum, sparse_checksum;
    struct iovec iov;
    ompi_convertor_t* convertor;
    long extent;

    ompi_ddt_init();

    ompi_ddt_create_vector( SIZE, 1, 2, MPI_INT, &sparse );
    ompi_ddt_commit( &sparse );

    ompi_ddt_type_extent( sparse, &extent );

    sparse_array = (int*)malloc( extent );
    array = (int*)malloc( sizeof(int) * SIZE );
    packed = (int*)malloc( sizeof(int) * SIZE );
    
    for( i = 0; i < SIZE; packed[i] = i, i++ );

    convertor = ompi_convertor_create( ompi_mpi_local_arch, CONVERTOR_WITH_CHECKSUM );
    position = 0;
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, &position, NULL, NULL );
    ompi_convertor_prepare_for_recv( convertor, MPI_INT, SIZE, array );

    iov.iov_base = packed;
    iov.iov_len = sizeof(int) * SIZE;
    max_data = iov.iov_len;

    iov_count = 1;
    ompi_convertor_unpack( convertor, &iov, &iov_count, &max_data, &free_after );
    contiguous_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);

    convertor = ompi_convertor_create( ompi_mpi_local_arch, 0 );
    position = 0;
    ompi_convertor_personalize( convertor, CONVERTOR_WITH_CHECKSUM, &position, NULL, NULL );
    ompi_convertor_prepare_for_recv( convertor, sparse, SIZE, sparse_array );

    iov.iov_base = packed;
    iov.iov_len = sizeof(int) * SIZE;
    max_data = iov.iov_len;

    iov_count = 1;
    ompi_convertor_unpack( convertor, &iov, &iov_count, &max_data, &free_after );
    sparse_checksum = convertor->checksum;

    OBJ_RELEASE(convertor);
    OBJ_RELEASE(sparse);

    if( sparse_checksum != contiguous_checksum ) {
        printf( "ERROR!!! the checksum algorithm does not work as expected\n" );
        printf( "contiguous checksum %x != sparse checksum %x\n",
                contiguous_checksum, sparse_checksum );
        return 1;
    }
    printf( "contiguous checksum %x == sparse checksum %x\n",
            contiguous_checksum, sparse_checksum );

    return 0;
}
