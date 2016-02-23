/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

void dump_hex(void* what, size_t length)
{
    int i;
    for( i = 0; i < length; i++ ) {
        printf("%02x", (unsigned int)(((unsigned char*)what)[i]));
    }
}

int pack_unpack_datatype( void* send_data, MPI_Datatype datatype, int count,
                          void* recv_data )
{
    MPI_Aint position = 0, buffer_size;
    void* buffer;
    int error;

    error = MPI_Pack_external_size("external32",
                                   count, datatype, &buffer_size);
    if( MPI_SUCCESS != error ) goto return_error_code;

    buffer = (void*)malloc(buffer_size);
    if( NULL == buffer ) { error = MPI_ERR_UNKNOWN; goto return_error_code; }

    error = MPI_Pack_external("external32", (void*)send_data, count, datatype,
                              buffer, buffer_size, &position);
    if( MPI_SUCCESS != error ) goto return_error_code;

    printf("packed "); dump_hex(buffer, buffer_size); printf("\n");

    position = 0;
    error = MPI_Unpack_external("external32", buffer, buffer_size, &position,
                                recv_data, count, datatype);
    if( MPI_SUCCESS != error ) goto return_error_code;
    free(buffer);

 return_error_code:
    return (error == MPI_SUCCESS ? 0 : -1);
}

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);

    /* Simple contiguous data */
    {
        int send_data[2] = {1234, 5678};
        int recv_data[2] = {-1, -1};

        printf("send data %08x %08x \n", send_data[0], send_data[1]);
        printf("data "); dump_hex(&send_data, sizeof(int) * 2); printf("\n");

        (void)pack_unpack_datatype( send_data, MPI_INT, 2,
                                    recv_data );

        printf("recv "); dump_hex(&recv_data, sizeof(int) * 2); printf("\n");
        printf("recv data %08x %08x \n", recv_data[0], recv_data[1]);
        if( (send_data[0] != recv_data[0]) || (send_data[1] != recv_data[1]) ) {
            printf("Error duing external32 pack/unack for contiguous types\n");
            exit(-1);
        }
    }

    /* Vector datatype */
    printf("\n\nVector datatype\n\n");
    {
        int send_data[3] = {1234, 0, 5678};
        int recv_data[3] = {-1, -1, -1};
        MPI_Datatype ddt;

        MPI_Type_vector(2, 1, 2, MPI_INT, &ddt);
        MPI_Type_commit(&ddt);
        printf("send data %08x %x08x %08x \n", send_data[0], send_data[1], send_data[2]);
        printf("data "); dump_hex(&send_data, sizeof(int) * 3); printf("\n");

        (void)pack_unpack_datatype( send_data, ddt, 1, recv_data );

        printf("recv "); dump_hex(&recv_data, sizeof(int) * 3); printf("\n");
        printf("recv data %08x %08x %08x \n", recv_data[0], recv_data[1], recv_data[2]);
        MPI_Type_free(&ddt);
        if( (send_data[0] != recv_data[0]) || (send_data[2] != recv_data[2]) ) {
            printf("Error duing external32 pack/unack for vector types\n");
            exit(-1);
        }
    }

    MPI_Finalize();

    return 0;
}
