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
#include <arpa/inet.h>

static int verbose = 0;

void dump_hex(void* what, size_t length)
{
    int i;
    for( i = 0; i < length; i++ ) {
        printf("%02x", (unsigned int)(((unsigned char*)what)[i]));
    }
}

typedef int (*checker_t)(void*, void*, MPI_Datatype, int, void*);

int check_contiguous( void* send_buffer, void* packed,
                      MPI_Datatype datatype, int count, void* arg )
{
    int i, resultlen;
    char typename[MPI_MAX_OBJECT_NAME];

    MPI_Type_get_name(datatype, typename, &resultlen);

    if( (datatype == MPI_INT) || (datatype == MPI_INT32_T) ) {
        uint32_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htonl(((uint32_t*)send_buffer)[i]);
            if( val != ((uint32_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint32_t*)packed)[i], ((uint32_t*)send_buffer)[i], typename);
                return -1;
            }
        }
    } else if( (datatype == MPI_SHORT) || (datatype == MPI_INT16_T) ) {
        uint16_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htons(((uint16_t*)send_buffer)[i]);
            if( val != ((uint16_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint16_t*)packed)[i], ((uint16_t*)send_buffer)[i], typename);
                return -1;
            }
        }
    } else {
        printf("Unknown type\n");
        return -1;
    }
    return 0;
}

int check_vector( void* send_buffer, void* packed,
                  MPI_Datatype datatype, int count, void* arg )
{
    int i, resultlen;
    char typename[MPI_MAX_OBJECT_NAME];
    MPI_Datatype origtype = (MPI_Datatype)arg;

    MPI_Type_get_name(datatype, typename, &resultlen);

    if( (origtype == MPI_INT) || (origtype == MPI_INT32_T) ) {
        uint32_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htonl(((uint32_t*)send_buffer)[2*i]);
            if( val != ((uint32_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint32_t*)packed)[i], ((uint32_t*)send_buffer)[2*i], typename);
                return -1;
            }
        }
    } else if( (origtype == MPI_SHORT) || (origtype == MPI_INT16_T) ) {
        uint16_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htons(((uint16_t*)send_buffer)[2*i]);
            if( val != ((uint16_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint16_t*)packed)[i], ((uint16_t*)send_buffer)[2*i], typename);
                return -1;
            }
        }
    } else {
        printf("Unknown %s type\n", typename);
        return -1;
    }
    return 0;
}

int pack_unpack_datatype( void* send_data, MPI_Datatype datatype, int count,
                          void* recv_data,
                          checker_t validator, void* validator_arg)
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
    if( 0 != validator(send_data, buffer, datatype, count, validator_arg) ) {
        printf("Error during pack external. Bailing out\n");
        return -1;
    }

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

    /* Simple contiguous data: MPI_INT32_T */
    {
        int32_t send_data[2] = {1234, 5678};
        int32_t recv_data[2] = {-1, -1};

        if( verbose ) {
            printf("send data %08x %08x \n", send_data[0], send_data[1]);
            printf("data "); dump_hex(&send_data, sizeof(int32_t) * 2); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, MPI_INT32_T, 2,
                                    recv_data, check_contiguous, (void*)(ptrdiff_t)MPI_INT32_T );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int32_t) * 2); printf("\n");
            printf("recv data %08x %08x \n", recv_data[0], recv_data[1]);
        }
        if( (send_data[0] != recv_data[0]) || (send_data[1] != recv_data[1]) ) {
            printf("Error during external32 pack/unack for contiguous types (MPI_INT32_T)\n");
            exit(-1);
        }
    }
    /* Simple contiguous data: MPI_INT16_T */
    {
        int16_t send_data[2] = {1234, 5678};
        int16_t recv_data[2] = {-1, -1};

        if( verbose ) {
            printf("send data %08x %08x \n", send_data[0], send_data[1]);
            printf("data "); dump_hex(&send_data, sizeof(int16_t) * 2); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, MPI_INT16_T, 2,
                                    recv_data, check_contiguous, (void*)(ptrdiff_t)MPI_INT16_T );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int16_t) * 2); printf("\n");
            printf("recv data %08x %08x \n", recv_data[0], recv_data[1]);
        }
        if( (send_data[0] != recv_data[0]) || (send_data[1] != recv_data[1]) ) {
            printf("Error during external32 pack/unack for contiguous types (MPI_INT16_T)\n");
            exit(-1);
        }
    }

    /* Vector datatype */
    printf("\n\nVector datatype\n\n");
    {
        int32_t send_data[3] = {1234, 0, 5678};
        int32_t recv_data[3] = {-1, -1, -1};
        MPI_Datatype ddt;

        MPI_Type_vector(2, 1, 2, MPI_INT32_T, &ddt);
        MPI_Type_commit(&ddt);
        if( verbose ) {
            printf("send data %08x %x08x %08x \n", send_data[0], send_data[1], send_data[2]);
            printf("data "); dump_hex(&send_data, sizeof(int32_t) * 3); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, ddt, 1, recv_data, check_vector, (void*)(ptrdiff_t)MPI_INT32_T );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int32_t) * 3); printf("\n");
            printf("recv data %08x %08x %08x \n", recv_data[0], recv_data[1], recv_data[2]);
        }
        MPI_Type_free(&ddt);
        if( (send_data[0] != recv_data[0]) || (send_data[2] != recv_data[2]) ) {
            printf("Error duing external32 pack/unack for vector types (MPI_INT32_T)\n");
            exit(-1);
        }
    }
    {
        int16_t send_data[3] = {1234, 0, 5678};
        int16_t recv_data[3] = {-1, -1, -1};
        MPI_Datatype ddt;

        MPI_Type_vector(2, 1, 2, MPI_INT16_T, &ddt);
        MPI_Type_commit(&ddt);
        if( verbose ) {
            printf("send data %08x %x08x %08x \n", send_data[0], send_data[1], send_data[2]);
            printf("data "); dump_hex(&send_data, sizeof(int16_t) * 3); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, ddt, 1, recv_data, check_vector, (void*)(ptrdiff_t)MPI_INT16_T );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int16_t) * 3); printf("\n");
            printf("recv data %08x %08x %08x \n", recv_data[0], recv_data[1], recv_data[2]);
        }
        MPI_Type_free(&ddt);
        if( (send_data[0] != recv_data[0]) || (send_data[2] != recv_data[2]) ) {
            printf("Error duing external32 pack/unack for vector types (MPI_INT16_T)\n");
            exit(-1);
        }
    }

    MPI_Finalize();

    return 0;
}
