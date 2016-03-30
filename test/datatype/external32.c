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

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/runtime/opal.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include <arpa/inet.h>

static int verbose = 0;
 
typedef int (*checker_t)(void*, void*, ompi_datatype_t *, int, void*);

int check_contiguous( void* send_buffer, void* packed,
                      ompi_datatype_t * datatype, int count, void* arg );

int check_vector( void* send_buffer, void* packed,
                  ompi_datatype_t * datatype, int count, void* arg );

static int pack_unpack_datatype( void* send_data, ompi_datatype_t *datatype, int count,
                                 void* recv_data, checker_t validator, void *validator_arg );

static void dump_hex(void* what, size_t length);

static void dump_hex(void* what, size_t length)
{
    size_t i;
    for( i = 0; i < length; i++ ) {
        printf("%02x", (unsigned int)(((unsigned char*)what)[i]));
    }
}

int check_contiguous( void* send_buffer, void* packed,
                      ompi_datatype_t* datatype, int count, void* arg )
{
    int i;

    if( (datatype == &ompi_mpi_int.dt) || (datatype == &ompi_mpi_int32_t.dt) ) {
        uint32_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htonl(((uint32_t*)send_buffer)[i]);
            if( val != ((uint32_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint32_t*)packed)[i], ((uint32_t*)send_buffer)[i], datatype->name);
                return -1;
            }
        }
    } else if( (datatype == &ompi_mpi_short.dt) || (datatype == &ompi_mpi_int16_t.dt) ) {
        uint16_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htons(((uint16_t*)send_buffer)[i]);
            if( val != ((uint16_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint16_t*)packed)[i], ((uint16_t*)send_buffer)[i], datatype->name);
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
                  ompi_datatype_t* datatype, int count, void* arg )
{
    int i;
    ompi_datatype_t *origtype = (ompi_datatype_t *)arg;

    if( (origtype == &ompi_mpi_int.dt) || (origtype == &ompi_mpi_int32_t.dt) ) {
        uint32_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htonl(((uint32_t*)send_buffer)[2*i]);
            if( val != ((uint32_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint32_t*)packed)[i], ((uint32_t*)send_buffer)[2*i], datatype->name);
                return -1;
            }
        }
    } else if( (origtype == &ompi_mpi_short.dt) || (origtype == &ompi_mpi_int16_t.dt) ) {
        uint16_t val;
        for( i = 0 ; i < count; i++ ) {
            val = htons(((uint16_t*)send_buffer)[2*i]);
            if( val != ((uint16_t*)packed)[i] ) {
                printf("Error at position %d expected %x found %x (type %s)\n",
                       i, ((uint16_t*)packed)[i], ((uint16_t*)send_buffer)[2*i], datatype->name);
                return -1;
            }
        }
    } else {
        printf("Unknown %s type\n", datatype->name);
        return -1;
    }
    return 0;
}

static int pack_unpack_datatype( void* send_data, ompi_datatype_t *datatype, int count,
                                 void* recv_data,
                                 checker_t validator, void* validator_arg)
{
    MPI_Aint position = 0, buffer_size;
    void* buffer;
    int error;

    error = ompi_datatype_pack_external_size("external32",
                                             count, datatype, &buffer_size);
    if( MPI_SUCCESS != error ) goto return_error_code;

    buffer = (void*)malloc(buffer_size);
    if( NULL == buffer ) { error = MPI_ERR_UNKNOWN; goto return_error_code; }

    error = ompi_datatype_pack_external("external32", (void*)send_data, count, datatype,
                                        buffer, buffer_size, &position);
    if( MPI_SUCCESS != error ) goto return_error_code;
    if( 0 != validator(send_data, buffer, datatype, count, validator_arg) ) {
        printf("Error during pack external. Bailing out\n");
        return -1;
    }

    printf("packed %ld bytes into a %ld bytes buffer ", position, buffer_size); dump_hex(buffer, position); printf("\n");

    position = 0;
    error = ompi_datatype_unpack_external("external32", buffer, buffer_size, &position,
                                          recv_data, count, datatype);
    if( MPI_SUCCESS != error ) goto return_error_code;
    free(buffer);

 return_error_code:
    return (error == MPI_SUCCESS ? 0 : -1);
}

int main(int argc, char *argv[])
{
    opal_init_util(&argc, &argv);
    ompi_datatype_init();

    /* Simple contiguous data: MPI_INT32_T */
    {
        int32_t send_data[2] = {1234, 5678};
        int32_t recv_data[2] = {-1, -1};

        if( verbose ) {
            printf("send data %08x %08x \n", send_data[0], send_data[1]);
            printf("data "); dump_hex(&send_data, sizeof(int32_t) * 2); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, &ompi_mpi_int32_t.dt, 2,
                                    recv_data, check_contiguous, (void*)&ompi_mpi_int32_t.dt );
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
        (void)pack_unpack_datatype( send_data, &ompi_mpi_int16_t.dt, 2,
                                    recv_data, check_contiguous, (void*)&ompi_mpi_int16_t.dt );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int16_t) * 2); printf("\n");
            printf("recv data %08x %08x \n", recv_data[0], recv_data[1]);
        }
        if( (send_data[0] != recv_data[0]) || (send_data[1] != recv_data[1]) ) {
            printf("Error during external32 pack/unack for contiguous types\n");
            exit(-1);
        }
    }

    /* Vector datatype */
    printf("\n\nVector datatype\n\n");
    {
        int count=2, blocklength=1, stride=2;
        int send_data[3] = {1234, 0, 5678};
        int recv_data[3] = {-1, -1, -1};
        ompi_datatype_t *ddt;

        ompi_datatype_create_vector ( count, blocklength, stride, &ompi_mpi_int.dt, &ddt );
        {
            const int* a_i[3] = {&count, &blocklength, &stride};
            ompi_datatype_t *type = &ompi_mpi_int.dt;

            ompi_datatype_set_args( ddt, 3, a_i, 0, NULL, 1, &type, MPI_COMBINER_VECTOR );
        }
        ompi_datatype_commit(&ddt);

        if( verbose ) {
            printf("send data %08x %x08x %08x \n", send_data[0], send_data[1], send_data[2]);
            printf("data "); dump_hex(&send_data, sizeof(int32_t) * 3); printf("\n");
        }
        (void)pack_unpack_datatype( send_data, ddt, 1, recv_data, check_vector, (void*)&ompi_mpi_int32_t.dt );
        if( verbose ) {
            printf("recv "); dump_hex(&recv_data, sizeof(int32_t) * 3); printf("\n");
            printf("recv data %08x %08x %08x \n", recv_data[0], recv_data[1], recv_data[2]);
        }
        ompi_datatype_destroy(&ddt);
        if( (send_data[0] != recv_data[0]) || (send_data[2] != recv_data[2]) ) {
            printf("Error during external32 pack/unack for vector types (MPI_INT32_T)\n");
            exit(-1);
        }
    }

    ompi_datatype_finalize();

    return 0;
}
