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
// #include <mpi.h>

static int pack_unpack_datatype( void* send_data, ompi_datatype_t *datatype, int count,
                                 void* recv_data );

static void dump_hex(void* what, size_t length);

static void dump_hex(void* what, size_t length)
{
    size_t i;
    for( i = 0; i < length; i++ ) {
        printf("%02x", (unsigned int)(((unsigned char*)what)[i]));
    }
}

int MPI_Pack_external_size(const char datarep[], int incount,
                           ompi_datatype_t *datatype, MPI_Aint *size)
{
    opal_convertor_t local_convertor;
    size_t length;

    OBJ_CONSTRUCT(&local_convertor, opal_convertor_t);

    /* the resulting convertor will be set to the position ZERO */
    opal_convertor_copy_and_prepare_for_recv( ompi_mpi_external32_convertor,
                                              &(datatype->super), incount, NULL,
                                              CONVERTOR_SEND_CONVERSION,
                                              &local_convertor );

    opal_convertor_get_unpacked_size( &local_convertor, &length );
    *size = (MPI_Aint)length;
    OBJ_DESTRUCT( &local_convertor );

    return OMPI_SUCCESS;
}

int MPI_Pack_external(const char datarep[], const void *inbuf, int incount,
                      ompi_datatype_t *datatype, void *outbuf,
                      MPI_Aint outsize, MPI_Aint *position)
{
    int rc = MPI_SUCCESS;
    opal_convertor_t local_convertor;
    struct iovec invec;
    unsigned int iov_count;
    size_t size;

    OBJ_CONSTRUCT(&local_convertor, opal_convertor_t);

    /* The resulting convertor will be set to the position zero. We have to use
     * CONVERTOR_SEND_CONVERSION in order to force the convertor to do anything
     * more than just packing the data.
     */
    opal_convertor_copy_and_prepare_for_send( ompi_mpi_external32_convertor,
                                              &(datatype->super), incount, (void *) inbuf,
                                              CONVERTOR_SEND_CONVERSION,
                                              &local_convertor );

    /* Check for truncation */
    opal_convertor_get_packed_size( &local_convertor, &size );
    if( (*position + size) > (size_t)outsize ) {  /* we can cast as we already checked for < 0 */
        OBJ_DESTRUCT( &local_convertor );
        return MPI_ERR_TRUNCATE;
    }

    /* Prepare the iovec with all informations */
    invec.iov_base = (char*) outbuf + (*position);
    invec.iov_len = size;

    /* Do the actual packing */
    iov_count = 1;
    rc = opal_convertor_pack( &local_convertor, &invec, &iov_count, &size );
    *position += size;
    OBJ_DESTRUCT( &local_convertor );

    /* All done.  Note that the convertor returns 1 upon success, not
       OMPI_SUCCESS. */
    return (rc == 1) ? OMPI_SUCCESS : OMPI_ERROR;
}

int MPI_Unpack_external (const char datarep[], const void *inbuf, MPI_Aint insize,
                         MPI_Aint *position, void *outbuf, int outcount,
                         ompi_datatype_t *datatype)
{
    int rc = MPI_SUCCESS;
    opal_convertor_t local_convertor;
    struct iovec outvec;
    unsigned int iov_count;
    size_t size;

    OBJ_CONSTRUCT(&local_convertor, opal_convertor_t);

    /* the resulting convertor will be set to the position ZERO */
    opal_convertor_copy_and_prepare_for_recv( ompi_mpi_external32_convertor,
                                              &(datatype->super), outcount, outbuf,
                                              0,
                                              &local_convertor );

    /* Check for truncation */
    opal_convertor_get_packed_size( &local_convertor, &size );
    if( (*position + size) > (unsigned int)insize ) {
        OBJ_DESTRUCT( &local_convertor );
        return MPI_ERR_TRUNCATE;
    }

    /* Prepare the iovec with all informations */
    outvec.iov_base = (char*) inbuf + (*position);
    outvec.iov_len = size;

    /* Do the actual unpacking */
    iov_count = 1;
    rc = opal_convertor_unpack( &local_convertor, &outvec, &iov_count, &size );
    *position += size;
    OBJ_DESTRUCT( &local_convertor );

    /* All done.  Note that the convertor returns 1 upon success, not
       OMPI_SUCCESS. */
    return (rc == 1) ? OMPI_SUCCESS : OMPI_ERROR;
}

static int pack_unpack_datatype( void* send_data, ompi_datatype_t *datatype, int count,
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

    printf("packed %ld bytes into a %ld bytes buffer ", position, buffer_size); dump_hex(buffer, position); printf("\n");

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
    opal_init_util(&argc, &argv);
    ompi_datatype_init();

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

        printf("send data %08x %x08x %08x \n", send_data[0], send_data[1], send_data[2]);
        printf("data "); dump_hex(&send_data, sizeof(int) * 3); printf("\n");

        (void)pack_unpack_datatype( send_data, ddt, 1, recv_data );

        printf("recv "); dump_hex(&recv_data, sizeof(int) * 3); printf("\n");
        printf("recv data %08x %08x %08x \n", recv_data[0], recv_data[1], recv_data[2]);
        ompi_datatype_destroy(&ddt);
        if( (send_data[0] != recv_data[0]) || (send_data[2] != recv_data[2]) ) {
            printf("Error during external32 pack/unack for vector types\n");
            exit(-1);
        }
    }

    ompi_datatype_finalize();

    return 0;
}
