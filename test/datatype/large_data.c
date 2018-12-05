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

static size_t
count_length_via_convertor_raw(MPI_Datatype dtype, int count)
{
    opal_convertor_t* pconv;
    struct iovec iov[MAX_IOVEC];
    uint32_t iov_count = MAX_IOVEC, i;
    size_t length = MAX_CHUNK, packed_iovec = 0, packed = 0;

    pconv = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_send(pconv, (const struct opal_datatype_t *)dtype, 1, NULL);
    while( 0 == opal_convertor_raw(pconv, iov, &iov_count, &length) ) {
        printf("iov_count = %d packed_iovec = %"PRIsize_t"\n", iov_count, packed_iovec);
        packed += length;
        for( i = 0; i < iov_count; i++ ) {
            packed_iovec += iov[i].iov_len;
        }
        if( packed != packed_iovec ) {
            printf( "Packed send amount diverges %"PRIsize_t" != %"PRIsize_t"\n", packed, packed_iovec);
            exit(-1);
        }
        iov_count = MAX_IOVEC;  /* number of available iov */
        length = MAX_CHUNK;
    }
    packed += length;
    for( i = 0; i < iov_count; i++ ) {
        packed_iovec += iov[i].iov_len;
    }
    if( packed != packed_iovec ) {
        printf( "Packed send amount diverges %"PRIsize_t" != %"PRIsize_t"\n", packed, packed_iovec);
        exit(-1);
    }
    return packed_iovec;
}

int main(int argc, char * argv[])
{

    int const per_process = 192;
    int const per_type = 20000000;

    int scounts[2] = {per_process, per_process};
    int sdispls[2] = {3*per_process, 0*per_process};
    int rcounts[2] = {per_process, per_process};
    int rdispls[2] = {1*per_process, 2*per_process};

    MPI_Datatype ddt, stype, rtype;

    opal_init_util(&argc, &argv);
    ompi_datatype_init();

    ompi_datatype_create_contiguous( per_type, MPI_FLOAT, &ddt);
    ompi_datatype_create_indexed(2, scounts, sdispls, ddt, &stype);
    ompi_datatype_commit(&stype);
    ompi_datatype_create_indexed(2, rcounts, rdispls, ddt, &rtype);
    ompi_datatype_commit(&rtype);

    size_t packed = count_length_via_convertor_raw(stype, 1);
    size_t length;
    opal_datatype_type_size(&stype->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }

    packed = count_length_via_convertor_raw(rtype, 1);
    opal_datatype_type_size(&rtype->super, &length);
    if( length != packed ) {
        printf("Mismatched length of packed data to datatype size (%"PRIsize_t" != %"PRIsize_t")\n",
               packed, length);
        exit(-2);
    }

    ompi_datatype_destroy(&stype);
    ompi_datatype_destroy(&rtype);

    return 0;
}
