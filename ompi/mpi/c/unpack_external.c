/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Unpack_external = PMPI_Unpack_external
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Unpack_external ";


int MPI_Unpack_external (char *datarep, void *inbuf, MPI_Aint insize,
                         MPI_Aint *position, void *outbuf, int outcount,
                         MPI_Datatype datatype) 
{
    int rc;
    ompi_convertor_t local_convertor;
    struct iovec outvec;
    unsigned int iov_count;
    size_t size;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((NULL == inbuf) || (NULL == position)) {  /* outbuf can be MPI_BOTTOM */
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if (outcount < 0) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
        } else if (MPI_DATATYPE_NULL == datatype) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        }
    }

    OBJ_CONSTRUCT(&local_convertor, ompi_convertor_t);

    /* the resulting convertor will be set to the position ZERO */
    ompi_convertor_copy_and_prepare_for_recv( ompi_mpi_external32_convertor,
                                              datatype, outcount, outbuf, 0, &local_convertor );

    /* Check for truncation */
    ompi_convertor_get_packed_size( &local_convertor, &size );
    if( (*position + size) > (unsigned int)insize ) {
        OBJ_DESTRUCT( &local_convertor );
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TRUNCATE, FUNC_NAME);
    }

    /* Prepare the iovec with all informations */
    outvec.iov_base = (char*) inbuf + (*position);
    outvec.iov_len = size;

    /* Do the actual unpacking */
    iov_count = 1;
    rc = ompi_convertor_unpack( &local_convertor, &outvec, &iov_count, &size );
    *position += size;
    OBJ_DESTRUCT( &local_convertor );

    /* All done.  Note that the convertor returns 1 upon success, not
       OMPI_SUCCESS. */
    OMPI_ERRHANDLER_RETURN((rc == 1) ? OMPI_SUCCESS : OMPI_ERROR,
                           MPI_COMM_WORLD, MPI_ERR_UNKNOWN, FUNC_NAME);
}
