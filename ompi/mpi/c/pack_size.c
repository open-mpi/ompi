/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "opal/class/opal_object.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "datatype/convertor.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Pack_size = PMPI_Pack_size
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Pack_size";

int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
                  int *size) 
{
    int ret;
    ompi_convertor_t local_convertor;
    size_t length;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_COMM_NULL == comm) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        } else if (NULL == size) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
        } else if (MPI_DATATYPE_NULL == datatype) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_TYPE, FUNC_NAME);
        }
    }

    /* the resulting convertor will be set to the position ZERO */
    ompi_convertor_copy_and_prepare_for_send( NULL, datatype, incount, NULL, &local_convertor );

    ret = ompi_convertor_get_packed_size( &local_convertor, &length );
    *size = (int)length;
    OBJ_DESTRUCT( &local_convertor );

    OMPI_ERRHANDLER_RETURN(ret, comm, MPI_ERR_UNKNOWN, FUNC_NAME);
}
