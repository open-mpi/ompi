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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "ompi/mpi/c/bindings.h"
#include "opal/util/strncpy.h"
#include "ompi/totalview.h"
#include "opal/threads/mutex.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_get_name = PMPI_Comm_get_name
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_get_name";


int MPI_Comm_get_name(MPI_Comm comm, char *name, int *length)  
{

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_comm_invalid ( comm ) )
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM, 
                                            FUNC_NAME);

        if ( NULL == name || NULL == length ) 
            return OMPI_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                            FUNC_NAME);
    }
#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_LOCK(&(comm->c_lock));
#endif
    if ( comm->c_flags & OMPI_COMM_NAMEISSET ) {
        strncpy ( name, comm->c_name, MPI_MAX_OBJECT_NAME );
        *length = strlen ( comm->c_name );
    }
    else {
        memset ( name, 0, MPI_MAX_OBJECT_NAME );
        *length = 0;
    }
#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_UNLOCK(&(comm->c_lock));
#endif

    return MPI_SUCCESS;
}
