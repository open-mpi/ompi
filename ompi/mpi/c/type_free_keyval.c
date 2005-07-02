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

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_free_keyval = PMPI_Type_free_keyval
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_free_keyval";


int MPI_Type_free_keyval(int *type_keyval)
{
    int ret;

    /* Check for valid key pointer */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if (NULL == type_keyval) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
					 MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }

    ret = ompi_attr_free_keyval(TYPE_ATTR, type_keyval, 0);

    OMPI_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD,
			  MPI_ERR_OTHER, FUNC_NAME);
}
