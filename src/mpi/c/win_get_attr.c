/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "attribute/attribute.h"
#include "win/win.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_get_attr = PMPI_Win_get_attr
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_get_attr";


int MPI_Win_get_attr(MPI_Win win, int win_keyval,
                     void *attribute_val, int *flag) 
{
    int ret;

    if (MPI_PARAM_CHECK) {
       OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	if ((NULL == attribute_val) || (NULL == flag)) {
	    return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_ARG, 
					 FUNC_NAME);
	}
    }
    
    ret = ompi_attr_get(win->w_keyhash, win_keyval, 
		       attribute_val, flag);
    OMPI_ERRHANDLER_RETURN(ret, win, MPI_ERR_OTHER, FUNC_NAME);  
}
