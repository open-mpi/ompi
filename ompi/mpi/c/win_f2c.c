/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi/win/win.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/mpi/f77/fint_2_int.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_f2c = PMPI_Win_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_f2c";


MPI_Win MPI_Win_f2c(MPI_Fint win) 
{
    int o_index= OMPI_FINT_2_INT(win);

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle.  If we get an invalid fortran handle,
       return an invalid C handle. */

    if ( 0 > o_index ||
         o_index >= ompi_pointer_array_get_size(&ompi_mpi_windows)) {
        return NULL;
    }
        
    return (MPI_Win)ompi_pointer_array_get_item(&ompi_mpi_windows, o_index);
}
