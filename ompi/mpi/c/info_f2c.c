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

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpi/f77/fint_2_int.h"
#include "ompi/info/info.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Info_f2c";


/**
 * Converts the MPI_Fint info into a valid C MPI_Info handle
 *
 * @param info Integer handle to an MPI_INFO object
 * @retval C handle corresponding to MPI_INFO object
 */
MPI_Info MPI_Info_f2c(MPI_Fint info) 
{
    int info_index = OMPI_FINT_2_INT(info);

    /* check the arguments */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle.  If we get an invalid fortran handle,
       return an invalid C handle. */
    
    if (info_index < 0 || 
        info_index >= 
        ompi_pointer_array_get_size(&ompi_info_f_to_c_table)) {
        return NULL;
    }

    return (MPI_Info)ompi_pointer_array_get_item(&ompi_info_f_to_c_table, info_index);
}
