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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"

#include <float.h>

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_real = PMPI_Type_create_f90_real
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_f90_real";


int MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype)

{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* Note: These functions accept negative integers for the p and r
         * arguments.  This is because for the SELECTED_INT_KIND,
         * negative numbers are equivalent to zero values.  See section
         * 13.14.95 of the Fortran 95 standard. */

        if ((MPI_UNDEFINED == p && MPI_UNDEFINED == r)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    /* if the user does not care about p or r set them to 0 so the
     * test associate with them will always succeed.
     */
    if( MPI_UNDEFINED == p ) p = 0;
    if( MPI_UNDEFINED == r ) r = 0;

    if( (LDBL_DIG < p) || (LDBL_MAX_10_EXP < r) )    *newtype = &ompi_mpi_datatype_null;
    else if( (DBL_DIG < p) || (DBL_MAX_10_EXP < r) ) *newtype = &ompi_mpi_long_double;
    else if( (FLT_DIG < p) || (FLT_MAX_10_EXP < r) ) *newtype = &ompi_mpi_double;
    else                                             *newtype = &ompi_mpi_float;

    if( *newtype != &ompi_mpi_datatype_null ) {
        return MPI_SUCCESS;
    }

    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
