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
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#include <float.h>

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_complex = PMPI_Type_create_f90_complex
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_f90_complex";


int MPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if( 0 > p ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if( 0 > r ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    /* if the user does not care about p or r set them to 0 so the test associate with them will
     * always succeed.
     */
    if( MPI_UNDEFINED == p ) p = 0;
    if( MPI_UNDEFINED == r ) r = 0;

    if( (LDBL_DIG < p) || (LDBL_MAX_10_EXP < r) )    *newtype = &ompi_mpi_datatype_null;
    else if( (DBL_DIG < p) || (DBL_MAX_10_EXP < r) ) *newtype = &ompi_mpi_ldblcplex;
    else if( (FLT_DIG < p) || (FLT_MAX_10_EXP < r) ) *newtype = &ompi_mpi_dblcplex;
    else                                             *newtype = &ompi_mpi_cplex;

    if( *newtype != &ompi_mpi_datatype_null )
        return MPI_SUCCESS;

    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}
