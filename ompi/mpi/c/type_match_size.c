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

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_match_size = PMPI_Type_match_size
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_match_size";


int MPI_Type_match_size(int typeclass, int size, MPI_Datatype *type)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    switch( typeclass ) {
    case MPI_TYPECLASS_REAL:
        *type = (MPI_Datatype)ompi_ddt_match_size( size, DT_FLAG_DATA_FLOAT, DT_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_INTEGER:
        *type = (MPI_Datatype)ompi_ddt_match_size( size, DT_FLAG_DATA_INT, DT_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_COMPLEX:
        *type = (MPI_Datatype)ompi_ddt_match_size( size, DT_FLAG_DATA_COMPLEX, DT_FLAG_DATA_FORTRAN );
        break;
    default:
        *type = &ompi_mpi_datatype_null;
    }
    if( *type != &ompi_mpi_datatype_null ) 
        return MPI_SUCCESS;

    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
