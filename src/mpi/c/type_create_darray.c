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
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_darray = PMPI_Type_create_darray
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_darray";


int MPI_Type_create_darray(int size,
                           int rank,
                           int ndims,
                           int gsize_array[],
                           int distrib_array[],
                           int darg_array[],
                           int psize_array[],
                           int order,
                           MPI_Datatype oldtype,
                           MPI_Datatype *newtype)

{
    int32_t i;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if( (rank < 0) || (size < 0) || (rank >= size) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if( ndims < 0 ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
        } else if( (NULL == gsize_array) || (NULL == distrib_array) || (NULL == darg_array) || (NULL == psize_array)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if (NULL == newtype) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        } else if( !(DT_FLAG_DATA & oldtype ->flags) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        } else if( (MPI_ORDER_C != order) && (MPI_ORDER_FORTRAN != order) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
        for( i = 0; i < ndims; i++ ) {
            if( (MPI_DISTRIBUTE_BLOCK != distrib_array[i]) && (MPI_DISTRIBUTE_CYCLIC != distrib_array[i]) &&
                (MPI_DISTRIBUTE_NONE != distrib_array[i]) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( (gsize_array[i] < 1) || (darg_array[i] < 0) || (psize_array[i] < 0) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( (MPI_DISTRIBUTE_DFLT_DARG != darg_array[i]) && (MPI_DISTRIBUTE_BLOCK == distrib_array[i]) &&
                       ((darg_array[i] * psize_array[i]) < gsize_array[i]) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            }
        }
    }
    if( ndims < 1 ) {
        *newtype = &ompi_mpi_datatype_null;
        return MPI_SUCCESS;
    }

    /* This function is not yet implemented */

    {
        int* a_i[8];

        a_i[0] = &size;
        a_i[1] = &rank;
        a_i[2] = &ndims;
        a_i[3] = gsize_array;
        a_i[4] = distrib_array;
        a_i[5] = darg_array;
        a_i[6] = psize_array;
        a_i[7] = &order;

        ompi_ddt_set_args( *newtype, 4 * ndims + 4, a_i, 0, NULL, 1, &oldtype,
                           MPI_COMBINER_DARRAY );
    }

    return MPI_SUCCESS;
}
