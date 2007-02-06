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
#pragma weak MPI_Type_create_subarray = PMPI_Type_create_subarray
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_subarray";


int MPI_Type_create_subarray(int ndims,
                             int size_array[],
                             int subsize_array[],
                             int start_array[],
                             int order,
                             MPI_Datatype oldtype,
                             MPI_Datatype *newtype)

{
    MPI_Datatype last_type; 
    int32_t i, step, start_loop, end_loop;
    MPI_Aint size, displ, extent;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if( ndims < 0 ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
        } else if( (NULL == size_array) || (NULL == subsize_array) || (NULL == start_array) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if( (NULL == oldtype) || (MPI_DATATYPE_NULL == oldtype) || (NULL == newtype) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        } else if( (MPI_ORDER_C != order) && (MPI_ORDER_FORTRAN != order) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
        for( i = 0; i < ndims; i++ ) {
            if( (subsize_array[i] < 1) || (subsize_array[i] > size_array[i]) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( (start_array[i] < 0) || (start_array[i] > (size_array[i] - subsize_array[i])) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } 
        }
    }

    ompi_ddt_type_extent( oldtype, &extent );

    /* If the ndims is zero then return the NULL datatype */
    if( ndims < 2 ) {
        if( 0 == ndims ) {
            *newtype = &ompi_mpi_datatype_null;
            return MPI_SUCCESS;
        }
        ompi_ddt_create_contiguous( subsize_array[0], oldtype, &last_type );
        size = size_array[0];
        displ = start_array[0];
        goto replace_subarray_type;
    }

    if( MPI_ORDER_C == order ) {
        start_loop = i = ndims - 1;
        step = -1;
        end_loop = -1;
        if( end_loop > (start_loop + 2 * step) )
            end_loop = start_loop + 2 * step;
    } else {
        start_loop = i = 0;
        step = 1;
        end_loop = ndims;
        if( end_loop < (start_loop + 2 * step) )
            end_loop = start_loop + 2 * step;
    }

    /* As we know that the ndims is at least 1 we can start by creating the first dimension data
     * outside the loop, such that we dont have to create a duplicate of the oldtype just to be able
     * to free it.
     */
    ompi_ddt_create_vector( subsize_array[i+step], subsize_array[i], size_array[i],
                            oldtype, newtype );

    last_type = *newtype;
    size = size_array[i] * size_array[i+step];
    displ = start_array[i] + start_array[i+step] * size_array[i];
    for( i += 2 * step; i != end_loop; i += step ) {
        ompi_ddt_create_hvector( subsize_array[i], 1, size * extent,
                                 last_type, newtype );
        ompi_ddt_destroy( &last_type );
        displ += size * start_array[i];
        size *= size_array[i];
        last_type = *newtype;
    }

  replace_subarray_type:    
    /**
     * We cannot use resized here. Resized will only set the soft lb and ub markers
     * without moving the real data inside. What we need is to force the displacement
     * of the data create upward to the right position AND set the LB and UB. A type
     * struct is the function we need.
     */
    {
        MPI_Aint displs[3];
        MPI_Datatype types[3];
        int blength[3] = { 1, 1, 1 };

        displs[0] = 0; displs[1] = displ * extent; displs[2] = size * extent;
        types[0] = MPI_LB; types[1] = last_type; types[2] = MPI_UB;
        ompi_ddt_create_struct( 3, blength, displs, types, newtype );
    }
    ompi_ddt_destroy( &last_type );
    
    {
        int* a_i[5];

        a_i[0] = &ndims;
        a_i[1] = size_array;
        a_i[2] = subsize_array;
        a_i[3] = start_array;
        a_i[4] = &order;

        ompi_ddt_set_args( *newtype, 3 * ndims + 2, a_i, 0, NULL, 1, &oldtype,
                           MPI_COMBINER_SUBARRAY );
    }

    return MPI_SUCCESS;
}
