/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"

int ADIO_Type_create_subarray(int ndims,
                              int *array_of_sizes,
                              int *array_of_subsizes,
                              int *array_of_starts,
                              int order, MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    MPI_Aint lb, ub, extent, disps[1], size;
    int i, blklens[3];
    MPI_Datatype tmp1, tmp2, types[3];

    MPI_Type_get_extent(oldtype, &lb, &extent);

    if (order == MPI_ORDER_FORTRAN) {
        /* dimension 0 changes fastest */
        if (ndims == 1) {
            MPI_Type_contiguous(array_of_subsizes[0], oldtype, &tmp1);
        } else {
            MPI_Type_vector(array_of_subsizes[1],
                            array_of_subsizes[0], array_of_sizes[0], oldtype, &tmp1);

            size = (MPI_Aint) array_of_sizes[0] * extent;
            for (i = 2; i < ndims; i++) {
                size *= (MPI_Aint) array_of_sizes[i - 1];
                MPI_Type_create_hvector(array_of_subsizes[i], 1, size, tmp1, &tmp2);
                MPI_Type_free(&tmp1);
                tmp1 = tmp2;
            }
        }

        /* add displacement and UB */
        disps[0] = array_of_starts[0];
        size = 1;
        for (i = 1; i < ndims; i++) {
            size *= (MPI_Aint) array_of_sizes[i - 1];
            disps[0] += size * (MPI_Aint) array_of_starts[i];
        }
        /* rest done below for both Fortran and C order */
    }

    else {      /* order == MPI_ORDER_C */

        /* dimension ndims-1 changes fastest */
        if (ndims == 1) {
            MPI_Type_contiguous(array_of_subsizes[0], oldtype, &tmp1);
        } else {
            MPI_Type_vector(array_of_subsizes[ndims - 2],
                            array_of_subsizes[ndims - 1],
                            array_of_sizes[ndims - 1], oldtype, &tmp1);

            size = (MPI_Aint) array_of_sizes[ndims - 1] * extent;
            for (i = ndims - 3; i >= 0; i--) {
                size *= (MPI_Aint) array_of_sizes[i + 1];
                MPI_Type_create_hvector(array_of_subsizes[i], 1, size, tmp1, &tmp2);
                MPI_Type_free(&tmp1);
                tmp1 = tmp2;
            }
        }

        /* add displacement and UB */
        disps[0] = array_of_starts[ndims - 1];
        size = 1;
        for (i = ndims - 2; i >= 0; i--) {
            size *= (MPI_Aint) array_of_sizes[i + 1];
            disps[0] += size * (MPI_Aint) array_of_starts[i];
        }
    }

    disps[0] *= extent;

    ub = extent;
    for (i = 0; i < ndims; i++)
        ub *= (MPI_Aint) array_of_sizes[i];

    lb = 0;
    blklens[0] = 1;
    types[0] = tmp1;

    MPI_Type_create_struct(1, blklens, disps, types, &tmp2);
    MPI_Type_create_resized(tmp2, lb, ub, newtype);

    MPI_Type_free(&tmp1);
    MPI_Type_free(&tmp2);

    return MPI_SUCCESS;
}
