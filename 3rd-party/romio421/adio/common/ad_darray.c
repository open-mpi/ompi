/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"

static int MPIOI_Type_block(int *array_of_gsizes, int dim, int ndims, int nprocs,
                            int rank, int darg, int order, MPI_Aint orig_extent,
                            MPI_Datatype type_old, MPI_Datatype * type_new, MPI_Aint * st_offset);
static int MPIOI_Type_cyclic(int *array_of_gsizes, int dim, int ndims, int nprocs,
                             int rank, int darg, int order, MPI_Aint orig_extent,
                             MPI_Datatype type_old, MPI_Datatype * type_new, MPI_Aint * st_offset);


int ADIO_Type_create_darray(int size, int rank, int ndims,
                            int *array_of_gsizes, int *array_of_distribs,
                            int *array_of_dargs, int *array_of_psizes,
                            int order, MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    MPI_Datatype type_old, type_new = MPI_DATATYPE_NULL, types[1];
    int procs, tmp_rank, i, tmp_size, blklens[1], *coords;
    MPI_Aint *st_offsets, lb, ub, orig_extent, disps[1];

    MPI_Type_get_extent(oldtype, &lb, &orig_extent);

/* calculate position in Cartesian grid as MPI would (row-major
   ordering) */
    coords = (int *) ADIOI_Malloc(ndims * sizeof(int));
    procs = size;
    tmp_rank = rank;
    for (i = 0; i < ndims; i++) {
        procs = procs / array_of_psizes[i];
        coords[i] = tmp_rank / procs;
        tmp_rank = tmp_rank % procs;
    }

    st_offsets = (MPI_Aint *) ADIOI_Malloc(ndims * sizeof(MPI_Aint));
    type_old = oldtype;

    if (order == MPI_ORDER_FORTRAN) {
        /* dimension 0 changes fastest */
        for (i = 0; i < ndims; i++) {
            switch (array_of_distribs[i]) {
                case MPI_DISTRIBUTE_BLOCK:
                    MPIOI_Type_block(array_of_gsizes, i, ndims,
                                     array_of_psizes[i],
                                     coords[i], array_of_dargs[i],
                                     order, orig_extent, type_old, &type_new, st_offsets + i);
                    break;
                case MPI_DISTRIBUTE_CYCLIC:
                    MPIOI_Type_cyclic(array_of_gsizes, i, ndims,
                                      array_of_psizes[i], coords[i],
                                      array_of_dargs[i], order,
                                      orig_extent, type_old, &type_new, st_offsets + i);
                    break;
                case MPI_DISTRIBUTE_NONE:
                    /* treat it as a block distribution on 1 process */
                    MPIOI_Type_block(array_of_gsizes, i, ndims, 1, 0,
                                     MPI_DISTRIBUTE_DFLT_DARG, order,
                                     orig_extent, type_old, &type_new, st_offsets + i);
                    break;
            }
            if (i)
                MPI_Type_free(&type_old);
            type_old = type_new;
        }

        /* add displacement and UB */
        disps[0] = st_offsets[0];
        tmp_size = 1;
        for (i = 1; i < ndims; i++) {
            tmp_size *= array_of_gsizes[i - 1];
            disps[0] += (MPI_Aint) tmp_size *st_offsets[i];
        }
        /* rest done below for both Fortran and C order */
    }

    else {      /* order == MPI_ORDER_C */

        /* dimension ndims-1 changes fastest */
        for (i = ndims - 1; i >= 0; i--) {
            switch (array_of_distribs[i]) {
                case MPI_DISTRIBUTE_BLOCK:
                    MPIOI_Type_block(array_of_gsizes, i, ndims, array_of_psizes[i],
                                     coords[i], array_of_dargs[i], order,
                                     orig_extent, type_old, &type_new, st_offsets + i);
                    break;
                case MPI_DISTRIBUTE_CYCLIC:
                    MPIOI_Type_cyclic(array_of_gsizes, i, ndims,
                                      array_of_psizes[i], coords[i],
                                      array_of_dargs[i], order,
                                      orig_extent, type_old, &type_new, st_offsets + i);
                    break;
                case MPI_DISTRIBUTE_NONE:
                    /* treat it as a block distribution on 1 process */
                    MPIOI_Type_block(array_of_gsizes, i, ndims, array_of_psizes[i],
                                     coords[i], MPI_DISTRIBUTE_DFLT_DARG, order, orig_extent,
                                     type_old, &type_new, st_offsets + i);
                    break;
            }
            if (i != ndims - 1)
                MPI_Type_free(&type_old);
            type_old = type_new;
        }

        /* add displacement and UB */
        disps[0] = st_offsets[ndims - 1];
        tmp_size = 1;
        for (i = ndims - 2; i >= 0; i--) {
            tmp_size *= array_of_gsizes[i + 1];
            disps[0] += (MPI_Aint) tmp_size *st_offsets[i];
        }
    }

    disps[0] *= orig_extent;

    lb = 0;
    ub = orig_extent;
    for (i = 0; i < ndims; i++)
        ub *= (MPI_Aint) array_of_gsizes[i];

    blklens[0] = 1;
    types[0] = type_new;

    MPI_Type_create_struct(1, blklens, disps, types, &type_old);
    MPI_Type_create_resized(type_old, lb, ub, newtype);

    MPI_Type_free(&type_old);
    MPI_Type_free(&type_new);

    ADIOI_Free(st_offsets);
    ADIOI_Free(coords);
    return MPI_SUCCESS;
}


/* Returns MPI_SUCCESS on success, an MPI error code on failure.  Code above
 * needs to call MPIO_Err_return_xxx.
 */
static int MPIOI_Type_block(int *array_of_gsizes, int dim, int ndims, int nprocs,
                            int rank, int darg, int order, MPI_Aint orig_extent,
                            MPI_Datatype type_old, MPI_Datatype * type_new, MPI_Aint * st_offset)
{
/* nprocs = no. of processes in dimension dim of grid
   rank = coordinate of this process in dimension dim */
    int blksize, global_size, mysize, i, j;
    MPI_Aint stride;

    global_size = array_of_gsizes[dim];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG)
        blksize = (global_size + nprocs - 1) / nprocs;
    else {
        blksize = darg;

        /* --BEGIN ERROR HANDLING-- */
        if (blksize <= 0) {
            return MPI_ERR_ARG;
        }

        if (blksize * nprocs < global_size) {
            return MPI_ERR_ARG;
        }
        /* --END ERROR HANDLING-- */
    }

    j = global_size - blksize * rank;
    mysize = MPL_MIN(blksize, j);
    if (mysize < 0)
        mysize = 0;

    stride = orig_extent;
    if (order == MPI_ORDER_FORTRAN) {
        if (dim == 0)
            MPI_Type_contiguous(mysize, type_old, type_new);
        else {
            for (i = 0; i < dim; i++)
                stride *= (MPI_Aint) array_of_gsizes[i];
            MPI_Type_create_hvector(mysize, 1, stride, type_old, type_new);
        }
    } else {
        if (dim == ndims - 1)
            MPI_Type_contiguous(mysize, type_old, type_new);
        else {
            for (i = ndims - 1; i > dim; i--)
                stride *= (MPI_Aint) array_of_gsizes[i];
            MPI_Type_create_hvector(mysize, 1, stride, type_old, type_new);
        }

    }

    *st_offset = (MPI_Aint) blksize *(MPI_Aint) rank;
    /* in terms of no. of elements of type oldtype in this dimension */
    if (mysize == 0)
        *st_offset = 0;

    MPI_Aint lb, ex;
    MPI_Type_get_extent(type_old, &lb, &ex);
    MPI_Datatype type_tmp;
    MPI_Type_create_resized(*type_new, 0, array_of_gsizes[dim] * ex, &type_tmp);
    MPI_Type_free(type_new);
    *type_new = type_tmp;

    return MPI_SUCCESS;
}


/* Returns MPI_SUCCESS on success, an MPI error code on failure.  Code above
 * needs to call MPIO_Err_return_xxx.
 */
static int MPIOI_Type_cyclic(int *array_of_gsizes, int dim, int ndims, int nprocs,
                             int rank, int darg, int order, MPI_Aint orig_extent,
                             MPI_Datatype type_old, MPI_Datatype * type_new, MPI_Aint * st_offset)
{
/* nprocs = no. of processes in dimension dim of grid
   rank = coordinate of this process in dimension dim */
    int blksize, i, blklens[3], st_index, end_index, local_size, rem, count;
    MPI_Aint stride, disps[3];
    MPI_Datatype type_tmp, types[3];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG)
        blksize = 1;
    else
        blksize = darg;

    /* --BEGIN ERROR HANDLING-- */
    if (blksize <= 0) {
        return MPI_ERR_ARG;
    }
    /* --END ERROR HANDLING-- */

    st_index = rank * blksize;
    end_index = array_of_gsizes[dim] - 1;

    if (end_index < st_index)
        local_size = 0;
    else {
        local_size = ((end_index - st_index + 1) / (nprocs * blksize)) * blksize;
        rem = (end_index - st_index + 1) % (nprocs * blksize);
        local_size += MPL_MIN(rem, blksize);
    }

    count = local_size / blksize;
    rem = local_size % blksize;

    stride = (MPI_Aint) nprocs *(MPI_Aint) blksize *orig_extent;
    if (order == MPI_ORDER_FORTRAN)
        for (i = 0; i < dim; i++)
            stride *= (MPI_Aint) array_of_gsizes[i];
    else
        for (i = ndims - 1; i > dim; i--)
            stride *= (MPI_Aint) array_of_gsizes[i];

    MPI_Type_create_hvector(count, blksize, stride, type_old, type_new);

    if (rem) {
        /* if the last block is of size less than blksize, include
         * it separately using MPI_Type_create_struct */

        types[0] = *type_new;
        types[1] = type_old;
        disps[0] = 0;
        disps[1] = (MPI_Aint) count *stride;
        blklens[0] = 1;
        blklens[1] = rem;

        MPI_Type_create_struct(2, blklens, disps, types, &type_tmp);

        MPI_Type_free(type_new);
        *type_new = type_tmp;
    }

    /* In the first iteration, we need to set the displacement in that
     * dimension correctly. */
    if (((order == MPI_ORDER_FORTRAN) && (dim == 0)) ||
        ((order == MPI_ORDER_C) && (dim == ndims - 1))) {
        MPI_Datatype tmp;
        MPI_Aint lb, ub;
        types[0] = *type_new;
        disps[0] = (MPI_Aint) rank *(MPI_Aint) blksize *orig_extent;
        lb = 0;
        ub = orig_extent * (MPI_Aint) array_of_gsizes[dim];
        blklens[0] = 1;
        MPI_Type_create_struct(1, blklens, disps, types, &tmp);
        MPI_Type_create_resized(tmp, lb, ub, &type_tmp);
        MPI_Type_free(&tmp);
        MPI_Type_free(type_new);
        *type_new = type_tmp;

        *st_offset = 0; /* set it to 0 because it is taken care of in
                         * the struct above */
    } else {
        *st_offset = (MPI_Aint) rank *(MPI_Aint) blksize;
        /* st_offset is in terms of no. of elements of type oldtype in
         * this dimension */
    }

    if (local_size == 0)
        *st_offset = 0;

    MPI_Aint lb, ex;
    MPI_Type_get_extent(type_old, &lb, &ex);
    MPI_Type_create_resized(*type_new, 0, array_of_gsizes[dim] * ex, &type_tmp);
    MPI_Type_free(type_new);
    *type_new = type_tmp;

    return MPI_SUCCESS;
}
