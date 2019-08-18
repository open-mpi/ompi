/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2014      Argonne National Laboratory.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ts.h"

#include <assert.h>

int ompi_ts_create_datatype(CFI_cdesc_t *cdesc, int oldcount, MPI_Datatype oldtype, MPI_Datatype *newtype)
{
    const int MAX_RANK = 15; /* Fortran 2008 specifies a maximum rank of 15 */
    MPI_Datatype types[MAX_RANK + 1]; /* Use a fixed size array to avoid malloc. + 1 for oldtype */
    int mpi_errno = MPI_SUCCESS;
    int accum_elems = 1;
    int accum_sm = cdesc->elem_len;
    int done = 0; /* Have we created a datatype for oldcount of oldtype? */
    int last; /* Index of the last successfully created datatype in types[] */
    int extent;
    int i, j;

#ifdef OPAL_ENABLE_DEBUG
    {
        size_t size;
        assert(cdesc->rank <= MAX_RANK);
        ompi_datatype_type_size(oldtype, &size);
        /* When cdesc->elem_len != size, things suddenly become complicated. Generally, it is hard to create
         * a composite datatype based on two datatypes. Currently we don't support it and doubt it is usefull.
         */
        assert(cdesc->elem_len == size);
    }
#endif

    types[0] = oldtype;
    i = 0;
    done = 0;
    while (i < cdesc->rank && !done) {
        if (oldcount % accum_elems) {
            /* oldcount should be a multiple of accum_elems, otherwise we might need an
             * MPI indexed datatype to describle the irregular region, which is not supported yet.
             */
            mpi_errno = MPI_ERR_INTERN;
            last = i;
            goto fn_exit;
        }

        extent = oldcount / accum_elems;
        if (extent > cdesc->dim[i].extent) {
            extent = cdesc->dim[i].extent;
        } else {
            /* Up to now, we have accumlated enough elements */
            done = 1;
        }

        if (cdesc->dim[i].sm == accum_sm) {
            mpi_errno = PMPI_Type_contiguous(extent, types[i], &types[i+1]);
        } else {
            mpi_errno = PMPI_Type_create_hvector(extent, 1, cdesc->dim[i].sm, types[i], &types[i+1]);
        }
        if (mpi_errno != MPI_SUCCESS) {
            last = i;
            goto fn_exit;
        }

        accum_sm = cdesc->dim[i].sm * cdesc->dim[i].extent;
        accum_elems  *= cdesc->dim[i].extent;
        i++;
    }

    if (done) {
        *newtype = types[i];
        MPI_Type_commit(newtype);
        last = i - 1; /* To avoid freeing newtype */
    } else {
        /* If # of elements given by "oldcount oldtype" is bigger than
         * what cdesc describles, then we will reach here.
         */
        last = i;
        mpi_errno = MPI_ERR_ARG;
        goto fn_exit;
    }

fn_exit:
    for (j = 1; j <= last; j++)
        PMPI_Type_free(&types[j]);
    return mpi_errno;
}

static void copy(CFI_dim_t *dim, int rank, char * base, char **dest, size_t len) {
    for (size_t i=0; i<dim->extent; i++) {
        if (rank > 1) {
            copy(dim-1, rank-1, base, dest, len);
        } else {
            int v;
            memcpy(*dest, base, len);
            *dest += len;
        }
        base += dim->sm;
    }
}

int ompi_ts_copy(CFI_cdesc_t *cdesc, char *buffer) {
    copy(&cdesc->dim[cdesc->rank - 1], cdesc->rank, cdesc->base_addr, &buffer, cdesc->elem_len);
    return OMPI_SUCCESS;
}

static void copy_back(CFI_dim_t *dim, int rank, char * base, char **source, size_t len) {
    for (size_t i=0; i<dim->extent; i++) {
        if (rank > 1) {
            copy_back(dim-1, rank-1, base, source, len);
        } else {
            int v;
            memcpy(base, *source, len);
            *source += len;
        }
        base += dim->sm;
    }
}

int ompi_ts_copy_back(char *buffer, CFI_cdesc_t *cdesc) {
    copy_back(&cdesc->dim[cdesc->rank - 1], cdesc->rank, cdesc->base_addr, &buffer, cdesc->elem_len);
    return OMPI_SUCCESS;
}

size_t ompi_ts_size(CFI_cdesc_t *cdesc) {
    size_t res = cdesc->elem_len;
    for (int i=0; i<cdesc->rank; i++) {
         res *= cdesc->dim[i].extent;
    }
    return res;
}
