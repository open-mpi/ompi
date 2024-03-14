/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All right
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/topo.h"

static int mca_topo_base_cart_allocate (ompi_group_t *group, int ndims, const int *dims, const int *periods,
                                        int *my_rank, int *num_procs, mca_topo_base_comm_cart_2_2_0_t **cart_out)
{
    mca_topo_base_comm_cart_2_2_0_t *cart = OBJ_NEW(mca_topo_base_comm_cart_2_2_0_t);
    if (OPAL_UNLIKELY(NULL == cart)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    int nprocs = 1;

    *num_procs = group->grp_proc_count;
    *my_rank = group->grp_my_rank;

    /* Calculate the number of processes in this grid */
    for (int i = 0 ; i < ndims ; ++i) {
        if (dims[i] <= 0) {
            OBJ_RELEASE(cart);
            return OMPI_ERROR;
        }
        nprocs *= dims[i];
    }

    /* check for the error condition */
    if (OPAL_UNLIKELY(*num_procs < nprocs)) {
        OBJ_RELEASE(cart);
        return MPI_ERR_DIMS;
    }

    /* check if we have to trim the list of processes */
    if (nprocs < *num_procs) {
        *num_procs = nprocs;
    }

    if (*my_rank > (nprocs - 1)) {
        *my_rank = MPI_UNDEFINED;
    }

    if (MPI_UNDEFINED == *my_rank) {
        /* nothing more to do */
        OBJ_RELEASE(cart);
        *cart_out = NULL;
        return OMPI_SUCCESS;
    }

    cart->ndims = ndims;

    /* MPI-2.1 allows 0-dimension cartesian communicators, so prevent
       a 0-byte malloc -- leave dims as NULL */
    if (0 == ndims) {
        *cart_out = cart;
        return OMPI_SUCCESS;
    }

    cart->dims = (int *) malloc (sizeof (int) * ndims);
    cart->periods = (int *) malloc (sizeof (int) * ndims);
    cart->coords = (int *) malloc (sizeof (int) * ndims);
    if (OPAL_UNLIKELY(NULL == cart->dims || NULL == cart->periods || NULL == cart->coords)) {
        OBJ_RELEASE(cart);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Cartesian communicator; copy the right data to the common information */
    memcpy(cart->dims, dims, ndims * sizeof(int));
    memcpy(cart->periods, periods, ndims * sizeof(int));

    nprocs = *num_procs;
    for (int i = 0, rank = *my_rank ; i < ndims ; ++i) {
        nprocs /= cart->dims[i];
        cart->coords[i] = rank / nprocs;
        rank %= nprocs;
    }

    *cart_out = cart;
    return OMPI_SUCCESS;
}

/*
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm input communicator (handle)
 * @param ndims number of dimensions of cartesian grid (integer)
 * @param dims integer array of size ndims specifying the number of processes in
 *             each dimension
 * @param periods logical array of size ndims specifying whether the grid is
 *                periodic (true) or not (false) in each dimension
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_cart communicator with new cartesian topology (handle)
 *
 * Open MPI currently ignores the 'reorder' flag.
 *
 * @retval OMPI_SUCCESS
 */

int mca_topo_base_cart_create(mca_topo_base_module_t *topo,
                              ompi_communicator_t* old_comm,
                              int ndims,
                              const int *dims,
                              const int *periods,
                              bool reorder,
                              ompi_communicator_t** comm_topo)
{
    int new_rank, num_procs, ret;
    ompi_communicator_t *new_comm;
    mca_topo_base_comm_cart_2_2_0_t* cart;
    ompi_group_t *c_local_group;

    assert(topo->type == OMPI_COMM_CART);

    ret = mca_topo_base_cart_allocate (old_comm->c_local_group, ndims, dims, periods,
                                       &new_rank, &num_procs, &cart);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return ret;
    }

    /* Copy the proc structure from the previous communicator over to
       the new one.  The topology module is then able to work on this
       copy and rearrange it as it deems fit. NTH: seems odd that this
       function has always clipped the group size here. It might be
       worthwhile to clip the group in the module (if reordering) */
    c_local_group = ompi_group_flatten (old_comm->c_local_group, num_procs);
    if (OPAL_UNLIKELY(NULL == c_local_group)) {
        OBJ_RELEASE(cart);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = ompi_comm_create (old_comm, c_local_group, &new_comm);

    ompi_group_free (&c_local_group);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_RELEASE(cart);
        return ret;
    }

    *comm_topo = new_comm;

    if (MPI_COMM_NULL == new_comm) {
        /* not part of this new communicator */
        return OMPI_SUCCESS;
    }

    new_comm->c_topo           = topo;
    new_comm->c_topo->mtc.cart = cart;
    new_comm->c_topo->reorder  = reorder;
    new_comm->c_flags         |= OMPI_COMM_CART;

    /* end here */
    return OMPI_SUCCESS;
}

static void mca_topo_base_comm_cart_2_2_0_construct(mca_topo_base_comm_cart_2_2_0_t * cart) {
    cart->ndims = 0;
    cart->dims = NULL;
    cart->periods = NULL;
    cart->coords = NULL;
}

static void mca_topo_base_comm_cart_2_2_0_destruct(mca_topo_base_comm_cart_2_2_0_t * cart) {
    free(cart->dims);
    free(cart->periods);
    free(cart->coords);
}

OBJ_CLASS_INSTANCE(mca_topo_base_comm_cart_2_2_0_t, opal_object_t,
                   mca_topo_base_comm_cart_2_2_0_construct,
                   mca_topo_base_comm_cart_2_2_0_destruct);
