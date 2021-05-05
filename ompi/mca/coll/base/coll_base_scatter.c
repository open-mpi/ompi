/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

/*
 * ompi_coll_base_scatter_intra_binomial
 *
 * Function:  Binomial tree algorithm for scatter
 * Accepts:   Same as MPI_Scatter
 * Returns:   MPI_SUCCESS or error code
 *
 * Time complexity: \alpha\log(p) + \beta*m((p-1)/p),
 *                  where m = scount * comm_size, p = comm_size
 *
 * Memory requirements (per process):
 *   root process (root > 0): scount * comm_size * sdtype_size
 *   non-root, non-leaf process: rcount * comm_size * rdtype_size
 *
 * Examples:
 *   comm_size=8          comm_size=10          comm_size=12
 *         0                    0                     0
 *       / | \             /  / | \               /  / \  \
 *      4  2  1           8  4  2  1            8   4   2  1
 *    / |  |            /  / |  |             / |  / |  |
 *   6  5  3           9  6  5  3            10 9 6  5  3
 *   |                    |                  |    |
 *   7                    7                  11   7
 */
int
ompi_coll_base_scatter_intra_binomial(
    const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
    void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
    int root, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*)module;
    mca_coll_base_comm_t *data = base_module->base_data;
    int line = -1, rank, vrank, size, err, packed_size, curr_count;
    char *ptmp, *tempbuf = NULL;
    size_t max_data, packed_sizet;
    opal_convertor_t convertor;
    ptrdiff_t sextent;
    MPI_Status status;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:scatter_intra_binomial rank %d/%d", rank, size));

    /* Create the binomial tree */
    COLL_BASE_UPDATE_IN_ORDER_BMTREE(comm, base_module, root);
    if (NULL == data->cached_in_order_bmtree) {
        err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
    }
    ompi_coll_tree_t *bmtree = data->cached_in_order_bmtree;

    vrank = (rank - root + size) % size;
    ptmp = (char *)rbuf;  /* by default suppose leaf nodes, just use rbuf */

    if ( vrank % 2 ) {  /* leaves */
        /* recv from parent on leaf nodes */
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_SCATTER, comm, &status));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        return MPI_SUCCESS;

    }
    OBJ_CONSTRUCT( &convertor, opal_convertor_t );
    if (rank == root) {  /* root and non-leafs */
        ompi_datatype_type_extent(sdtype, &sextent);
        ptmp = (char *)sbuf;  /* if root == 0, just use the send buffer */
        if (0 != root) {
            opal_convertor_copy_and_prepare_for_send( ompi_mpi_local_convertor, &(sdtype->super),
                                                      scount * size, sbuf, 0, &convertor );
            opal_convertor_get_packed_size( &convertor, &packed_sizet );
            packed_size = (int)packed_sizet;
            packed_sizet = packed_sizet / size;
            ptmp = tempbuf = (char *)malloc(packed_size);
            if (NULL == tempbuf) {
                err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }
            /* rotate data so they will eventually be in the right place */
            struct iovec iov[1];
            uint32_t iov_size = 1;

            iov[0].iov_base = ptmp + (ptrdiff_t)(size - root) * packed_sizet;
            iov[0].iov_len = max_data = packed_sizet * (ptrdiff_t)root;
            opal_convertor_pack(&convertor, iov, &iov_size, &max_data);
            
            iov[0].iov_base = ptmp;
            iov[0].iov_len = max_data = packed_sizet * (ptrdiff_t)(size - root);
            opal_convertor_pack(&convertor, iov, &iov_size, &max_data);
            OBJ_DESTRUCT(&convertor);

            sdtype = MPI_PACKED;
            sextent = 1;  /* bytes */
            scount = packed_size / size;
        }
        curr_count = scount * size;
    } else {  /* (!(vrank % 2)) */
        opal_convertor_copy_and_prepare_for_send( ompi_mpi_local_convertor, &(rdtype->super),
                                                  rcount, NULL, 0, &convertor );
        opal_convertor_get_packed_size( &convertor, &packed_sizet );
        scount = (int)packed_sizet;

        sdtype = MPI_PACKED;  /* default to MPI_PACKED as the send type */

        /* non-root, non-leaf nodes, allocate temp buffer for recv the most we need is rcount*size/2 (an upper bound) */
        int vparent = (bmtree->tree_prev - root + size) % size;
        int subtree_size = vrank - vparent;
        if (size - vrank < subtree_size)
            subtree_size = size - vrank;
        packed_size = scount * subtree_size;

        ptmp = tempbuf = (char *)malloc(packed_size);
        if (NULL == tempbuf) {
            err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }

        /* recv from parent on non-root */
        err = MCA_PML_CALL(recv(ptmp, (ptrdiff_t)packed_size, MPI_PACKED, bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_SCATTER, comm, &status));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        /* Get received count */
        curr_count = (int)status._ucount;  /* no need for conversion, work in bytes */
        sextent = 1;  /* bytes */
    }

    if (rbuf != MPI_IN_PLACE) {  /* local copy to rbuf */
        err = ompi_datatype_sndrcv(ptmp, scount, sdtype,
                                   rbuf, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    /* send to children on all non-leaf */
    for (int i = bmtree->tree_nextsize - 1; i >= 0; i--) {
        /* figure out how much data I have to send to this child */
        int vchild = (bmtree->tree_next[i] - root + size) % size;
        int send_count = vchild - vrank;
        if (send_count > size - vchild)
            send_count = size - vchild;
        send_count *= scount;

        err = MCA_PML_CALL(send(ptmp + (ptrdiff_t)(curr_count - send_count) * sextent,
                                send_count, sdtype, bmtree->tree_next[i],
                                MCA_COLL_BASE_TAG_SCATTER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        curr_count -= send_count;
    }
    if (NULL != tempbuf)
        free(tempbuf);

    return MPI_SUCCESS;

 err_hndl:
    if (NULL != tempbuf)
        free(tempbuf);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they
 * are just as fast as base/tree based segmenting operations
 * and as such may be selected by the decision functions
 * These are copied into this module due to the way we select modules
 * in V1. i.e. in V2 we will handle this differently and so will not
 * have to duplicate code.
 * JPG following the examples from other coll_base implementations. Dec06.
 */

/* copied function (with appropriate renaming) starts here */
/*
 *	scatter_intra
 *
 *	Function:	- basic scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_base_scatter_intra_basic_linear(const void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, int rcount,
                                          struct ompi_datatype_t *rdtype,
                                          int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    ptrdiff_t incr;
    char *ptmp;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */

    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        return err;
    }

    /* I am the root, loop sending data. */

    err = ompi_datatype_type_extent(sdtype, &incr);
    if (OMPI_SUCCESS != err) {
        return OMPI_ERROR;
    }

    incr *= scount;
    for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {

        /* simple optimization */

        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err =
                    ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                         rdtype);
            }
        } else {
            err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                    MCA_COLL_BASE_TAG_SCATTER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
        }
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */

    return MPI_SUCCESS;
}

/* copied function (with appropriate renaming) ends here */

/*
 * Use isends for distributing the data with periodic sync by blocking send.
 * Blocking send acts like a local resources flush, because it ensures
 * progression until the message is sent/(copied to some sort of transmit buffer).
 */
int
ompi_coll_base_scatter_intra_linear_nb(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount,
                                       struct ompi_datatype_t *rdtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module,
                                       int max_reqs)
{
    int i, rank, size, err, line, nreqs;
    ptrdiff_t incr;
    char *ptmp;
    ompi_request_t **reqs = NULL, **preq;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */
    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            line = __LINE__; goto err_hndl;
        }

        return MPI_SUCCESS;
    }

    if (max_reqs <= 1) {
        max_reqs = 0;
        nreqs = size - 1; /* no send for myself */
    } else {
        /* We use blocking MPI_Send (which does not need a request)
         * every max_reqs send operation (which is size/max_reqs at most),
         * therefore no need to allocate requests for these sends. */
        nreqs = size - (size / max_reqs);
    }

    reqs = ompi_coll_base_comm_get_reqs(module->base_data, nreqs);
    if (NULL == reqs) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        line = __LINE__; goto err_hndl;
    }

    err = ompi_datatype_type_extent(sdtype, &incr);
    if (OMPI_SUCCESS != err) {
        line = __LINE__; goto err_hndl;
    }
    incr *= scount;

    /* I am the root, loop sending data. */
    for (i = 0, ptmp = (char *)sbuf, preq = reqs; i < size; ++i, ptmp += incr) {
        /* simple optimization */
        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err = ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                           rdtype);
            }
        } else {
            if (!max_reqs || (i % max_reqs)) {
                err = MCA_PML_CALL(isend(ptmp, scount, sdtype, i,
                                         MCA_COLL_BASE_TAG_SCATTER,
                                         MCA_PML_BASE_SEND_STANDARD,
                                         comm, preq++));
            } else {
                err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                        MCA_COLL_BASE_TAG_SCATTER,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        comm));
            }
        }
        if (MPI_SUCCESS != err) {
            line = __LINE__; goto err_hndl;
        }
    }

    err = ompi_request_wait_all(preq - reqs, reqs, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) {
        line = __LINE__; goto err_hndl;
    }

    return MPI_SUCCESS;

err_hndl:
    if (NULL != reqs) {
        /* find a real error code */
        if (MPI_ERR_IN_STATUS == err) {
            for (i = 0; i < nreqs; i++) {
                if (MPI_REQUEST_NULL == reqs[i]) continue;
                if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR) continue;
                if (reqs[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    err = reqs[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, nreqs);
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err, rank));
    (void)line;  /* silence compiler warning */
    return err;
}

