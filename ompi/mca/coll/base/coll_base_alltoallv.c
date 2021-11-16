/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2013      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
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
 * We want to minimize the amount of temporary memory needed while allowing as many ranks
 * to exchange data simultaneously. We use a variation of the ring algorithm, where in a
 * single step a process echange the data with both neighbors at distance k (on the left
 * and the right on a logical ring topology). With this approach we need to pack the data
 * for a single of the two neighbors, as we can then use the original buffer (and datatype
 * and count) to send the data to the other.
 */
int
mca_coll_base_alltoallv_intra_basic_inplace(const void *rbuf, const int *rcounts, const int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int i, size, rank, left, right, err = MPI_SUCCESS, line;
    ptrdiff_t extent;
    ompi_request_t *req = MPI_REQUEST_NULL;
    char *tmp_buffer;
    size_t packed_size = 0, max_size;
    opal_convertor_t convertor;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    ompi_datatype_type_size(rdtype, &max_size);
    max_size *= rcounts[rank];

    /* Easy way out */
    if ((1 == size) || (0 == max_size) ) {
        return MPI_SUCCESS;
    }

    /* Find the largest amount of packed send/recv data among all peers where
     * we need to pack before the send.
     */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    for (i = 1 ; i <= (size >> 1) ; ++i) {
        right = (rank + i) % size;
        ompi_proc_t *ompi_proc = ompi_comm_peer_lookup(comm, right);

        if( OPAL_UNLIKELY(opal_local_arch != ompi_proc->super.proc_convertor->master->remote_arch))  {
            packed_size = opal_datatype_compute_remote_size(&rdtype->super,
                                                            ompi_proc->super.proc_convertor->master->remote_sizes);
            packed_size *= rcounts[right];
            max_size = packed_size > max_size ? packed_size : max_size;
        }
    }
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT */

    ompi_datatype_type_extent(rdtype, &extent);

    /* Allocate a temporary buffer */
    tmp_buffer = calloc (max_size, 1);
    if( NULL == tmp_buffer) { err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto error_hndl; }

    for (i = 1 ; i <= (size >> 1) ; ++i) {
        struct iovec iov = {.iov_base = tmp_buffer, .iov_len = max_size};
        uint32_t iov_count = 1;

        right = (rank + i) % size;
        left  = (rank + size - i) % size;

        if( 0 != rcounts[right] ) {  /* nothing to exchange with the peer on the right */
            ompi_proc_t *right_proc = ompi_comm_peer_lookup(comm, right);
            opal_convertor_clone(right_proc->super.proc_convertor, &convertor, 0);
            opal_convertor_prepare_for_send(&convertor, &rdtype->super, rcounts[right],
                                            (char *) rbuf + rdisps[right] * extent);
            packed_size = max_size;
            err = opal_convertor_pack(&convertor, &iov, &iov_count, &packed_size);
            if (1 != err) { goto error_hndl; }

            /* Receive data from the right */
            err = MCA_PML_CALL(irecv ((char *) rbuf + rdisps[right] * extent, rcounts[right], rdtype,
                                      right, MCA_COLL_BASE_TAG_ALLTOALLV, comm, &req));
            if (MPI_SUCCESS != err) { goto error_hndl; }
        }

        if( (left != right) && (0 != rcounts[left]) ) {
            /* Send data to the left */
            err = MCA_PML_CALL(send ((char *) rbuf + rdisps[left] * extent, rcounts[left], rdtype,
                                     left, MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                                     comm));
            if (MPI_SUCCESS != err) { goto error_hndl; }

            err = ompi_request_wait (&req, MPI_STATUSES_IGNORE);
            if (MPI_SUCCESS != err) { goto error_hndl; }

            /* Receive data from the left */
            err = MCA_PML_CALL(irecv ((char *) rbuf + rdisps[left] * extent, rcounts[left], rdtype,
                                      left, MCA_COLL_BASE_TAG_ALLTOALLV, comm, &req));
            if (MPI_SUCCESS != err) { goto error_hndl; }
        }

        if( 0 != rcounts[right] ) {  /* nothing to exchange with the peer on the right */
            /* Send data to the right */
            err = MCA_PML_CALL(send ((char *) tmp_buffer,  packed_size, MPI_PACKED,
                                     right, MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                                     comm));
            if (MPI_SUCCESS != err) { goto error_hndl; }
        }

        err = ompi_request_wait (&req, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) { goto error_hndl; }
    }

 error_hndl:
    /* Free the temporary buffer */
    if( NULL != tmp_buffer )
        free (tmp_buffer);

    if( MPI_SUCCESS != err ) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err,
                     rank));
        (void)line;  // silence compiler warning
    }

    /* All done */
    return err;
}

int
ompi_coll_base_alltoallv_intra_pairwise(const void *sbuf, const int *scounts, const int *sdisps,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, const int *rcounts, const int *rdisps,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, size, step = 0, sendto, recvfrom;
    void *psnd, *prcv;
    ptrdiff_t sext, rext;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoallv_intra_basic_inplace (rbuf, rcounts, rdisps,
                                                             rdtype, comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:alltoallv_intra_pairwise rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

   /* Perform pairwise exchange starting from 1 since local exhange is done */
    for (step = 0; step < size; step++) {

        /* Determine sender and receiver for this step. */
        sendto  = (rank + step) % size;
        recvfrom = (rank + size - step) % size;

        /* Determine sending and receiving locations */
        psnd = (char*)sbuf + (ptrdiff_t)sdisps[sendto] * sext;
        prcv = (char*)rbuf + (ptrdiff_t)rdisps[recvfrom] * rext;

        /* send and receive */
        err = ompi_coll_base_sendrecv( psnd, scounts[sendto], sdtype, sendto,
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        prcv, rcounts[recvfrom], rdtype, recvfrom,
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "%s:%4d\tError occurred %d, rank %2d at step %d", __FILE__, line,
                 err, rank, step));
    (void)line;  // silence compiler warning
    return err;
}

/**
 * Linear functions are copied from the basic coll module.  For
 * some small number of nodes and/or small data sizes they are just as
 * fast as base/tree based segmenting operations and as such may be
 * selected by the decision functions.  These are copied into this module
 * due to the way we select modules in V1. i.e. in V2 we will handle this
 * differently and so will not have to duplicate code.
 */
int
ompi_coll_base_alltoallv_intra_basic_linear(const void *sbuf, const int *scounts, const int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void *rbuf, const int *rcounts, const int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int i, size, rank, err, nreqs;
    char *psnd, *prcv;
    ptrdiff_t sext, rext;
    ompi_request_t **preq, **reqs;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    if (MPI_IN_PLACE == sbuf) {
        return  mca_coll_base_alltoallv_intra_basic_inplace (rbuf, rcounts, rdisps,
                                                              rdtype, comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:alltoallv_intra_basic_linear rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

    /* Simple optimization - handle send to self first */
    psnd = ((char *) sbuf) + (ptrdiff_t)sdisps[rank] * sext;
    prcv = ((char *) rbuf) + (ptrdiff_t)rdisps[rank] * rext;
    if (0 != scounts[rank]) {
        err = ompi_datatype_sndrcv(psnd, scounts[rank], sdtype,
                              prcv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* If only one process, we're done. */
    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Now, initiate all send/recv to/from others. */
    nreqs = 0;
    reqs = preq = ompi_coll_base_comm_get_reqs(data, 2 * size);
    if( NULL == reqs ) { err = OMPI_ERR_OUT_OF_RESOURCE; goto err_hndl; }

    /* Post all receives first */
    for (i = 0; i < size; ++i) {
        if (i == rank) {
            continue;
        }

        ++nreqs;
        prcv = ((char *) rbuf) + (ptrdiff_t)rdisps[i] * rext;
        err = MCA_PML_CALL(irecv_init(prcv, rcounts[i], rdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV, comm,
                                      preq++));
        if (MPI_SUCCESS != err) { goto err_hndl; }
    }

    /* Now post all sends */
    for (i = 0; i < size; ++i) {
        if (i == rank) {
            continue;
        }

        ++nreqs;
        psnd = ((char *) sbuf) + (ptrdiff_t)sdisps[i] * sext;
        err = MCA_PML_CALL(isend_init(psnd, scounts[i], sdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      preq++));
        if (MPI_SUCCESS != err) { goto err_hndl; }
    }

    /* Start your engines.  This will never return an error. */
    MCA_PML_CALL(start(nreqs, reqs));

    /* Wait for them all.  If there's an error, note that we don't care
     * what the error was -- just that there *was* an error.  The PML
     * will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return the
     * error after we free everything. */
    err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);

 err_hndl:
    /* find a real error code */
    if (MPI_ERR_IN_STATUS == err) {
        for( i = 0; i < nreqs; i++ ) {
            if (MPI_REQUEST_NULL == reqs[i]) continue;
            if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR) continue;
            if (reqs[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                err = reqs[i]->req_status.MPI_ERROR;
                break;
            }
        }
    }
    /* Free the requests in all cases as they are persistent */
    ompi_coll_base_free_reqs(reqs, nreqs);

    return err;
}
