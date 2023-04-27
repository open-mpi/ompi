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
 * Copyright (c) 2013-2016 Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2022 IBM Corporation. All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2022      BULL S.A.S. All rights reserved.
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
#include "opal/datatype/opal_convertor_internal.h"
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
mca_coll_base_alltoall_intra_basic_inplace(const void *rbuf, int rcount,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    int i, size, rank, left, right, err = MPI_SUCCESS, line;
    ptrdiff_t extent;
    ompi_request_t *req;
    char *tmp_buffer;
    size_t packed_size = 0, max_size;
    opal_convertor_t convertor;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    ompi_datatype_type_size(rdtype, &max_size);

    /* Easy way out */
    if ((1 == size) || (0 == rcount) || (0 == max_size) ) {
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
            max_size = packed_size > max_size ? packed_size : max_size;
        }
    }
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT */
    max_size *= rcount;

    ompi_datatype_type_extent(rdtype, &extent);

    /* Allocate a temporary buffer */
    tmp_buffer = calloc (max_size, 1);
    if( NULL == tmp_buffer) { err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto error_hndl; }

    for (i = 1 ; i <= (size >> 1) ; ++i) {
        struct iovec iov = {.iov_base = tmp_buffer, .iov_len = max_size};
        uint32_t iov_count = 1;

        right = (rank + i) % size;
        left  = (rank + size - i) % size;

        ompi_proc_t *right_proc = ompi_comm_peer_lookup(comm, right);
        opal_convertor_clone(right_proc->super.proc_convertor, &convertor, 0);
        opal_convertor_prepare_for_send(&convertor, &rdtype->super, rcount,
                                        (char *) rbuf + (MPI_Aint) right * rcount * extent);
        packed_size = max_size;
        err = opal_convertor_pack(&convertor, &iov, &iov_count, &packed_size);
        if (1 != err) {
            line = __LINE__;
            goto error_hndl;
        }

        /* Receive data from the right */
        err = MCA_PML_CALL(irecv ((char *) rbuf + (MPI_Aint) right * rcount * extent, rcount, rdtype,
                                  right, MCA_COLL_BASE_TAG_ALLTOALL, comm, &req));
        if (MPI_SUCCESS != err) {
            line = __LINE__;
            goto error_hndl;
        }

        if( left != right ) {
            /* Send data to the left */
            err = MCA_PML_CALL(send ((char *) rbuf + (MPI_Aint) left * rcount * extent, rcount, rdtype,
                                     left, MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                                     comm));
            if (MPI_SUCCESS != err) {
                line = __LINE__;
                goto error_hndl;
            }

            err = ompi_request_wait (&req, MPI_STATUSES_IGNORE);
            if (MPI_SUCCESS != err) {
                line = __LINE__;
                goto error_hndl;
            }

            /* Receive data from the left */
            err = MCA_PML_CALL(irecv ((char *) rbuf + (MPI_Aint) left * rcount * extent, rcount, rdtype,
                                      left, MCA_COLL_BASE_TAG_ALLTOALL, comm, &req));
            if (MPI_SUCCESS != err) {
                line = __LINE__;
                goto error_hndl;
            }
        }

        /* Send data to the right */
        err = MCA_PML_CALL(send ((char *) tmp_buffer,  packed_size, MPI_PACKED,
                                 right, MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                                 comm));
        if (MPI_SUCCESS != err) {
            line = __LINE__;
            goto error_hndl;
        }

        err = ompi_request_wait (&req, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            line = __LINE__;
            goto error_hndl;
        }
    }

 error_hndl:
    /* Free the temporary buffer */
    if( NULL != tmp_buffer )
        free (tmp_buffer);

    if( MPI_SUCCESS != err ) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err, rank));
        (void)line;  // silence compiler warning
    }

    /* All done */
    return err;
}

int ompi_coll_base_alltoall_intra_pairwise(const void *sbuf, int scount,
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int rcount,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, size, step, sendto, recvfrom;
    char *tmpsend, *tmprecv;
    ptrdiff_t lb, sext, rext;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoall_intra_basic_inplace (rbuf, rcount, rdtype,
                                                            comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:alltoall_intra_pairwise rank %d", rank));

    err = ompi_datatype_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    err = ompi_datatype_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }


    /* Perform pairwise exchange - starting from 1 so the local copy is last */
    for (step = 1; step < size + 1; step++) {

        /* Determine sender and receiver for this step. */
        sendto  = (rank + step) % size;
        recvfrom = (rank + size - step) % size;

        /* Determine sending and receiving locations */
        tmpsend = (char*)sbuf + (ptrdiff_t)sendto * sext * (ptrdiff_t)scount;
        tmprecv = (char*)rbuf + (ptrdiff_t)recvfrom * rext * (ptrdiff_t)rcount;

        /* send and receive */
        err = ompi_coll_base_sendrecv( tmpsend, scount, sdtype, sendto,
                                        MCA_COLL_BASE_TAG_ALLTOALL,
                                        tmprecv, rcount, rdtype, recvfrom,
                                        MCA_COLL_BASE_TAG_ALLTOALL,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "%s:%4d\tError occurred %d, rank %2d", __FILE__, line,
                 err, rank));
    (void)line;  // silence compiler warning
    return err;
}


int ompi_coll_base_alltoall_intra_bruck(const void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int i, line = -1, rank, size, err = 0;
    int sendto, recvfrom, distance, *displs = NULL;
    char *tmpbuf = NULL, *tmpbuf_free = NULL;
    ptrdiff_t sext, rext, span, gap = 0;
    struct ompi_datatype_t *new_ddt;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoall_intra_basic_inplace (rbuf, rcount, rdtype,
                                                            comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:alltoall_intra_bruck rank %d", rank));

    err = ompi_datatype_type_extent (sdtype, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_datatype_type_extent (rdtype, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    span = opal_datatype_span(&rdtype->super, (int64_t)size * rcount, &gap);

    /* tmp buffer allocation for message data */
    tmpbuf_free = (char *)malloc(span);
    if (tmpbuf_free == NULL) { line = __LINE__; err = -1; goto err_hndl; }
    tmpbuf = tmpbuf_free - gap;

    /* Step 1 - local rotation - shift up by rank */
    err = ompi_datatype_sndrcv ((char*)sbuf + ((ptrdiff_t) rank * scount * sext),
                                (int32_t) (size - rank) * scount,
                                sdtype,
                                tmpbuf,
                                (int32_t) (size - rank) * rcount,
                                rdtype);
    if (err<0) {
        line = __LINE__; err = -1; goto err_hndl;
    }

    if (rank != 0) {
        err = ompi_datatype_sndrcv (sbuf,
                                    (int32_t) rank * scount,
                                    sdtype,
                                    tmpbuf + ((ptrdiff_t) (size - rank) * rcount * rext),
                                    (int32_t) rank * rcount,
                                    rdtype);
        if (err<0) {
            line = __LINE__; err = -1; goto err_hndl;
        }
    }

    /* perform communication step */
    for (distance = 1; distance < size; distance<<=1) {

        sendto = (rank + distance) % size;
        recvfrom = (rank - distance + size) % size;

        new_ddt = ompi_datatype_create((1 + size/distance) * (2 + rdtype->super.desc.used));

        /* Create datatype describing data sent/received */
        for (i = distance; i < size; i += 2*distance) {
            int nblocks = distance;
            if (i + distance >= size) {
                nblocks = size - i;
            }
            ompi_datatype_add(new_ddt, rdtype, rcount * nblocks,
                              i * rcount * rext, rext);
        }

        /* Commit the new datatype */
        err = ompi_datatype_commit(&new_ddt);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

        /* Sendreceive */
        err = ompi_coll_base_sendrecv ( tmpbuf, 1, new_ddt, sendto,
                                         MCA_COLL_BASE_TAG_ALLTOALL,
                                         rbuf, 1, new_ddt, recvfrom,
                                         MCA_COLL_BASE_TAG_ALLTOALL,
                                         comm, MPI_STATUS_IGNORE, rank );
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

        /* Copy back new data from recvbuf to tmpbuf */
        err = ompi_datatype_copy_content_same_ddt(new_ddt, 1,tmpbuf, (char *) rbuf);
        if (err < 0) { line = __LINE__; err = -1; goto err_hndl;  }

        /* free ddt */
        err = ompi_datatype_destroy(&new_ddt);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    } /* end of for (distance = 1... */

    /* Step 3 - local rotation - */
    for (i = 0; i < size; i++) {

        err = ompi_datatype_copy_content_same_ddt (rdtype, (int32_t) rcount,
                                                   ((char*)rbuf) + ((ptrdiff_t)((rank - i + size) % size) * (ptrdiff_t)rcount * rext),
                                                   tmpbuf + (ptrdiff_t)i * (ptrdiff_t)rcount * rext);
        if (err < 0) { line = __LINE__; err = -1; goto err_hndl;  }
    }

    /* Step 4 - clean up */
    if (tmpbuf_free != NULL) free(tmpbuf_free);
    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err,
                 rank));
    (void)line;  // silence compiler warning
    if (tmpbuf_free != NULL) free(tmpbuf_free);
    if (displs != NULL) free(displs);
    return err;
}

/*
 * alltoall_intra_linear_sync
 *
 * Function:       Linear implementation of alltoall with limited number
 *                 of outstanding requests.
 * Accepts:        Same as MPI_Alltoall(), and the maximum number of
 *                 outstanding requests (actual number is 2 * max, since
 *                 we count receive and send requests separately).
 * Returns:        MPI_SUCCESS or error code
 *
 * Description:    Algorithm is the following:
 *                 1) post K irecvs, K <= N
 *                 2) post K isends, K <= N
 *                 3) while not done
 *                    - wait for any request to complete
 *                    - replace that request by the new one of the same type.
 */
int ompi_coll_base_alltoall_intra_linear_sync(const void *sbuf, int scount,
                                               struct ompi_datatype_t *sdtype,
                                               void* rbuf, int rcount,
                                               struct ompi_datatype_t *rdtype,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module,
                                               int max_outstanding_reqs)
{
    int line, error, ri, si, rank, size, nrreqs, nsreqs, total_reqs;
    int nreqs = 0;
    char *psnd, *prcv;
    ptrdiff_t slb, sext, rlb, rext;

    ompi_request_t **reqs = NULL;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoall_intra_basic_inplace (rbuf, rcount, rdtype,
                                                            comm, module);
    }

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_alltoall_intra_linear_sync rank %d", rank));

    error = ompi_datatype_get_extent(sdtype, &slb, &sext);
    if (OMPI_SUCCESS != error) {
        return error;
    }
    sext *= scount;

    error = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (OMPI_SUCCESS != error) {
        return error;
    }
    rext *= rcount;

    /* simple optimization */

    psnd = ((char *) sbuf) + (ptrdiff_t)rank * sext;
    prcv = ((char *) rbuf) + (ptrdiff_t)rank * rext;

    error = ompi_datatype_sndrcv(psnd, scount, sdtype, prcv, rcount, rdtype);
    if (MPI_SUCCESS != error) {
        return error;
    }

    /* If only one process, we're done. */

    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Initiate send/recv to/from others. */
    total_reqs =  (((max_outstanding_reqs > (size - 1)) ||
                    (max_outstanding_reqs <= 0)) ?
                   (size - 1) : (max_outstanding_reqs));
    if (0 < total_reqs) {
        reqs = ompi_coll_base_comm_get_reqs(module->base_data, 2 * total_reqs);
        if (NULL == reqs) { error = -1; line = __LINE__; goto error_hndl; }
        reqs[0] = reqs[1] = MPI_REQUEST_NULL;
    }

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post first batch of irecv and isend requests  */
    for (nreqs = 0, nrreqs = 0, ri = (rank + 1) % size; nreqs < total_reqs;
         ri = (ri + 1) % size, ++nrreqs) {
        error = MCA_PML_CALL(irecv
                             (prcv + (ptrdiff_t)ri * rext, rcount, rdtype, ri,
                              MCA_COLL_BASE_TAG_ALLTOALL, comm, &reqs[nreqs]));
        nreqs++;
        if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }
    }
    for (nsreqs = 0, si =  (rank + size - 1) % size; nreqs < 2 * total_reqs;
         si = (si + size - 1) % size, ++nsreqs) {
        error = MCA_PML_CALL(isend
                             (psnd + (ptrdiff_t)si * sext, scount, sdtype, si,
                              MCA_COLL_BASE_TAG_ALLTOALL,
                              MCA_PML_BASE_SEND_STANDARD, comm, &reqs[nreqs]));
        nreqs++;
        if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }
    }

    /* Wait for requests to complete */
    if (nreqs == 2 * (size - 1)) {
        /* Optimization for the case when all requests have been posted  */
        error = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }

    } else {
        /* As requests complete, replace them with corresponding requests:
           - wait for any request to complete, mark the request as
           MPI_REQUEST_NULL
           - If it was a receive request, replace it with new irecv request
           (if any)
           - if it was a send request, replace it with new isend request (if any)
        */
        int ncreqs = 0;
        while (ncreqs < 2 * (size - 1)) {
            int completed;
            error = ompi_request_wait_any(2 * total_reqs, reqs, &completed,
                                          MPI_STATUS_IGNORE);
            if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }
            reqs[completed] = MPI_REQUEST_NULL;
            ncreqs++;
            if (completed < total_reqs) {
                if (nrreqs < (size - 1)) {
                    error = MCA_PML_CALL(irecv
                                         (prcv + (ptrdiff_t)ri * rext, rcount, rdtype, ri,
                                          MCA_COLL_BASE_TAG_ALLTOALL, comm,
                                          &reqs[completed]));
                    if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }
                    ++nrreqs;
                    ri = (ri + 1) % size;
                }
            } else {
                if (nsreqs < (size - 1)) {
                    error = MCA_PML_CALL(isend
                                         (psnd + (ptrdiff_t)si * sext, scount, sdtype, si,
                                          MCA_COLL_BASE_TAG_ALLTOALL,
                                          MCA_PML_BASE_SEND_STANDARD, comm,
                                          &reqs[completed]));
                    if (MPI_SUCCESS != error) { line = __LINE__; goto error_hndl; }
                    ++nsreqs;
                    si = (si + size - 1) % size;
                }
            }
        }
    }

    /* All done */
    return MPI_SUCCESS;

 error_hndl:
    /* find a real error code */
    if (MPI_ERR_IN_STATUS == error) {
        for( ri = 0; ri < nreqs; ri++ ) {
            if (MPI_REQUEST_NULL == reqs[ri]) continue;
            if (MPI_ERR_PENDING == reqs[ri]->req_status.MPI_ERROR) continue;
            if (reqs[ri]->req_status.MPI_ERROR != MPI_SUCCESS) {
                error = reqs[ri]->req_status.MPI_ERROR;
                break;
            }
        }
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, error,
                 rank));
    (void)line;  // silence compiler warning
    ompi_coll_base_free_reqs(reqs, nreqs);
    return error;
}


int ompi_coll_base_alltoall_intra_two_procs(const void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype,
                                             void* rbuf, int rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, remote;
    void * tmpsend, *tmprecv;
    ptrdiff_t sext, rext, lb;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoall_intra_basic_inplace (rbuf, rcount, rdtype,
                                                            comm, module);
    }

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_alltoall_intra_two_procs rank %d", rank));

    if (2 != ompi_comm_size(comm)) {
        return MPI_ERR_UNSUPPORTED_OPERATION;
    }

    err = ompi_datatype_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_datatype_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    /* exchange data */
    remote  = rank ^ 1;

    tmpsend = (char*)sbuf + (ptrdiff_t)remote * sext * (ptrdiff_t)scount;
    tmprecv = (char*)rbuf + (ptrdiff_t)remote * rext * (ptrdiff_t)rcount;

    /* send and receive */
    err = ompi_coll_base_sendrecv ( tmpsend, scount, sdtype, remote,
                                     MCA_COLL_BASE_TAG_ALLTOALL,
                                     tmprecv, rcount, rdtype, remote,
                                     MCA_COLL_BASE_TAG_ALLTOALL,
                                     comm, MPI_STATUS_IGNORE, rank );
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* ddt sendrecv your own data */
    err = ompi_datatype_sndrcv((char*) sbuf + (ptrdiff_t)rank * sext * (ptrdiff_t)scount,
                               (int32_t) scount, sdtype,
                               (char*) rbuf + (ptrdiff_t)rank * rext * (ptrdiff_t)rcount,
                               (int32_t) rcount, rdtype);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* done */
    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err,
                 rank));
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
 * GEF Oct05 after asking Jeff.
 */

/* copied function (with appropriate renaming) starts here */

int ompi_coll_base_alltoall_intra_basic_linear(const void *sbuf, int scount,
                                               struct ompi_datatype_t *sdtype,
                                               void* rbuf, int rcount,
                                               struct ompi_datatype_t *rdtype,
                                               struct ompi_communicator_t *comm,
                                               mca_coll_base_module_t *module)
{
    int i, rank, size, err, line;
    int nreqs = 0;
    char *psnd, *prcv;
    MPI_Aint lb, sndinc, rcvinc;
    ompi_request_t **req, **sreq, **rreq;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoall_intra_basic_inplace (rbuf, rcount, rdtype,
                                                            comm, module);
    }

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_alltoall_intra_basic_linear rank %d", rank));

    err = ompi_datatype_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_datatype_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* simple optimization */

    psnd = ((char *) sbuf) + (ptrdiff_t)rank * sndinc;
    prcv = ((char *) rbuf) + (ptrdiff_t)rank * rcvinc;

    err = ompi_datatype_sndrcv(psnd, scount, sdtype, prcv, rcount, rdtype);
    if (MPI_SUCCESS != err) {
        return err;
    }

    /* If only one process, we're done. */

    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Initiate all send/recv to/from others. */

    req = rreq = ompi_coll_base_comm_get_reqs(data, (size - 1) * 2);
    if (NULL == req) { err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl; }

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first -- a simple optimization */

    for (nreqs = 0, i = (rank + 1) % size; i != rank;
         i = (i + 1) % size, ++rreq) {
        nreqs++;
        err = MCA_PML_CALL(irecv_init
                           (prcv + (ptrdiff_t)i * rcvinc, rcount, rdtype, i,
                           MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    /* Now post all sends in reverse order
       - We would like to minimize the search time through message queue
         when messages actually arrive in the order in which they were posted.
     */
    sreq = rreq;
    for (i = (rank + size - 1) % size; i != rank;
         i = (i + size - 1) % size, ++sreq) {
        nreqs++;
        err = MCA_PML_CALL(isend_init
                           (psnd + (ptrdiff_t)i * sndinc, scount, sdtype, i,
                           MCA_COLL_BASE_TAG_ALLTOALL,
                           MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    /* Start your engines.  This will never return an error. */

    MCA_PML_CALL(start(nreqs, req));

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */

    err = ompi_request_wait_all(nreqs, req, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

 err_hndl:
    if (MPI_SUCCESS != err) {
        /* find a real error code */
        if (MPI_ERR_IN_STATUS == err) {
            for( i = 0; i < nreqs; i++ ) {
                if (MPI_REQUEST_NULL == req[i]) continue;
                if (MPI_ERR_PENDING == req[i]->req_status.MPI_ERROR) continue;
                if (req[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    err = req[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        OPAL_OUTPUT( (ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                      __FILE__, line, err, rank) );
        (void)line;  // silence compiler warning
    }
    /* Free the reqs in all cases as they are persistent requests */
    ompi_coll_base_free_reqs(req, nreqs);

    /* All done */
    return err;
}

/* copied function (with appropriate renaming) ends here */
