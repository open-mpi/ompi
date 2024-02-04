/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

/*
 *	gatherv_intra
 *
 *	Function:	- basic gatherv operation
 *	Accepts:	- same arguments as MPI_Gatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_gatherv_intra(const void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, const int *rcounts, const int *disps,
                             struct ompi_datatype_t *rdtype, int root,
                             struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    int err, i, peer, rank, size;
    char *ptmp;
    ptrdiff_t lb, extent;
    size_t rdsize;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if (root == rank) {
        /* Root receives from everyone else */
        ompi_datatype_type_size(rdtype, &rdsize);
        if (OPAL_UNLIKELY(0 == rdsize)) {
            /* bozzo case */
            return MPI_SUCCESS;
        }

        err = ompi_datatype_get_extent(rdtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        if (MPI_IN_PLACE != sbuf && (0 < scount) && (0 < rcounts[rank])) {
            /* Directly copy self sbuf to rbuf */
            err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                       ((char *) rbuf) + (extent * disps[rank]), rcounts[rank],
                                       rdtype);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }

        ompi_request_t **reqs;
        size_t nrecv = 0, recv_iter = 0;

        for (i = 0; i < size; ++i) {
            /* We directly copied the data from self */
            if (0 < rcounts[i] && rank != i) {
                ++nrecv;
            }
        }

        if (0 == nrecv) {
            /* Nothing to receive */
            return MPI_SUCCESS;
        }

        reqs = ompi_coll_base_comm_get_reqs(module->base_data, nrecv);

        for (i = 1; i < size; ++i) {
            peer = (rank + i) % size;
            ptmp = ((char *) rbuf) + (extent * disps[peer]);
            /* Only receive if there is something to receive */
            if (0 < rcounts[peer]) {
                err = MCA_PML_CALL(irecv(ptmp, rcounts[peer], rdtype, peer,
                                         MCA_COLL_BASE_TAG_GATHERV, comm, &reqs[recv_iter++]));
            }
        }

        assert(nrecv == recv_iter);

        err = ompi_request_wait_all(nrecv, reqs, MPI_STATUSES_IGNORE);

        if (MPI_ERR_IN_STATUS == err) {
            for (int i = 0; i < nrecv; i++) {
                if (MPI_REQUEST_NULL == reqs[i])
                    continue;
                if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR)
                    continue;
                if (MPI_SUCCESS != reqs[i]->req_status.MPI_ERROR) {
                    err = reqs[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }

        ompi_coll_base_free_reqs(reqs, nrecv);
        return err;
    }

    /* Everyone but root sends data and returns.  Don't send anything
       for sendcounts of 0 (even though MPI_Gatherv has a guard for 0
       counts, this routine is used elsewhere, like the implementation
       of allgatherv, so it's possible to get here with a scount of
       0) */

    size_t sdsize;
    ompi_datatype_type_size(sdtype, &sdsize);
    if (scount > 0 && sdsize > 0) {
        return MCA_PML_CALL(send(sbuf, scount, sdtype, root, MCA_COLL_BASE_TAG_GATHERV,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }
    return MPI_SUCCESS;
}

/*
 *	gatherv_inter
 *
 *	Function:	- basic gatherv operation
 *	Accepts:	- same arguments as MPI_Gatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_gatherv_inter(const void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, const int *rcounts, const int *disps,
                             struct ompi_datatype_t *rdtype, int root,
                             struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    int i, size, err;
    char *ptmp;
    ptrdiff_t lb, extent;
    ompi_request_t **reqs = NULL;

    size = ompi_comm_remote_size(comm);

    /* If not root, receive data.  Note that we will only get here if
     * scount > 0 or rank == root. */

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* Everyone but root sends data and returns. */
        err = MCA_PML_CALL(send(sbuf, scount, sdtype, root,
                                MCA_COLL_BASE_TAG_GATHERV,
                                MCA_PML_BASE_SEND_STANDARD, comm));
    } else {
        /* I am the root, loop receiving data. */
        err = ompi_datatype_get_extent(rdtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
        if( NULL == reqs ) { return OMPI_ERR_OUT_OF_RESOURCE; }

        for (i = 0; i < size; ++i) {
            ptmp = ((char *) rbuf) + (extent * disps[i]);
            err = MCA_PML_CALL(irecv(ptmp, rcounts[i], rdtype, i,
                                     MCA_COLL_BASE_TAG_GATHERV,
                                     comm, &reqs[i]));
            if (OMPI_SUCCESS != err) {
                ompi_coll_base_free_reqs(reqs, i + 1);
                return err;
            }
        }

        err = ompi_request_wait_all(size, reqs, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, size);
        }
    }

    /* All done */
    return err;
}
