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
#include "coll_basic.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"


/*
 *	alltoall_intra
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoall_intra(void *sbuf, int scount,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount,
                              struct ompi_datatype_t *rdtype,
                              struct ompi_communicator_t *comm)
{
    int i;
    int rank;
    int size;
    int err;
    int nreqs;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    err = ompi_ddt_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_ddt_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* simple optimization */

    psnd = ((char *) sbuf) + (rank * sndinc);
    prcv = ((char *) rbuf) + (rank * rcvinc);

    err = ompi_ddt_sndrcv(psnd, scount, sdtype, prcv, rcount, rdtype);
    if (MPI_SUCCESS != err) {
        return err;
    }

    /* If only one process, we're done. */

    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Initiate all send/recv to/from others. */

    nreqs = (size - 1) * 2;
    req = rreq = comm->c_coll_basic_data->mccb_reqs;
    sreq = rreq + size - 1;

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first -- a simple optimization */

    for (i = (rank + 1) % size; i != rank; i = (i + 1) % size, ++rreq) {
        err =
            MCA_PML_CALL(irecv_init
                         (prcv + (i * rcvinc), rcount, rdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(req, rreq - req);
            return err;
        }
    }

    /* Now post all sends */

    for (i = (rank + 1) % size; i != rank; i = (i + 1) % size, ++sreq) {
        err =
            MCA_PML_CALL(isend_init
                         (psnd + (i * sndinc), scount, sdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL,
                          MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(req, sreq - req);
            return err;
        }
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

    /* Free the reqs */

    mca_coll_basic_free_reqs(req, nreqs);

    /* All done */

    return err;
}


/*
 *	alltoall_inter
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoall_inter(void *sbuf, int scount,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount,
                              struct ompi_datatype_t *rdtype,
                              struct ompi_communicator_t *comm)
{
    int i;
    int rank;
    int size;
    int err;
    int nreqs;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;

    /* Initialize. */

    size = ompi_comm_remote_size(comm);
    rank = ompi_comm_rank(comm);

    err = ompi_ddt_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_ddt_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* Initiate all send/recv to/from others. */
    nreqs = size * 2;
    req = rreq = comm->c_coll_basic_data->mccb_reqs;
    sreq = rreq + size;

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first */
    for (i = 0; i < size; i++, ++rreq) {
        err = MCA_PML_CALL(irecv(prcv + (i * rcvinc), rcount, rdtype, i,
                                 MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (OMPI_SUCCESS != err) {
            return err;
        }
    }

    /* Now post all sends */
    for (i = 0; i < size; i++, ++sreq) {
        err = MCA_PML_CALL(isend(psnd + (i * sndinc), scount, sdtype, i,
                                 MCA_COLL_BASE_TAG_ALLTOALL,
                                 MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (OMPI_SUCCESS != err) {
            return err;
        }
    }

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */
    err = ompi_request_wait_all(nreqs, req, MPI_STATUSES_IGNORE);

    /* All done */
    return err;
}
