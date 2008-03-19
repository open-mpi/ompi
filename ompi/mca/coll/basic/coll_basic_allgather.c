/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdlib.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allgather_intra
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allgather_intra(void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype, void *rbuf,
                               int rcount, struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm)
{
    int err;
    char *rbuf_original = NULL, *inplace_temp = NULL;
    ptrdiff_t true_lb, true_extent, lb, extent;

    /* Handle MPI_IN_PLACE (see explanantion in reduce.c for how to
       allocate temp buffer) -- note that rank 0 can use IN_PLACE
       natively, and we'll be ok (actually saves a little bit of
       copying around) */

    if (MPI_IN_PLACE == sbuf && 0 != ompi_comm_rank(comm)) {
        ompi_ddt_get_extent(rdtype, &lb, &extent);
        ompi_ddt_get_true_extent(rdtype, &true_lb, &true_extent);

        rbuf_original = (char*)rbuf;
        sbuf = ((char*) rbuf) + (ompi_comm_rank(comm) * extent * rcount);
        sdtype = rdtype;
        scount = rcount;

        inplace_temp = (char*)malloc((true_extent + (rcount - 1) * extent) * 
                                     ompi_comm_size(comm));
        if (NULL == inplace_temp) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        rbuf = inplace_temp - lb;
    } 

    /* Gather and broadcast. */

    err = comm->c_coll.coll_gather(sbuf, scount, sdtype, rbuf, rcount,
                                   rdtype, 0, comm);
    if (MPI_SUCCESS == err) {
        err = comm->c_coll.coll_bcast(rbuf, rcount * ompi_comm_size(comm), 
                                      rdtype, 0, comm);
    }

    /* If we've got a temp buffer, copy back out */

    if (MPI_SUCCESS == err && NULL != inplace_temp) {
        ompi_ddt_copy_content_same_ddt(rdtype, rcount * ompi_comm_size(comm),
                                       rbuf_original, (char*)rbuf);
        free(inplace_temp);
    }

    /* All done */

    return err;
}


/*
 *	allgather_inter
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allgather_inter(void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype,
                               void *rbuf, int rcount,
                               struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm)
{
    int rank;
    int root = 0;
    int size, rsize;
    int err;
    int i;
    char *tmpbuf = NULL, *ptmp;
    ptrdiff_t rlb, slb, rextent, sextent;
    ptrdiff_t incr;
    ompi_request_t *req;
    ompi_request_t **reqs = comm->c_coll_basic_data->mccb_reqs;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    rsize = ompi_comm_remote_size(comm);

    /* Algorithm:
     * - a gather to the root in remote group (simultaniously executed,
     * thats why we cannot use coll_gather).
     * - exchange the temp-results between two roots 
     * - inter-bcast (again simultanious).
     */

    /* Step one: gather operations: */
    if (rank != root) {
        /* send your data to root */
        err = MCA_PML_CALL(send(sbuf, scount, sdtype, root,
                                MCA_COLL_BASE_TAG_ALLGATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (OMPI_SUCCESS != err) {
            return err;
        }
    } else {
        /* receive a msg. from all other procs. */
        err = ompi_ddt_get_extent(rdtype, &rlb, &rextent);
        if (OMPI_SUCCESS != err) {
            return err;
        }
        err = ompi_ddt_get_extent(sdtype, &slb, &sextent);
        if (OMPI_SUCCESS != err) {
            return err;
        }

        /* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(isend(sbuf, scount, sdtype, 0,
                                 MCA_COLL_BASE_TAG_ALLGATHER,
                                 MCA_PML_BASE_SEND_STANDARD,
                                 comm, &reqs[rsize]));
        if (OMPI_SUCCESS != err) {
            return err;
        }

        err = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, 0,
                                 MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                 &reqs[0]));
        if (OMPI_SUCCESS != err) {
            return err;
        }

        incr = rextent * rcount;
        ptmp = (char *) rbuf + incr;
        for (i = 1; i < rsize; ++i, ptmp += incr) {
            err = MCA_PML_CALL(irecv(ptmp, rcount, rdtype, i,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm, &reqs[i]));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }

        err = ompi_request_wait_all(rsize + 1, reqs, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            return err;
        }

        /* Step 2: exchange the resuts between the root processes */
        tmpbuf = (char *) malloc(scount * size * sextent);
        if (NULL == tmpbuf) {
            return err;
        }

        err = MCA_PML_CALL(isend(rbuf, rsize * rcount, rdtype, 0,
                                 MCA_COLL_BASE_TAG_ALLGATHER,
                                 MCA_PML_BASE_SEND_STANDARD, comm, &req));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(recv(tmpbuf, size * scount, sdtype, 0,
                                MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                MPI_STATUS_IGNORE));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    }


    /* Step 3: bcast the data to the remote group. This 
     * happens in both groups simultaniously, thus we can 
     * not use coll_bcast (this would deadlock). 
     */
    if (rank != root) {
        /* post the recv */
        err = MCA_PML_CALL(recv(rbuf, rsize * rcount, rdtype, 0,
                                MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                MPI_STATUS_IGNORE));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    } else {
        /* Send the data to every other process in the remote group
         * except to rank zero. which has it already. */
        for (i = 1; i < rsize; i++) {
            err = MCA_PML_CALL(isend(tmpbuf, size * scount, sdtype, i,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     comm, &reqs[i - 1]));
            if (OMPI_SUCCESS != err) {
                goto exit;
            }
        }

        err = ompi_request_wait_all(rsize - 1, reqs, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    }

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }

    return err;
}
