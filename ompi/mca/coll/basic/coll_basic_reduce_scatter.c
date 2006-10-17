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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>
#include <errno.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"
#include "ompi/op/op.h"


/*
 *	reduce_scatter
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_reduce_scatter_intra(void *sbuf, void *rbuf, int *rcounts,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm)
{
    int i, err, rank, size, count;
    ptrdiff_t true_lb, true_extent, lb, extent;
    int *disps = NULL;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* Initialize reduce & scatterv info at the root (rank 0). */

    for (i = 0, count = 0; i < size; ++i) {
        if (rcounts[i] < 0) {
            return MPI_ERR_ARG;
        }
        count += rcounts[i];
    }

    if (0 == rank) {
        disps = (int*)malloc((unsigned) size * sizeof(int));
        if (NULL == disps) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* There is lengthy rationale about how this malloc works in
         * coll_basic_reduce.c */

        ompi_ddt_get_extent(dtype, &lb, &extent);
        ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

        free_buffer = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == free_buffer) {
            free(disps);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - lb;

        disps[0] = 0;
        for (i = 0; i < (size - 1); ++i) {
            disps[i + 1] = disps[i] + rcounts[i];
        }
    }

    /* Handle MPI_IN_PLACE */

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    /* reduction */

    err =
        comm->c_coll.coll_reduce(sbuf, pml_buffer, count, dtype, op, 0,
                                 comm);

    /* scatter */

    if (MPI_SUCCESS == err) {
        err = comm->c_coll.coll_scatterv(pml_buffer, rcounts, disps, dtype,
                                         rbuf, rcounts[rank], dtype, 0,
                                         comm);
    }

    /* All done */

    if (NULL != disps) {
        free(disps);
    }
    if (NULL != free_buffer) {
        free(free_buffer);
    }

    return err;
}


/*
 *	reduce_scatter_inter
 *
 *	Function:	- reduce/scatter operation
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_reduce_scatter_inter(void *sbuf, void *rbuf, int *rcounts,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm)
{
    int err, i, rank, root = 0, rsize;
    int totalcounts, tcount;
    ptrdiff_t lb, extent;
    char *tmpbuf = NULL, *tmpbuf2 = NULL, *tbuf = NULL;
    ompi_request_t *req;
    ompi_request_t **reqs = comm->c_coll_basic_data->mccb_reqs;

    rank = ompi_comm_rank(comm);
    rsize = ompi_comm_remote_size(comm);

    /* According to MPI-2, the total sum of elements transfered has to 
     * be identical in both groups. Thus, it is enough to calculate
     * that locally.
     */
    for (totalcounts = 0, i = 0; i < rsize; i++) {
        totalcounts += rcounts[i];
    }

    /* determine result of the remote group, you cannot
     * use coll_reduce for inter-communicators, since than
     * you would need to determine an order between the
     * two groups (e.g. which group is providing the data
     * and which one enters coll_reduce with providing
     * MPI_PROC_NULL as root argument etc.) Here,
     * we execute the data exchange for both groups
     * simultaniously. */
    /*****************************************************************/
    if (rank == root) {
        err = ompi_ddt_get_extent(dtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        tmpbuf = (char *) malloc(totalcounts * extent);
        tmpbuf2 = (char *) malloc(totalcounts * extent);
        if (NULL == tmpbuf || NULL == tmpbuf2) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(isend(sbuf, totalcounts, dtype, 0,
                                 MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                 MCA_PML_BASE_SEND_STANDARD, comm, &req));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(recv(tmpbuf2, totalcounts, dtype, 0,
                                MCA_COLL_BASE_TAG_REDUCE_SCATTER, comm,
                                MPI_STATUS_IGNORE));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }


        /* Loop receiving and calling reduction function (C or Fortran)
         * The result of this reduction operations is then in 
         * tmpbuf2. 
         */
        for (i = 1; i < rsize; i++) {
            err = MCA_PML_CALL(recv(tmpbuf, totalcounts, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE_SCATTER, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                goto exit;
            }

            /* Perform the reduction */
            ompi_op_reduce(op, tmpbuf, tmpbuf2, totalcounts, dtype);
        }
    } else {
        /* If not root, send data to the root. */
        err = MCA_PML_CALL(send(sbuf, totalcounts, dtype, root,
                                MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    }


    /* now we have on one process the result of the remote group. To distribute
     * the data to all processes in the local group, we exchange the data between
     * the two root processes. They then send it to every other process in the 
     * remote group. 
     */
    /***************************************************************************/
    if (rank == root) {
        /* sendrecv between the two roots */
        err = MCA_PML_CALL(irecv(tmpbuf, totalcounts, dtype, 0,
                                 MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                 comm, &req));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(send(tmpbuf2, totalcounts, dtype, 0,
                                MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        /* distribute the data to other processes in remote group.
         * Note that we start from 1 (not from zero), since zero
         * has already the correct data AND we avoid a potential 
         * deadlock here. 
         */
        err = MCA_PML_CALL(irecv(rbuf, rcounts[rank], dtype, root,
                                 MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                 comm, &req));

        tcount = 0;
        for (i = 0; i < rsize; i++) {
            tbuf = (char *) tmpbuf + tcount * extent;
            err = MCA_PML_CALL(isend(tbuf, rcounts[i], dtype, i,
                                     MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     reqs++));
            if (OMPI_SUCCESS != err) {
                goto exit;
            }
            tcount += rcounts[i];
        }

        err =
            ompi_request_wait_all(rsize,
                                  comm->c_coll_basic_data->mccb_reqs,
                                  MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait_all(1, &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    } else {
        err = MCA_PML_CALL(recv(rbuf, rcounts[rank], dtype, root,
                                MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                comm, MPI_STATUS_IGNORE));
    }

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }

    if (NULL != tmpbuf2) {
        free(tmpbuf2);
    }

    return err;
}
