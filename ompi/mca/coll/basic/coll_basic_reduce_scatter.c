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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>
#include <errno.h>

#include "mpi.h"
#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/ompi_datatype.h"
#include "coll_basic.h"
#include "ompi/op/op.h"

#define COMMUTATIVE_LONG_MSG (8 * 1024 * 1024)

/*
 *	reduce_scatter
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 *
 * Algorithm:
 *   Commutative, reasonable sized messages
 *     recursive halving algorithm (base component)
 *   Others:
 *     reduce and scatterv (base component)
 *
 * NOTE: the recursive halving algorithm needs more memory than reduce
 * and scatterv, so its use is limited to messages under
 * COMMUTATIVE_LONG_MSG bytes to be nice to the application memory wise.
 */
int
mca_coll_basic_reduce_scatter_intra(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module)
{
    int size = ompi_comm_size(comm);
    size_t count = 0;
    ptrdiff_t gap;

    for (int i = 0; i < size; ++i) {
        count += ompi_count_array_get(rcounts, i);
    }

    /* short cut the trivial case */
    if (0 == count) {
        return OMPI_SUCCESS;
    }

    if (ompi_op_is_commute(op) &&
        opal_datatype_span(&dtype->super, count, &gap) < COMMUTATIVE_LONG_MSG) {
        return ompi_coll_base_reduce_scatter_intra_basic_recursivehalving(sbuf, rbuf, rcounts, dtype,
                                                                          op, comm, module);
    }
    return ompi_coll_base_reduce_scatter_intra_nonoverlapping(sbuf, rbuf, rcounts, dtype,
                                                              op, comm, module);
}


/*
 *	reduce_scatter_inter
 *
 *	Function:	- reduce/scatter operation
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_reduce_scatter_inter(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module)
{
    int err, i, rank, root = 0, rsize, lsize, totalcounts;
    char *tmpbuf = NULL, *tmpbuf2 = NULL, *lbuf = NULL, *buf;
    ptrdiff_t gap, span;
    ompi_request_t *req;
    ptrdiff_t *disps = NULL;
    ompi_disp_array_t disps_desc;

    rank = ompi_comm_rank(comm);
    rsize = ompi_comm_remote_size(comm);
    lsize = ompi_comm_size(comm);

    /* Figure out the total amount of data for the reduction. */
    for (totalcounts = 0, i = 0; i < lsize; i++) {
        totalcounts += ompi_count_array_get(rcounts, i);
    }

    /*
     * The following code basically does an interreduce followed by a
     * intrascatterv.  This is implemented by having the roots of each
     * group exchange their sbuf.  Then, the roots receive the data
     * from each of the remote ranks and execute the reduce.  When
     * this is complete, they have the reduced data available to them
     * for doing the scatterv.  They do this on the local communicator
     * associated with the intercommunicator.
     *
     * Note: There are other ways to implement MPI_Reduce_scatter on
     * intercommunicators.  For example, one could do a MPI_Reduce locally,
     * then send the results to the other root which could scatter it.
     *
     * Note: It is also worth pointing out that the rcounts argument
     * represents how the data is going to be scatter locally.  Therefore,
     * its size is the same as the local communicator size.
     */
    if (rank == root) {
        span = opal_datatype_span(&dtype->super, totalcounts, &gap);

        /* Generate displacements for the scatterv part */
        disps = (ptrdiff_t*) malloc(sizeof(ptrdiff_t) * lsize);
        if (NULL == disps) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        disps[0] = 0;
        for (i = 0; i < (lsize - 1); ++i) {
            disps[i + 1] = disps[i] + ompi_count_array_get(rcounts, i);
        }

        tmpbuf = (char *) malloc(span);
        tmpbuf2 = (char *) malloc(span);
        if (NULL == tmpbuf || NULL == tmpbuf2) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        lbuf = tmpbuf - gap;
        buf = tmpbuf2 - gap;

        /* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(isend(sbuf, totalcounts, dtype, 0,
                                 MCA_COLL_BASE_TAG_REDUCE_SCATTER,
                                 MCA_PML_BASE_SEND_STANDARD, comm, &req));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(recv(lbuf, totalcounts, dtype, 0,
                                MCA_COLL_BASE_TAG_REDUCE_SCATTER, comm,
                                MPI_STATUS_IGNORE));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait( &req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }


        /* Loop receiving and calling reduction function (C or Fortran)
         * The result of this reduction operations is then in
         * lbuf.
         */
        for (i = 1; i < rsize; i++) {
            char *tbuf;
            err = MCA_PML_CALL(recv(buf, totalcounts, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE_SCATTER, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                goto exit;
            }

            /* Perform the reduction */
            ompi_op_reduce(op, lbuf, buf, totalcounts, dtype);
            /* swap the buffers */
            tbuf = lbuf; lbuf = buf; buf = tbuf;
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

    /* Now do a scatterv on the local communicator */
    OMPI_DISP_ARRAY_INIT(&disps_desc, disps);
    err = comm->c_local_comm->c_coll->coll_scatterv(lbuf, rcounts, disps_desc, dtype,
                                                   rbuf, ompi_count_array_get(rcounts, rank), dtype, 0,
                                                   comm->c_local_comm,
                                                   comm->c_local_comm->c_coll->coll_scatterv_module);

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }

    if (NULL != tmpbuf2) {
        free(tmpbuf2);
    }

    if (NULL != disps) {
        free(disps);
    }

    return err;
}
