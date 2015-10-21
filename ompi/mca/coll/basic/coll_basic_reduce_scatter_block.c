/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/ompi_datatype.h"
#include "coll_basic.h"
#include "ompi/op/op.h"

#define COMMUTATIVE_LONG_MSG 8 * 1024 * 1024

/*
 *	reduce_scatter_block
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter_block()
 *	Returns:	- MPI_SUCCESS or error code
 *
 * Algorithm:
 *     reduce and scatter (needs to be cleaned
 *     up at some point)
 */
int
mca_coll_basic_reduce_scatter_block_intra(const void *sbuf, void *rbuf, int rcount,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int rank, size, count, err = OMPI_SUCCESS;
    ptrdiff_t extent, buf_size, gap;
    char *recv_buf = NULL, *recv_buf_free = NULL;

    /* Initialize */
    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* short cut the trivial case */
    count = rcount * size;
    if (0 == count) {
        return OMPI_SUCCESS;
    }

    /* get datatype information */
    ompi_datatype_type_extent(dtype, &extent);
    buf_size = opal_datatype_span(&dtype->super, count, &gap);

    /* Handle MPI_IN_PLACE */
    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    if (0 == rank) {
        /* temporary receive buffer.  See coll_basic_reduce.c for
           details on sizing */
        recv_buf_free = (char*) malloc(buf_size);
        recv_buf = recv_buf_free - gap;
        if (NULL == recv_buf_free) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
    }

    /* reduction */
    err =
        comm->c_coll.coll_reduce(sbuf, recv_buf, count, dtype, op, 0,
                                 comm, comm->c_coll.coll_reduce_module);

    /* scatter */
    if (MPI_SUCCESS == err) {
        err = comm->c_coll.coll_scatter(recv_buf, rcount, dtype,
                                        rbuf, rcount, dtype, 0,
                                        comm, comm->c_coll.coll_scatter_module);
    }

 cleanup:
    if (NULL != recv_buf_free) free(recv_buf_free);

    return err;
}


/*
 *	reduce_scatter_block_inter
 *
 *	Function:	- reduce/scatter operation
 *	Accepts:	- same arguments as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_reduce_scatter_block_inter(const void *sbuf, void *rbuf, int rcount,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int err, i, rank, root = 0, rsize, lsize;
    int totalcounts;
    ptrdiff_t lb, extent;
    char *tmpbuf = NULL, *tmpbuf2 = NULL;
    ompi_request_t *req;

    rank = ompi_comm_rank(comm);
    rsize = ompi_comm_remote_size(comm);
    lsize = ompi_comm_size(comm);

    totalcounts = lsize * rcount;

    /*
     * The following code basically does an interreduce followed by a
     * intrascatter.  This is implemented by having the roots of each
     * group exchange their sbuf.  Then, the roots receive the data
     * from each of the remote ranks and execute the reduce.  When
     * this is complete, they have the reduced data available to them
     * for doing the scatter.  They do this on the local communicator
     * associated with the intercommunicator.
     *
     * Note: There are other ways to implement MPI_Reduce_scatter_block on
     * intercommunicators.  For example, one could do a MPI_Reduce locally,
     * then send the results to the other root which could scatter it.
     *
     */
    if (rank == root) {
        err = ompi_datatype_get_extent(dtype, &lb, &extent);
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

        err = ompi_request_wait( &req, MPI_STATUS_IGNORE);
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

    /* Now do a scatterv on the local communicator */
    err = comm->c_local_comm->c_coll.coll_scatter(tmpbuf2, rcount, dtype,
				   rbuf, rcount, dtype, 0,
				   comm->c_local_comm,
				   comm->c_local_comm->c_coll.coll_scatter_module);

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }

    if (NULL != tmpbuf2) {
        free(tmpbuf2);
    }

    return err;
}
