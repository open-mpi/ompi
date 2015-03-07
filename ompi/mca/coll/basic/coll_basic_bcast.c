/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
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
#include "coll_basic.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"


/*
 *	bcast_log_intra
 *
 *	Function:	- broadcast using O(log(N)) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_bcast_log_intra(void *buff, int count,
                               struct ompi_datatype_t *datatype, int root,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int i;
    int size;
    int rank;
    int vrank;
    int peer;
    int dim;
    int hibit;
    int mask;
    int err;
    int nreqs;
    ompi_request_t **preq;
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;
    ompi_request_t **reqs = basic_module->mccb_reqs;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    vrank = (rank + size - root) % size;

    dim = comm->c_cube_dim;
    hibit = opal_hibit(vrank, dim);
    if (hibit < 0) {
        return MPI_ERR_OTHER;
    }
    --dim;

    /* Receive data from parent in the tree. */

    if (vrank > 0) {
        peer = ((vrank & ~(1 << hibit)) + root) % size;

        err = MCA_PML_CALL(recv(buff, count, datatype, peer,
                                MCA_COLL_BASE_TAG_BCAST,
                                comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* Send data to the children. */

    err = MPI_SUCCESS;
    preq = reqs;
    nreqs = 0;
    for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
        peer = vrank | mask;
        if (peer < size) {
            peer = (peer + root) % size;
            ++nreqs;

            err = MCA_PML_CALL(isend_init(buff, count, datatype, peer,
                                          MCA_COLL_BASE_TAG_BCAST,
                                          MCA_PML_BASE_SEND_STANDARD,
                                          comm, preq++));
            if (MPI_SUCCESS != err) {
                mca_coll_basic_free_reqs(reqs, nreqs);
                return err;
            }
        }
    }

    /* Start and wait on all requests. */

    if (nreqs > 0) {

        /* Start your engines.  This will never return an error. */

        MCA_PML_CALL(start(nreqs, reqs));

        /* Wait for them all.  If there's an error, note that we don't
         * care what the error was -- just that there *was* an error.
         * The PML will finish all requests, even if one or more of them
         * fail.  i.e., by the end of this call, all the requests are
         * free-able.  So free them anyway -- even if there was an
         * error, and return the error after we free everything. */

        err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);

        /* Free the reqs */

        mca_coll_basic_free_reqs(reqs, nreqs);
    }

    /* All done */

    return err;
}


/*
 *	bcast_lin_inter
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_bcast_lin_inter(void *buff, int count,
                               struct ompi_datatype_t *datatype, int root,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int i;
    int rsize;
    int err;
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;
    ompi_request_t **reqs = basic_module->mccb_reqs;

    rsize = ompi_comm_remote_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* Non-root receive the data. */
        err = MCA_PML_CALL(recv(buff, count, datatype, root,
                                MCA_COLL_BASE_TAG_BCAST, comm,
                                MPI_STATUS_IGNORE));
    } else {
        /* root section */
        for (i = 0; i < rsize; i++) {
            err = MCA_PML_CALL(isend(buff, count, datatype, i,
                                     MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     comm, &(reqs[i])));
            if (OMPI_SUCCESS != err) {
                return err;
            }
        }
        err = ompi_request_wait_all(rsize, reqs, MPI_STATUSES_IGNORE);
    }


    /* All done */
    return err;
}


/*
 *	bcast_log_inter
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_bcast_log_inter(void *buff, int count,
                               struct ompi_datatype_t *datatype, int root,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
