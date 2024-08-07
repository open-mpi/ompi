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
 * Copyright (c) 2017-2022 IBM Corporation.  All rights reserved.
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
#include "coll_basic.h"


/*
 *	scatterv_intra
 *
 *	Function:	- scatterv operation
 *	Accepts:	- same arguments as MPI_Scatterv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_scatterv_intra(const void *sbuf, ompi_count_array_t scounts,
                              ompi_disp_array_t disps, struct ompi_datatype_t *sdtype,
                              void *rbuf, size_t rcount,
                              struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    char *ptmp;
    ptrdiff_t lb, extent;
    size_t sdsize;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */

    if (rank != root) {
        size_t rdsize;
        ompi_datatype_type_size(rdtype, &rdsize);
        /* Only receive if there is something to receive */
        if (rcount > 0 && rdsize > 0) {
            return MCA_PML_CALL(recv(rbuf, rcount, rdtype,
                                     root, MCA_COLL_BASE_TAG_SCATTERV,
                                     comm, MPI_STATUS_IGNORE));
        }
        return MPI_SUCCESS;
    }

    ompi_datatype_type_size(sdtype, &sdsize);
    if (OPAL_UNLIKELY(0 == sdsize)) {
        /* bozzo case */
        return MPI_SUCCESS;
    }

    /* I am the root, loop sending data. */

    err = ompi_datatype_get_extent(sdtype, &lb, &extent);
    if (OMPI_SUCCESS != err) {
        return OMPI_ERROR;
    }

    for (i = 0; i < size; ++i) {
        ptmp = ((char *) sbuf) + (extent * ompi_disp_array_get(disps, i));

        /* simple optimization */

        if (i == rank) {
            /* simple optimization or a local operation */
            if (ompi_count_array_get(scounts, i) > 0 && MPI_IN_PLACE != rbuf) {
                err = ompi_datatype_sndrcv(ptmp, ompi_count_array_get(scounts, i), sdtype, rbuf, rcount,
                                      rdtype);
                if (MPI_SUCCESS != err) {
                    return err;
                }
            }
        } else {
            /* Only send if there is something to send */
            if (ompi_count_array_get(scounts, i) > 0) {
                err = MCA_PML_CALL(send(ptmp, ompi_count_array_get(scounts, i), sdtype, i,
                                        MCA_COLL_BASE_TAG_SCATTERV,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err) {
                    return err;
                }
            }
        }
    }

    /* All done */

    return MPI_SUCCESS;
}


/*
 *	scatterv_inter
 *
 *	Function:	- scatterv operation
 *	Accepts:	- same arguments as MPI_Scatterv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_scatterv_inter(const void *sbuf, ompi_count_array_t scounts,
                              ompi_disp_array_t disps, struct ompi_datatype_t *sdtype,
                              void *rbuf, size_t rcount,
                              struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int i, size, err;
    char *ptmp;
    ptrdiff_t lb, extent;
    ompi_request_t **reqs;

    /* Initialize */
    size = ompi_comm_remote_size(comm);

    /* If not root, receive data.  Note that we will only get here if
     * rcount > 0 or rank == root. */

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* If not root, receive data. */
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype,
                                root, MCA_COLL_BASE_TAG_SCATTERV,
                                comm, MPI_STATUS_IGNORE));
    } else {
        /* I am the root, loop sending data. */
        err = ompi_datatype_get_extent(sdtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
        if( NULL == reqs ) { return OMPI_ERR_OUT_OF_RESOURCE; }

        for (i = 0; i < size; ++i) {
            ptmp = ((char *) sbuf) + (extent * ompi_disp_array_get(disps, i));
            err = MCA_PML_CALL(isend(ptmp, ompi_count_array_get(scounts, i), sdtype, i,
                                     MCA_COLL_BASE_TAG_SCATTERV,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     &(reqs[i])));
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
