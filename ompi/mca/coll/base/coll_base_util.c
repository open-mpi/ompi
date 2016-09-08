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
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/pml/pml.h"
#include "coll_base_util.h"

int ompi_coll_base_sendrecv_nonzero_actual( void* sendbuf, size_t scount,
                                             ompi_datatype_t* sdatatype,
                                             int dest, int stag,
                                             void* recvbuf, size_t rcount,
                                             ompi_datatype_t* rdatatype,
                                             int source, int rtag,
                                             struct ompi_communicator_t* comm,
                                             ompi_status_public_t* status )

{ /* post receive first, then send, then waitall... should be fast (I hope) */
    int err, line = 0;
    size_t rtypesize, stypesize;
    ompi_request_t *req;
    ompi_status_public_t rstatus;

    /* post new irecv */
    ompi_datatype_type_size(rdatatype, &rtypesize);
    if (0 != rcount && 0 != rtypesize) {
        err = MCA_PML_CALL(irecv( recvbuf, rcount, rdatatype, source, rtag,
                                  comm, &req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }
    }

    /* send data to children */
    ompi_datatype_type_size(sdatatype, &stypesize);
    if (0 != scount && 0 != stypesize) {
        err = MCA_PML_CALL(send( sendbuf, scount, sdatatype, dest, stag,
                                  MCA_PML_BASE_SEND_STANDARD, comm));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }
    }

    if (0 != rcount && 0 != rtypesize) {
        err = ompi_request_wait( &req, &rstatus);
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

        if (MPI_STATUS_IGNORE != status) {
            *status = rstatus;
        }
    } else {
        if( MPI_STATUS_IGNORE != status )
            *status = ompi_status_empty;
    }

    return (MPI_SUCCESS);

 error_handler:
    /* Error discovered during the posting of the irecv or send,
     * and no status is available.
     */
    OPAL_OUTPUT ((ompi_coll_base_framework.framework_output, "%s:%d: Error %d occurred\n",
                  __FILE__, line, err));
    if (MPI_STATUS_IGNORE != status) {
        status->MPI_ERROR = err;
    }
    return (err);
}

