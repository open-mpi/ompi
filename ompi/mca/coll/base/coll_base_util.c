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
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_tuned_util.h"

int ompi_coll_tuned_sendrecv_nonzero_actual( void* sendbuf, size_t scount, 
                                             ompi_datatype_t* sdatatype,
                                             int dest, int stag,
                                             void* recvbuf, size_t rcount, 
                                             ompi_datatype_t* rdatatype,
                                             int source, int rtag,
                                             struct ompi_communicator_t* comm,
                                             ompi_status_public_t* status )

{ /* post receive first, then send, then waitall... should be fast (I hope) */
    int err, line = 0, nreqs = 0;
    size_t typesize;
    ompi_request_t* reqs[2], **req = reqs;
    ompi_status_public_t statuses[2];

    /* post new irecv */
    ompi_datatype_type_size(rdatatype, &typesize);
    if (0 != rcount && 0 != typesize) {
        err = MCA_PML_CALL(irecv( recvbuf, rcount, rdatatype, source, rtag, 
                                  comm, req++));
        ++nreqs;
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }
    }

    /* send data to children */
    ompi_datatype_type_size(sdatatype, &typesize);
    if (0 != scount && 0 != typesize) {
        err = MCA_PML_CALL(isend( sendbuf, scount, sdatatype, dest, stag, 
                                  MCA_PML_BASE_SEND_STANDARD, comm, req++));
        ++nreqs;
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }
    }

    if (0 != nreqs) {
        err = ompi_request_wait_all( nreqs, reqs, statuses );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

        if (MPI_STATUS_IGNORE != status) {
            *status = statuses[0];
        }
    } else {
        if( MPI_STATUS_IGNORE != status )
            *status = ompi_status_empty;
    }

    return (MPI_SUCCESS);

 error_handler:
    /* As we use wait_all we will get MPI_ERR_IN_STATUS which is not an error
     * code that we can propagate up the stack. Instead, look for the real
     * error code from the MPI_ERROR in the status.
     */
    if( MPI_ERR_IN_STATUS == err ) {
        /* At least we know the error was detected during the wait_all */
        int err_index = 1;
        if( MPI_SUCCESS == statuses[0].MPI_ERROR ) {
            err_index = 0;
        }
        if (MPI_STATUS_IGNORE != status) {
            *status = statuses[err_index];
        }
        err = statuses[err_index].MPI_ERROR;
        OPAL_OUTPUT ((ompi_coll_tuned_stream, "%s:%d: Error %d occurred in the %s"
                                              " stage of ompi_coll_tuned_sendrecv_zero\n",
                      __FILE__, line, err, (0 == err_index ? "receive" : "send")));
    } else {
        /* Error discovered during the posting of the irecv or isend,
         * and no status is available.
         */
        OPAL_OUTPUT ((ompi_coll_tuned_stream, "%s:%d: Error %d occurred\n",
                      __FILE__, line, err));
        if (MPI_STATUS_IGNORE != status) {
            status->MPI_ERROR = err;
        }
    }
    return (err);
}

