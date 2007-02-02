/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_tuned_util.h"

int ompi_coll_tuned_sendrecv_actual( void* sendbuf, int scount, 
                                     ompi_datatype_t* sdatatype,
                                     int dest, int stag,
                                     void* recvbuf, int rcount, 
                                     ompi_datatype_t* rdatatype,
                                     int source, int rtag,
                                     struct ompi_communicator_t* comm,
                                     ompi_status_public_t* status )

{ /* post receive first, then send, then waitall... should be fast (I hope) */
    int err, line = 0;
    ompi_request_t* reqs[2];
    ompi_status_public_t statuses[2];

    /* post new irecv */
    err = MCA_PML_CALL(irecv( recvbuf, rcount, rdatatype, source, rtag, 
                              comm, &reqs[0]));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    /* send data to children */
    err = MCA_PML_CALL(isend( sendbuf, scount, sdatatype, dest, stag, 
                              MCA_PML_BASE_SEND_STANDARD, comm, &reqs[1]));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    err = ompi_request_wait_all( 2, reqs, statuses );
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    if (MPI_STATUS_IGNORE!=status) {
        *status = statuses[0];
    }
    
    return (MPI_SUCCESS);

 error_handler:
    OPAL_OUTPUT ((ompi_coll_tuned_stream, "%s:%d: Error %d occurred\n",
                  __FILE__,line,err));
    return (err);
}

/*
 * localcompleted version that makes sure the send has completed locally 
 * Currently this is a sync call, but will change to locally completed
 * version when available
 */

int ompi_coll_tuned_sendrecv_actual_localcompleted( void* sendbuf, int scount, 
                                                    ompi_datatype_t* sdatatype, 
                                                    int dest, int stag,
                                                    void* recvbuf, int rcount, 
                                                    ompi_datatype_t* rdatatype, 
                                                    int source, int rtag, 
                                                    struct ompi_communicator_t* comm, 
                                                    ompi_status_public_t* status )

{ /* post receive first, then [local] sync send, then wait... should be fast (I hope) */
    int err, line = 0;
    ompi_request_t* req[2];
    ompi_status_public_t tmpstatus[2];

    /* post new irecv */
    err = MCA_PML_CALL(irecv( recvbuf, rcount, rdatatype, source, rtag, 
                              comm, &(req[0])));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    /* send data to children */
    err = MCA_PML_CALL(isend( sendbuf, scount, sdatatype, dest, stag,
                              MCA_PML_BASE_SEND_SYNCHRONOUS, comm, &(req[1])));
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    err = ompi_request_wait_all( 2, req, tmpstatus );
    if (err != MPI_SUCCESS) { line = __LINE__; goto error_handler; }

    if (MPI_STATUS_IGNORE!=status) {
        *status = tmpstatus[0];
    }

    return (MPI_SUCCESS);

 error_handler:
    OPAL_OUTPUT ((ompi_coll_tuned_stream, "%s:%d: Error %d occurred\n",__FILE__,line,err));
    return (err);
}

