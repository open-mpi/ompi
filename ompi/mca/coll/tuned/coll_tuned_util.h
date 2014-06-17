/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
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

#ifndef MCA_COLL_TUNED_UTIL_EXPORT_H
#define MCA_COLL_TUNED_UTIL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

/**
 * A MPI_like function doing a send and a receive simultaneously.
 * If one of the communications results in a zero-byte message the
 * communication is ignored, and no message will cross to the peer.
 */
int ompi_coll_tuned_sendrecv_nonzero_actual( void* sendbuf, size_t scount, 
                                             ompi_datatype_t* sdatatype,
                                             int dest, int stag,
                                             void* recvbuf, size_t rcount, 
                                             ompi_datatype_t* rdatatype,
                                             int source, int rtag,
                                             struct ompi_communicator_t* comm,
                                             ompi_status_public_t* status );


/**
 * Similar to the function above this implementation of send-receive
 * do not generate communications for zero-bytes messages. Thus, it is
 * improper to use in the context of some algorithms for collective
 * communications.
 */
static inline int
ompi_coll_tuned_sendrecv( void* sendbuf, size_t scount, ompi_datatype_t* sdatatype,
                          int dest, int stag,
                          void* recvbuf, size_t rcount, ompi_datatype_t* rdatatype,
                          int source, int rtag, 
                          struct ompi_communicator_t* comm,
                          ompi_status_public_t* status, int myid )
{
    if ((dest == source) && (source == myid)) {
        return (int) ompi_datatype_sndrcv(sendbuf, (int32_t) scount, sdatatype, 
                                          recvbuf, (int32_t) rcount, rdatatype);
    }
    return ompi_coll_tuned_sendrecv_nonzero_actual (sendbuf, scount, sdatatype, 
                                            dest, stag, 
                                            recvbuf, rcount, rdatatype,
                                            source, rtag, comm, status);
}

END_C_DECLS
#endif /* MCA_COLL_TUNED_UTIL_EXPORT_H */


