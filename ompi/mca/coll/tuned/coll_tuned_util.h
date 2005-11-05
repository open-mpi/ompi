/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#ifndef MCA_COLL_TUNED_UTIL_EXPORT_H
#define MCA_COLL_TUNED_UTIL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "datatype/datatype.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* prototypes */
int coll_tuned_sendrecv_actual( void* sendbuf, int scount, ompi_datatype_t* sdatatype,
                              int dest, int stag,
                              void* recvbuf, int rcount, ompi_datatype_t* rdatatype,
                              int source, int rtag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status );


/* inline functions */

static inline int coll_tuned_sendrecv( void* sendbuf, int scount, ompi_datatype_t* sdatatype,
                              int dest, int stag,
                              void* recvbuf, int rcount, ompi_datatype_t* rdatatype,
                              int source, int rtag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status, int myid )
{
    if ((dest==myid)&&(source==myid)) {
        return (int) ompi_ddt_sndrcv(sendbuf, (int32_t) scount, sdatatype, recvbuf, (int32_t) rcount, rdatatype);
    }
    else {
        return coll_tuned_sendrecv_actual (sendbuf, scount, sdatatype, dest, stag, recvbuf, rcount, rdatatype,
                                        source, rtag, comm, status);
    }
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_TUNED_UTIL_EXPORT_H */


