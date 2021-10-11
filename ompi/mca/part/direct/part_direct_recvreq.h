/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2021 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_PERSIST_RECVREQ_H
#define PART_PERSIST_RECVREQ_H

#include "ompi/mca/part/persist/part_persist_request.h"
#include "ompi/mca/part/base/part_base_precvreq.h"

struct mca_part_persist_precv_request_t {
    mca_part_persist_request_t req_base;
};
typedef struct mca_part_persist_precv_request_t mca_part_persist_precv_request_t;
OBJ_CLASS_DECLARATION(mca_part_persist_precv_request_t);

/**
 *  Allocate a recv request from the modules free list.
 *
 *  @param rc (OUT)  OMPI_SUCCESS or error status on failure.
 *  @return          Receive request.
 */
#define MCA_PART_PERSIST_PRECV_REQUEST_ALLOC(precvreq)                           \
do {                                                                         \
    precvreq = (mca_part_persist_precv_request_t*)                               \
      opal_free_list_get (&mca_part_base_precv_requests);                    \
    precvreq->req_base.req_type = MCA_PART_PERSIST_REQUEST_PRECV;            \
 } while (0)

/**
 * Initialize a receive request with call parameters.
 *
 * @param request (IN)       Receive request.
 * @param addr (IN)          User buffer.
 * @param count (IN)         Number of elements of indicated datatype.
 * @param datatype (IN)      User defined datatype.
 * @param src (IN)           Source rank w/in the communicator.
 * @param comm (IN)          Communicator.
 * @param persistent (IN)    Is this a ersistent request.
 */
#define MCA_PART_PERSIST_PRECV_REQUEST_INIT( request,                     \
                                         ompi_proc,                   \
                                         comm,                        \
                                         tag,                         \
                                         src,                         \
                                         datatype,                    \
                                         addr,                        \
                                         parts,                       \
                                         count,                       \
                                         flags )                      \
do {                                                                  \
        OBJ_RETAIN(comm);                                             \
        OMPI_DATATYPE_RETAIN(datatype);                               \
        (request)->req_base.req_comm = comm;                         \
        (request)->req_base.req_datatype = datatype;                 \
        (request)->req_base.req_ompi.req_mpi_object.comm = comm;     \
        (request)->req_base.req_ompi.req_status.MPI_SOURCE = src;    \
        (request)->req_base.req_ompi.req_status.MPI_TAG = tag;       \
        (request)->req_base.req_part_complete = true;                \
        (request)->req_base.req_ompi.req_status._ucount = count;     \
        (request)->req_base.req_free_called = false;                 \
        (request)->req_base.req_addr = addr;                       /**< pointer to application buffer */\
        (request)->req_base.req_parts = parts;                /**< number of partitions */\
        (request)->req_base.req_count = count;                     /**< count of user datatype elements */\
        (request)->req_base.req_peer = src;                     /**< peer process - rank w/in this communicator */\
        (request)->req_base.req_tag = tag;                           \
} while(0)

/**
 *  Free the PART receive request
 */
#define MCA_PART_PERSIST_PRECV_REQUEST_RETURN(recvreq)                      \
{                                                                       \
    OBJ_RELEASE((recvreq)->req_comm);                          \
    OMPI_DATATYPE_RELEASE((recvreq)->req_datatype);            \
    OMPI_REQUEST_FINI(&(recvreq)->req_ompi);                   \
    opal_convertor_cleanup( &((recvreq)->req_convertor) );     \
    opal_free_list_return ( &mca_part_base_precv_requests,               \
                           (opal_free_list_item_t*)(recvreq));          \
}

#endif


