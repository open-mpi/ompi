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
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2020-2021 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_PERSIST_PSENDREQ_H
#define PART_PERSIST_PSENDREQ_H

#include "ompi/mca/part/persist/part_persist_request.h"
#include "ompi/mca/part/base/part_base_psendreq.h"
#include "ompi/mca/part/part.h"
#include "opal/prefetch.h"

struct mca_part_persist_psend_request_t {
    mca_part_persist_request_t req_base;
};
typedef struct mca_part_persist_psend_request_t mca_part_persist_psend_request_t;
OBJ_CLASS_DECLARATION(mca_part_persist_psend_request_t);


#define MCA_PART_PERSIST_PSEND_REQUEST_ALLOC(sendreq, comm, dst,          \
                                           ompi_proc)                 \
do {                                                                  \
    sendreq = (mca_part_persist_psend_request_t*)                         \
        opal_free_list_wait (&mca_part_base_psend_requests);          \
    sendreq->req_base.req_type = MCA_PART_PERSIST_REQUEST_PSEND;          \
} while(0)

#define MCA_PART_PERSIST_PSEND_REQUEST_INIT( req_send,                    \
                                         ompi_proc,                   \
                                         comm,                        \
                                         tag,                         \
                                         dst,                         \
                                         datatype,                    \
                                         buf,                         \
                                         parts,                       \
                                         count,                       \
                                         flags )                      \
    do {                                                              \
        OMPI_REQUEST_INIT(&(sendreq->req_base.req_ompi),     \
                          false);                                     \
        OBJ_RETAIN(comm);                                             \
        OMPI_DATATYPE_RETAIN(datatype);                               \
        (req_send)->req_base.req_comm = comm;                         \
        (req_send)->req_base.req_datatype = datatype;                 \
        (req_send)->req_base.req_ompi.req_mpi_object.comm = comm;     \
        (req_send)->req_base.req_ompi.req_status.MPI_SOURCE =         \
        comm->c_my_rank;                                              \
        (req_send)->req_base.req_ompi.req_status.MPI_TAG = tag;       \
        (req_send)->req_base.req_part_complete = true;                  \
        (req_send)->req_base.req_ompi.req_status._ucount = count;     \
        (req_send)->req_base.req_free_called = false;                 \
        (req_send)->req_base.req_addr = buf;                       /**< pointer to application buffer */\
        (req_send)->req_base.req_parts = parts;                /**< number of partitions */\
        (req_send)->req_base.req_count = count;                     /**< count of user datatype elements */\
        (req_send)->req_base.req_peer = dst;                     /**< peer process - rank w/in this communicator */\
        (req_send)->req_base.req_tag = tag;                      /**< user defined tag */\
    } while(0)

/*
 * Release resources associated with a request
 */
#define MCA_PART_PERSIST_PSEND_REQUEST_RETURN(sendreq)                      \
    {                                                                   \
        /*  Let the base handle the reference counts */                 \
        OMPI_DATATYPE_RELEASE(sendreq->req_datatype);  \
        OBJ_RELEASE(sendreq->req_comm);               \
        OMPI_REQUEST_FINI(&sendreq->req_ompi);        \
        opal_convertor_cleanup( &(sendreq->req_convertor) ); \
        opal_free_list_return ( &mca_part_base_psend_requests,            \
                               (opal_free_list_item_t*)sendreq);        \
    }

#endif
