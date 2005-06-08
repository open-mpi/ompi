/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
/**
 * @file
 */
#ifndef MCA_PML_BASE_RECV_REQUEST_H
#define MCA_PML_BASE_RECV_REQUEST_H

#include "mca/pml/base/pml_base_request.h"
#include "datatype/convertor.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Base type for receive requests.
 */
struct mca_pml_base_recv_request_t {
   mca_pml_base_request_t req_base;  /**< base request */
   ompi_convertor_t req_convertor;   /**< convertor that describes this datatype */
   size_t req_bytes_packed;          /**< size of message being received */
};
typedef struct mca_pml_base_recv_request_t mca_pml_base_recv_request_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_base_recv_request_t);

/**
 * Initialize a receive request with call parameters.
 *
 * @param request (IN)       Receive request.
 * @param addr (IN)          User buffer.
 * @param count (IN)         Number of elements of indicated datatype.
 * @param datatype (IN)      User defined datatype.
 * @param src (IN)           Source rank w/in the communicator.
 * @param tag (IN)           User defined tag.
 * @param comm (IN)          Communicator.
 * @param persistent (IN)    Is this a ersistent request.
 */
#define MCA_PML_BASE_RECV_REQUEST_INIT(                                  \
    request,                                                             \
    addr,                                                                \
    count,                                                               \
    datatype,                                                            \
    src,                                                                 \
    tag,                                                                 \
    comm,                                                                \
    persistent)                                                          \
{                                                                        \
    /* increment reference count on communicator */                      \
    OBJ_RETAIN(comm);                                                    \
    OBJ_RETAIN(datatype);                                                \
                                                                         \
    OMPI_REQUEST_INIT(&(request)->req_base.req_ompi);                    \
    (request)->req_bytes_packed = 0;                                     \
    (request)->req_base.req_sequence = 0;                                \
    (request)->req_base.req_addr = addr;                                 \
    (request)->req_base.req_count = count;                               \
    (request)->req_base.req_datatype = datatype;                         \
    (request)->req_base.req_peer = src;                                  \
    (request)->req_base.req_tag = tag;                                   \
    (request)->req_base.req_comm = comm;                                 \
    (request)->req_base.req_proc = NULL;                                 \
    (request)->req_base.req_persistent = persistent;                     \
    (request)->req_base.req_pml_complete = (persistent ? true : false);  \
    (request)->req_base.req_free_called = false;                         \
}

/** 
 *  Return a receive request. Handle the release of the communicator and the
 *  attached datatype.
 *
 *  @param request (IN)     Receive request.
 */
#define MCA_PML_BASE_RECV_REQUEST_FINI( request )         \
    do {                                                  \
        OMPI_REQUEST_FINI(&(request)->req_base.req_ompi); \
        OBJ_RELEASE( (request)->req_base.req_comm);       \
        OBJ_RELEASE( (request)->req_base.req_datatype );  \
    } while (0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

