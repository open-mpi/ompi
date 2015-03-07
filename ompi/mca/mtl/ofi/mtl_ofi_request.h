/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_OFI_REQUEST_H
#define OMPI_MTL_OFI_REQUEST_H

#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/mtl.h"

#define TO_OFI_REQ(_ptr_ctx) \
    container_of((_ptr_ctx), struct ompi_mtl_ofi_request_t, ctx)

struct ompi_mtl_ofi_message_t;

typedef enum {
    OMPI_MTL_OFI_SEND,
    OMPI_MTL_OFI_RECV,
    OMPI_MTL_OFI_ACK,
    OMPI_MTL_OFI_PROBE
} ompi_mtl_ofi_request_type_t;

struct ompi_mtl_ofi_request_t;

struct ompi_mtl_ofi_request_t {
    struct mca_mtl_request_t super;

    /** OFI Request type */
    ompi_mtl_ofi_request_type_t type;

    /** OFI context */
    struct fi_context ctx;

    /** Completion count used by blocking and/or synchronous operations */
    volatile int completion_count;

    /** Event callback */
    int (*event_callback)(struct fi_cq_tagged_entry *wc,
                          struct ompi_mtl_ofi_request_t*);

    /** Error callback */
    int (*error_callback)(struct fi_cq_err_entry *error,
                          struct ompi_mtl_ofi_request_t*);

    /** Request status */
    struct ompi_status_public_t status;

    /** Match state used by Probe */
    int match_state;

    /** Associated message */
    struct ompi_mtl_ofi_message_t *message;

    /** Reference to the communicator used to  */
    /*  lookup source of an ANY_SOURCE Recv    */
    struct ompi_communicator_t *comm;

    /** Pack buffer */
    void *buffer;

    /** Pack buffer size */
    size_t length;

    /** Pack buffer convertor */
    struct opal_convertor_t *convertor;

    /** Flag to prevent MPI_Cancel from cancelling a started Recv request */
    volatile bool req_started;

    /** Request's tag used in case of an error. */
    uint64_t match_bits;

    /** Remote OFI address used when a Recv needs to be ACKed */
    fi_addr_t remote_addr;

    /** Parent request which needs to be ACKed (e.g. Synchronous Send) */
    struct ompi_mtl_ofi_request_t *parent;
};
typedef struct ompi_mtl_ofi_request_t ompi_mtl_ofi_request_t;

#endif
