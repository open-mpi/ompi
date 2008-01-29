/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __INCLUDE_VPROTOCOL_PESSIMIST_REQUEST_H_
#define __INCLUDE_VPROTOCOL_PESSIMIST_REQUEST_H_

#include "ompi_config.h"
#include "vprotocol_pessimist_event.h"
#include "ompi/request/request.h"
#include "../base/vprotocol_base_request.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef struct mca_vprotocol_pessimist_request_t {
    ompi_request_free_fn_t pml_req_free;
    vprotocol_pessimist_clock_t reqid;
    /* ompi_request_t *sb_reqs[2]; */
    mca_vprotocol_pessimist_event_t *event;
    
    uintptr_t sb_cursor;
    convertor_advance_fct_t sb_conv_advance;
    uint32_t sb_conv_flags;
} mca_vprotocol_pessimist_request_t;
typedef mca_vprotocol_pessimist_request_t mca_vprotocol_pessimist_recv_request_t;
typedef mca_vprotocol_pessimist_request_t mca_vprotocol_pessimist_send_request_t;

OBJ_CLASS_DECLARATION(mca_vprotocol_pessimist_recv_request_t);
OBJ_CLASS_DECLARATION(mca_vprotocol_pessimist_send_request_t);

#define VPESSIMIST_REQ(req) \
    ((mca_vprotocol_pessimist_request_t *) VPROTOCOL_REQ(req))

#define VPESSIMIST_RECV_REQ(req) \
    ((mca_vprotocol_pessimist_recv_request_t *) VPROTOCOL_RECV_REQ(req))

#define VPESSIMIST_SEND_REQ(req) \
    ((mca_vprotocol_pessimist_send_request_t *) VPROTOCOL_SEND_REQ(req))

#define VPESSIMIST_REQ_INIT(req) do {                                          \
        VPESSIMIST_REQ(req)->reqid = mca_vprotocol_pessimist.clock++;          \
} while(0)

int mca_vprotocol_pessimist_request_free(ompi_request_t **req);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* __INCLUDE_VPROTOCOL_PESSIMIST_REQUEST_H_ */
