/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_GM_SEND_REQUEST_H
#define MCA_PTL_GM_SEND_REQUEST_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_gm_sendfrag.h"

OBJ_CLASS_DECLARATION (mca_ptl_gm_send_request_t);


struct mca_ptl_gm_send_request_t {
    mca_pml_base_send_request_t super;
    /* add stuff here */
    mca_ptl_gm_send_frag_t *req_frag;
    int need_ack;
};
typedef struct mca_ptl_gm_send_request_t mca_ptl_gm_send_request_t;

#endif
