/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_GM_SEND_REQUEST_H
#define MCA_PTL_GM_SEND_REQUEST_H

#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_gm_sendfrag.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION (mca_ptl_gm_send_request_t);

struct mca_ptl_gm_send_request_t {
    mca_pml_base_send_request_t super;
    /* add stuff here */
    mca_ptl_gm_send_frag_t *req_frag;
    int need_ack;
};
typedef struct mca_ptl_gm_send_request_t mca_ptl_gm_send_request_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
