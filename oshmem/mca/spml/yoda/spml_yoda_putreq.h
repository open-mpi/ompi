/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_SPML_YODA_PUT_REQUEST_H
#define OSHMEM_SPML_YODA_PUT_REQUEST_H

#include "opal/mca/btl/btl.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "opal/mca/mpool/base/base.h"
#include "ompi/mca/bml/bml.h"
#include "oshmem/mca/spml/yoda/spml_yoda_rdmafrag.h"
#include "oshmem/mca/spml/yoda/spml_yoda.h"
#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

struct mca_spml_yoda_put_request_t {
    mca_spml_base_put_request_t req_put;
    mca_spml_yoda_rdma_frag_t put_frag;
};

typedef struct mca_spml_yoda_put_request_t mca_spml_yoda_put_request_t;

OBJ_CLASS_DECLARATION(mca_spml_yoda_put_request_t);

static inline mca_spml_yoda_put_request_t *mca_spml_yoda_putreq_alloc(int dst) {
    opal_free_list_item_t *item;
    mca_spml_yoda_put_request_t *putreq;

    item = opal_free_list_wait (&mca_spml_base_put_requests);
    putreq = (mca_spml_yoda_put_request_t*) item;
    assert(putreq);
    putreq->req_put.req_base.req_free_called = false;
    putreq->req_put.req_base.req_oshmem.req_complete = false;

    return putreq;
}

void mca_spml_yoda_put_completion(mca_btl_base_module_t* btl,
                                  struct mca_btl_base_endpoint_t* ep,
                                  struct mca_btl_base_descriptor_t* des,
                                  int status);

void mca_spml_yoda_put_completion_rdma (struct mca_btl_base_module_t* module,
					struct mca_btl_base_endpoint_t* endpoint,
					void *local_address,
					struct mca_btl_base_registration_handle_t *local_handle,
					void *context, void *cbdata, int status);

END_C_DECLS

#endif  /* OSHMEM_SPML_YODA_PUT_REQUEST_H */
