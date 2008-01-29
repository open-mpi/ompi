/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_pessimist_request.h"
#include "vprotocol_pessimist_eventlog.h"
#include "ompi/mca/pml/base/pml_base_request.h"

static void vprotocol_pessimist_request_construct(mca_pml_base_request_t *req);

OBJ_CLASS_INSTANCE(mca_vprotocol_pessimist_recv_request_t, mca_pml_base_request_t, 
                   vprotocol_pessimist_request_construct, NULL);
OBJ_CLASS_INSTANCE(mca_vprotocol_pessimist_send_request_t, mca_pml_base_request_t, 
                   vprotocol_pessimist_request_construct, NULL);


static void vprotocol_pessimist_request_construct(mca_pml_base_request_t *req)
{
    mca_vprotocol_pessimist_request_t *preq;
    
    preq = VPESSIMIST_REQ(req);
    V_OUTPUT_VERBOSE(250, "pessimist:\treq\tnew\treq=%p\tPreq=%p (aligned to %p)", (void *) req, (void *) preq, (void *) &preq->pml_req_free);
    req->req_ompi.req_status.MPI_SOURCE = -1; /* no matching made flag */
    preq->pml_req_free = req->req_ompi.req_free;
    preq->event = NULL;
/*    preq->sb_reqs[0] = NULL;*/
    assert(preq->pml_req_free == req->req_ompi.req_free); /* detection of aligment issues on different arch */
    req->req_ompi.req_free = mca_vprotocol_pessimist_request_free;
}

int mca_vprotocol_pessimist_request_free(ompi_request_t **req)
{
  mca_pml_base_request_t *pml_req = (mca_pml_base_request_t *) *req; 
  V_OUTPUT_VERBOSE(50, "pessimist:\treq\tfree\t%"PRIpclock"\tpeer %d\ttag %d\tsize %ld", VPESSIMIST_REQ(pml_req)->reqid, pml_req->req_peer, pml_req->req_tag, (long) pml_req->req_count); 
  VPROTOCOL_PESSIMIST_MATCHING_LOG_FINALIZE(pml_req);
  pml_req->req_ompi.req_status.MPI_SOURCE = -1; /* no matching made flag */
  VPROTOCOL_PESSIMIST_SENDER_BASED_FLUSH(pml_req);
  return VPESSIMIST_REQ(pml_req)->pml_req_free(req);
}
