/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __VPROTOCOL_PESSIMIST_WAIT_H__
#define __VPROTOCOL_PESSIMIST_WAIT_H__

#include "ompi_config.h"
#include "vprotocol_pessimist.h"
#include "ompi/request/request.h"

BEGIN_C_DECLS

#define VPROTOCOL_PESSIMIST_WAIT(req, status, rc) \
  ((rc) = ompi_request_wait(req, status))

int mca_vprotocol_pessimist_test(ompi_request_t ** rptr, int *completed,
                                 ompi_status_public_t * status);

int mca_vprotocol_pessimist_test_all(size_t count, ompi_request_t ** requests,
                                     int *completed, 
                                     ompi_status_public_t * statuses);

int mca_vprotocol_pessimist_test_any(size_t count, ompi_request_t ** requests,
                                     int *index, int *completed,
                                     ompi_status_public_t * status);

int mca_vprotocol_pessimist_test_some(size_t count, ompi_request_t ** requests,
                                      int * outcount, int * indices,
                                      ompi_status_public_t * statuses);

int mca_vprotocol_pessimist_wait_any(size_t count, ompi_request_t ** requests, 
                                     int *index, ompi_status_public_t * status);

int mca_vprotocol_pessimist_wait_some(size_t count, ompi_request_t ** requests, 
                                      int *outcount, int *indexes, 
                                      ompi_status_public_t * statuses);

END_C_DECLS

#endif /* __VPROTOCOL_PESSIMIST_WAIT_H__ */
















#if 0
**** Previous implementation of wait, now outdated but kept for future references *****
int vprotocol_pessimist_request_null_free(ompi_request_t **req);


do { \
    if(*(req) == MPI_REQUEST_NULL) (rc) = ompi_request_wait(req, status); \
        else \
        { \
            mca_pml_base_request_t *pml_req = (mca_pml_base_request_t *) *(req); \
            ompi_request_free_fn_t free_fn = pml_req->req_ompi.req_free; \
            pml_req->req_ompi.req_free = vprotocol_pessimist_request_null_free; \
            V_OUTPUT_VERBOSE(50, "pessimist:\twait\tdeliver\t%d:%llx\tpeer %d\ttag %d\tsize %d", pml_req->req_comm->c_contextid, pml_req->req_sequence, pml_req->req_peer, pml_req->req_tag, pml_req->req_count); \
            (rc) = ompi_request_wait(req, status); \
            VPROTOCOL_PESSIMIST_MATCHING_LOG(pml_req); \
            pml_req->req_ompi.req_free = free_fn; \
            ompi_request_free(req); \
        } \
} while(0)

OMPI_DECLSPEC int mca_vprotocol_pessimist_wait(ompi_request_t **request, ompi_status_public_t *status);
OMPI_DECLSPEC int mca_vprotocol_pessimist_wait_all(size_t count, ompi_request_t ** requests, ompi_status_public_t * statuses);
#endif 
