/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2015.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Huawei Technologies Co., Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef COLL_UCX_REQUEST_H_
#define COLL_UCX_REQUEST_H_

#include "coll_ucx.h"

enum {
    MCA_PML_UCX_REQUEST_FLAG_SEND         = (1 << 0), /* Persistent send */
    MCA_PML_UCX_REQUEST_FLAG_FREE_CALLED  = (1 << 1),
    MCA_PML_UCX_REQUEST_FLAG_COMPLETED    = (1 << 2)
};

struct coll_ucx_persistent_op {
    ompi_request_t  ompi;
    ompi_request_t *tmp_req;
    ucg_coll_h      coll_desc;
    ucg_worker_h    ucg_worker;
    unsigned        flags;
};


void mca_coll_ucx_coll_completion(void *request, ucs_status_t status);

void mca_coll_ucx_pcoll_completion(void *request, ucs_status_t status);

void mca_coll_ucx_persistent_op_complete(mca_coll_ucx_persistent_op_t *preq,
        ompi_request_t *tmp_req);

void mca_coll_ucx_completed_request_init(ompi_request_t *ompi_req);

void mca_coll_ucx_request_init(void *request);

void mca_coll_ucx_request_cleanup(void *request);


static inline void mca_coll_ucx_request_reset(ompi_request_t *req)
{
    req->req_complete          = REQUEST_PENDING;
}

static inline void mca_coll_ucx_set_coll_status(ompi_status_public_t* mpi_status,
                                                ucs_status_t status)
{
    if (OPAL_LIKELY(status == UCS_OK)) {
        mpi_status->MPI_ERROR  = MPI_SUCCESS;
        mpi_status->_cancelled = false;
    } else if (status == UCS_ERR_CANCELED) {
        mpi_status->_cancelled = true;
    } else {
        mpi_status->MPI_ERROR  = MPI_ERR_INTERN;
    }
}

OBJ_CLASS_DECLARATION(mca_coll_ucx_persistent_op_t);


#endif /* COLL_UCX_REQUEST_H_ */
