/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_UCX_REQUEST_H
#define OMPI_OSC_UCX_REQUEST_H

#include "ompi/request/request.h"


enum req_type {
    ACCUMULATE_REQ,
    RPUT_REQ,
    RGET_REQ
};

enum acc_rma_type {
    NONE,
    ACCUMULATE,
    GET_ACCUMULATE,
    ANY
};

enum acc_phases {
    ACC_INIT,
    ACC_GET_RESULTS_DATA,
    ACC_GET_STAGE_DATA,
    ACC_PUT_TARGET_DATA,
    ACC_FINALIZE
};

typedef struct ompi_osc_ucx_request {
    ompi_request_t super;
    int request_type;
    ompi_osc_ucx_module_t *module;
} ompi_osc_ucx_request_t;

typedef struct ompi_osc_ucx_generic_request {
    ompi_osc_ucx_request_t super;
} ompi_osc_ucx_generic_request_t;

typedef struct ompi_osc_ucx_accumulate_request {
    ompi_osc_ucx_request_t super;
    struct ompi_op_t *op;
    int phase;
    int acc_type;
    bool lock_acquired;
    int target;
    struct ompi_win_t *win;
    const void *origin_addr;
    int origin_count;
    struct ompi_datatype_t *origin_dt;
    void *stage_addr;
    int stage_count;
    struct ompi_datatype_t *stage_dt;
    struct ompi_datatype_t *target_dt;
    int target_disp;
    int target_count;
    void *free_ptr;
} ompi_osc_ucx_accumulate_request_t;

OBJ_CLASS_DECLARATION(ompi_osc_ucx_request_t);
OBJ_CLASS_DECLARATION(ompi_osc_ucx_generic_request_t);
OBJ_CLASS_DECLARATION(ompi_osc_ucx_accumulate_request_t);

#define OMPI_OSC_UCX_GENERIC_REQUEST_ALLOC(win, req, _req_type)                         \
    do {                                                                                \
        opal_free_list_item_t *item;                                                    \
        do {                                                                            \
            item = opal_free_list_get(&mca_osc_ucx_component.requests);                 \
            if (item == NULL) {                                                         \
                if (module->ctx->num_incomplete_req_ops > 0) {                          \
                    opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);        \
                }                                                                       \
            }                                                                           \
        } while (item == NULL);                                                         \
        req = (ompi_osc_ucx_generic_request_t*) item;                                   \
        OMPI_REQUEST_INIT(&req->super.super, false);                                          \
        req->super.super.req_mpi_object.win = win;                                            \
        req->super.super.req_complete = false;                                                \
        req->super.super.req_state = OMPI_REQUEST_ACTIVE;                                     \
        req->super.super.req_status.MPI_ERROR = MPI_SUCCESS;                                  \
        req->super.module = NULL;                                                             \
        req->super.request_type = _req_type;                                                  \
    } while (0)

#define OMPI_OSC_UCX_ACCUMULATE_REQUEST_ALLOC(win, req)                                 \
    do {                                                                                \
        opal_free_list_item_t *item;                                                    \
        do {                                                                            \
            item = opal_free_list_get(&mca_osc_ucx_component.accumulate_requests);      \
            if (item == NULL) {                                                         \
                if (module->ctx->num_incomplete_req_ops > 0) {                          \
                    opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);        \
                }                                                                       \
            }                                                                           \
        } while (item == NULL);                                                         \
        req = (ompi_osc_ucx_accumulate_request_t*) item;                                \
        OMPI_REQUEST_INIT(&req->super.super, false);                                          \
        req->super.super.req_mpi_object.win = win;                                            \
        req->super.super.req_complete = false;                                                \
        req->super.super.req_state = OMPI_REQUEST_ACTIVE;                                     \
        req->super.super.req_status.MPI_ERROR = MPI_SUCCESS;                                  \
        req->super.module = NULL;                                                             \
        req->super.request_type = ACCUMULATE_REQ;                                             \
        req->acc_type = NONE;                                                   \
        req->op = MPI_NO_OP;                                                    \
        req->phase = ACC_INIT;                                                  \
        req->target = -1;                                                       \
        req->lock_acquired = false;                                             \
        req->win = NULL;                                                        \
        req->origin_addr = NULL;                                                \
        req->origin_count = 0;                                                  \
        req->origin_dt = NULL;                                                  \
        req->stage_addr = NULL;                                                 \
        req->stage_count = 0;                                                   \
        req->stage_dt = NULL;                                                   \
        req->target_dt = NULL;                                                  \
        req->target_count = 0;                                                  \
        req->target_disp = 0;                                                   \
        req->free_ptr = NULL;                                                   \
    } while (0)

#define OMPI_OSC_UCX_REQUEST_RETURN(req)                                                \
    do {                                                                                \
        OMPI_REQUEST_FINI(&req->super.super);                                           \
        if (req->super.request_type == ACCUMULATE_REQ) {                                \
            opal_free_list_return (&mca_osc_ucx_component.accumulate_requests,          \
                                   (opal_free_list_item_t*) req);                       \
        } else {                                                                        \
            opal_free_list_return (&mca_osc_ucx_component.requests,                     \
                                   (opal_free_list_item_t*) req);                       \
        }                                                                               \
    } while (0)

void ompi_osc_ucx_req_completion(void *request);

#endif /* OMPI_OSC_UCX_REQUEST_H */
