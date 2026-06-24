/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_UBCL_REQUEST_H
#define MCA_OSC_UBCL_REQUEST_H

#include <ubcl_api.h>
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"

struct mca_osc_ubcl_request_s {
    opal_free_list_item_t super;
    ompi_request_t ompi_req; /**< Base request */
    struct ompi_win_t *win;

    uint64_t is_request_based : 1;
    uint64_t unused : 63;

    ompi_datatype_t *origin_dt;
    ompi_datatype_t *target_dt;

    opal_convertor_t origin_convertor;

    /* Non contiguous accumulate are segmented */
    size_t segment_count;

    /* Track that all segments are finished before completing the user's request */
    size_t segment_ack;
};
typedef struct mca_osc_ubcl_request_s mca_osc_ubcl_request_t;
OBJ_CLASS_DECLARATION(mca_osc_ubcl_request_t);

/* callback required by ubcl */
void ubcl_request_complete_cb(ubcl_status_t status, void *cb_data);

/**
 * Generic convenient macros
 */
#define MCA_OSC_UBCL_REQUEST_INIT(req, _dst, _origin_dt, _target_dt, _win, _is_request_based) \
    do {                                                                                      \
        OBJ_RETAIN(_win);                                                                     \
        if (NULL != _origin_dt) {                                                             \
            OMPI_DATATYPE_RETAIN(_origin_dt);                                                 \
        }                                                                                     \
        if (NULL != _target_dt) {                                                             \
            OMPI_DATATYPE_RETAIN(_target_dt);                                                 \
        }                                                                                     \
        OMPI_REQUEST_INIT(&(req)->ompi_req, false);                                           \
        (req)->ompi_req.req_state = OMPI_REQUEST_ACTIVE;                                      \
        (req)->origin_dt = _origin_dt;                                                        \
        (req)->target_dt = _target_dt;                                                        \
        (req)->win = _win;                                                                    \
        (req)->is_request_based = _is_request_based;                                          \
        (req)->segment_count = 1;                                                             \
        (req)->segment_ack = 0;                                                               \
        OBJ_CONSTRUCT(&((req)->origin_convertor), opal_convertor_t);                          \
    } while (0)

#define MCA_OSC_UBCL_REQUEST_FINI(req)               \
    do {                                             \
        OBJ_RELEASE((req)->win);                     \
        if (NULL != (req)->origin_dt) {              \
            OMPI_DATATYPE_RELEASE((req)->origin_dt); \
        }                                            \
        if (NULL != (req)->target_dt) {              \
            OMPI_DATATYPE_RELEASE((req)->target_dt); \
        }                                            \
        OBJ_DESTRUCT(&((req)->origin_convertor));    \
    } while (0)

#endif //MCA_OSC_UBCL_REQUEST_H
