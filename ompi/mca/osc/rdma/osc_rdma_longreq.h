/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSC_RDMA_LONGREQ_H
#define OSC_RDMA_LONGREQ_H

#include "osc_rdma.h"

#include "opal/class/opal_free_list.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"

struct ompi_osc_rdma_longreq_t {
    opal_free_list_item_t super;
    ompi_request_t *request;

    union {
        struct ompi_osc_rdma_sendreq_t *req_sendreq;
        struct ompi_osc_rdma_replyreq_t *req_replyreq;
        struct ompi_osc_rdma_send_header_t *req_sendhdr;
    } req_basereq;

    /* warning - this doesn't always have a sane value */
    ompi_osc_rdma_module_t *req_module;

    /* for long receives, to avoid a longrecvreq type */
    struct ompi_op_t *req_op;
    struct ompi_datatype_t *req_datatype;
};
typedef struct ompi_osc_rdma_longreq_t ompi_osc_rdma_longreq_t;
OBJ_CLASS_DECLARATION(ompi_osc_rdma_longreq_t);

static inline int
ompi_osc_rdma_longreq_alloc(ompi_osc_rdma_longreq_t **longreq)
{
    opal_free_list_item_t *item;
    int ret;

    OPAL_FREE_LIST_GET(&mca_osc_rdma_component.c_longreqs,
                       item, ret);

    *longreq = (ompi_osc_rdma_longreq_t*) item;
    return ret;
}

static inline int
ompi_osc_rdma_longreq_free(ompi_osc_rdma_longreq_t *longreq)
{
    OPAL_FREE_LIST_RETURN(&mca_osc_rdma_component.c_longreqs,
                          &longreq->super.super);
    return OMPI_SUCCESS;
}

#endif
