/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSC_PT2PT_LONGREQ_H
#define OSC_PT2PT_LONGREQ_H

#include "osc_pt2pt.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "ompi/request/request.h"

struct ompi_osc_pt2pt_longreq_t;
typedef struct ompi_osc_pt2pt_longreq_t ompi_osc_pt2pt_longreq_t;

typedef void (*ompi_osc_pt2pt_longreq_comp_cb_t)(ompi_osc_pt2pt_longreq_t *longreq);

struct ompi_osc_pt2pt_longreq_t {
    opal_free_list_item_t super;

    /* warning - this doesn't always have a sane value */
    ompi_osc_pt2pt_module_t *req_module;

    ompi_request_t *req_pml_req;
    ompi_osc_pt2pt_longreq_comp_cb_t req_comp_cb;

    /* general storage place - usually holds a request of some type */
    void *req_comp_cbdata;

    /* for long receives, to avoid a longrecvreq type */
    /* BWB - I don't like this, but I don't want another free list.  What to do? */
    struct ompi_op_t *req_op;
    struct ompi_datatype_t *req_datatype;
};
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_longreq_t);

static inline int
ompi_osc_pt2pt_longreq_alloc(ompi_osc_pt2pt_longreq_t **longreq)
{
    opal_free_list_item_t *item;
    int ret;

    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_longreqs,
                       item, ret);

    *longreq = (ompi_osc_pt2pt_longreq_t*) item;
    return ret;
}

static inline int
ompi_osc_pt2pt_longreq_free(ompi_osc_pt2pt_longreq_t *longreq)
{
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_longreqs,
                          &longreq->super.super);
    return OMPI_SUCCESS;
}

#endif
