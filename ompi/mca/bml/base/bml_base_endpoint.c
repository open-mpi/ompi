/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/sys/atomic.h"
#include "ompi/constants.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/proc/proc.h"



static void mca_bml_base_endpoint_construct(mca_bml_base_endpoint_t* ep)
{
    ep->btl_pipeline_send_length = 0;
    ep->btl_send_limit = 0;

    OBJ_CONSTRUCT(&ep->btl_eager, mca_bml_base_btl_array_t);
    OBJ_CONSTRUCT(&ep->btl_send,  mca_bml_base_btl_array_t);
    OBJ_CONSTRUCT(&ep->btl_rdma,  mca_bml_base_btl_array_t);
}


static void mca_bml_base_endpoint_destruct(mca_bml_base_endpoint_t* ep)
{
    OBJ_DESTRUCT(&ep->btl_eager);
    OBJ_DESTRUCT(&ep->btl_send);
    OBJ_DESTRUCT(&ep->btl_rdma);
}


OBJ_CLASS_INSTANCE(
    mca_bml_base_endpoint_t,
    opal_object_t,
    mca_bml_base_endpoint_construct,
    mca_bml_base_endpoint_destruct
);


mca_bml_base_endpoint_t *mca_bml_base_endpoint_create (ompi_proc_t *proc, int *status)
{
    mca_bml_base_endpoint_t *endpoint = mca_bml_base_endpoint_peek (proc);
    int rc;

    assert (NULL != status);

    /* Another thread may have wired this peer since our caller peeked. */
    if (NULL != endpoint) {
        *status = OMPI_SUCCESS;
        return endpoint;
    }

    /* add_proc selects BTLs from the peer's locality, which a proc
     * created on demand does not have yet. In a heterogeneous build
     * this also seeds the peer's architecture, hence its convertor:
     * that comes from the same blob the BTL keys live in, so a failure
     * here would repeat inside add_proc. Report it now rather than hand
     * out an endpoint we would pack for with the local convertor. */
    rc = ompi_proc_complete_init_single (proc);
    if (OMPI_SUCCESS != rc) {
        *status = rc;
        return NULL;
    }

    OPAL_THREAD_LOCK(&mca_bml_lock);
    endpoint = mca_bml_base_endpoint_peek (proc);
    if (NULL == endpoint) {
        rc = mca_bml.bml_add_proc (proc);
        endpoint = mca_bml_base_endpoint_peek (proc);
        if (NULL != endpoint) {
            /* add_proc can report a per-BTL failure and still publish a
             * usable endpoint built from the BTLs that did claim the
             * peer. */
            rc = OMPI_SUCCESS;
        } else if (OMPI_SUCCESS == rc) {
            /* add_proc claimed success without publishing an endpoint. */
            rc = OMPI_ERR_UNREACH;
        }
    }
    OPAL_THREAD_UNLOCK(&mca_bml_lock);

    *status = rc;

    return endpoint;
}

