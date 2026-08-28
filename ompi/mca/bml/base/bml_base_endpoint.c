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
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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

    /* Must precede add_proc: it selects BTLs from the peer's locality,
     * which an on-demand proc does not have yet, and in a heterogeneous
     * build seeds the peer's architecture, hence its convertor. The btls
     * read proc_arch as a plain value -- tcp byte-swaps modex addresses
     * on OPAL_ARCH_ISBIGENDIAN, portals4 refuses a peer whose arch is
     * not ours -- and would see their own architecture if this ran
     * after them. */
    rc = ompi_proc_complete_init_single (proc);
    if (OMPI_SUCCESS != rc) {
        *status = rc;
        return NULL;
    }

    /* add_proc serializes on mca_bml_lock itself, and publishes at most
     * one endpoint per proc however many threads race here. */
    rc = mca_bml.bml_add_proc (proc);
    endpoint = mca_bml_base_endpoint_peek (proc);
    if (NULL != endpoint) {
        /* add_proc can report a per-BTL failure and still publish a
         * usable endpoint built from the btls that claimed the peer. */
        rc = OMPI_SUCCESS;
    } else if (OMPI_SUCCESS == rc) {
        rc = OMPI_ERR_UNREACH;
    }

    *status = rc;

    return endpoint;
}

