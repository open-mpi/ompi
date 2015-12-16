/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "btl_portals4.h"

int
mca_btl_portals4_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    opal_output(opal_btl_base_framework.framework_output, "mca_btl_portals4_put not implemented\n");

    BTL_ERROR(("mca_btl_portals4_put not implemented\n"));
    return OPAL_SUCCESS;
}


int
mca_btl_portals4_get(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    void *local_address,
                    uint64_t remote_address,
                    struct mca_btl_base_registration_handle_t *local_handle,
                    struct mca_btl_base_registration_handle_t *remote_handle,
                    size_t size,
                    int flags,
                    int order,
                    mca_btl_base_rdma_completion_fn_t cbfunc,
                    void *cbcontext,
                    void *cbdata)
{
    mca_btl_portals4_module_t *portals4_btl = (mca_btl_portals4_module_t *) btl_base;
    mca_btl_portals4_frag_t   *frag         = NULL;
    ptl_md_t md;
    int ret;

    /* reserve space in the event queue for rdma operations immediately */
    while (OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, 1) >
           portals4_btl->portals_max_outstanding_ops) {
        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (1)\n"));
        mca_btl_portals4_component_progress();
    }

    OPAL_BTL_PORTALS4_FRAG_ALLOC_USER(portals4_btl, frag);
    if (NULL == frag){
        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
        return OPAL_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "mca_btl_portals4_get: Incrementing portals_outstanding_ops=%d frag=%p",
        portals4_btl->portals_outstanding_ops, (void *)frag));

    frag->rdma_cb.func         = cbfunc;
    frag->rdma_cb.context      = cbcontext;
    frag->rdma_cb.data         = cbdata;
    frag->rdma_cb.local_handle = local_handle;

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;

    frag->match_bits = remote_handle->key;
    frag->addr = local_address;
    frag->length = size;
    frag->peer_proc = btl_peer->ptl_proc;

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlGet start=%p length=%ld nid=%x pid=%x match_bits=%lx\n",
        md.start, md.length, btl_peer->ptl_proc.phys.nid, btl_peer->ptl_proc.phys.pid, frag->match_bits));

    ret = PtlGet(portals4_btl->send_md_h,
                 (ptl_size_t) local_address,
                 size,
                 btl_peer->ptl_proc,
                 portals4_btl->recv_idx,
                 frag->match_bits, /* match bits */
                 0,
                 frag);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlGet failed: %d",
                            __FILE__, __LINE__, ret);
        return OPAL_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "SUCCESS: PtlGet start=%p length=%ld nid=%x pid=%x match_bits=%lx\n",
        md.start, md.length, btl_peer->ptl_proc.phys.nid, btl_peer->ptl_proc.phys.pid, frag->match_bits));

    return OPAL_SUCCESS;
}
