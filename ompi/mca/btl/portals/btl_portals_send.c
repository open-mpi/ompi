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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include <assert.h>

#include "ompi/constants.h"
#include "orte/util/output.h"

#include "btl_portals.h"
#include "btl_portals_send.h"


int
mca_btl_portals_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    frag->endpoint = endpoint;
    frag->hdr.tag = tag;

    ORTE_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "PtlPut (send) fragment %lx",
                         (unsigned long) frag));

    if (OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, 1) >
        mca_btl_portals_module.portals_max_outstanding_ops) {
        /* no space - queue and continute */
        orte_output_verbose(50, mca_btl_portals_component.portals_output,
                            "no space for message 0x%lx.  Adding to back of queue",
                            (unsigned long) frag);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        opal_list_append(&(mca_btl_portals_module.portals_queued_sends),
                         (opal_list_item_t*) frag);
    }

    if (frag->md_h == PTL_INVALID_HANDLE) {
        /* setup the send - always describe entire fragment */
        mca_btl_portals_module.md_send.start = frag->segments[0].seg_addr.pval;
        mca_btl_portals_module.md_send.length = 
            0 == frag->size ? frag->segments[0].seg_len : frag->size;
#if OMPI_ENABLE_DEBUG 
        mca_btl_portals_module.md_send.options = 
            PTL_MD_EVENT_START_DISABLE;
#else 
        /* optimized build, we can get rid of the END event */
        /*  if we are using portals ACK's for completion */
        mca_btl_portals_module.md_send.options = 
            (PTL_MD_EVENT_START_DISABLE | 
             (mca_btl_portals_component.portals_need_ack ? PTL_MD_EVENT_END_DISABLE : 0));
#endif
        mca_btl_portals_module.md_send.user_ptr = frag; /* keep a pointer to ourselves */

        /* make a free-floater */
        ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
                        mca_btl_portals_module.md_send,
                        PTL_UNLINK,
                        &frag->md_h);
        if (ret != PTL_OK) {
            orte_output(mca_btl_portals_component.portals_output,
                        "PtlMDBind failed with error %d", ret);
            return OMPI_ERROR;
        }
    } 

    ORTE_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "fragment info:\n"
                         "\tstart: 0x%lx\n"
                         "\tlen: %d",
                         (unsigned long) frag->segments[0].seg_addr.pval,
                         frag->segments[0].seg_len)); 
    
    ret = PtlPutRegion(frag->md_h,                /* memory descriptor */
                       0,                         /* fragment offset */
                       frag->segments[0].seg_len, /* fragment length */
                       (mca_btl_portals_component.portals_need_ack ? PTL_ACK_REQ : PTL_NO_ACK_REQ),
                       *((mca_btl_base_endpoint_t*) endpoint),
                       OMPI_BTL_PORTALS_SEND_TABLE_ID,
                       0,                         /* ac_index - not used */
                       0,                         /* match bits */
                       0,                         /* remote offset - not used */
                       frag->hdr.tag);            /* hdr_data: tag */
    if (ret != PTL_OK) {
        orte_output(mca_btl_portals_component.portals_output,
                    "send: PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
