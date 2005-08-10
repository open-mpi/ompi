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


#include "ompi_config.h"

#include <assert.h>

#include "include/constants.h"

#include "btl_portals.h"
#include "btl_portals_send.h"


int
mca_btl_portals_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int32_t num_sends;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    frag->endpoint = endpoint;
    frag->hdr.tag = tag;
    frag->type = mca_btl_portals_frag_type_send;
    
    num_sends = OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_sends, 1);

    /* make sure that we have enough space to send.  This means that
       there is enough space in the event queue for all the events
       that may be deposited by outstanding sends */
    if (num_sends >= mca_btl_portals_module.portals_max_outstanding_sends) {
        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                    "no space for message 0x%x.  Adding to back of queue",
                    frag);
        opal_list_append(&(mca_btl_portals_module.portals_queued_sends),
                         (opal_list_item_t*) frag);
        
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_sends, -1);

        ret = OMPI_SUCCESS;
    } else {
        ptl_handle_md_t md_h;
        int ret;

        /* setup the send */
        mca_btl_portals_module.md_send.start = frag->segment.seg_addr.pval;
        mca_btl_portals_module.md_send.length = frag->segment.seg_len;
        mca_btl_portals_module.md_send.user_ptr = frag; /* keep a pointer to ourselves */

        /* make a free-floater */
        ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
                        mca_btl_portals_module.md_send,
                        PTL_UNLINK,
                        &md_h);
        if (ret != PTL_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "PtlMDBind failed with error %d", ret);
            return OMPI_ERROR;
        }
        
        ret = PtlPut(md_h,
                     PTL_ACK_REQ,
                     *((mca_btl_base_endpoint_t*) endpoint),
                     OMPI_BTL_PORTALS_SEND_TABLE_ID,
                     0, /* ac_index - not used */
                     0, /* match bits */
                     0, /* remote offset - not used */
                     frag->hdr.tag); /* hdr_data - tag */
        if (ret != PTL_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "PtlPut failed with error %d", ret);
            PtlMDUnlink(md_h);
            return OMPI_ERROR;
        }

        return OMPI_SUCCESS;
    }

    return ret;
}
