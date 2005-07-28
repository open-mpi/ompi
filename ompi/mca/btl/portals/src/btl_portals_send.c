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
mca_btl_portals_process_send(mca_btl_portals_module_t *btl, 
                             ptl_event_t *ev)
{
    mca_btl_portals_frag_t *frag = 
        (mca_btl_portals_frag_t*) ev->md.user_ptr;

    switch (ev->type) {
    case PTL_EVENT_SEND_START:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "send: PTL_EVENT_SEND_START for 0x%x",
                             frag));

            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "start threshold: %d", ev->md.threshold);
        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to start send event\n");
            /* unlink, since we don't expect to get an end or ack */
            PtlMDUnlink(ev->md_handle);
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        }
        break;
    case PTL_EVENT_SEND_END:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "send: PTL_EVENT_SEND_END for 0x%x",
                             frag));
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "end threshold: %d", ev->md.threshold);

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to end send event\n");
            /* unlink, since we don't expect to get an ack */
            PtlMDUnlink(ev->md_handle);
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        }
        break;
    case PTL_EVENT_ACK:
        /* ok, this is the real work - the message has been received
           on the other side.  If mlength == 0, that means that we hit
           the reject md and we need to try to retransmit */

        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "send: PTL_EVENT_ACK for 0x%x, Ox%x",
                             frag, frag->base.des_cbfunc));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure in send event ack\n");
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        } else if (0 == ev->mlength) {
            /* other side did not receive the message */

            opal_output(mca_btl_portals_component.portals_output,
                        "message was dropped.  Adding to front of queue list");
            opal_list_prepend(&(btl->portals_queued_sends),
                              (opal_list_item_t*) frag);

        } else {
            /* the other side received the message */
            OPAL_THREAD_ADD32(&btl->portals_outstanding_sends, -1);

            assert(ev->mlength == frag->segment.seg_len);

            /* let the PML know we're done... */
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_SUCCESS);

            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "ack threshold: %d", ev->md.threshold);

            /* see if we can send someone else */
            mca_btl_portals_progress_queued_sends(btl);
        }
        break;
    default:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "send: unexpected event %d for 0x%x",
                             ev->type, frag));

        break;
    }


    return OMPI_SUCCESS;
}



int
mca_btl_portals_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_module_t *btl = (mca_btl_portals_module_t*) btl_base;
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int32_t num_sends;
    int ret;

    frag->endpoint = endpoint;
    frag->hdr.tag = tag;
    
    num_sends = OPAL_THREAD_ADD32(&btl->portals_outstanding_sends, 1);

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                        "send called for frag 0x%x, 0x%x", 
                        frag, frag->base.des_cbfunc));

    if (num_sends >= btl->portals_max_outstanding_sends) {
        opal_output(mca_btl_portals_component.portals_output,
                    "no space for message 0x%x.  Adding to back of queue",
                    frag);
        opal_list_append(&(btl->portals_queued_sends),
                         (opal_list_item_t*) frag);
        
        OPAL_THREAD_ADD32(&btl->portals_outstanding_sends, 1);

        ret = OMPI_SUCCESS;
    } else {
        ret = mca_btl_portals_send_frag(frag);
        /* try to progress some events before we return */
    }

    mca_btl_portals_component_progress();
    return ret;
}
