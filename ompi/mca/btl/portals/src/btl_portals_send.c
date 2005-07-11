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

#include "include/constants.h"

#include "btl_portals.h"
#include "btl_portals_send.h"


int
mca_btl_portals_process_send(mca_btl_portals_module_t *module, 
                             ptl_event_t *ev)
{
    mca_btl_portals_frag_t *frag = 
        (mca_btl_portals_frag_t*) ev->md.user_ptr;

    switch (ev->type) {
    case PTL_EVENT_SEND_START:
        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "send: PTL_EVENT_SEND_START for 0x%x",
                            frag);

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to start send event\n");
            frag->base.des_cbfunc(&module->super,
                                  frag->u.send_frag.endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        }
        break;
    case PTL_EVENT_SEND_END:
        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "send: PTL_EVENT_SEND_END for 0x%x",
                            frag);

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to end send event\n");
            frag->base.des_cbfunc(&module->super,
                                  frag->u.send_frag.endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        }
        break;
    case PTL_EVENT_ACK:
        /* ok, this is the real work - the message has been received
           on the other side.  If mlength == 0, that means that we hit
           the reject md and we need to try to retransmit */

        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "send: PTL_EVENT_ACK for 0x%x",
                            frag);

        if (0 == ev->mlength) {
            /* other side did not receive the message */

            /* BWB - implement check for retransmit */
            opal_output(mca_btl_portals_component.portals_output,
                        "message was dropped and retransmits not implemented");
            frag->base.des_cbfunc(&module->super,
                                  frag->u.send_frag.endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        } else {
            /* the other side received the message */
            OPAL_THREAD_ADD32(&module->portals_outstanding_sends, -1);
            /* we're done with the md - return it.  Do this before
               anything else in case the PML releases resources, then
               gets more resources (ie, what's currently in this
               md) */
            PtlMDUnlink(ev->md_handle);

            /* let the PML know we're done... */
            frag->base.des_cbfunc(&module->super,
                                  frag->u.send_frag.endpoint,
                                  &frag->base,
                                  OMPI_SUCCESS);
        }
        break;
    default:
        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "send: unexpected event %d for 0x%x",
                            ev->type, frag);

        break;
    }


    return OMPI_SUCCESS;
}



int
mca_btl_portals_send(struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_module_t *ptl_btl = (mca_btl_portals_module_t*) btl;
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int32_t num_sends;

    frag->u.send_frag.endpoint = endpoint;
    frag->u.send_frag.hdr.tag = tag;
    frag->u.send_frag.btl = ptl_btl;
    
    num_sends = OPAL_THREAD_ADD32(&ptl_btl->portals_outstanding_sends, 1);

    /* BWB - implement check for too many pending messages */
    opal_output_verbose(90, mca_btl_portals_component.portals_output,
                        "send called for frag 0x%x", frag);

    return mca_btl_portals_send_frag(frag);
}
