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
#include "btl_portals_rdma.h"
#include "btl_portals_frag.h"


int
mca_btl_portals_process_rdma(mca_btl_portals_module_t *btl, 
                             ptl_event_t *ev)
{
    mca_btl_portals_frag_t *frag = 
        (mca_btl_portals_frag_t*) ev->md.user_ptr;

    switch (ev->type) {
    case PTL_EVENT_SEND_START:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma: PTL_EVENT_SEND_START for 0x%x",
                             frag));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to start rdma send event\n");
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
                             "rdma: PTL_EVENT_SEND_END for 0x%x",
                             frag));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to end rdma send event\n");
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
           on the other side. */
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma: PTL_EVENT_ACK for 0x%x, Ox%x",
                             frag, frag->base.des_cbfunc));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure in rdma send event ack\n");
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        } else {
            assert(ev->mlength == frag->segment.seg_len);

            /* let the PML know we're done... */
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_SUCCESS);
        }
        break;

    case PTL_EVENT_PUT_START:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma: PTL_EVENT_PUT_START for 0x%x",
                             frag));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure in rdma put start\n");
            /* unlink, since we don't expect to get an end */
            PtlMDUnlink(ev->md_handle);
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        }
        break;

    case PTL_EVENT_PUT_END:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma: PTL_EVENT_PUT_END for 0x%x, Ox%x",
                             frag, frag->base.des_cbfunc));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure in rdma put end\n");
            frag->base.des_cbfunc(&btl->super,
                                  frag->endpoint,
                                  &frag->base,
                                  OMPI_ERROR);
        } else {
            assert(ev->mlength == frag->segment.seg_len);
        }
        break;

    default:
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma: unexpected event %d for 0x%x",
                             ev->type, frag));

        break;
    }

    return OMPI_SUCCESS;
}


int
mca_btl_portals_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    int ret;

    frag->endpoint = btl_peer;

    /* setup the send */
    md.start = frag->segment.seg_addr.pval;
    md.length = frag->segment.seg_len;
    md.threshold = 2; /* unlink after send, ack */
    md.max_size = 0;
    md.options = 0; 
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = frag->btl->portals_eq_handles[OMPI_BTL_PORTALS_EQ_RDMA];

    /* make a free-floater */
    ret = PtlMDBind(frag->btl->portals_ni_h,
                    md,
                    PTL_UNLINK,
                    &md_h);
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "PtlMDBind failed with error %d", ret);
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "rdma put called for frag 0x%x, callback 0x%xbits %lld",
                         frag, frag->base.des_cbfunc, frag->base.des_dst[0].seg_key.key64));

    ret = PtlPut(md_h,
                 PTL_ACK_REQ,
                 btl_peer->endpoint_ptl_id,
                 OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                 0, /* ac_index - not used*/
                 frag->base.des_dst[0].seg_key.key64, /* match bits */
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


int
mca_btl_portals_get(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* decriptor)
{
    opal_output(mca_btl_portals_component.portals_output,
                "Warning: call to unimplemented function get()");
    return OMPI_ERR_NOT_IMPLEMENTED;
}
