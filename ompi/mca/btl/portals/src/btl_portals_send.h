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

#ifndef MCA_BTL_PORTALS_SEND_H
#define MCA_BTL_PORTALS_SEND_H

#include "btl_portals_frag.h"

int mca_btl_portals_process_send(mca_btl_portals_module_t *module, 
                                 ptl_event_t *ev);


static inline int
mca_btl_portals_send_frag(mca_btl_portals_frag_t *frag)
{
    ptl_md_t md;
    ptl_handle_md_t md_h;
    int ret;

    
    /* setup the send */
    md.start = frag->segment.seg_addr.pval;
    md.length = frag->segment.seg_len;
    md.threshold = PTL_MD_THRESH_INF; /* unlink based on protocol */
    md.max_size = 0;
    md.options = 0; /* BWB - can we optimize? */
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = frag->btl->portals_eq_handles[MCA_BTL_PORTALS_EQ_SEND];

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

    ret = PtlPut(md_h,
                 PTL_ACK_REQ,
                 frag->endpoint->endpoint_ptl_id,
                 BTL_PORTALS_SEND_TABLE_ID,
                 0, /* ac_index */
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

#endif /* MCA_BTL_PORTALS_SEND_H */
