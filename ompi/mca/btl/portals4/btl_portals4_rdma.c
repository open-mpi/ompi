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
#include "ompi/constants.h"
#include "btl_portals4.h"

int
mca_btl_portals4_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    opal_output(ompi_btl_base_framework.framework_output, "mca_btl_portals4_put not implemented\n");

    MPI_Abort(MPI_COMM_WORLD, 10);
    return OMPI_SUCCESS;
}


int
mca_btl_portals4_get(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_portals4_module_t *portals4_btl = (mca_btl_portals4_module_t *) btl_base;
    mca_btl_portals4_segment_t *src_seg = (mca_btl_portals4_segment_t *) descriptor->des_src;
    mca_btl_portals4_frag_t *frag = (mca_btl_portals4_frag_t*) descriptor;
    ptl_md_t md;
    int ret;

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
	"mca_btl_portals4_get frag=%p src_seg=%p frag->md_h=%d\n", (void *)frag, (void *)src_seg, frag->md_h));

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;

    /* Bind the memory */
    md.start = (void *)frag->segments[0].base.seg_addr.pval;
    md.length = frag->segments[0].base.seg_len;
    md.options = 0;
    md.eq_handle = portals4_btl->recv_eq_h;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(portals4_btl->portals_ni_h,
                    &md,
                    &frag->md_h);

    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    frag->match_bits = src_seg->key;
    frag->length = md.length;
    frag->peer_proc = btl_peer->ptl_proc;
    ret = PtlGet(frag->md_h,
                 0,
                 md.length,
                 btl_peer->ptl_proc,
                 portals4_btl->recv_idx,
                 frag->match_bits, /* match bits */
                 0,
                 frag);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlGet failed: %d",
                            __FILE__, __LINE__, ret);
        PtlMDRelease(frag->md_h);
        frag->md_h = PTL_INVALID_HANDLE;
        return OMPI_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlGet start=%p length=%ld nid=%x pid=%x match_bits=%lx\n",
        md.start, md.length, btl_peer->ptl_proc.phys.nid, btl_peer->ptl_proc.phys.pid, frag->match_bits));

    return OMPI_SUCCESS;
}
