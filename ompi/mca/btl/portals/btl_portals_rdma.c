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

#include "ompi/include/constants.h"

#include "btl_portals.h"
#include "btl_portals_frag.h"

int
mca_btl_portals_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;
    frag->type = mca_btl_portals_frag_type_rdma;

    /* setup the send */
    assert(1 == frag->base.des_src_cnt);

    md.start = frag->segments[0].seg_addr.pval;
    md.length = frag->segments[0].seg_len;
    md.threshold = 2; /* unlink after send & ack */
    md.max_size = 0;
    md.options = PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ];

    /* make a free-floater */
    ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
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
                 *((mca_btl_base_endpoint_t*) btl_peer),
                 OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                 0, /* ac_index - not used*/
                 frag->base.des_dst[0].seg_key.key64, /* match bits */
                 0, /* remote offset - not used */
                 MCA_BTL_TAG_MAX); /* hdr_data - invalid tag */
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
                    struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;
    frag->type = mca_btl_portals_frag_type_rdma;

    /* setup the get */
    md.start = frag->segments[0].seg_addr.pval;
    md.length = frag->segments[0].seg_len;
    md.threshold = 2; /* unlink after send & ack */
    md.max_size = 0;
    md.options = PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ];

    /* make a free-floater */
    ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
                    md,
                    PTL_UNLINK,
                    &md_h);
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "PtlMDBind failed with error %d", ret);
        return OMPI_ERROR;
    }

    ret = PtlGet(md_h,
                 *((mca_btl_base_endpoint_t*) btl_peer),
                 OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                 0, /* ac_index - not used*/
                 frag->base.des_dst[0].seg_key.key64, /* match bits */
                 0); /* remote offset - not used */
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "PtlGet failed with error %d", ret);
        PtlMDUnlink(md_h);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
