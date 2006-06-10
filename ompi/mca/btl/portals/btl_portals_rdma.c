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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/util/output.h"

#include "btl_portals.h"
#include "btl_portals_frag.h"

int
mca_btl_portals_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int ret;

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "PtlPut (rdma) fragment %x, bits %lld", frag,
                         frag->base.des_dst[0].seg_key.key64));

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    assert(frag->md_h != PTL_INVALID_HANDLE);

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;

    /* setup the send */
    assert(1 == frag->base.des_src_cnt);

    ret = PtlPut(frag->md_h,
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
    int ret;

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "PtlGet (rdma) fragment %x, bits %lld", frag,
                         frag->base.des_src[0].seg_key.key64));

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    assert(frag->md_h != PTL_INVALID_HANDLE);

    frag->endpoint = btl_peer;
    frag->hdr.tag = MCA_BTL_TAG_MAX;

    ret = PtlGet(frag->md_h,
                 *((mca_btl_base_endpoint_t*) btl_peer),
                 OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                 0, /* ac_index - not used*/
                 frag->base.des_src[0].seg_key.key64, /* match bits */
                 0); /* remote offset - not used */
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "PtlGet failed with error %d", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
