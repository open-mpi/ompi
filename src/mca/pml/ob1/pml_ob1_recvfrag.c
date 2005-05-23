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

/**
 * @file
 */

#include "ompi_config.h"

#include "mca/pml/pml.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_match.h"


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_frag_t,
    ompi_list_item_t,
    NULL,
    NULL
);




void mca_pml_ob1_recv_callback(
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_descriptor_t* des,
    void* cbdata)
{
    mca_bmi_base_segment_t* segments = des->des_src;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    if(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) {
        return;
    }

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_GEN2_HDR_TYPE_MATCH:
            mca_pml_ob1_match(bmi,&hdr->hdr_match,segments,des->des_src_cnt);
            break;
        case MCA_PML_GEN2_HDR_TYPE_RNDV:
            mca_pml_ob1_match(bmi,&hdr->hdr_match,segments,des->des_src_cnt);
            break;
        default:
            break;
    }
}
                                                                                                           

