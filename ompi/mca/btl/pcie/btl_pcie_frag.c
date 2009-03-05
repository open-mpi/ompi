/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_pcie_frag.h" 
#include "btl_pcie.h"


static void
mca_btl_pcie_sma_buf_eager_constructor(mca_btl_pcie_sma_buf_t* buf)
{
    buf->pcie_data.pval = buf + 1;
    buf->type = MCA_BTL_PCIE_TYPE_EAGER;
}

static void
mca_btl_pcie_sma_buf_max_constructor(mca_btl_pcie_sma_buf_t* buf)
{
    buf->pcie_data.pval = buf + 1;
    buf->type = MCA_BTL_PCIE_TYPE_MAX;
}

OBJ_CLASS_INSTANCE(mca_btl_pcie_sma_buf_eager_t,
                   ompi_free_list_item_t,
                   mca_btl_pcie_sma_buf_eager_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(mca_btl_pcie_sma_buf_max_t,
                   ompi_free_list_item_t,
                   mca_btl_pcie_sma_buf_max_constructor,
                   NULL);


static void
mca_btl_pcie_frag_dma_constructor(mca_btl_pcie_frag_t* frag) 
{ 
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;

    frag->segment.seg_addr.pval = NULL;
    frag->segment.seg_len = 0;

    frag->endpoint = NULL;
    frag->hdr = NULL;
    frag->size = 0;
    frag->registration = NULL;
    frag->type = MCA_BTL_PCIE_TYPE_RDMA;
    frag->sma_buf = NULL;
}


static void
mca_btl_pcie_frag_common_constructor(mca_btl_pcie_frag_t* frag) 
{ 
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;

    frag->hdr = (mca_btl_pcie_header_t*) (frag + 1);
    frag->hdr->send_frag.pval = frag;

    frag->segment.seg_addr.pval = ((unsigned char*) frag->hdr) + sizeof(mca_btl_pcie_header_t);
    frag->segment.seg_len = frag->size;

    frag->endpoint = NULL;
    frag->registration = NULL;
    frag->sma_buf = NULL;
}

static void
mca_btl_pcie_frag_eager_constructor(mca_btl_pcie_frag_t* frag) 
{ 
    frag->size = mca_btl_pcie_module.super.btl_eager_limit;  
    mca_btl_pcie_frag_common_constructor(frag); 
    frag->type = MCA_BTL_PCIE_TYPE_EAGER;
}

static void mca_btl_pcie_frag_max_constructor(mca_btl_pcie_frag_t* frag) 
{ 
    frag->size = mca_btl_pcie_module.super.btl_max_send_size; 
    mca_btl_pcie_frag_common_constructor(frag); 
    frag->type = MCA_BTL_PCIE_TYPE_MAX;
}


static void mca_btl_pcie_frag_recv_constructor(mca_btl_pcie_frag_t *frag)
{
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;

    frag->segment.seg_addr.pval = NULL;
    frag->segment.seg_len = 0;

    frag->endpoint = NULL;
    frag->hdr = NULL;
    frag->size = 0;
    frag->registration = NULL;
    frag->type = MCA_BTL_PCIE_TYPE_RECV;
    frag->sma_buf = NULL;
}


OBJ_CLASS_INSTANCE(
    mca_btl_pcie_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_pcie_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_pcie_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_pcie_frag_max_constructor, 
    NULL); 


OBJ_CLASS_INSTANCE(
    mca_btl_pcie_frag_recv_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_pcie_frag_recv_constructor,
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_pcie_frag_dma_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_pcie_frag_dma_constructor,
    NULL); 
