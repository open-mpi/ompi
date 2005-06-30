#include "bmi_self_frag.h"


static inline void mca_bmi_self_frag_constructor(mca_bmi_self_frag_t* frag)
{
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len = frag->size;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
}

static void mca_bmi_self_frag_eager_constructor(mca_bmi_self_frag_t* frag)
{
    frag->size = mca_bmi_self.bmi_eager_limit;
    mca_bmi_self_frag_constructor(frag);
}

static void mca_bmi_self_frag_send_constructor(mca_bmi_self_frag_t* frag)
{
    frag->size = mca_bmi_self.bmi_max_send_size;
    mca_bmi_self_frag_constructor(frag);
}

static void mca_bmi_self_frag_rdma_constructor(mca_bmi_self_frag_t* frag)
{
    frag->size = 0;
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len = frag->size;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
}

static void mca_bmi_self_frag_destructor(mca_bmi_self_frag_t* frag)
{
}


OBJ_CLASS_INSTANCE(
    mca_bmi_self_frag_eager_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_self_frag_eager_constructor,
    mca_bmi_self_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_bmi_self_frag_send_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_self_frag_send_constructor,
    mca_bmi_self_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_bmi_self_frag_rdma_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_self_frag_rdma_constructor,
    mca_bmi_self_frag_destructor);

