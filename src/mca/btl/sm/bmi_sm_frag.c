#include "bmi_sm_frag.h"


static inline void mca_bmi_sm_frag_constructor(mca_bmi_sm_frag_t* frag)
{
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len = frag->size;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
}

static void mca_bmi_sm_frag1_constructor(mca_bmi_sm_frag_t* frag)
{
    frag->size = mca_bmi_sm_component.eager_limit;
    mca_bmi_sm_frag_constructor(frag);
}

static void mca_bmi_sm_frag2_constructor(mca_bmi_sm_frag_t* frag)
{
    frag->size = mca_bmi_sm_component.max_frag_size;
    mca_bmi_sm_frag_constructor(frag);
}

static void mca_bmi_sm_frag_destructor(mca_bmi_sm_frag_t* frag)
{
}


OBJ_CLASS_INSTANCE(
    mca_bmi_sm_frag_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_sm_frag_constructor,
    mca_bmi_sm_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_bmi_sm_frag1_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_sm_frag1_constructor,
    mca_bmi_sm_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_bmi_sm_frag2_t,
    mca_bmi_base_descriptor_t,
    mca_bmi_sm_frag2_constructor,
    mca_bmi_sm_frag_destructor);

