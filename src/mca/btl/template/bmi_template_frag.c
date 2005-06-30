#include "bmi_template_frag.h" 



static void mca_bmi_template_frag_common_constructor(mca_bmi_template_frag_t* frag) 
{ 
    mca_bmi_template_frag_common_constructor(frag); 
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
}

static void mca_bmi_template_frag_eager_constructor(mca_bmi_template_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->size = mca_bmi_template_module.super.bmi_eager_limit;  
    mca_bmi_template_frag_common_constructor(frag); 
}

static void mca_bmi_template_frag_max_constructor(mca_bmi_template_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->size = mca_bmi_template_module.super.bmi_max_send_size; 
    mca_bmi_template_frag_common_constructor(frag); 
}

static void mca_bmi_template_frag_user_constructor(mca_bmi_template_frag_t* frag) 
{ 
    frag->size = 0; 
    mca_bmi_template_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
    mca_bmi_template_frag_t, 
    mca_bmi_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_bmi_template_frag_eager_t, 
    mca_bmi_base_descriptor_t, 
    mca_bmi_template_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_bmi_template_frag_max_t, 
    mca_bmi_base_descriptor_t, 
    mca_bmi_template_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_bmi_template_frag_user_t, 
    mca_bmi_base_descriptor_t, 
    mca_bmi_template_frag_user_constructor, 
    NULL); 

