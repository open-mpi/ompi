#include "btl_template_frag.h" 



static void mca_btl_template_frag_eager_constructor(mca_btl_template_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->size = mca_btl_template_module.super.btl_eager_limit;  
}

static void mca_btl_template_frag_max_constructor(mca_btl_template_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->size = mca_btl_template_module.super.btl_max_send_size; 
}

static void mca_btl_template_frag_user_constructor(mca_btl_template_frag_t* frag) 
{ 
    frag->size = 0; 
    frag->registration = NULL;
}


OBJ_CLASS_INSTANCE(
    mca_btl_template_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_template_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_template_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_template_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_template_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_template_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_template_frag_user_constructor, 
    NULL); 

