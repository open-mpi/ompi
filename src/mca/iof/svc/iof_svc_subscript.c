#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_fragment.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_publish.h"
#include "iof_svc_subscript.h"



static void mca_iof_svc_subscript_construct(mca_iof_svc_subscript_t* subscript)
{
}


static void mca_iof_svc_subscript_destruct(mca_iof_svc_subscript_t* subscript)
{
}


OBJ_CLASS_INSTANCE(
    mca_iof_svc_subscript_t,
    ompi_list_item_t,
    mca_iof_svc_subscript_construct,
    mca_iof_svc_subscript_destruct);

/**
 *
 */

int mca_iof_svc_subscript_create(
    const ompi_process_name_t *src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    const ompi_process_name_t *dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag)
{
    mca_iof_svc_subscript_t* sub = OBJ_NEW(mca_iof_svc_subscript_t);
    sub->src_name = *src_name;
    sub->src_mask = src_mask;
    sub->src_tag = src_tag;
    sub->dst_name = *dst_name;
    sub->dst_mask = dst_mask;
    sub->dst_tag = dst_tag;
    sub->sub_endpoint = mca_iof_base_endpoint_match(&sub->dst_name, sub->dst_mask, sub->dst_tag);
    ompi_list_append(&mca_iof_svc_component.svc_subscribed, &sub->super);
    return OMPI_SUCCESS;
}

/**
 *
 */
                                                                                                           
int mca_iof_svc_subscript_delete(
    const ompi_process_name_t *src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    const ompi_process_name_t *dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag)
{
    return OMPI_SUCCESS;
}


/*
 *
 */
                                                                                                        
static void mca_iof_svc_subscript_send_cb(
    int status,
    ompi_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    mca_iof_base_frag_t* frag = (mca_iof_base_frag_t*)frag;
    MCA_IOF_BASE_FRAG_RETURN(frag);
}

/**
 *
 */

int mca_iof_svc_subscript_forward(
    mca_iof_svc_subscript_t* sub,
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* hdr,
    const unsigned char* data)
{
    ompi_list_item_t* item;
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_published);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_published);
        item =  ompi_list_get_next(item)) {
        mca_iof_svc_publish_t* pub = (mca_iof_svc_publish_t*)item;
        int rc;

        if(sub->dst_tag != pub->pub_tag && pub->pub_tag != MCA_IOF_ANY)
            continue;

        if(pub->pub_endpoint != NULL) {
            rc = mca_iof_base_endpoint_forward(pub->pub_endpoint,src,hdr,data);
        } else {
            /* forward */
            mca_iof_base_frag_t* frag;
            MCA_IOF_BASE_FRAG_ALLOC(frag,rc);
            frag->frag_hdr.hdr_msg = *hdr;
            frag->frag_len = frag->frag_hdr.hdr_msg.msg_len;
            frag->frag_iov[1].iov_len = frag->frag_len;
            memcpy(frag->frag_data, data, frag->frag_len);
            rc = mca_oob_send_nb(
                &pub->pub_proxy,
                frag->frag_iov,
                2,
                MCA_OOB_TAG_IOF_SVC,
                0,
                mca_iof_svc_subscript_send_cb,
                frag);
        }
        if(rc != OMPI_SUCCESS) {
            return rc;
        }
    }
    if(sub->sub_endpoint != NULL) {
        return mca_iof_base_endpoint_forward(sub->sub_endpoint,src,hdr,data); 
    }
    return OMPI_SUCCESS;
}

