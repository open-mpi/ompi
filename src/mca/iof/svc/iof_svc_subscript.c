#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_fragment.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_publish.h"
#include "iof_svc_subscript.h"



static void orte_iof_svc_subscript_construct(orte_iof_svc_subscript_t* subscript)
{
    subscript->sub_endpoint = NULL;
}


static void orte_iof_svc_subscript_destruct(orte_iof_svc_subscript_t* subscript)
{
    if(subscript->sub_endpoint != NULL)
        OBJ_RELEASE(subscript->sub_endpoint);
}


OBJ_CLASS_INSTANCE(
    orte_iof_svc_subscript_t,
    ompi_list_item_t,
    orte_iof_svc_subscript_construct,
    orte_iof_svc_subscript_destruct);

/**
 *  Create a subscription/forwarding entry.
 */

int orte_iof_svc_subscript_create(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag)
{
    orte_iof_svc_subscript_t* sub = OBJ_NEW(orte_iof_svc_subscript_t);
    sub->src_name = *src_name;
    sub->src_mask = src_mask;
    sub->src_tag = src_tag;
    sub->dst_name = *dst_name;
    sub->dst_mask = dst_mask;
    sub->dst_tag = dst_tag;
    sub->sub_endpoint = orte_iof_base_endpoint_match(&sub->dst_name, sub->dst_mask, sub->dst_tag);
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    ompi_list_append(&mca_iof_svc_component.svc_subscribed, &sub->super);
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return OMPI_SUCCESS;
}

/**
 *  Delete all matching subscriptions.
 */

int orte_iof_svc_subscript_delete(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag)
{
    ompi_list_item_t *item;
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    item =  ompi_list_get_first(&mca_iof_svc_component.svc_subscribed);
    while(item != ompi_list_get_end(&mca_iof_svc_component.svc_subscribed)) {
        ompi_list_item_t* next =  ompi_list_get_next(item);
        orte_iof_svc_subscript_t* sub = (orte_iof_svc_subscript_t*)item;
        if (sub->src_mask == src_mask &&
            orte_ns.compare(sub->src_mask,&sub->src_name,src_name) == 0 &&
            sub->src_tag == src_tag &&
            sub->dst_mask == dst_mask &&
            orte_ns.compare(sub->dst_mask,&sub->dst_name,dst_name) == 0 &&
            sub->dst_tag == dst_tag) {
            ompi_list_remove_item(&mca_iof_svc_component.svc_subscribed, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return OMPI_SUCCESS;
}


/*
 * Callback on send completion. Release send resources (fragment).
 */
                                                                                                        
static void orte_iof_svc_subscript_send_cb(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)frag;
    ORTE_IOF_BASE_FRAG_RETURN(frag);
}

/**
 *  Check for matching endpoints that have been published to the
 *  server. Forward data out each matching endpoint.
 */

int orte_iof_svc_subscript_forward(
    orte_iof_svc_subscript_t* sub,
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data)
{
    ompi_list_item_t* item;
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_published);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_published);
        item =  ompi_list_get_next(item)) {
        orte_iof_svc_publish_t* pub = (orte_iof_svc_publish_t*)item;
        int rc;

        if(sub->dst_tag != pub->pub_tag && pub->pub_tag != ORTE_IOF_ANY)
            continue;

        if(pub->pub_endpoint != NULL) {
            rc = orte_iof_base_endpoint_forward(pub->pub_endpoint,src,hdr,data);
        } else {
            /* forward */
            orte_iof_base_frag_t* frag;
            ORTE_IOF_BASE_FRAG_ALLOC(frag,rc);
            frag->frag_hdr.hdr_msg = *hdr;
            frag->frag_len = frag->frag_hdr.hdr_msg.msg_len;
            frag->frag_iov[1].iov_len = frag->frag_len;
            memcpy(frag->frag_data, data, frag->frag_len);
            rc = mca_oob_send_nb(
                &pub->pub_proxy,
                frag->frag_iov,
                2,
                ORTE_RML_TAG_IOF_SVC,
                0,
                orte_iof_svc_subscript_send_cb,
                frag);
        }
        if(rc != OMPI_SUCCESS) {
            return rc;
        }
    }
    if(sub->sub_endpoint != NULL) {
        return orte_iof_base_endpoint_forward(sub->sub_endpoint,src,hdr,data); 
    }
    return OMPI_SUCCESS;
}

