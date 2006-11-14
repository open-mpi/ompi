/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "orte_config.h"

#include <string.h>

#include "opal/util/output.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/iof/base/iof_base_header.h"
#include "orte/mca/iof/base/iof_base_fragment.h"
#include "orte/mca/errmgr/errmgr.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_pub.h"
#include "iof_svc_sub.h"



/**
 * Subscription onstructor/destructor 
 */

static void orte_iof_svc_sub_construct(orte_iof_svc_sub_t* sub)
{
    sub->sub_endpoint = NULL;
    OBJ_CONSTRUCT(&sub->sub_forward, opal_list_t);
}


static void orte_iof_svc_sub_destruct(orte_iof_svc_sub_t* sub)
{
    opal_list_item_t* item;
    if(sub->sub_endpoint != NULL)
        OBJ_RELEASE(sub->sub_endpoint);
    while(NULL != (item = opal_list_remove_first(&sub->sub_forward))) {
        OBJ_RELEASE(item);
    }
}


OBJ_CLASS_INSTANCE(
    orte_iof_svc_sub_t,
    opal_list_item_t,
    orte_iof_svc_sub_construct,
    orte_iof_svc_sub_destruct);

/**
 *  Create a subscription/forwarding entry.
 */

int orte_iof_svc_sub_create(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag)
{
    orte_iof_svc_sub_t* sub;
    opal_list_item_t* item;

    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(item =  opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  opal_list_get_next(item)) {
        sub = (orte_iof_svc_sub_t*)item;
        if (sub->src_mask == src_mask &&
            orte_ns.compare_fields(sub->src_mask,&sub->src_name,src_name) == 0 &&
            sub->src_tag == src_tag &&
            sub->dst_mask == dst_mask &&
            orte_ns.compare_fields(sub->dst_mask,&sub->dst_name,dst_name) == 0 &&
            sub->dst_tag == dst_tag) {
                OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
                return ORTE_SUCCESS;
        }
    }

    sub = OBJ_NEW(orte_iof_svc_sub_t);
    sub->src_name = *src_name;
    sub->src_mask = src_mask;
    sub->src_tag = src_tag;
    sub->dst_name = *dst_name;
    sub->dst_mask = dst_mask;
    sub->dst_tag = dst_tag;
    sub->sub_endpoint = orte_iof_base_endpoint_match(&sub->dst_name, sub->dst_mask, sub->dst_tag);

    /* search through published endpoints for a match */
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_published);
        item != opal_list_get_end(&mca_iof_svc_component.svc_published);
        item  = opal_list_get_next(item)) {
        orte_iof_svc_pub_t* pub = (orte_iof_svc_pub_t*)item;
        if(orte_iof_svc_fwd_match(sub,pub)) {
            orte_iof_svc_fwd_create(sub,pub);
        }
    }

    opal_list_append(&mca_iof_svc_component.svc_subscribed, &sub->super);
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}

/**
 *  Delete all matching subscriptions.
 */

int orte_iof_svc_sub_delete(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag)
{
    opal_list_item_t *item;
    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    item =  opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
    while(item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed)) {
        opal_list_item_t* next =  opal_list_get_next(item);
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if (sub->src_mask == src_mask &&
            orte_ns.compare_fields(sub->src_mask,&sub->src_name,src_name) == 0 &&
            sub->src_tag == src_tag &&
            sub->dst_mask == dst_mask &&
            orte_ns.compare_fields(sub->dst_mask,&sub->dst_name,dst_name) == 0 &&
            sub->dst_tag == dst_tag) {
            opal_list_remove_item(&mca_iof_svc_component.svc_subscribed, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}


int orte_iof_svc_sub_delete_all(
    const orte_process_name_t *name)
{
    opal_list_item_t *item;
    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    item =  opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
    while(item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed)) {
        opal_list_item_t* next =  opal_list_get_next(item);
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if ((sub->src_mask == ORTE_NS_CMP_ALL &&
             orte_ns.compare_fields(ORTE_NS_CMP_ALL,&sub->src_name,name) == 0) ||
            (sub->dst_mask == ORTE_NS_CMP_ALL &&
             orte_ns.compare_fields(ORTE_NS_CMP_ALL,&sub->dst_name,name) == 0)) {
            opal_list_remove_item(&mca_iof_svc_component.svc_subscribed, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}



/*
 * Callback on send completion. Release send resources (fragment).
 */
                                                                                                        
static void orte_iof_svc_sub_send_cb(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)cbdata;
    ORTE_IOF_BASE_FRAG_RETURN(frag);
    if(status < 0) {
        ORTE_ERROR_LOG(status);
    }
}

/**
 *  Check for matching endpoints that have been published to the
 *  server. Forward data out each matching endpoint.
 */

int orte_iof_svc_sub_forward(
    orte_iof_svc_sub_t* sub,
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data,
    bool *forward)
{
    opal_list_item_t* item;
    for(item  = opal_list_get_first(&sub->sub_forward);
        item != opal_list_get_end(&sub->sub_forward);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_fwd_t* fwd = (orte_iof_svc_fwd_t*)item;
        orte_iof_svc_pub_t* pub = fwd->fwd_pub;
        int rc;

        if(pub->pub_endpoint != NULL) {
            rc = orte_iof_base_endpoint_forward(pub->pub_endpoint,src,hdr,data);
        } else {
            /* forward */
            orte_iof_base_frag_t* frag;
            ORTE_IOF_BASE_FRAG_ALLOC(frag,rc);
            frag->frag_hdr.hdr_msg = *hdr;
            frag->frag_len = frag->frag_hdr.hdr_msg.msg_len;
            frag->frag_iov[0].iov_base = (IOVBASE_TYPE*)&frag->frag_hdr;
            frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
            frag->frag_iov[1].iov_base = (IOVBASE_TYPE*)frag->frag_data;
            frag->frag_iov[1].iov_len = frag->frag_len;
            memcpy(frag->frag_data, data, frag->frag_len);
            ORTE_IOF_BASE_HDR_MSG_NTOH(frag->frag_hdr.hdr_msg);
            rc = mca_oob_send_nb(
                &pub->pub_proxy,
                frag->frag_iov,
                2,
                ORTE_RML_TAG_IOF_SVC,
                0,
                orte_iof_svc_sub_send_cb,
                frag);
        }
        if(rc != ORTE_SUCCESS) {
            return rc;
        }
        *forward = true;
    }
    if(sub->sub_endpoint != NULL) {
        *forward = true;
        return orte_iof_base_endpoint_forward(sub->sub_endpoint,src,hdr,data); 
    }
    return ORTE_SUCCESS;
}


/**
 * I/O Forwarding entry - relates a published endpoint to
 * a subscription.
 */

static void orte_iof_svc_fwd_construct(orte_iof_svc_fwd_t* fwd)
{
    fwd->fwd_pub = NULL;
    OBJ_CONSTRUCT(&fwd->fwd_seq, opal_hash_table_t);
    opal_hash_table_init(&fwd->fwd_seq, 256);
}

static void orte_iof_svc_fwd_destruct(orte_iof_svc_fwd_t* fwd)
{
    if(NULL != fwd->fwd_pub)
        OBJ_RELEASE(fwd->fwd_pub);
    OBJ_DESTRUCT(&fwd->fwd_seq);
}


OBJ_CLASS_INSTANCE(
    orte_iof_svc_fwd_t,
    opal_list_item_t,
    orte_iof_svc_fwd_construct,
    orte_iof_svc_fwd_destruct);

/**
 *  Does the published endpoint match the destination specified
 *  in the subscription?
 */

bool orte_iof_svc_fwd_match(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub)
{
    if (orte_ns.compare_fields(sub->dst_mask,&sub->dst_name,&pub->pub_name) == 0 &&
        sub->src_tag == pub->pub_tag) {
        return true;
    } else {
        return false;
    }
}


/**
 *  Create a forwarding entry
 */

int orte_iof_svc_fwd_create(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub)
{
    orte_iof_svc_fwd_t* fwd = OBJ_NEW(orte_iof_svc_fwd_t);
    if(NULL == fwd) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    OBJ_RETAIN(pub);
    fwd->fwd_pub = pub;
    opal_list_append(&sub->sub_forward, &fwd->super);
    return ORTE_SUCCESS;
}


/**
 *  Remove any forwarding entries that match the
 *  published endpoint.
 */

int orte_iof_svc_fwd_delete(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub)
{
    opal_list_item_t* item;
    for(item =  opal_list_get_first(&sub->sub_forward);
        item != opal_list_get_end(&sub->sub_forward);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_fwd_t* fwd = (orte_iof_svc_fwd_t*)item;
        if(fwd->fwd_pub == pub) {
            opal_list_remove_item(&sub->sub_forward,item);
            OBJ_RELEASE(fwd);
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}


