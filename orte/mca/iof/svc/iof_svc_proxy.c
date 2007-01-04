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
#include "opal/util/output.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/base/iof_base_header.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/iof/base/iof_base_fragment.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/class/orte_proc_table.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_pub.h"
#include "iof_svc_sub.h"


static void orte_iof_svc_proxy_msg(const orte_process_name_t*, orte_iof_base_msg_header_t*, unsigned char*);
static void orte_iof_svc_proxy_ack(const orte_process_name_t*, orte_iof_base_msg_header_t*);
static void orte_iof_svc_proxy_pub(const orte_process_name_t*, orte_iof_base_pub_header_t*);
static void orte_iof_svc_proxy_unpub(const orte_process_name_t*, orte_iof_base_pub_header_t*);
static void orte_iof_svc_proxy_sub(const orte_process_name_t*, orte_iof_base_sub_header_t*);
static void orte_iof_svc_proxy_unsub(const orte_process_name_t*, orte_iof_base_sub_header_t*);



/**
 *  Callback function from OOB on receipt of IOF request.
 *
 *  @param status (IN)  Completion status.
 *  @param peer (IN)    Opaque name of peer process.
 *  @param msg (IN)     Array of iovecs describing user buffers and lengths.
 *  @param count (IN)   Number of elements in iovec array.
 *  @param tag (IN)     User defined tag for matching send/recv.
 *  @param cbdata (IN)  User data.
*/
                                                                                                     
void orte_iof_svc_proxy_recv(
    int status,
    orte_process_name_t* peer,
    struct iovec* iov,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_header_t* hdr = (orte_iof_base_header_t*)iov[0].iov_base;
    if(status < 0) {
        ORTE_ERROR_LOG(status);
        goto done;
    }

    switch(hdr->hdr_common.hdr_type) {
        case ORTE_IOF_BASE_HDR_MSG:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_svc_proxy_msg(peer, &hdr->hdr_msg,
                ((unsigned char*)iov[0].iov_base)+sizeof(orte_iof_base_header_t));
            break;
        case ORTE_IOF_BASE_HDR_ACK:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_svc_proxy_ack(peer, &hdr->hdr_msg);
            break;
        case ORTE_IOF_BASE_HDR_PUB:
            ORTE_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            orte_iof_svc_proxy_pub(peer, &hdr->hdr_pub);
            break;
        case ORTE_IOF_BASE_HDR_UNPUB:
            ORTE_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            orte_iof_svc_proxy_unpub(peer, &hdr->hdr_pub);
            break;
        case ORTE_IOF_BASE_HDR_SUB:
            ORTE_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            orte_iof_svc_proxy_sub(peer, &hdr->hdr_sub);
            break;
        case ORTE_IOF_BASE_HDR_UNSUB:
            ORTE_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            orte_iof_svc_proxy_unsub(peer, &hdr->hdr_sub);
            break;
        default:
            opal_output(0, "orte_iof_svc_recv: invalid message type: %d\n", hdr->hdr_common.hdr_type);
            break;
    }

done:
    free(hdr);

    mca_iof_svc_component.svc_iov[0].iov_base = NULL;
    mca_iof_svc_component.svc_iov[0].iov_len = 0;
}


/**
 * Release resources when ack completed.
 */
static void orte_iof_svc_ack_send_cb(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)cbdata;
    ORTE_IOF_BASE_FRAG_RETURN(frag);
    if(status < 0) {
        ORTE_ERROR_LOG(status);
    }
}
                                                                                                                    
/**
 * Receive a data message. Check the subscription list for a match
 * on the source - and on matches forward to any published endpoints
 * that match the subscriptions destination.
 */

static void orte_iof_svc_proxy_msg(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr, 
    unsigned char* data)
{
    opal_list_item_t* item;
    bool forward = false;
    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_msg: tag %d seq %d\n",hdr->msg_tag,hdr->msg_seq);
    }

    /* dispatch based on subscription list */
    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;

        /* tags match */
        if(sub->src_tag != hdr->msg_tag && hdr->msg_tag != ORTE_IOF_ANY)
            continue;

        /* source match */
        if(orte_ns.compare_fields(sub->src_mask,&sub->src_name,&hdr->msg_src) == 0) {
            if(mca_iof_svc_component.svc_debug > 1) {
                opal_output(0, "[%lu,%lu,%lu] orte_iof_svc_proxy_msg: tag %d sequence %d\n",
                    ORTE_NAME_ARGS(&sub->src_name),hdr->msg_tag,hdr->msg_seq);
            }
            orte_iof_svc_sub_forward(sub,src,hdr,data,&forward);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);

    /* if there is no one to forward to - go ahead and ack */
    if(forward == false) {
        orte_iof_base_frag_t* frag;
        int rc;
                                                                                                                 
        ORTE_IOF_BASE_FRAG_ALLOC(frag,rc);
        if(NULL == frag) {
            ORTE_ERROR_LOG(rc);
            return;
        }
                                                                                                                 
        frag->frag_hdr.hdr_msg = *hdr;
        frag->frag_hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_ACK;
        frag->frag_iov[0].iov_base = (IOVBASE_TYPE*)&frag->frag_hdr;
        frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
        ORTE_IOF_BASE_HDR_MSG_HTON(frag->frag_hdr.hdr_msg);
                                                                                                                 
        rc = orte_rml.send_nb(
            &hdr->msg_proxy,
            frag->frag_iov,
            1,
            ORTE_RML_TAG_IOF_SVC,
            0,
            orte_iof_svc_ack_send_cb,
            frag);
        if(rc < 0) {
            ORTE_ERROR_LOG(rc);
        }
    }
}

/**
 *  Received an acknowledgment from an endpoint - forward on
 *  towards the source if all other endpoints have also
 *  acknowledged the data.
 */

static void orte_iof_svc_proxy_ack(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr)
{
    opal_list_item_t *s_item;
    uint32_t seq_min = hdr->msg_seq + hdr->msg_len;
    union {
        uint32_t uval;
        void *vval;
    } value;

    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_ack");
    }

    /* for each of the subscriptions that match the source of the data:
     * (1) find all forwarding entries that match the source of the ack
     * (2) update their sequence number
     * (3) find the minimum sequence number across all endpoints 
    */
     
    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(s_item =  opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        s_item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        s_item =  opal_list_get_next(s_item)) {

        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)s_item;
        opal_list_item_t *f_item;

        if (orte_ns.compare_fields(sub->src_mask,&sub->src_name,&hdr->msg_src) != 0 ||
            sub->src_tag != hdr->msg_tag) {
            continue;
        }

        /* look for this endpoint in the forwarding table */
        for(f_item =  opal_list_get_first(&sub->sub_forward);
            f_item != opal_list_get_end(&sub->sub_forward);
            f_item =  opal_list_get_next(f_item)) {
            orte_iof_svc_fwd_t* fwd = (orte_iof_svc_fwd_t*)f_item;
            orte_iof_svc_pub_t* pub = fwd->fwd_pub;
            if (orte_ns.compare_fields(pub->pub_mask,&pub->pub_name,src) == 0 ||
                orte_ns.compare_fields(ORTE_NS_CMP_ALL,&pub->pub_proxy,src) == 0) {
                value.uval = hdr->msg_seq + hdr->msg_len;
                orte_hash_table_set_proc(&fwd->fwd_seq,
                                         &hdr->msg_src, &value.vval);
            } else {
                value.vval = orte_hash_table_get_proc(&fwd->fwd_seq,
                                                      &hdr->msg_src);
                if(value.uval < seq_min) {
                    seq_min = value.uval;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);

    /* if all destination endpoints have acknowledged up to this 
     * sequence number ack the source
    */
    if(seq_min == hdr->msg_seq+hdr->msg_len) {
 
        if(orte_ns.compare_fields(ORTE_NS_CMP_ALL,orte_process_info.my_name,&hdr->msg_src) == 0) {
            orte_iof_base_endpoint_t* endpoint;
            /*
             * Local delivery
             */
            endpoint = orte_iof_base_endpoint_match(&hdr->msg_src, ORTE_NS_CMP_ALL, hdr->msg_tag);
            if(endpoint != NULL) {
                orte_iof_base_endpoint_ack(endpoint, hdr->msg_seq + hdr->msg_len);
                OBJ_RELEASE(endpoint);
            }

        } else {

            /*
             * forward on to source 
             */
            orte_iof_base_frag_t* frag;
            int rc;
    
            ORTE_IOF_BASE_FRAG_ALLOC(frag,rc);
            if(NULL == frag) {
                ORTE_ERROR_LOG(rc);
                return;
            }

            frag->frag_hdr.hdr_msg = *hdr;
            frag->frag_iov[0].iov_base = (IOVBASE_TYPE*)&frag->frag_hdr;
            frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
            ORTE_IOF_BASE_HDR_MSG_HTON(frag->frag_hdr.hdr_msg);

            rc = orte_rml.send_nb(
                &hdr->msg_proxy,
                frag->frag_iov,
                    1,
                ORTE_RML_TAG_IOF_SVC,
                0,
                orte_iof_svc_ack_send_cb,
                frag);
            if(rc < 0) {
                ORTE_ERROR_LOG(rc);
            }
        }
    }
}

/**
 *  Create an entry to represent the published endpoint. This
 *  also checks to see if the endpoint matches any pending
 *  subscriptions.
 */

static void orte_iof_svc_proxy_pub(
    const orte_process_name_t* src,
    orte_iof_base_pub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_pub");
    }

    rc = orte_iof_svc_pub_create(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
    }
}

/*
 * 
 */

static void orte_iof_svc_proxy_unpub(
    const orte_process_name_t* src,
    orte_iof_base_pub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_unpub");
    }

    rc = orte_iof_svc_pub_delete(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 * Create a subscription entry. A subscription entry
 * determines the set of source(s) that will forward
 * to any matching published endpoints.
 */

static void orte_iof_svc_proxy_sub(
    const orte_process_name_t* src,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_sub");
    }

    rc = orte_iof_svc_sub_create(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 * Remove a subscription.
 */

static void orte_iof_svc_proxy_unsub(
    const orte_process_name_t* src,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        opal_output(0, "orte_iof_svc_proxy_unsub");
    }

    rc = orte_iof_svc_sub_delete(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
    }
}

