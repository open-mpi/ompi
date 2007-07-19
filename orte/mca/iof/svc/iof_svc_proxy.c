/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
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
static void orte_iof_svc_proxy_ack(const orte_process_name_t*, orte_iof_base_msg_header_t*, bool do_close);
static void orte_iof_svc_proxy_pub(const orte_process_name_t*, orte_iof_base_pub_header_t*);
static void orte_iof_svc_proxy_unpub(const orte_process_name_t*, orte_iof_base_pub_header_t*);
static void orte_iof_svc_proxy_sub(const orte_process_name_t*, orte_iof_base_sub_header_t*);
static void orte_iof_svc_proxy_unsub(const orte_process_name_t*, orte_iof_base_sub_header_t*);



/**
 *  Callback function from RML on receipt of IOF request.
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
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_MSG\n");
            orte_iof_svc_proxy_msg(peer, &hdr->hdr_msg,
                ((unsigned char*)iov[0].iov_base)+sizeof(orte_iof_base_header_t));
            break;
        case ORTE_IOF_BASE_HDR_ACK:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_ACK\n");
            orte_iof_svc_proxy_ack(peer, &hdr->hdr_msg, false);
            break;
        case ORTE_IOF_BASE_HDR_CLOSE:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_CLOSE\n");
            orte_iof_svc_proxy_ack(peer, &hdr->hdr_msg, true);
            break;
        case ORTE_IOF_BASE_HDR_PUB:
            ORTE_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_PUB\n");
            orte_iof_svc_proxy_pub(peer, &hdr->hdr_pub);
            break;
        case ORTE_IOF_BASE_HDR_UNPUB:
            ORTE_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_UNPUB\n");
            orte_iof_svc_proxy_unpub(peer, &hdr->hdr_pub);
            break;
        case ORTE_IOF_BASE_HDR_SUB:
            ORTE_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_SUB\n");
            orte_iof_svc_proxy_sub(peer, &hdr->hdr_sub);
            break;
        case ORTE_IOF_BASE_HDR_UNSUB:
            ORTE_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_proxy_recv: HDR_UNSUB\n");
            orte_iof_svc_proxy_unsub(peer, &hdr->hdr_sub);
            break;
        default:
            opal_output(orte_iof_base.iof_output,
                        "orte_iof_svc_recv: invalid message type: %d (ignored)\n",
                        hdr->hdr_common.hdr_type);
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
static void ack_send_cb(
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
    const orte_process_name_t* peer,
    orte_iof_base_msg_header_t* hdr, 
    unsigned char* data)
{
    opal_list_item_t* item;
    bool forward = false;
    opal_output(orte_iof_base.iof_output,
                "orte_iof_svc_proxy_msg: tag %d seq %d",
                hdr->msg_tag,hdr->msg_seq);

    /* dispatch based on subscription list */
    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;

        /* if the tags don't match, skip this subscription */
        if(sub->origin_tag != hdr->msg_tag && hdr->msg_tag != ORTE_IOF_ANY) {
            continue;
        }

        /* if the subscription origin doesn't match the message's
           origin, skip this subscription */
        if(orte_ns.compare_fields(sub->origin_mask,&sub->origin_name,&hdr->msg_origin) == 0) {
            opal_output(orte_iof_base.iof_output, "sub origin [%lu,%lu,%lu], msg origin [%lu,%lu,%lu], msg proxy [%lu,%lu,%lu] orte_iof_svc_proxy_msg: tag %d sequence %d, len %d\n",
                        ORTE_NAME_ARGS(&sub->origin_name),
                        ORTE_NAME_ARGS(&hdr->msg_origin),
                        ORTE_NAME_ARGS(&hdr->msg_proxy),
                        hdr->msg_tag, hdr->msg_seq, hdr->msg_len);
            /* Everthing matched -- forward the message */
            OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
            orte_iof_svc_sub_forward(sub,peer,hdr,data,&forward);
            OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);

    /* If there was no one to forward to, then we effectively drop it.
       But ACK it so that the sender doesn't block. */
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
            ack_send_cb,
            frag);
        if(rc < 0) {
            ORTE_ERROR_LOG(rc);
        }
    }
}


static void orte_iof_svc_proxy_ack(
    const orte_process_name_t* peer,
    orte_iof_base_msg_header_t* hdr,
    bool do_close)
{
    orte_iof_svc_sub_ack(peer, hdr, do_close);
}

/**
 * A remote process has announced that it has an endpoint that it is
 * making available.  Create an entry for it, and also check to see if
 * the new publication matches any pending subscriptions.
 */

static void orte_iof_svc_proxy_pub(
    const orte_process_name_t* peer,
    orte_iof_base_pub_header_t* hdr)
{
    int rc;
    opal_output(orte_iof_base.iof_output, "orte_iof_svc_proxy_pub: mask %d, tag %d, proc [%lu,%lu,%lu], proxy [%lu,%lu,%lu]",
                hdr->pub_mask, hdr->pub_tag, 
                ORTE_NAME_ARGS(&hdr->pub_name),
                ORTE_NAME_ARGS(&hdr->pub_proxy));

    rc = orte_iof_svc_pub_create(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 * Opposite of publish -- remove any previous publications and unmap
 * it from any subscriptions that were using it.
 */

static void orte_iof_svc_proxy_unpub(
    const orte_process_name_t* peer,
    orte_iof_base_pub_header_t* hdr)
{
    int rc;
    opal_output(orte_iof_base.iof_output, "orte_iof_svc_proxy_unpub: mask %d, tag %d, proc [%lu,%lu,%lu], proxy [%lu,%lu,%lu]",
                hdr->pub_mask, hdr->pub_tag, 
                ORTE_NAME_ARGS(&hdr->pub_name),
                ORTE_NAME_ARGS(&hdr->pub_proxy));

    rc = orte_iof_svc_pub_delete(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if (ORTE_SUCCESS != rc && ORTE_ERR_NOT_FOUND != rc) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 * Create a subscription entry. A subscription entry determines the
 * set of origin(s) that will forward to any matching published
 * endpoint targets.
 */

static void orte_iof_svc_proxy_sub(
    const orte_process_name_t* peer,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    opal_output(orte_iof_base.iof_output, "orte_iof_svc_proxy_sub");

    rc = orte_iof_svc_sub_create(
        &hdr->origin_name,
        hdr->origin_mask,
        hdr->origin_tag,
        &hdr->target_name,
        hdr->target_mask,
        hdr->target_tag);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 * Remove a subscription.
 */

static void orte_iof_svc_proxy_unsub(
    const orte_process_name_t* peer,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    opal_output(orte_iof_base.iof_output, "orte_iof_svc_proxy_unsub");

    rc = orte_iof_svc_sub_delete(
        &hdr->origin_name,
        hdr->origin_mask,
        hdr->origin_tag,
        &hdr->target_name,
        hdr->target_mask,
        hdr->target_tag);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
}

