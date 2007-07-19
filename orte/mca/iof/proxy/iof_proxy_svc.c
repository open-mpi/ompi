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
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_header.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/errmgr/errmgr.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


/*
 * Local function prototypes.
 */

static void orte_iof_proxy_svc_msg(
    const orte_process_name_t* origin,
    orte_iof_base_msg_header_t* msg,
    unsigned char* data);

static void orte_iof_proxy_svc_ack(
    const orte_process_name_t* origin,
    orte_iof_base_msg_header_t* msg);


/*
 * Send a "publish" request to the svc component
 */

int orte_iof_proxy_svc_publish(
    const orte_process_name_t* name,
    int tag)
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_PUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_pub.pub_name = *name;
    hdr.hdr_pub.pub_proxy = *ORTE_PROC_MY_NAME;
    hdr.hdr_pub.pub_mask = ORTE_NS_CMP_ALL;
    hdr.hdr_pub.pub_tag = tag;
    ORTE_IOF_BASE_HDR_PUB_HTON(hdr.hdr_pub);

    iov.iov_base = (IOVBASE_TYPE*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * Send an "unpublish" request to the svc component
 */

int orte_iof_proxy_svc_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    int tag)
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_UNPUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_pub.pub_name = *name;
    hdr.hdr_pub.pub_proxy = *ORTE_PROC_MY_NAME;
    hdr.hdr_pub.pub_mask = mask;
    hdr.hdr_pub.pub_tag = tag;
    ORTE_IOF_BASE_HDR_PUB_HTON(hdr.hdr_pub);

    iov.iov_base = (IOVBASE_TYPE*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * Send a "subscribe" request to the svc component
 */

int orte_iof_proxy_svc_subscribe(
    const orte_process_name_t* origin_name,
    orte_ns_cmp_bitmask_t origin_mask,
    int origin_tag,
    const orte_process_name_t* target_name,
    orte_ns_cmp_bitmask_t target_mask,
    int target_tag
    )
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_SUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_sub.origin_name = *origin_name;
    hdr.hdr_sub.origin_mask = origin_mask;
    hdr.hdr_sub.origin_tag = origin_tag;
    hdr.hdr_sub.target_name = *target_name;
    hdr.hdr_sub.target_mask = target_mask;
    hdr.hdr_sub.target_tag = target_tag;
    ORTE_IOF_BASE_HDR_SUB_HTON(hdr.hdr_sub);

    iov.iov_base = (IOVBASE_TYPE*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * Send an "unsubscribe" request to the svc component
 */

int orte_iof_proxy_svc_unsubscribe(
    const orte_process_name_t* origin_name,
    orte_ns_cmp_bitmask_t origin_mask,
    int origin_tag,
    const orte_process_name_t* target_name,
    orte_ns_cmp_bitmask_t target_mask,
    int target_tag
    )
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_UNSUB;
    hdr.hdr_common.hdr_reserve = (uint8_t)0;
    hdr.hdr_common.hdr_status = (int16_t)0;
    hdr.hdr_sub.origin_name = *origin_name;
    hdr.hdr_sub.origin_mask = origin_mask;
    hdr.hdr_sub.origin_tag = origin_tag;
    hdr.hdr_sub.target_name = *target_name;
    hdr.hdr_sub.target_mask = target_mask;
    hdr.hdr_sub.target_tag = target_tag;
    ORTE_IOF_BASE_HDR_SUB_HTON(hdr.hdr_sub);

    iov.iov_base = (IOVBASE_TYPE*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * Receive messages via the RML from the svc component.
 */

void orte_iof_proxy_svc_recv(
    int status,
    orte_process_name_t* origin,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_header_t* hdr = (orte_iof_base_header_t*)msg->iov_base;
    if(NULL == msg->iov_base) {
        opal_output(orte_iof_base.iof_output,
                    "orte_iof_proxy_svc_recv: invalid message\n");
        return;
    }

    /* We only receive 2 types of messages from the svc component:

       - Messages: containing forwarded data intended to be consumed
         by endpoints in this process either representing local fd's
         or pipes to proxied processes (e.g., orted's fronting ORTE
         processes)

       - ACKs: acknowledging data sent from this process to the svc
         component (which may have been forwarded on to other
         processes).
    */

    switch(hdr->hdr_common.hdr_type) {
        case ORTE_IOF_BASE_HDR_MSG:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_proxy_svc_msg(origin,&hdr->hdr_msg,(unsigned char*)(hdr+1));
            break;
        case ORTE_IOF_BASE_HDR_ACK:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_proxy_svc_ack(origin,&hdr->hdr_msg);
            break;
        default:
            break;
    }
    free(hdr);

    /* reset the data in the RML receive */
    mca_iof_proxy_component.proxy_iov[0].iov_base = NULL;
    mca_iof_proxy_component.proxy_iov[0].iov_len = 0;
}


/*
 * The svc component has sent data to us that matches a tag that we
 * must have previously published.  Forward the data to the
 * corresponding endpoint.
 */

static void orte_iof_proxy_svc_msg(
    const orte_process_name_t* origin,
    orte_iof_base_msg_header_t* msg,
    unsigned char* data)
{
    orte_iof_base_endpoint_t* endpoint;

    /* Look for the endpoint corresponding to the tag in the message.
       If we don't find the endpoint, this means that we have already
       unpublished the endpoint and this message must have already
       been enroute to us when we unpublished.  So just discard it. */
    endpoint = orte_iof_base_endpoint_match(ORTE_NAME_WILDCARD, ORTE_NS_CMP_NONE, msg->msg_tag);
    if (NULL != endpoint) {
        orte_iof_base_endpoint_forward(endpoint,origin,msg,data);
        /* RELEASE the endpoint because endpoint_match() RETAINed it */
        OBJ_RELEASE(endpoint);
    }
}

/*
 * The svc component has sent an ACK to us that matches a tag that we
 * must have previously published.  Forward the ACK to the
 * corresponding endpoint.
 */

static void orte_iof_proxy_svc_ack(
    const orte_process_name_t* origin,
    orte_iof_base_msg_header_t* msg)
{
    orte_iof_base_endpoint_t* endpoint;
    /* Look for the endpoint corresponding to the tag in the ACK.  If
       we don't find the endpoint, this means that we have already
       unpublished the endpoint and this ACK must have already been
       enroute to us when we unpublished.  So just discard it. */
    endpoint = orte_iof_base_endpoint_match(&msg->msg_origin, ORTE_NS_CMP_ALL, msg->msg_tag);
    if(endpoint != NULL) {
        orte_iof_base_endpoint_ack(endpoint,msg->msg_seq + msg->msg_len);
        /* RELEASE the endpoint because endpoint_match() RETAINed it */
        OBJ_RELEASE(endpoint);
    }
}

