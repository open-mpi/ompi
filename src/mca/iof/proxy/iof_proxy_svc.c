#include "ompi_config.h"
#include "util/output.h"
#include "mca/rml/rml.h"
#include "mca/rml/rml_types.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


/*
 * Local function prototypes.
 */

static void orte_iof_proxy_svc_msg(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* msg,
    unsigned char* data);

static void orte_iof_proxy_svc_ack(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* msg);


/*
 *  Publish the availability of a local endpoint
 *  to the servver.
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
    hdr.hdr_pub.pub_proxy = *ORTE_RML_NAME_SELF;
    hdr.hdr_pub.pub_mask = ORTE_NS_CMP_ALL;
    hdr.hdr_pub.pub_tag = tag;
    ORTE_IOF_BASE_HDR_PUB_NTOH(hdr.hdr_pub);

    iov.iov_base = (void*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "orte_iof_proxy_svc_publish: orte_rml.send() failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 * Remove published endpoint from the server.
 */

int orte_iof_proxy_svc_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    int tag)
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_PUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_pub.pub_name = *name;
    hdr.hdr_pub.pub_proxy = *ORTE_RML_NAME_SELF;
    hdr.hdr_pub.pub_mask = mask;
    hdr.hdr_pub.pub_tag = tag;
    ORTE_IOF_BASE_HDR_PUB_NTOH(hdr.hdr_pub);

    iov.iov_base = (void*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "orte_iof_proxy_svc_unpublish: orte_rml.send() failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 *  Subscribe one or more destination process(es) to
 *  one/more source process.
 */

int orte_iof_proxy_svc_subscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    )
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_SUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_sub.src_name = *src_name;
    hdr.hdr_sub.src_mask = src_mask;
    hdr.hdr_sub.src_tag = src_tag;
    hdr.hdr_sub.dst_name = *dst_name;
    hdr.hdr_sub.dst_mask = dst_mask;
    hdr.hdr_sub.dst_tag = dst_tag;
    ORTE_IOF_BASE_HDR_SUB_NTOH(hdr.hdr_sub);

    iov.iov_base = (void*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "orte_iof_proxy_svc_subscribe: orte_rml.send() failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 * Remove subscription message from the server.
 */

int orte_iof_proxy_svc_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    )
{
    orte_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = ORTE_IOF_BASE_HDR_UNSUB;
    hdr.hdr_sub.src_name = *src_name;
    hdr.hdr_sub.src_mask = src_mask;
    hdr.hdr_sub.src_tag = src_tag;
    hdr.hdr_sub.dst_name = *dst_name;
    hdr.hdr_sub.dst_mask = dst_mask;
    hdr.hdr_sub.dst_tag = dst_tag;
    ORTE_IOF_BASE_HDR_SUB_NTOH(hdr.hdr_sub);

    iov.iov_base = (void*)&hdr;
    iov.iov_len = sizeof(hdr);

    rc = orte_rml.send(
        orte_iof_base.iof_service,
        &iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "orte_iof_proxy_svc_unsubscribe: orte_rml.send() failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 *  Handle receipt of data/ack messages from the server
 *  and forward on to the appropriate endpoint.
 */

void orte_iof_proxy_svc_recv(
    int status,
    orte_process_name_t* src,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_header_t* hdr = (orte_iof_base_header_t*)msg->iov_base;
    int rc;

    switch(hdr->hdr_common.hdr_type) {
        case ORTE_IOF_BASE_HDR_MSG:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_proxy_svc_msg(src,&hdr->hdr_msg,(unsigned char*)(hdr+1));
            break;
        case ORTE_IOF_BASE_HDR_ACK:
            ORTE_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            orte_iof_proxy_svc_ack(src,&hdr->hdr_msg);
            break;
        default:
            break;
    }
    free(hdr);
                                                                                                              
    /* repost receive */
    mca_iof_proxy_component.proxy_iov[0].iov_base = NULL;
    mca_iof_proxy_component.proxy_iov[0].iov_len = 0;
                                                                                                              
    rc = orte_rml.recv_nb(
        ORTE_RML_NAME_ANY,
        mca_iof_proxy_component.proxy_iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        ORTE_RML_ALLOC,
        orte_iof_proxy_svc_recv,
        NULL
    );
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_proxy_svc_recv: unable to post non-blocking recv");
        return;
    }
}


/*
 * Forward data message to the matching endpoint.
 */

static void orte_iof_proxy_svc_msg(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* msg,
    unsigned char* data)
{
    orte_iof_base_endpoint_t* endpoint;
    endpoint = orte_iof_base_endpoint_match(ORTE_RML_NAME_ANY, ORTE_NS_CMP_NONE, msg->msg_tag);
    if(endpoint != NULL) {
        orte_iof_base_endpoint_forward(endpoint,src,msg,data);
        OBJ_RELEASE(endpoint);
    }
}

/**
 * Forward ack message to the matching endpoint.
 */

static void orte_iof_proxy_svc_ack(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* msg)
{
    orte_iof_base_endpoint_t* endpoint;
    endpoint = orte_iof_base_endpoint_match(&msg->msg_src, ORTE_NS_CMP_ALL, msg->msg_tag);
    if(endpoint != NULL) {
        orte_iof_base_endpoint_ack(endpoint,msg->msg_seq + msg->msg_len);
        OBJ_RELEASE(endpoint);
    }
}

