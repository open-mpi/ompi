#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


/**
 *
 */

static void mca_iof_proxy_svc_msg(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* msg,
    unsigned char* data);

static void mca_iof_proxy_svc_ack(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* msg);


/**
 *
 */

int mca_iof_proxy_svc_publish(
    const ompi_process_name_t* name,
    int tag)
{
    mca_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = MCA_IOF_BASE_HDR_PUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_pub.pub_name = *name;
    hdr.hdr_pub.pub_proxy = mca_oob_name_self;
    hdr.hdr_pub.pub_mask = OMPI_NS_CMP_ALL;
    hdr.hdr_pub.pub_tag = tag;
    MCA_IOF_BASE_HDR_PUB_NTOH(hdr.hdr_pub);

    iov.iov_base = &hdr;
    iov.iov_len = sizeof(hdr);

    rc = mca_oob_send(
        mca_iof_base.iof_service,
        &iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "mca_iof_proxy_svc_publish: mca_oob_send failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/**
 *
 */

int mca_iof_proxy_svc_unpublish(
    const ompi_process_name_t* name,
    ompi_ns_cmp_bitmask_t mask,
    int tag)
{
    mca_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = MCA_IOF_BASE_HDR_PUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_pub.pub_name = *name;
    hdr.hdr_pub.pub_proxy = mca_oob_name_self;
    hdr.hdr_pub.pub_mask = mask;
    hdr.hdr_pub.pub_tag = tag;
    MCA_IOF_BASE_HDR_PUB_NTOH(hdr.hdr_pub);

    iov.iov_base = &hdr;
    iov.iov_len = sizeof(hdr);

    rc = mca_oob_send(
        mca_iof_base.iof_service,
        &iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "mca_iof_proxy_svc_unpublish: mca_oob_send failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/**
 *
 */

int mca_iof_proxy_svc_subscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    )
{
    mca_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = MCA_IOF_BASE_HDR_SUB;
    hdr.hdr_common.hdr_status = 0;
    hdr.hdr_sub.src_name = *src_name;
    hdr.hdr_sub.src_mask = src_mask;
    hdr.hdr_sub.src_tag = src_tag;
    hdr.hdr_sub.dst_name = *dst_name;
    hdr.hdr_sub.dst_mask = dst_mask;
    hdr.hdr_sub.dst_tag = dst_tag;
    MCA_IOF_BASE_HDR_SUB_NTOH(hdr.hdr_sub);

    iov.iov_base = &hdr;
    iov.iov_len = sizeof(hdr);

    rc = mca_oob_send(
        mca_iof_base.iof_service,
        &iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "mca_iof_proxy_svc_subscribe: mca_oob_send failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


int mca_iof_proxy_svc_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    )
{
    mca_iof_base_header_t hdr;
    struct iovec iov;
    int rc;

    hdr.hdr_common.hdr_type = MCA_IOF_BASE_HDR_UNSUB;
    hdr.hdr_sub.src_name = *src_name;
    hdr.hdr_sub.src_mask = src_mask;
    hdr.hdr_sub.src_tag = src_tag;
    hdr.hdr_sub.dst_name = *dst_name;
    hdr.hdr_sub.dst_mask = dst_mask;
    hdr.hdr_sub.dst_tag = dst_tag;
    MCA_IOF_BASE_HDR_SUB_NTOH(hdr.hdr_sub);

    iov.iov_base = &hdr;
    iov.iov_len = sizeof(hdr);

    rc = mca_oob_send(
        mca_iof_base.iof_service,
        &iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        0);
    if(rc < 0) {
        ompi_output(0, "mca_iof_proxy_svc_unsubscribe: mca_oob_send failed with status=%d\n", rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


void mca_iof_proxy_svc_recv(
    int status,
    ompi_process_name_t* src,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    mca_iof_base_header_t* hdr = (mca_iof_base_header_t*)msg->iov_base;
    int rc;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_IOF_BASE_HDR_MSG:
            MCA_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            mca_iof_proxy_svc_msg(src,&hdr->hdr_msg,(unsigned char*)(hdr+1));
            break;
        case MCA_IOF_BASE_HDR_ACK:
            MCA_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            mca_iof_proxy_svc_ack(src,&hdr->hdr_msg);
            break;
        default:
            break;
    }
    free(hdr);
                                                                                                              
    /* repost receive */
    mca_iof_proxy_component.proxy_iov[0].iov_base = NULL;
    mca_iof_proxy_component.proxy_iov[0].iov_len = 0;
                                                                                                              
    rc = mca_oob_recv_nb(
        MCA_OOB_NAME_ANY,
        mca_iof_proxy_component.proxy_iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        MCA_OOB_ALLOC,
        mca_iof_proxy_svc_recv,
        NULL
    );
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_proxy_svc_recv: unable to post non-blocking recv");
        return;
    }
}


static void mca_iof_proxy_svc_msg(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* msg,
    unsigned char* data)
{
    mca_iof_base_endpoint_t* endpoint;
    endpoint = mca_iof_base_endpoint_match(MCA_OOB_NAME_ANY, OMPI_NS_CMP_NONE, msg->msg_tag);
    if(endpoint != NULL) {
        mca_iof_base_endpoint_forward(endpoint,src,msg,data);
        OBJ_RELEASE(endpoint);
    }
}

/**
 *
 */

static void mca_iof_proxy_svc_ack(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* msg)
{
    mca_iof_base_endpoint_t* endpoint;
    endpoint = mca_iof_base_endpoint_match(&msg->msg_src, OMPI_NS_CMP_ALL, msg->msg_tag);
    if(endpoint != NULL) {
        mca_iof_base_endpoint_ack(endpoint,msg->msg_seq + msg->msg_len);
        OBJ_RELEASE(endpoint);
    }
}

