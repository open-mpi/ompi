#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "mca/iof/base/iof_base_fragment.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_publish.h"
#include "iof_svc_subscript.h"


static void mca_iof_svc_proxy_msg(const ompi_process_name_t*, mca_iof_base_msg_header_t*, unsigned char*);
static void mca_iof_svc_proxy_ack(const ompi_process_name_t*, mca_iof_base_msg_header_t*);
static void mca_iof_svc_proxy_pub(const ompi_process_name_t*, mca_iof_base_pub_header_t*);
static void mca_iof_svc_proxy_unpub(const ompi_process_name_t*, mca_iof_base_pub_header_t*);
static void mca_iof_svc_proxy_sub(const ompi_process_name_t*, mca_iof_base_sub_header_t*);
static void mca_iof_svc_proxy_unsub(const ompi_process_name_t*, mca_iof_base_sub_header_t*);



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
                                                                                                     
void mca_iof_svc_proxy_recv(
    int status,
    ompi_process_name_t* peer,
    struct iovec* iov,
    int count,
    int tag,
    void* cbdata)
{
    int rc;
    mca_iof_base_header_t* hdr = (mca_iof_base_header_t*)iov[0].iov_base;

    if(status < 0) {
        ompi_output(0, "mca_iof_svc_recv: receive failed with status: %d", status);
        goto done;
    }

    switch(hdr->hdr_common.hdr_type) {
        case MCA_IOF_BASE_HDR_MSG:
            MCA_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            mca_iof_svc_proxy_msg(peer, &hdr->hdr_msg,
                ((unsigned char*)iov[0].iov_base)+sizeof(mca_iof_base_header_t));
            break;
        case MCA_IOF_BASE_HDR_ACK:
            MCA_IOF_BASE_HDR_MSG_NTOH(hdr->hdr_msg);
            mca_iof_svc_proxy_ack(peer, &hdr->hdr_msg);
            break;
        case MCA_IOF_BASE_HDR_PUB:
            MCA_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            mca_iof_svc_proxy_pub(peer, &hdr->hdr_pub);
            break;
        case MCA_IOF_BASE_HDR_UNPUB:
            MCA_IOF_BASE_HDR_PUB_NTOH(hdr->hdr_pub);
            mca_iof_svc_proxy_unpub(peer, &hdr->hdr_pub);
            break;
        case MCA_IOF_BASE_HDR_SUB:
            MCA_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            mca_iof_svc_proxy_sub(peer, &hdr->hdr_sub);
            break;
        case MCA_IOF_BASE_HDR_UNSUB:
            MCA_IOF_BASE_HDR_SUB_NTOH(hdr->hdr_sub);
            mca_iof_svc_proxy_unsub(peer, &hdr->hdr_sub);
            break;
        default:
            ompi_output(0, "mca_iof_svc_recv: invalid message type: %d\n", hdr->hdr_common.hdr_type);
            break;
    }

done:
    free(hdr);

    mca_iof_svc_component.svc_iov[0].iov_base = NULL;
    mca_iof_svc_component.svc_iov[0].iov_len = 0;

    rc = mca_oob_recv_nb(
        MCA_OOB_NAME_ANY,
        mca_iof_svc_component.svc_iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        MCA_OOB_ALLOC,
        mca_iof_svc_proxy_recv,
        NULL
    );
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_proxy_recv: unable to post non-blocking recv");
        return;
    }
}


/**
 *
 */

static void mca_iof_svc_proxy_msg(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* hdr, 
    unsigned char* data)
{
    ompi_list_item_t* item;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_msg");
    }

    /* dispatch based on subscription list */
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  ompi_list_get_next(item)) {
        mca_iof_svc_subscript_t* sub = (mca_iof_svc_subscript_t*)item;

        /* tags match */
        if(sub->src_tag != hdr->msg_tag && hdr->msg_tag != MCA_IOF_ANY)
            continue;

        /* source match */
        if(ompi_name_server.compare(sub->src_mask,&sub->src_name,&hdr->msg_src) == 0) {
            mca_iof_svc_subscript_forward(sub,src,hdr,data);
        }
    }
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
}

/**
 * 
 */

static void mca_iof_svc_proxy_ack(
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* hdr)
{
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_ack");
    }
}


/**
 *
 */

static void mca_iof_svc_proxy_pub(
    const ompi_process_name_t* src,
    mca_iof_base_pub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_pub");
    }

    rc = mca_iof_svc_publish_create(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_pub: mca_iof_svc_publish_create failed with status=%d\n", rc);
    }
}

/*
 * 
 */

static void mca_iof_svc_proxy_unpub(
    const ompi_process_name_t* src,
    mca_iof_base_pub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_unpub");
    }

    rc = mca_iof_svc_publish_delete(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_proxy_unpub: mca_iof_svc_publish_delete failed with status=%d\n", rc);
    }
}

/**
 *
 */

static void mca_iof_svc_proxy_sub(
    const ompi_process_name_t* src,
    mca_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_sub");
    }

    rc = mca_iof_svc_subscript_create(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_proxy_sub: mca_iof_svc_subcript_create failed with status=%d\n", rc);
    }
}

/**
 *
 */

static void mca_iof_svc_proxy_unsub(
    const ompi_process_name_t* src,
    mca_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "mca_iof_svc_proxy_unsub");
    }

    rc = mca_iof_svc_subscript_delete(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_proxy_unsub: mca_iof_svc_subcript_delete failed with status=%d\n", rc);
    }
}

