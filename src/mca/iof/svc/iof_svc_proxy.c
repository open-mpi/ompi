#include "ompi_config.h"
#include "util/output.h"
#include "mca/rml/rml.h"
#include "mca/rml/rml_types.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "mca/iof/base/iof_base_fragment.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_publish.h"
#include "iof_svc_subscript.h"


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
    int rc;
    orte_iof_base_header_t* hdr = (orte_iof_base_header_t*)iov[0].iov_base;

    if(status < 0) {
        ompi_output(0, "orte_iof_svc_recv: receive failed with status: %d", status);
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
            ompi_output(0, "orte_iof_svc_recv: invalid message type: %d\n", hdr->hdr_common.hdr_type);
            break;
    }

done:
    free(hdr);

    mca_iof_svc_component.svc_iov[0].iov_base = NULL;
    mca_iof_svc_component.svc_iov[0].iov_len = 0;

    rc = orte_rml.recv_nb(
        ORTE_RML_NAME_ANY,
        mca_iof_svc_component.svc_iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        ORTE_RML_ALLOC,
        orte_iof_svc_proxy_recv,
        NULL
    );
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_svc_proxy_recv: unable to post non-blocking recv");
        return;
    }
}


/**
 *
 */

static void orte_iof_svc_proxy_msg(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr, 
    unsigned char* data)
{
    ompi_list_item_t* item;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "orte_iof_svc_proxy_msg: tag %d seq %d\n",hdr->msg_tag,hdr->msg_seq);
    }

    /* dispatch based on subscription list */
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  ompi_list_get_next(item)) {
        orte_iof_svc_subscript_t* sub = (orte_iof_svc_subscript_t*)item;

        /* tags match */
        if(sub->src_tag != hdr->msg_tag && hdr->msg_tag != ORTE_IOF_ANY)
            continue;

        /* source match */
        if(orte_ns.compare(sub->src_mask,&sub->src_name,&hdr->msg_src) == 0) {
            if(mca_iof_svc_component.svc_debug > 1) {
                ompi_output(0, "[%d,%d,%d] orte_iof_svc_proxy_msg: tag %d sequence %d\n",
                    ORTE_NAME_ARGS(&sub->src_name),hdr->msg_tag,hdr->msg_seq);
            }
            orte_iof_svc_subscript_forward(sub,src,hdr,data);
        }
    }
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
}

/**
 * 
 */

static void orte_iof_svc_proxy_ack(
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr)
{
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "orte_iof_svc_proxy_ack");
    }
}


/**
 *
 */

static void orte_iof_svc_proxy_pub(
    const orte_process_name_t* src,
    orte_iof_base_pub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "orte_iof_svc_proxy_pub");
    }

    rc = orte_iof_svc_publish_create(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_svc_pub: orte_iof_svc_publish_create failed with status=%d\n", rc);
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
        ompi_output(0, "orte_iof_svc_proxy_unpub");
    }

    rc = orte_iof_svc_publish_delete(
        &hdr->pub_name,
        &hdr->pub_proxy,
        hdr->pub_mask,
        hdr->pub_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_svc_proxy_unpub: orte_iof_svc_publish_delete failed with status=%d\n", rc);
    }
}

/**
 *
 */

static void orte_iof_svc_proxy_sub(
    const orte_process_name_t* src,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "orte_iof_svc_proxy_sub");
    }

    rc = orte_iof_svc_subscript_create(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_svc_proxy_sub: orte_iof_svc_subcript_create failed with status=%d\n", rc);
    }
}

/**
 *
 */

static void orte_iof_svc_proxy_unsub(
    const orte_process_name_t* src,
    orte_iof_base_sub_header_t* hdr)
{
    int rc;
    if(mca_iof_svc_component.svc_debug > 1) {
        ompi_output(0, "orte_iof_svc_proxy_unsub");
    }

    rc = orte_iof_svc_subscript_delete(
        &hdr->src_name,
        hdr->src_mask,
        hdr->src_tag,
        &hdr->dst_name,
        hdr->dst_mask,
        hdr->dst_tag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "orte_iof_svc_proxy_unsub: orte_iof_svc_subcript_delete failed with status=%d\n", rc);
    }
}

