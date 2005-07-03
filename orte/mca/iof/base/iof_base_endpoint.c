#include "ompi_config.h"
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "util/output.h"
#include "mca/rml/rml.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "mca/iof/base/iof_base_fragment.h"

/**
 *  Construct/Destructor
 */

static void orte_iof_base_endpoint_construct(orte_iof_base_endpoint_t* endpoint)
{
    endpoint->ep_mode = 0;
    endpoint->ep_state = ORTE_IOF_EP_CLOSED;
    endpoint->ep_seq = 0;
    endpoint->ep_ack = 0;
    endpoint->ep_fd = -1;
    memset(&endpoint->ep_event,0,sizeof(endpoint->ep_event));
    OBJ_CONSTRUCT(&endpoint->ep_frags, opal_list_t);
}

static void orte_iof_base_endpoint_destruct(orte_iof_base_endpoint_t* endpoint)
{
    if(endpoint->ep_fd >= 0) {
        opal_event_del(&endpoint->ep_event);
    }
    OBJ_DESTRUCT(&endpoint->ep_frags);
}

OBJ_CLASS_INSTANCE(
    orte_iof_base_endpoint_t,
    opal_list_item_t,
    orte_iof_base_endpoint_construct,
    orte_iof_base_endpoint_destruct);


/*
 * Callback when non-blocking OOB send completes.
 */

static void orte_iof_base_endpoint_send_cb(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)cbdata;
    orte_iof_base_endpoint_t* endpoint = frag->frag_owner;
    opal_list_remove_item(&endpoint->ep_frags, &frag->super);
    ORTE_IOF_BASE_FRAG_RETURN(frag);
}


/*
 *  Callback when data is available on the endpoint to read.
 */

static void orte_iof_base_endpoint_read_handler(int fd, short flags, void *cbdata)
{
    orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)cbdata;
    orte_iof_base_frag_t* frag;
    orte_iof_base_header_t* hdr;
    int rc;

    /* allocate a fragment */
    ORTE_IOF_BASE_FRAG_ALLOC(frag,rc);
    if(NULL == frag) {
        return;
    }

    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);

    /* read up to the fragment size */
    rc = read(fd, frag->frag_data, sizeof(frag->frag_data));
    if(rc <= 0) {
        /* non-blocking */
        if(rc < 0 && errno == EAGAIN) {
            ORTE_IOF_BASE_FRAG_RETURN(frag);
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return;
        }

        /* peer has closed the connection */
        orte_iof_base_endpoint_closed(endpoint);
        rc = 0;
    }
    /* Do not append the fragment before we know that we have some data (even 0 bytes it's OK) */
    frag->frag_owner = endpoint;
    opal_list_append(&endpoint->ep_frags, &frag->super);

    frag->frag_iov[1].iov_len = frag->frag_len = rc;

    /* fill in the header */
    hdr = &frag->frag_hdr;
    hdr->hdr_common.hdr_type = ORTE_IOF_BASE_HDR_MSG;
    hdr->hdr_msg.msg_src = endpoint->ep_name;
    hdr->hdr_msg.msg_proxy = *ORTE_RML_NAME_SELF;
    hdr->hdr_msg.msg_tag = endpoint->ep_tag;
    hdr->hdr_msg.msg_seq = endpoint->ep_seq;
    hdr->hdr_msg.msg_len = frag->frag_len;
    ORTE_IOF_BASE_HDR_MSG_HTON(hdr->hdr_msg);

    /* if window size has been exceeded - disable forwarding */
    endpoint->ep_seq += frag->frag_len;
    if(ORTE_IOF_BASE_SEQDIFF(endpoint->ep_seq,endpoint->ep_ack) > orte_iof_base.iof_window_size) {
        opal_event_del(&endpoint->ep_event);
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);

    /* start non-blocking OOB call to forward received data */
    rc = orte_rml.send_nb(
        orte_iof_base.iof_service, 
        frag->frag_iov, 
        2,
        ORTE_RML_TAG_IOF_SVC,
        0,
        orte_iof_base_endpoint_send_cb,
        frag);
}


/**
 * Callback when the endpoint is available for write.
 */

static void orte_iof_base_endpoint_write_handler(int sd, short flags, void *user)
{
    orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)user; 

    /*
     * step through the list of queued fragments and attempt to write
     * until the output descriptor would block
    */
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    while(opal_list_get_size(&endpoint->ep_frags)) {
        orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)opal_list_get_first(&endpoint->ep_frags);
        int rc = write(endpoint->ep_fd, frag->frag_ptr, frag->frag_len);
        if(rc < 0) {
            if(errno == EAGAIN)
               break;
            orte_iof_base_endpoint_closed(endpoint);
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return;
        }
        frag->frag_len -= rc;
        frag->frag_ptr += rc;
        if(frag->frag_len > 0) {
            break;
        }
        opal_list_remove_item(&endpoint->ep_frags, &frag->super);
        orte_iof_base_frag_ack(frag);
    }

    /* is there anything left to write? */
    if(opal_list_get_size(&endpoint->ep_frags) == 0) {
        opal_event_del(&endpoint->ep_event);
        if(orte_iof_base.iof_waiting) {
            opal_condition_signal(&orte_iof_base.iof_condition);
        }
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
}

/*
 * Lookup existing endpoint matching parameters
 * supplied to create.
 */
 
static orte_iof_base_endpoint_t* orte_iof_base_endpoint_lookup(
    const orte_process_name_t* proc,
    orte_iof_base_mode_t mode,
    int tag)
{
    opal_list_item_t* item;
    for(item =  opal_list_get_first(&orte_iof_base.iof_endpoints);
        item != opal_list_get_end(&orte_iof_base.iof_endpoints);
        item =  opal_list_get_next(item)) {
        orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)item;
        if(orte_ns.compare(ORTE_NS_CMP_ALL,proc,&endpoint->ep_name) == 0 &&
           endpoint->ep_tag == tag && endpoint->ep_mode == mode) {
            OBJ_RETAIN(endpoint);
            return endpoint;
        }
    }
    return NULL;
}


/*
 *  Create a local endpoint.
 */

int orte_iof_base_endpoint_create(
    const orte_process_name_t* proc,
    orte_iof_base_mode_t mode,
    int tag,
    int fd)
{
    orte_iof_base_endpoint_t* endpoint;
    int flags;
 
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    if((endpoint = orte_iof_base_endpoint_lookup(proc,mode,tag)) != NULL) {
        OBJ_RELEASE(endpoint);
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return OMPI_SUCCESS;
    }
    endpoint = OBJ_NEW(orte_iof_base_endpoint_t);
    if(NULL == endpoint) {
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    endpoint->ep_name = *proc;
    endpoint->ep_mode = mode;
    endpoint->ep_tag = tag;
    endpoint->ep_fd = fd;

    /* set file descriptor to be non-blocking */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        ompi_output(0, "orte_iof_base_endpoint_create: fcntl(F_GETFL) failed with errno=%d\n", errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }
    
    /* setup event handler */
    switch(mode) {
        case ORTE_IOF_SOURCE:
            opal_event_set(
                &endpoint->ep_event,
                endpoint->ep_fd,
                OPAL_EV_READ|OPAL_EV_PERSIST,
                orte_iof_base_endpoint_read_handler,
                endpoint);
            opal_event_add(&endpoint->ep_event, 0);
            break;
        case ORTE_IOF_SINK:
            opal_event_set(
                &endpoint->ep_event,
                endpoint->ep_fd,
                OPAL_EV_WRITE|OPAL_EV_PERSIST,
                orte_iof_base_endpoint_write_handler,
                endpoint);
            break;
        default:
            ompi_output(0, "orte_iof_base_endpoint_create: invalid mode %d\n", mode);
            return OMPI_ERR_BAD_PARAM;
    }

    opal_list_append(&orte_iof_base.iof_endpoints, &endpoint->super);
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return ORTE_SUCCESS;
}


/*
 * Close one or more matching endpoints.
 */

int orte_iof_base_endpoint_delete(
    const orte_process_name_t* proc,
    orte_ns_cmp_bitmask_t mask,
    int tag)
{
    opal_list_item_t* item;
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    item =  opal_list_get_first(&orte_iof_base.iof_endpoints);
    while(item != opal_list_get_end(&orte_iof_base.iof_endpoints)) {
        opal_list_item_t* next =  opal_list_get_next(item);
        orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)item;
        if(orte_ns.compare(mask,proc,&endpoint->ep_name) == 0 &&
           endpoint->ep_tag == tag) {
            OBJ_RELEASE(endpoint);
            opal_list_remove_item(&orte_iof_base.iof_endpoints,&endpoint->super);
        }
        item = next;
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return ORTE_ERR_NOT_FOUND;
}

/*
 * 
 */

int orte_iof_base_endpoint_close(orte_iof_base_endpoint_t* endpoint)
{ 
    endpoint->ep_state = ORTE_IOF_EP_CLOSING;
    switch(endpoint->ep_mode) {
    case ORTE_IOF_SOURCE:
        opal_event_del(&endpoint->ep_event);
        if(endpoint->ep_seq == endpoint->ep_ack) {
            endpoint->ep_state = ORTE_IOF_EP_CLOSED;
        }
        break;
    case ORTE_IOF_SINK:
        if(opal_list_get_size(&endpoint->ep_frags) == 0) {
            endpoint->ep_state = ORTE_IOF_EP_CLOSED;
        }
        break;
    }
    return ORTE_SUCCESS;
}

/*
 *  Peer has gone away - cleanup and signal SOH monitor.
 */

void orte_iof_base_endpoint_closed(orte_iof_base_endpoint_t* endpoint)
{
    orte_iof_base_endpoint_close(endpoint);
}

/*
 *  Lookup endpoint based on destination process name/mask/tag.
 */

orte_iof_base_endpoint_t* orte_iof_base_endpoint_match(
    const orte_process_name_t* dst_name, 
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag)
{
    opal_list_item_t* item;
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    for(item =  opal_list_get_first(&orte_iof_base.iof_endpoints);
        item != opal_list_get_end(&orte_iof_base.iof_endpoints);
        item =  opal_list_get_next(item)) {
        orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)item;
        if(orte_ns.compare(dst_mask,dst_name,&endpoint->ep_name) == 0) {
            if(endpoint->ep_tag == dst_tag || endpoint->ep_tag == ORTE_IOF_ANY || dst_tag == ORTE_IOF_ANY) {
                OBJ_RETAIN(endpoint);
                OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
                return endpoint;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return NULL;
}

/*
 * Forward data out the endpoint as the destination 
 * is available. Queue incomplete fragments in order
 * received and process as the destination becomes available.
 */

int orte_iof_base_endpoint_forward(
    orte_iof_base_endpoint_t* endpoint, 
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data)
{
    orte_iof_base_frag_t* frag;
    size_t len = hdr->msg_len;
    int rc = 0;

    if(endpoint->ep_mode != ORTE_IOF_SINK) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* allocate and initialize a fragment */
    ORTE_IOF_BASE_FRAG_ALLOC(frag, rc);
    if(NULL == frag) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    endpoint->ep_seq = hdr->msg_seq + hdr->msg_len;
    frag->frag_owner = endpoint;
    frag->frag_src = *src;
    frag->frag_hdr.hdr_msg = *hdr;

    /* try to write w/out copying data */
    if(opal_list_get_size(&endpoint->ep_frags) == 0) {
        rc = write(endpoint->ep_fd,data,len);
        if(rc < 0) {
            orte_iof_base_endpoint_closed(endpoint);
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return ORTE_SUCCESS;
         } 
    } 

    frag->frag_len = len - rc;
    if(frag->frag_len > 0) {
        /* handle incomplete write */
        frag->frag_ptr = frag->frag_data;
        memcpy(frag->frag_ptr, data+rc, frag->frag_len);
        opal_list_append(&endpoint->ep_frags, &frag->super);
        if(opal_list_get_size(&endpoint->ep_frags) == 1) {
            opal_event_add(&endpoint->ep_event,0);
        }
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    } else {
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);

        /* acknowledge fragment */
        orte_iof_base_endpoint_ack(endpoint, frag->frag_hdr.hdr_msg.msg_seq + frag->frag_hdr.hdr_msg.msg_len);
        orte_iof_base_frag_ack(frag);
    }
    return ORTE_SUCCESS;
}


/**
 * Update the acknowledged sequence number. If forwarding had
 * previously been disabled as the window closed, and the window
 * is now open, re-enable forwarding.
 */

int orte_iof_base_endpoint_ack(
    orte_iof_base_endpoint_t* endpoint,
    uint32_t seq)
{
    bool window_closed, window_open;

    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    window_closed =
            ORTE_IOF_BASE_SEQDIFF(endpoint->ep_seq,endpoint->ep_ack) >= orte_iof_base.iof_window_size;
    endpoint->ep_ack = seq;
    window_open =
            ORTE_IOF_BASE_SEQDIFF(endpoint->ep_seq,endpoint->ep_ack) < orte_iof_base.iof_window_size;
                                                                                                              
    /* someone is waiting on all output to be flushed */
    if(orte_iof_base.iof_waiting && endpoint->ep_seq == endpoint->ep_ack) {
        opal_condition_signal(&orte_iof_base.iof_condition);
    }

    /* check to see if we need to reenable forwarding */
    if(window_closed && window_open) {
        opal_event_add(&endpoint->ep_event, 0);
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return ORTE_SUCCESS;
}

