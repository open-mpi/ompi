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
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif  /* HAVE_SIGNAL_H */
#include "opal/util/output.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/iof/base/iof_base_fragment.h"

/**
 *  Construct/Destructor
 */

static void orte_iof_base_endpoint_construct(orte_iof_base_endpoint_t* endpoint)
{
    endpoint->ep_mode = ORTE_IOF_SOURCE;  /* default value */
    endpoint->ep_seq = 0;
    endpoint->ep_ack = 0;
    endpoint->ep_fd = -1;
    memset(&endpoint->ep_event,0,sizeof(endpoint->ep_event));
    OBJ_CONSTRUCT(&endpoint->ep_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->ep_callbacks, opal_list_t);
}

static void orte_iof_base_endpoint_destruct(orte_iof_base_endpoint_t* endpoint)
{
    if(endpoint->ep_fd >= 0) {
        opal_event_del(&endpoint->ep_event);
    }
    OBJ_DESTRUCT(&endpoint->ep_frags);
    OBJ_DESTRUCT(&endpoint->ep_callbacks);
}

OBJ_CLASS_INSTANCE(
    orte_iof_base_endpoint_t,
    opal_list_item_t,
    orte_iof_base_endpoint_construct,
    orte_iof_base_endpoint_destruct);

/**
 *  Construct/Destructor
 */

static void orte_iof_base_callback_construct(orte_iof_base_callback_t* cb)
{
    cb->cb_func = 0;
    cb->cb_data = NULL;
}

static void orte_iof_base_callback_destruct(orte_iof_base_callback_t* cb)
{
}

OBJ_CLASS_INSTANCE(
    orte_iof_base_callback_t,
    opal_list_item_t,
    orte_iof_base_callback_construct,
    orte_iof_base_callback_destruct);



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
    opal_list_remove_item(&endpoint->ep_frags, &frag->super.super);
    ORTE_IOF_BASE_FRAG_RETURN(frag);

    /* Decrement the refcount on the endpoint; matches the RETAIN for
       when this frag's send was initiated in
       orte_iof_base_endpoint_read_handler() */
    OBJ_RELEASE(endpoint);
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
#if !defined(__WINDOWS__)
    rc = read(fd, frag->frag_data, sizeof(frag->frag_data));
#else
    {
        DWORD readed;
        HANDLE handle = (HANDLE)_get_osfhandle(fd);
        ReadFile(handle, frag->frag_data, sizeof(frag->frag_data), &readed, NULL);
        rc = (int)readed;
    }
#endif  /* !defined(__WINDOWS__) */
    if(rc < 0) {
        /* non-blocking, retry */
        if(errno == EAGAIN || errno == EINTR) {
            ORTE_IOF_BASE_FRAG_RETURN(frag);
        /* error on the connection */
        } else {
            orte_iof_base_endpoint_closed(endpoint);
        }
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return;
    } else if (rc == 0) {
        /* peer has closed connection - send 0 byte message to subscribers */
        orte_iof_base_endpoint_closed(endpoint);
    }

    /* Do not append the fragment before we know that we have some data (even 0 bytes it's OK) */
    frag->frag_owner = endpoint;
    opal_list_append(&endpoint->ep_frags, &frag->super.super);
    frag->frag_iov[1].iov_len = frag->frag_len = rc;

    /* fill in the header */
    hdr = &frag->frag_hdr;
    hdr->hdr_common.hdr_type = ORTE_IOF_BASE_HDR_MSG;
    hdr->hdr_msg.msg_src = endpoint->ep_name;
    hdr->hdr_msg.msg_proxy = *ORTE_PROC_MY_NAME;
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

    /* Increment the refcount on the endpoint so that it doesn't get
       deleted before the frag */
    OBJ_RETAIN(endpoint);

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
        int rc;

        /* close connection on zero byte message */
        if(frag->frag_len == 0) {
            orte_iof_base_endpoint_closed(endpoint);
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return;
        }

        /* progress pending messages */
        rc = write(endpoint->ep_fd, frag->frag_ptr, frag->frag_len);
        if(rc < 0) {
            if(errno == EAGAIN)
               break;
            if(errno == EINTR)
               continue;
            orte_iof_base_endpoint_closed(endpoint);
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return;
        }
        frag->frag_len -= rc;
        frag->frag_ptr += rc;
        if(frag->frag_len > 0) {
            break;
        }
        opal_list_remove_item(&endpoint->ep_frags, &frag->super.super);
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        orte_iof_base_frag_ack(frag);
        OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
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


/* return true if we should read stdin from fd, false otherwise */
static bool orte_iof_base_endpoint_stdin_check(int fd)
{
#if !defined(__WINDOWS__) && defined(HAVE_TCGETPGRP)
    if( isatty(fd) && (getpgrp() != tcgetpgrp(fd)) ) {
        return false;
    }
#endif  /* !defined(__WINDOWS__) */
    return true;
}


static void orte_iof_base_endpoint_stdin_cb(int sd, short flags, void *user)
{
    orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)user; 
    bool should_process = orte_iof_base_endpoint_stdin_check(endpoint->ep_fd);

    if (should_process) {
        opal_event_add(&endpoint->ep_event, 0);
    } else {
        opal_event_del(&endpoint->ep_event);
    }
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
        if(orte_ns.compare_fields(ORTE_NS_CMP_ALL,proc,&endpoint->ep_name) == 0 &&
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
    int rc;
 
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    if((endpoint = orte_iof_base_endpoint_lookup(proc,mode,tag)) != NULL) {
        OBJ_RELEASE(endpoint);
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return ORTE_SUCCESS;
    }
    endpoint = OBJ_NEW(orte_iof_base_endpoint_t);
    if(NULL == endpoint) {
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    endpoint->ep_name = *proc;
    endpoint->ep_mode = mode;
    endpoint->ep_tag = tag;
    endpoint->ep_fd = fd;

    /* If it looks like we're on the mpirun side of a standard IO
       stream (like we're a SOURCE and tag is STDIN and we're mucking
       with fd 0), we don't want to set nonblocking.  If we do so, we
       set the file descriptor to non-blocking for everyone that has
       that file descriptor, which includes everyone else in our shell
       pipeline chain.  (See
       http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
       This causes things like "mpirun -np 1 big_app | cat" to lose
       output, because cat's stdout is then ALSO non-blocking and cat
       isn't built to deal with that case (same with almost all other
       unix text utils). 

       Otherwise, we're probably on the non-mpirun end of things, and
       should be non-blocking.
    */
    if ( ! ((ORTE_IOF_SOURCE == mode && ORTE_IOF_STDIN == tag && 0 == fd) ||
            (ORTE_IOF_SINK == mode && ORTE_IOF_STDOUT == tag && 1 == fd) ||
            (ORTE_IOF_SINK == mode && ORTE_IOF_STDERR == tag && 2 == fd))) {
        if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
            opal_output(0, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                        __FILE__, __LINE__, errno);
        } else {
            flags |= O_NONBLOCK;
            fcntl(fd, F_SETFL, flags);
        }
    }

    /* setup event handler */
    switch(mode) {
        case ORTE_IOF_SOURCE:
            if (tag == ORTE_IOF_STDIN && isatty(endpoint->ep_fd)) {
                /* We should avoid trying to read from stdin if we
                   have a terminal, but are backgrounded.  Catch the
                   signals that are commonly used when we switch
                   between being backgrounded and not.  If the
                   filedescriptor is not a tty, don't worry about it
                   and always stay connected. */
#if !defined(__WINDOWS__)
                opal_signal_set(&(endpoint->ep_stdin_event),
                                SIGCONT,
                                orte_iof_base_endpoint_stdin_cb,
                                endpoint);
                opal_signal_add(&(endpoint->ep_stdin_event), NULL);
#endif  /* !defined(__WINDOWS__) */
            }

            /* always setup the event, but only add it if we should be
               reading from stdin right now (per rules above) */
            opal_event_set(
                           &endpoint->ep_event,
                           endpoint->ep_fd,
                           OPAL_EV_READ|OPAL_EV_PERSIST,
                           orte_iof_base_endpoint_read_handler,
                           endpoint);
            if (tag != ORTE_IOF_STDIN || 
                orte_iof_base_endpoint_stdin_check(endpoint->ep_fd)) {
                rc = opal_event_add(&endpoint->ep_event, 0);
                if (ORTE_SUCCESS != rc) return rc;
            }
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
            opal_output(0, "orte_iof_base_endpoint_create: invalid mode %d\n", mode);
            return ORTE_ERR_BAD_PARAM;
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
        if(orte_ns.compare_fields(mask,proc,&endpoint->ep_name) == 0 &&
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
 *  Connection has gone away - cleanup and signal SOH monitor.
 */

void orte_iof_base_endpoint_closed(orte_iof_base_endpoint_t* endpoint)
{
    /* Special case: if we're a sink and one of the special streams
       (stdout or stderr), don't close anything because we don't want
       to *actually* close stdout or stderr just because a remote
       process closes theirs (but we do if a remote source/stdin
       closes theirs, for example). */

    if (ORTE_IOF_SINK == endpoint->ep_mode && 
        (ORTE_IOF_STDOUT == endpoint->ep_tag ||
         ORTE_IOF_STDERR == endpoint->ep_tag)) {
        return;
    }

    /* remove any event handlers */
    switch(endpoint->ep_mode) {
    case ORTE_IOF_SOURCE:
        opal_event_del(&endpoint->ep_event);
        break;
    case ORTE_IOF_SINK:
        if(opal_list_get_size(&endpoint->ep_frags)) {
            opal_event_del(&endpoint->ep_event);
        }
        break;
    }

    /* close associated file descriptor */
    close(endpoint->ep_fd);
    endpoint->ep_fd = -1;
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
        if(orte_ns.compare_fields(dst_mask,dst_name,&endpoint->ep_name) == 0) {
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
    opal_list_item_t* item;
    orte_iof_base_frag_t* frag;
    size_t len = hdr->msg_len;
    int rc = 0;

    if(endpoint->ep_mode != ORTE_IOF_SINK) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* allocate and initialize a fragment */
    ORTE_IOF_BASE_FRAG_ALLOC(frag, rc);
    if(NULL == frag) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    frag->frag_owner = endpoint;
    frag->frag_src = *src;
    frag->frag_hdr.hdr_msg = *hdr;
    frag->frag_len = len;

    /* call any registered callbacks */ 
    for(item =  opal_list_get_first(&endpoint->ep_callbacks);
        item != opal_list_get_end(&endpoint->ep_callbacks);
        item =  opal_list_get_next(item)) {
        orte_iof_base_callback_t* cb = (orte_iof_base_callback_t*)item;
        cb->cb_func(
           &hdr->msg_src, 
            hdr->msg_tag, 
            cb->cb_data, 
            data, 
            hdr->msg_len);
    }

    if(endpoint->ep_fd >= 0) {

        /* try to write w/out copying data */
       
        if(opal_list_get_size(&endpoint->ep_frags) == 0) {
            if(len == 0) {
                ORTE_IOF_BASE_FRAG_RETURN(frag);
                orte_iof_base_endpoint_closed(endpoint);
                OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
                return ORTE_SUCCESS;
            }
            rc = write(endpoint->ep_fd,data,len);
            if(rc < 0) {
                if (errno != EAGAIN && errno != EINTR) {
                    ORTE_IOF_BASE_FRAG_RETURN(frag);
                    orte_iof_base_endpoint_closed(endpoint);
                    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
                    return ORTE_SUCCESS;
                }
                rc = 0;  /* don't affect the remaining length of the data */
            }
            frag->frag_len -= rc;
        }

        if(frag->frag_len > 0) {
            /* handle incomplete write - also queue up 0 byte message 
             * and recognize this as a request to close the descriptor
             * when all pending operations complete
             */
            frag->frag_ptr = frag->frag_data;
            memcpy(frag->frag_ptr, data+rc, frag->frag_len);
            opal_list_append(&endpoint->ep_frags, &frag->super.super);
            if(opal_list_get_size(&endpoint->ep_frags) == 1) {
                opal_event_add(&endpoint->ep_event,0);
            }
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        } else {
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            /* acknowledge fragment */
            orte_iof_base_frag_ack(frag);
        }
    } else {
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        /* acknowledge fragment */
        orte_iof_base_frag_ack(frag);
    }
    return ORTE_SUCCESS;
}


/**
 * Register a callback
 */

int orte_iof_base_callback_create(
    const orte_process_name_t* proc,
    int tag,
    orte_iof_base_callback_fn_t cbfunc,
    void *cbdata)
{
    orte_iof_base_callback_t* cb = OBJ_NEW(orte_iof_base_callback_t);
    orte_iof_base_endpoint_t* endpoint;
    if(NULL == cb)
        return ORTE_ERR_OUT_OF_RESOURCE;

    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    if((endpoint = orte_iof_base_endpoint_lookup(proc,ORTE_IOF_SINK,tag)) == NULL) {
        endpoint = OBJ_NEW(orte_iof_base_endpoint_t);
        if(NULL == endpoint) {
            OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        endpoint->ep_name = *proc;
        endpoint->ep_mode = ORTE_IOF_SINK;
        endpoint->ep_tag = tag;
        endpoint->ep_fd = -1;
        opal_list_append(&orte_iof_base.iof_endpoints, &endpoint->super);
    } else {
        OBJ_RELEASE(endpoint);
    }
    cb->cb_func = cbfunc;
    cb->cb_data = cbdata;
    opal_list_append(&endpoint->ep_callbacks, (opal_list_item_t*)cb);
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return ORTE_SUCCESS;
}


/**
 * Remove a callback
 */

int orte_iof_base_callback_delete(
    const orte_process_name_t* proc,
    int tag)
{
    orte_iof_base_endpoint_t* endpoint;
    opal_list_item_t* item;
    
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock); 
    if(NULL == (endpoint = orte_iof_base_endpoint_lookup(proc,ORTE_IOF_SINK, tag))) {
        OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
        return ORTE_ERR_NOT_FOUND;
    }

    while(NULL != (item = opal_list_remove_first(&endpoint->ep_callbacks))) {
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE(endpoint);
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
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

