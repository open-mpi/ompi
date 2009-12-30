/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "opal/types.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#include <errno.h>
#include <fcntl.h>

#include "opal/class/opal_list.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/dss/dss.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/rmcast/base/private.h"
#include "orte/mca/rmcast/base/base.h"
#include "rmcast_tcp.h"

/* LOCAL DATA */
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_list_t channels;
static bool init_completed = false;
static orte_rmcast_channel_t next_channel;

/* LOCAL FUNCTIONS */
static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);

/* LOCAL STRUCTURE VALUES */
static rmcast_base_channel_t *my_group_channel=NULL;

/* API FUNCTIONS */
static int init(void);

static void finalize(void);

static int tcp_send_buffer(orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           opal_buffer_t *buf);

static int tcp_send_buffer_nb(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              opal_buffer_t *buf,
                              orte_rmcast_callback_buffer_fn_t cbfunc,
                              void *cbdata);

static int tcp_send(orte_rmcast_channel_t channel,
                    orte_rmcast_tag_t tag,
                    struct iovec *msg, int count);

static int tcp_send_nb(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       struct iovec *msg, int count,
                       orte_rmcast_callback_fn_t cbfunc,
                       void *cbdata);

static int tcp_recv_buffer(orte_process_name_t *sender,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           opal_buffer_t *buf,
                           orte_rmcast_seq_t *seq_num);

static int tcp_recv_buffer_nb(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              orte_rmcast_flag_t flags,
                              orte_rmcast_callback_buffer_fn_t cbfunc,
                              void *cbdata);

static int tcp_recv(orte_process_name_t *sender,
                    orte_rmcast_channel_t channel,
                    orte_rmcast_tag_t tag,
                    struct iovec **msg, int *count,
                    orte_rmcast_seq_t *seq_num);

static int tcp_recv_nb(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       orte_rmcast_flag_t flags,
                       orte_rmcast_callback_fn_t cbfunc,
                       void *cbdata);

static void cancel_recv(orte_rmcast_channel_t channel,
                        orte_rmcast_tag_t tag);

static int open_channel(orte_rmcast_channel_t *channel, char *name,
                        char *network, int port, char *interface, uint8_t direction);

static int close_channel(orte_rmcast_channel_t channel);

static orte_rmcast_channel_t query(void);

/* Define the module */

orte_rmcast_module_t orte_rmcast_tcp_module = {
    init,
    finalize,
    tcp_send,
    tcp_send_nb,
    tcp_send_buffer,
    tcp_send_buffer_nb,
    tcp_recv,
    tcp_recv_nb,
    tcp_recv_buffer,
    tcp_recv_buffer_nb,
    cancel_recv,
    open_channel,
    close_channel,
    query
};

/* during init, we setup two channels for both xmit and recv:
 * (a) a public address announcement channel. There are two variants
 *     of this:
 *         (1) system processes - e.g., daemons, tools. This channel
 *             is reserved solely for their use in performing admin
 *             functions
 *         (2) application processes. This channel is used to announce
 *             their existence and contact info for auto-wireup
 * (b) our own group's channel, which is where our own output
 *     will be sent. At this time, we assume that we always
 *     want to hear our peers, so this channels is also
 *     bidirectional
 *
 * In addition, the HNP opens a third channel which is used solely
 * for cmd-control purposes. This is where a tool, for example, might
 * send a cmd to the HNP to take some action - there is no point in
 * having that cmd echo around to every daemon and/or other tool
 * in the system.
 */
static int init(void)
{
    int rc;
    orte_rmcast_channel_t channel;
    
    if (init_completed) {
        return ORTE_SUCCESS;
    }
    init_completed = true;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: init called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the globals */
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&recvs, opal_list_t);
    OBJ_CONSTRUCT(&channels, opal_list_t);
    next_channel = ORTE_RMCAST_DYNAMIC_CHANNELS;
    
    /* setup the respective public address channel */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_TOOL) {
        channel = ORTE_RMCAST_SYS_CHANNEL;
        if (ORTE_SUCCESS != (rc = open_channel(&channel, "system",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else if (ORTE_PROC_IS_APP) {
        channel = ORTE_RMCAST_APP_PUBLIC_CHANNEL;
        if (ORTE_SUCCESS != (rc = open_channel(&channel, "app-announce",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else {
        opal_output(0, "rmcast:tcp:init - unknown process type");
        return ORTE_ERR_SILENT;
    }
    
    /* finally, if we are an app, setup our grp channel, if one was given */
    if (ORTE_PROC_IS_APP && NULL != orte_rmcast_base.my_group_name) {
        channel = orte_rmcast_base.my_group_number;
        if (ORTE_SUCCESS != (rc = open_channel(&channel, orte_rmcast_base.my_group_name,
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        my_group_channel = (rmcast_base_channel_t*)opal_list_get_last(&channels);
    }

    if (ORTE_JOBID_WILDCARD == orte_process_info.my_hnp.jobid) {
        /* set the HNP info in our contact table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_hnp_uri))) {
            orte_show_help("help-orcm-ps.txt", "orcm-ps:hnp-uri-bad", true, orte_process_info.my_hnp_uri);
            return rc;
        }
        /* extract the name */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                     &orte_process_info.my_hnp, NULL))) {
            orte_show_help("help-orcm-ps.txt", "orcm-ps:hnp-uri-bad", true, orte_process_info.my_hnp_uri);
            return rc;
        }
    }
    
    /* now activate the non-blocking recv so we catch messages */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_MULTICAST,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_handler,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_MULTICAST);

    /* deconstruct the globals */
    OPAL_THREAD_LOCK(&lock);
    while (NULL != (item = opal_list_remove_first(&recvs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&recvs);
    while (NULL != (item = opal_list_remove_first(&channels))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&channels);
    OPAL_THREAD_UNLOCK(&lock);

    OBJ_DESTRUCT(&lock);
    
    return;
}

/* internal blocking send support */
static bool send_complete, send_buf_complete;

static void internal_snd_cb(int status,
                            orte_rmcast_channel_t channel,
                            orte_rmcast_tag_t tag,
                            orte_process_name_t *sender,
                            orte_rmcast_seq_t seq_num,
                            struct iovec *msg, int count, void *cbdata)
{
    send_complete = true;
}

static void internal_snd_buf_cb(int status,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_process_name_t *sender,
                                orte_rmcast_seq_t seq_num,
                                opal_buffer_t *buf, void *cbdata)
{
    send_buf_complete = true;
}

static int queue_xmit(rmcast_base_send_t *snd,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chptr, *ch;
    int32_t sz;
    int rc;
    int8_t flag;
    opal_buffer_t buf;
    int32_t tmp32;
    
    /* if we were asked to send this on our group output
     * channel, substitute it
     */
    if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        if (NULL == my_group_channel) {
            return ORTE_ERR_NOT_FOUND;
        }
        ch = my_group_channel;
        goto process;
    }
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_base_channel_t*)item;
        if (channel == chptr->channel) {
            ch = chptr;
            break;
        }
    }
    if (NULL == ch) {
        /* didn't find it */
        return ORTE_ERR_NOT_FOUND;
    }
    
process:    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: send of %d %s"
                         " called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         (int)ch->channel));
    

    OPAL_THREAD_LOCK(&ch->send_lock);
    /* setup a buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* pack our name */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* pack the channel */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &channel, 1, ORTE_RMCAST_CHANNEL_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* pack the tag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RMCAST_TAG_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* pack the sequence number */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &ch->seq_num, 1, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* are we sending a buffer? */
    if (NULL == snd->buf) {
        /* no, flag the buffer as containing iovecs */
        flag = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* pack the number of iovecs */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &snd->iovec_count, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* pack each iovec into a buffer in prep for sending
         * so we can recreate the array at the other end
         */
        for (sz=0; sz < snd->iovec_count; sz++) {
            /* pack the size */
            tmp32 = snd->iovec_array[sz].iov_len;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tmp32, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* pack the bytes */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, snd->iovec_array[sz].iov_base, tmp32, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        
    } else {
        /* flag it as being a buffer */
        flag = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* copy the payload */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, snd->buf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp multicasting %d bytes to channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)buf.bytes_used,
                         (int)channel, (int)tag));
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_JOBID_WILDCARD,
                                                 &buf, ORTE_RML_TAG_MULTICAST))) {
        /* didn't get the message out */
        opal_output(0, "%s failed to send message to multicast channel %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)channel);
        goto cleanup;
    }
    
    if (1 == flag) {
        /* call the cbfunc if required */
        if (NULL != snd->cbfunc_buffer) {
            snd->cbfunc_buffer(rc, channel, tag,
                               ORTE_PROC_MY_NAME, ch->seq_num,
                               snd->buf, snd->cbdata);
        }
    } else {
        /* call the cbfunc if required */
        if (NULL != snd->cbfunc_iovec) {
            snd->cbfunc_iovec(rc, channel, tag,
                              ORTE_PROC_MY_NAME, ch->seq_num,
                              snd->iovec_array, snd->iovec_count, snd->cbdata);
        }
    }
    
    /* roll to next message sequence number */
    ORTE_MULTICAST_NEXT_SEQUENCE_NUM(ch->seq_num);
    
cleanup:
    OBJ_DESTRUCT(&buf);

    OPAL_THREAD_UNLOCK(&ch->send_lock);
    
    return ORTE_SUCCESS;
}

static int tcp_send(orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec *msg, int count)
{
    rmcast_base_send_t snd;
    int ret;
    
    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.iovec_array = msg;
    snd.iovec_count = count;
    snd.tag = tag;
    snd.cbfunc_iovec = internal_snd_cb;
    send_complete = false;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(&snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&snd);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(send_complete, 0, 1);
    
    OBJ_DESTRUCT(&snd);
    return ORTE_SUCCESS;
}

static int tcp_send_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         struct iovec *msg, int count,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata)
{
    int ret;
    rmcast_base_send_t snd;
    
    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.iovec_array = msg;
    snd.iovec_count = count;
    snd.tag = tag;
    snd.cbfunc_iovec = cbfunc;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(&snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&snd);
        return ret;
    }
    OBJ_DESTRUCT(&snd);

    return ORTE_SUCCESS;
}

static int tcp_send_buffer(orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf)
{
    int ret;
    rmcast_base_send_t snd;
    
    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.buf = buf;
    snd.tag = tag;
    snd.cbfunc_buffer = internal_snd_buf_cb;
    send_buf_complete = false;

    if (ORTE_SUCCESS != (ret = queue_xmit(&snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&snd);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(send_buf_complete, 0, 1);
    OBJ_DESTRUCT(&snd);

    return ORTE_SUCCESS;
}

static int tcp_send_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                opal_buffer_t *buf,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata)
{
    int ret;
    rmcast_base_send_t snd;
    
    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.buf = buf;
    snd.tag = tag;
    snd.cbfunc_buffer = cbfunc;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(&snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&snd);
        return ret;
    }
    OBJ_DESTRUCT(&snd);

    return ORTE_SUCCESS;
}

static int queue_recv(rmcast_base_recv_t *recvptr,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      orte_rmcast_callback_fn_t cbfunc_iovec,
                      orte_rmcast_callback_buffer_fn_t cbfunc_buffer,
                      bool blocking)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *ch, *chptr;
    rmcast_base_recv_t *rptr;
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_base_channel_t*)item;
        if (channel == chptr->channel) {
            ch = chptr;
            break;
        }
    }
    if (NULL == ch) {
        /* didn't find it */
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: queue_recv called on multicast channel %03d.%03d.%03d.%03d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(ch->network), tag));
    
    if (!blocking) {
        /* do we already have a recv for this channel/tag/cbfunc? */
        OPAL_THREAD_LOCK(&lock);
        for (item = opal_list_get_first(&recvs);
             item != opal_list_get_end(&recvs);
             item = opal_list_get_next(item)) {
            rptr = (rmcast_base_recv_t*)item;
            if (channel == rptr->channel &&
                tag == rptr->tag &&
                ((NULL != cbfunc_iovec && cbfunc_iovec == rptr->cbfunc_iovec) ||
                (NULL != cbfunc_buffer && cbfunc_buffer == rptr->cbfunc_buffer))) {
                /* matching recv in place */
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:tcp: matching recv already active on multicast channel %d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ch->channel, tag));
                OPAL_THREAD_UNLOCK(&lock);
                return ORTE_EXISTS;
            }
        }
        OPAL_THREAD_UNLOCK(&lock);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: adding non-blocking recv on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ch->channel, tag));
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);

    return ORTE_SUCCESS;
}

static int tcp_recv(orte_process_name_t *name,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec **msg, int *count, orte_rmcast_seq_t *seq_num)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    
    if (ORTE_SUCCESS != (ret = queue_recv(recvptr, channel, tag, NULL, NULL, true))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(recvptr);
        return ret;
    }
    
    ORTE_PROGRESSED_WAIT(recvptr->recvd, 0, 1);
    
    /* xfer the data */
    if (NULL != name) {
        /* caller requested id of sender */
        name->jobid = recvptr->name.jobid;
        name->vpid = recvptr->name.vpid;
    }
    *msg = recvptr->iovec_array;
    *count = recvptr->iovec_count;
    *seq_num = recvptr->seq_num;
    
    /* remove the recv */
    OPAL_THREAD_LOCK(&lock);
    opal_list_remove_item(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int tcp_recv_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_nb called on channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));
    
    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    recvptr->flags = flags;
    recvptr->cbfunc_iovec = cbfunc;
    recvptr->cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = queue_recv(recvptr, channel, tag, cbfunc, NULL, true))) {
        if (ORTE_EXISTS == ret) {
            /* this recv already exists - just release the copy */
            OBJ_RELEASE(recvptr);
            return ORTE_SUCCESS;
        }
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(recvptr);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static int tcp_recv_buffer(orte_process_name_t *name,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           opal_buffer_t *buf, orte_rmcast_seq_t *seq_num)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_buffer called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));

    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    
    if (ORTE_SUCCESS != (ret = queue_recv(recvptr, channel, tag, NULL, NULL, true))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    ORTE_PROGRESSED_WAIT(recvptr->recvd, 0, 1);
    
    /* xfer the data */
    if (NULL != name) {
        /* caller requested id of sender */
        name->jobid = recvptr->name.jobid;
        name->vpid = recvptr->name.vpid;
    }
    *seq_num = recvptr->seq_num;
    if (ORTE_SUCCESS != (ret = opal_dss.copy_payload(buf, recvptr->buf))) {
        ORTE_ERROR_LOG(ret);
    }
    /* release the data */
    OBJ_RELEASE(recvptr->buf);
    
cleanup:
    OPAL_THREAD_LOCK(&lock);
    opal_list_remove_item(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);
    OBJ_RELEASE(recvptr);
    
    return ret;
}

static int tcp_recv_buffer_nb(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              orte_rmcast_flag_t flags,
                              orte_rmcast_callback_buffer_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_buffer_nb called on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
    
    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    recvptr->flags = flags;
    recvptr->cbfunc_buffer = cbfunc;
    recvptr->cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = queue_recv(recvptr, channel, tag, NULL, cbfunc, false))) {
        if (ORTE_EXISTS == ret) {
            /* this recv already exists - just release the copy */
            OBJ_RELEASE(recvptr);
            return ORTE_SUCCESS;
        }
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(recvptr);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static void cancel_recv(orte_rmcast_channel_t channel,
                        orte_rmcast_tag_t tag)
{
    opal_list_item_t *item, *next;
    rmcast_base_recv_t *ptr;
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&recvs);
    while (item != opal_list_get_end(&recvs)) {
        next = opal_list_get_next(item);
        
        ptr = (rmcast_base_recv_t*)item;
        if (channel == ptr->channel &&
            tag == ptr->tag) {
            OPAL_THREAD_LOCK(&lock);
            opal_list_remove_item(&recvs, &ptr->item);
            OBJ_RELEASE(ptr);
            OPAL_THREAD_UNLOCK(&lock);
        }
        item = next;
    }
}

/* for the tcp module, we will be using the RML to "fake" a
 * multicast in combination with the grpcomm "xcast" interface.
 * We cannot control the network and interface in this
 * combination as it gets auto-picked well before us, so we
 * ignore that info here
 */
static int open_channel(orte_rmcast_channel_t *channel, char *name,
                        char *network, int port, char *interface, uint8_t direction)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *nchan, *chan;
    
    /* see if this name has already been assigned a channel on the specified network */
    chan = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        nchan = (rmcast_base_channel_t*)item;
        
        if (0 == strcasecmp(nchan->name, name)) {
            /* check the channel, if one was given */
            if (ORTE_RMCAST_INVALID_CHANNEL != *channel &&
                nchan->channel != *channel) {
                continue;
            }
            /* all setup - nothing to do */
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp using existing channel",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return ORTE_SUCCESS;
        }
    }
    
    /* we didn't find an existing match, so create a new channel */
    chan = OBJ_NEW(rmcast_base_channel_t);
    chan->name = strdup(name);
    /* if we were given a channel, then just use it */
    if (ORTE_RMCAST_INVALID_CHANNEL != *channel) {
        chan->channel = *channel;
    } else {
        chan->channel = next_channel++;
        *channel = chan->channel;
    }
    /* add to list of known channels */
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&channels, &chan->item);
    OPAL_THREAD_UNLOCK(&lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp opening new channel for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                          (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));

    return ORTE_SUCCESS;
}

static int close_channel(orte_rmcast_channel_t channel)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chan;

    OPAL_THREAD_LOCK(&lock);
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chan = (rmcast_base_channel_t*)item;
        
        if (channel == chan->channel) {
            opal_list_remove_item(&channels, item);
            OBJ_RELEASE(chan);
            OPAL_THREAD_UNLOCK(&lock);
            return ORTE_SUCCESS;
        }
    }
    
    OPAL_THREAD_UNLOCK(&lock);
    return ORTE_ERR_NOT_FOUND;
}

static orte_rmcast_channel_t query(void)
{
    return orte_rmcast_base.my_group_number;
}


/****    LOCAL FUNCTIONS    ****/

static void process_recv(int fd, short event, void *cbdata)
{
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    opal_buffer_t *buf = mev->buffer;
    orte_rmcast_channel_t channel;
    opal_list_item_t *item, *next;
    rmcast_base_recv_t *ptr;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, sz, n;
    opal_buffer_t *recvd_buf=NULL;
    int rc;
    orte_rmcast_seq_t recvd_seq_num;
    
    /* extract the name of the original sender */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &name, &n, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if this message is from myself, ignore it */
    if (name.jobid == ORTE_PROC_MY_NAME->jobid && name.vpid == ORTE_PROC_MY_NAME->vpid) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:tcp:recv sent from myself: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name)));
        goto cleanup;
    }

    /* extract the "channel" */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &channel, &n, ORTE_RMCAST_CHANNEL_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* extract the tag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &tag, &n, ORTE_RMCAST_TAG_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* extract the sequence number */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &recvd_seq_num, &n, ORTE_RMCAST_SEQ_T))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp:recv sender: %s tag: %d seq_num: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), (int)tag, recvd_seq_num));
    
    
    /* unpack the iovec vs buf flag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &flag, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&recvs);
    while (item != opal_list_get_end(&recvs)) {
        next = opal_list_get_next(item);
        ptr = (rmcast_base_recv_t*)item;
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:tcp:recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if ((channel == ptr->channel || ORTE_RMCAST_WILDCARD_CHANNEL == ptr->channel) &&
            (tag == ptr->tag || ORTE_RMCAST_TAG_WILDCARD == ptr->tag)) {
            
            /* match found - see if data needs to be unpacked, or if
             * we already have it so we only unpack it once
             */
            if (0 == flag && NULL == iovec_array) {
                /* iovecs included and we still need to unpack it - get
                 * the number of iovecs in the buffer
                 */
                n=1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &iovec_count, &n, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* malloc the required space */
                iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
                /* unpack the iovecs */
                for (i=0; i < iovec_count; i++) {
                    /* unpack the number of bytes in this iovec */
                    n=1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &n, OPAL_INT32))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    /* allocate the space */
                    iovec_array[i].iov_base = (uint8_t*)malloc(sz);
                    iovec_array[i].iov_len = sz;
                    /* unpack the data */
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, iovec_array[i].iov_base, &sz, OPAL_UINT8))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }                    
                }
            } else if (1 == flag && NULL == recvd_buf) {
                /* buffer was included */
                recvd_buf = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recvd_buf, buf))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }                    
            }
            
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp:recv delivering message to channel %d tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
            
            if (0 == flag) {
                /* dealing with iovecs */
                if (NULL != ptr->cbfunc_iovec) {
                    ptr->cbfunc_iovec(ORTE_SUCCESS, ptr->channel, tag,
                                      &name, recvd_seq_num,
                                      iovec_array, iovec_count, ptr->cbdata);
                    /* if it isn't persistent, remove it */
                    if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                        OPAL_THREAD_LOCK(&lock);
                        opal_list_remove_item(&recvs, &ptr->item);
                        OPAL_THREAD_UNLOCK(&lock);
                        OBJ_RELEASE(ptr);
                    }
                } else {
                    /* copy over the iovec array since it will be released by
                     * the blocking recv
                     */
                    ptr->iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
                    ptr->iovec_count = iovec_count;
                    for (i=0; i < iovec_count; i++) {
                        ptr->iovec_array[i].iov_base = (uint8_t*)malloc(iovec_array[i].iov_len);
                        ptr->iovec_array[i].iov_len = iovec_array[i].iov_len;
                        memcpy(ptr->iovec_array[i].iov_base, iovec_array[i].iov_base, iovec_array[i].iov_len);
                    }
                    /* flag it as recvd to release blocking recv */
                    ptr->recvd = true;
                }
            } else {
                if (NULL != ptr->cbfunc_buffer) {
                    ptr->cbfunc_buffer(ORTE_SUCCESS, ptr->channel, tag,
                                       &name, recvd_seq_num,
                                       recvd_buf, ptr->cbdata);
                    /* if it isn't persistent, remove it */
                    if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                        OPAL_THREAD_LOCK(&lock);
                        opal_list_remove_item(&recvs, &ptr->item);
                        OPAL_THREAD_UNLOCK(&lock);
                        OBJ_RELEASE(ptr);
                    }
                } else {
                    /* copy the buffer across since it will be released
                     * by the blocking recv
                     */
                    ptr->buf = OBJ_NEW(opal_buffer_t);
                    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(ptr->buf, recvd_buf))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }                    
                    /* flag it as recvd to release blocking recv */
                    ptr->recvd = true;
                }
            }
        }
        /* move along list */
        item = next;
    }
    
cleanup:
    OBJ_RELEASE(mev);
    if (NULL != iovec_array) {
        for (i=0; i < iovec_count; i++) {
            free(iovec_array[i].iov_base);
        }
        free(iovec_array);
    }
    if (NULL != recvd_buf) {
        OBJ_RELEASE(recvd_buf);
    }
    return;
}

static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp recvd multicast msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* clear the way for the next message */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_recv);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_MULTICAST,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_handler,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
