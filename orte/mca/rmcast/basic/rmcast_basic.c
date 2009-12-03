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

#include "orte/mca/rmcast/base/private.h"
#include "orte/mca/rmcast/base/base.h"
#include "rmcast_basic.h"

/* LOCAL DATA */
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_list_t channels;
static bool init_completed = false;
static orte_rmcast_channel_t next_channel;

/* LOCAL FUNCTIONS */
static void recv_handler(int sd, short flags, void* user);

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction);

static int setup_socket(int *sd, rmcast_base_channel_t *chan, bool recvsocket);

static void xmit_data(int sd, short flags, void* send_req);

/* LOCAL STRUCTURE VALUES */
static rmcast_base_channel_t *my_group_channel=NULL;

/* API FUNCTIONS */
static int init(void);

static void finalize(void);

static int basic_send_buffer(orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf);

static int basic_send_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                opal_buffer_t *buf,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata);

static int basic_send(orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec *msg, int count);

static int basic_send_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         struct iovec *msg, int count,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata);

static int basic_recv_buffer(orte_process_name_t *sender,
                             orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf);

static int basic_recv_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_rmcast_flag_t flags,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata);

static int basic_recv(orte_process_name_t *sender,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec **msg, int *count);

static int basic_recv_nb(orte_rmcast_channel_t channel,
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

orte_rmcast_module_t orte_rmcast_basic_module = {
    init,
    finalize,
    basic_send,
    basic_send_nb,
    basic_send_buffer,
    basic_send_buffer_nb,
    basic_recv,
    basic_recv_nb,
    basic_recv_buffer,
    basic_recv_buffer_nb,
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
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:basic: init called",
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
        opal_output(0, "rmcast:basic:init - unknown process type");
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

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:basic: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
                            struct iovec *msg, int count, void *cbdata)
{
    send_complete = true;
}

static void internal_snd_buf_cb(int status,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_process_name_t *sender,
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
                         "%s rmcast:basic: send of %d %s"
                         " called on multicast channel %03d.%03d.%03d.%03d %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         OPAL_IF_FORMAT_ADDR(ch->network), ch->network));
    
    /* add it to this channel's pending sends */
    OPAL_THREAD_LOCK(&ch->send_lock);
    opal_list_append(&ch->pending_sends, &snd->item);
    
    /* do we need to start the send event? */
    if (!ch->sends_in_progress) {
        opal_event_add(&ch->send_ev, 0);
        ch->sends_in_progress = true;
    }
    OPAL_THREAD_UNLOCK(&ch->send_lock);
    
    return ORTE_SUCCESS;
}

static int basic_send(orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec *msg, int count)
{
    rmcast_base_send_t *snd;
    int ret;
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_base_send_t);
    snd->iovec_array = msg;
    snd->iovec_count = count;
    snd->tag = tag;
    snd->cbfunc_iovec = internal_snd_cb;
    snd->cbdata = snd;
    send_complete = false;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(send_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int basic_send_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         struct iovec *msg, int count,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata)
{
    int ret;
    rmcast_base_send_t *snd;
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_base_send_t);
    snd->iovec_array = msg;
    snd->iovec_count = count;
    snd->tag = tag;
    snd->cbfunc_iovec = cbfunc;
    snd->cbdata = snd;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static int basic_send_buffer(orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf)
{
    int ret;
    rmcast_base_send_t *snd;
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_base_send_t);
    snd->buf = buf;
    snd->tag = tag;
    snd->cbfunc_buffer = internal_snd_buf_cb;
    snd->cbdata = snd;
    send_buf_complete = false;

    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(snd);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(send_buf_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int basic_send_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                opal_buffer_t *buf,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata)
{
    int ret;
    rmcast_base_send_t *snd;
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_base_send_t);
    snd->buf = buf;
    snd->tag = tag;
    snd->cbfunc_buffer = cbfunc;
    snd->cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(snd);
        return ret;
    }
    
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
                         "%s rmcast:basic: queue_recv called on multicast channel %03d.%03d.%03d.%03d tag %d",
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
                                     "%s rmcast:basic: matching recv already active on multicast channel %03d.%03d.%03d.%03d tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(ch->network), tag));
                OPAL_THREAD_UNLOCK(&lock);
                return ORTE_EXISTS;
            }
        }
        OPAL_THREAD_UNLOCK(&lock);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic: adding non-blocking recv on multicast channel %03d.%03d.%03d.%03d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(ch->network), tag));
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);

    return ORTE_SUCCESS;
}

static int basic_recv(orte_process_name_t *name,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec **msg, int *count)
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
    
    /* remove the recv */
    OPAL_THREAD_LOCK(&lock);
    opal_list_remove_item(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int basic_recv_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic: recv_nb called on channel %d",
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

static int basic_recv_buffer(orte_process_name_t *name,
                             orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic: recv_buffer called on multicast channel %d",
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

static int basic_recv_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_rmcast_flag_t flags,
                                orte_rmcast_callback_buffer_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic: recv_buffer_nb called on multicast channel %d tag %d",
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

static int open_channel(orte_rmcast_channel_t *channel, char *name,
                        char *network, int port, char *interface, uint8_t direction)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *nchan, *chan;
    uint32_t netaddr=0, netmask=0, intr=0;
    int rc;
    
    /* parse the network, if provided */
    if (NULL != network) {
        if (ORTE_SUCCESS != (rc = opal_iftupletoaddr(network, &netaddr, &netmask))) {
            orte_show_help("help-rmcast-base.txt", "invalid-net-mask", true, network, ORTE_ERROR_NAME(rc));
            return ORTE_ERR_SILENT;
        }        
    }
    
    /* parse the interface, if provided */
    if (NULL != interface) {
        if (ORTE_SUCCESS != (rc = opal_iftupletoaddr(interface, &intr, NULL))) {
            orte_show_help("help-rmcast-base.txt", "invalid-net-mask", true, interface, ORTE_ERROR_NAME(rc));
            return ORTE_ERR_SILENT;
        }        
    }
    
    /* see if this name has already been assigned a channel on the specified network */
    chan = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        nchan = (rmcast_base_channel_t*)item;
        
        if (0 == strcasecmp(nchan->name, name)) {
            /* check the network, if one was specified */
            if (0 != netaddr && netaddr != (nchan->network & netmask)) {
                continue;
            }
            /* check the channel, if one was given */
            if (ORTE_RMCAST_INVALID_CHANNEL != *channel &&
                nchan->channel != *channel) {
                continue;
            }
            chan = nchan;
            break;
        }
    }
    
    if (NULL != chan) {
        /* already exists - check that the requested
         * sockets are setup
         */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic using existing channel network %03d.%03d.%03d.%03d port %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             OPAL_IF_FORMAT_ADDR(chan->network),
                             (int)chan->port));
        
        if (ORTE_SUCCESS != (rc = setup_channel(chan, direction))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
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
    /* if we were not given a network, use the default */
    if (NULL == network) {
        chan->network = orte_rmcast_base.xmit_network + chan->channel;
    } else {
        chan->network = netaddr;
    }
    /* if we were not given an interface, use the default */
    if (NULL == interface) {
        chan->interface = orte_rmcast_base.interface;
    } else {
        chan->interface = intr;
    }
    /* if we were not given a port, use a default one */
    if (port < 0) {
        chan->port = orte_rmcast_base.ports[chan->channel];
    } else {
        chan->port = port;
    }
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&channels, &chan->item);
    OPAL_THREAD_UNLOCK(&lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic opening new channel network %03d.%03d.%03d.%03d port %d for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         OPAL_IF_FORMAT_ADDR(chan->network),
                         (int)chan->port,
                         (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));

    if (ORTE_SUCCESS != (rc = setup_channel(chan, direction))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
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
    orte_mcast_msg_event_t *msg = (orte_mcast_msg_event_t*)cbdata;
    rmcast_base_channel_t *chan = msg->channel;
    opal_list_item_t *item, *next;
    rmcast_base_recv_t *ptr;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    opal_buffer_t buf;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, sz, n;
    opal_buffer_t *recvd_buf=NULL;
    int rc;

    /* extract the header */
    ORTE_MULTICAST_MESSAGE_HDR_NTOH(msg->data, &name, tag);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic:recv sender: %s tag: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), (int)tag));
    
    /* if this message is from myself, ignore it */
    if (name.jobid == ORTE_PROC_MY_NAME->jobid && name.vpid == ORTE_PROC_MY_NAME->vpid) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic:recv sent from myself: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name)));
        goto cleanup;
    }
    
    /* construct the buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* unload the message */
    ORTE_MULTICAST_UNLOAD_MESSAGE(&buf, msg->data, msg->sz);
    
    /* unpack the iovec vs buf flag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &flag, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&recvs);
    while (item != opal_list_get_end(&recvs)) {
        next = opal_list_get_next(item);
        ptr = (rmcast_base_recv_t*)item;
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic:recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if ((chan->channel == ptr->channel || ORTE_RMCAST_WILDCARD_CHANNEL == ptr->channel) &&
            (tag == ptr->tag || ORTE_RMCAST_TAG_WILDCARD == ptr->tag)) {
            
            /* match found - see if data needs to be unpacked, or if
             * we already have it so we only unpack it once
             */
            if (0 == flag && NULL == iovec_array) {
                /* iovecs included and we still need to unpack it - get
                 * the number of iovecs in the buffer
                 */
                n=1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &iovec_count, &n, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* malloc the required space */
                iovec_array = (struct iovec *)malloc(iovec_count * sizeof(struct iovec));
                /* unpack the iovecs */
                for (i=0; i < iovec_count; i++) {
                    /* unpack the number of bytes in this iovec */
                    n=1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &sz, &n, OPAL_INT32))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    /* allocate the space */
                    iovec_array[i].iov_base = (uint8_t*)malloc(sz);
                    iovec_array[i].iov_len = sz;
                    /* unpack the data */
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, iovec_array[i].iov_base, &sz, OPAL_UINT8))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }                    
                }
            } else if (1 == flag && NULL == recvd_buf) {
                /* buffer was included */
                recvd_buf = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recvd_buf, &buf))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }                    
            }
            
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:basic:recv delivering message to channel %d tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ptr->channel, (int)tag));
            
            if (0 == flag) {
                /* dealing with iovecs */
                if (NULL != ptr->cbfunc_iovec) {
                    ptr->cbfunc_iovec(ORTE_SUCCESS, ptr->channel, tag,
                                      &name, iovec_array, iovec_count, ptr->cbdata);
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
                    ptr->cbfunc_buffer(ORTE_SUCCESS, ptr->channel, tag, &name, recvd_buf, ptr->cbdata);
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
    OBJ_RELEASE(msg);
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

static void recv_handler(int sd, short flags, void* cbdata)
{
    uint8_t *data;
    ssize_t sz;
    rmcast_base_channel_t *chan = (rmcast_base_channel_t*)cbdata;
    
    /* read the data */
    data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size * sizeof(uint8_t));
    sz = read(sd, data, mca_rmcast_basic_component.max_msg_size);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic recvd %d bytes from channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)sz, (int)chan->channel));

    /* clear the way for the next message */
    ORTE_MULTICAST_MESSAGE_EVENT(data, sz, chan, process_recv);
    
    return;
}

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction)
{
    int rc;
    int xmitsd, recvsd;
    
    /* setup the IPv4 addr info */
    chan->addr.sin_family = AF_INET;
    chan->addr.sin_addr.s_addr = htonl(chan->network);
    chan->addr.sin_port = htons(chan->port);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s setup:channel addr %03d.%03d.%03d.%03d port %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port));
    
    if (0 > chan->xmit && ORTE_RMCAST_XMIT & direction) {
        /* create a xmit socket */
        if (ORTE_SUCCESS != (rc = setup_socket(&xmitsd, chan, false))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        chan->xmit = xmitsd;
        chan->send_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);
        /* setup the event to xmit messages, but don't activate it */
        opal_event_set(&chan->send_ev, chan->xmit, OPAL_EV_WRITE, xmit_data, chan);        
    }
    
    if (0 > chan->recv && ORTE_RMCAST_RECV & direction) {
        /* create a recv socket */
        if (ORTE_SUCCESS != (rc = setup_socket(&recvsd, chan, true))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        chan->recv = recvsd;
        
        /* setup an event to catch messages */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s setup:channel activating recv event on fd %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),(int)chan->recv));
        
        opal_event_set(&chan->recv_ev, chan->recv, OPAL_EV_READ|OPAL_EV_PERSIST, recv_handler, chan);
        opal_event_add(&chan->recv_ev, 0);
    }

    return ORTE_SUCCESS;
}

static int setup_socket(int *sd, rmcast_base_channel_t *chan, bool recvsocket)
{
    uint8_t ttl = 1;
    struct sockaddr_in inaddr;
    struct ip_mreq req;
    int addrlen;
    int target_sd;
    int flags;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "setup:socket addr %03d.%03d.%03d.%03d port %d",
                         OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port));
    
    target_sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if(target_sd < 0) {
        if (EAFNOSUPPORT != opal_socket_errno) {
            opal_output(0,"rmcast:init: socket() failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
        }
        return ORTE_ERR_IN_ERRNO;
    }
    
    /* set the multicast flags */
    if ((setsockopt(target_sd, IPPROTO_IP, IP_MULTICAST_TTL, 
                    (void *)&ttl, sizeof(ttl))) < 0) {
        opal_output(0,"rmcast:init: socketopt() failed on MULTICAST_TTL: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERR_IN_ERRNO;
    }
    
    /* enable port sharing */
    flags = 1;
    if (setsockopt (target_sd, SOL_SOCKET, SO_REUSEADDR, (const char *)&flags, sizeof(flags)) < 0) {
        opal_output(0, "rmcast:basic: unable to set the "
                    "SO_REUSEADDR option (%s:%d)\n",
                    strerror(opal_socket_errno), opal_socket_errno);
        CLOSE_THE_SOCKET(target_sd);
        return ORTE_ERROR;
    }

    /* if this is the recv side... */
    if (recvsocket) {
        memset(&inaddr, 0, sizeof(inaddr));
        inaddr.sin_family = AF_INET;
        inaddr.sin_addr.s_addr = htonl(chan->network);
        inaddr.sin_port = htons(chan->port);
        addrlen = sizeof(struct sockaddr_in);
        
        /* bind the socket */
        if (bind(target_sd, (struct sockaddr*)&inaddr, addrlen) < 0) {
            opal_output(0, "%s rmcast:init: bind() failed for addr %03d.%03d.%03d.%03d port %d\n\tError: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port,
                        strerror(opal_socket_errno), opal_socket_errno);
            CLOSE_THE_SOCKET(target_sd);
            return ORTE_ERROR;
        }
        /* set membership on the multicast interface */
        memset(&req, 0, sizeof (req));
        req.imr_multiaddr.s_addr = htonl(chan->network);
        req.imr_interface.s_addr = htonl(chan->interface);
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "setup:socket:membership addr %03d.%03d.%03d.%03d interface %03d.%03d.%03d.%03d",
                             OPAL_IF_FORMAT_ADDR(chan->network), OPAL_IF_FORMAT_ADDR(chan->interface)));

        if ((setsockopt(target_sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, 
                        (void *)&req, sizeof (req))) < 0) {
            opal_output(0, "%s rmcast:init: setsockopt() failed on ADD_MEMBERSHIP\n"
                        "\tfor multicast network %03d.%03d.%03d.%03d interface %03d.%03d.%03d.%03d\n\tError: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        OPAL_IF_FORMAT_ADDR(chan->network), OPAL_IF_FORMAT_ADDR(chan->interface),
                        strerror(opal_socket_errno), opal_socket_errno);
            CLOSE_THE_SOCKET(target_sd);
            return ORTE_ERROR;
        }
    } else {
        /* on the xmit side, need to set the interface */
        memset(&inaddr, 0, sizeof(inaddr));
        inaddr.sin_addr.s_addr = htonl(chan->interface);
        addrlen = sizeof(struct sockaddr_in);
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "setup:socket:xmit interface %03d.%03d.%03d.%03d",
                             OPAL_IF_FORMAT_ADDR(chan->interface)));
        
        if ((setsockopt(target_sd, IPPROTO_IP, IP_MULTICAST_IF, 
                        (void *)&inaddr, addrlen)) < 0) {
            opal_output(0, "%s rmcast:init: setsockopt() failed on MULTICAST_IF\n"
                        "\tfor multicast network %03d.%03d.%03d.%03d interface %03d.%03d.%03d.%03d\n\tError: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        OPAL_IF_FORMAT_ADDR(chan->network), OPAL_IF_FORMAT_ADDR(chan->interface),
                        strerror(opal_socket_errno), opal_socket_errno);
            CLOSE_THE_SOCKET(target_sd);
            return ORTE_ERROR;
        }
    }
    
    /* set socket up to be non-blocking */
    if((flags = fcntl(target_sd, F_GETFL, 0)) < 0) {
        opal_output(0, "rmcast:init: fcntl(F_GETFL) failed: %s (%d)", 
                    strerror(opal_socket_errno), opal_socket_errno);
        return ORTE_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(target_sd, F_SETFL, flags) < 0) {
            opal_output(0, "rmcast:init: fcntl(F_SETFL) failed: %s (%d)", 
                        strerror(opal_socket_errno), opal_socket_errno);
            return ORTE_ERROR;
        }
    }
    
    /* return the socket */
    *sd = target_sd;
    
    return ORTE_SUCCESS;
}

static void xmit_data(int sd, short flags, void* send_req)
{
    rmcast_base_channel_t *chan = (rmcast_base_channel_t*)send_req;
    rmcast_base_send_t *snd;
    opal_list_item_t *item;
    char *bytes;
    int32_t sz, outbound;
    int rc;
    int8_t flag;
    opal_buffer_t buf;
    int32_t tmp32;
    
    OPAL_THREAD_LOCK(&chan->send_lock);
    while (NULL != (item = opal_list_remove_first(&chan->pending_sends))) {
        snd = (rmcast_base_send_t*)item;
        
        /* start the send data area with our header */
        ORTE_MULTICAST_MESSAGE_HDR_HTON(chan->send_data, snd->tag);

        /* are we sending a buffer? */
        if (NULL == snd->buf) {
            /* no, we are sending iovecs - setup a tmp buffer
             * for a working area
             */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            
            /* flag the buffer as containing iovecs */
            flag = 0;
            opal_dss.pack(&buf, &flag, 1, OPAL_INT8);
            
            /* pack the number of iovecs */
            opal_dss.pack(&buf, &snd->iovec_count, 1, OPAL_INT32);
            
            /* pack each iovec into a buffer in prep for sending
             * so we can recreate the array at the other end
             */
            for (sz=0; sz < snd->iovec_count; sz++) {
                /* pack the size */
                tmp32 = snd->iovec_array[sz].iov_len;
                opal_dss.pack(&buf, &tmp32, 1, OPAL_INT32);
                /* pack the bytes */
                opal_dss.pack(&buf, &(snd->iovec_array[sz].iov_base), tmp32, OPAL_UINT8);
            }
            
            /* unload the working buf to obtain the payload */
            opal_dss.unload(&buf, (void**)&bytes, &sz);
            
            /* done with the working buf */
            OBJ_DESTRUCT(&buf);
        } else {
            /* setup a tmp buffer for a working area */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            /* flag it as being a buffer */
            flag = 1;
            opal_dss.pack(&buf, &flag, 1, OPAL_INT8);

            /* copy the payload */
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, snd->buf))) {
                ORTE_ERROR_LOG(rc);
                continue;
            }
            
            /* unload the working buf to obtain the payload */
            opal_dss.unload(&buf, (void**)&bytes, &sz);
            
            /* done with the working buf */
            OBJ_DESTRUCT(&buf);
        }
        
        /* add the payload, up to the limit */
        ORTE_MULTICAST_LOAD_MESSAGE(chan->send_data, bytes, sz,
                                    mca_rmcast_basic_component.max_msg_size,
                                    &outbound);
        
        if (0 == outbound) {
            /* message was too large */
            opal_output(0, "%s message to multicast network %03d.%03d.%03d.%03d failed - size %d was too large (limit: %d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(chan->network),
                        outbound, ORTE_RMCAST_BASIC_MAX_MSG_SIZE);
            if (1 == flag) {
                /* reload into original buffer */
                opal_dss.load(snd->buf, (void*)bytes, sz);
            }
            /* cleanup */
            OBJ_RELEASE(item);
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic multicasting %d bytes to network %03d.%03d.%03d.%03d port %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), outbound,
                             OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port, (int)snd->tag));
                
        if (outbound != (rc = sendto(chan->xmit, chan->send_data, outbound, 0,
                                     (struct sockaddr *)&(chan->addr), sizeof(struct sockaddr_in)))) {
            /* didn't get the message out */
            opal_output(0, "%s failed to send message to multicast network %03d.%03d.%03d.%03d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(chan->network));
            if (1 == flag) {
                /* reload into original buffer */
                opal_dss.load(snd->buf, (void*)bytes, sz);
            }
            /* cleanup */
            OBJ_RELEASE(item);
            continue;
        }
        
        if (1 == flag) {
            /* reload into original buffer */
            opal_dss.load(snd->buf, (void*)bytes, sz);
            
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_buffer) {
                snd->cbfunc_buffer(rc, chan->channel, snd->tag, ORTE_PROC_MY_NAME, snd->buf, snd->cbdata);
            }
        } else {
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_iovec) {
                snd->cbfunc_iovec(rc, chan->channel, snd->tag, ORTE_PROC_MY_NAME,
                                  snd->iovec_array, snd->iovec_count, snd->cbdata);
            }
        }
        /* cleanup */
        OBJ_RELEASE(item);
    }
    
    /* cleanup */
    opal_event_del(&chan->send_ev);
    chan->sends_in_progress = false;

    OPAL_THREAD_UNLOCK(&chan->send_lock);
}
