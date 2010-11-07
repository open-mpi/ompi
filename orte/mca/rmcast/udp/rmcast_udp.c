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
#include "rmcast_udp.h"

/* LOCAL DATA */
static bool init_completed = false;
static opal_pointer_array_t msg_log;

/* LOCAL FUNCTIONS */
static void recv_handler(int sd, short flags, void* user);

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction);

static int setup_socket(int *sd, rmcast_base_channel_t *chan, bool recvsocket);

static int xmit_data(rmcast_base_channel_t *chan, rmcast_base_send_t *snd);

/* API FUNCTIONS */
static int init(void);

static void finalize(void);

static int udp_send_buffer(orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf);

static int udp_send_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                opal_buffer_t *buf,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata);

static int udp_send(orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec *msg, int count);

static int udp_send_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         struct iovec *msg, int count,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata);

static int udp_recv_buffer(orte_process_name_t *sender,
                             orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                           orte_rmcast_seq_t *seq_num,
                             opal_buffer_t *buf);

static int udp_recv_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_rmcast_flag_t flags,
                                orte_rmcast_callback_buffer_fn_t cbfunc,
                                void *cbdata);

static int udp_recv(orte_process_name_t *sender,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                    orte_rmcast_seq_t *seq_num,
                      struct iovec **msg, int *count);

static int udp_recv_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata);

static int open_channel(orte_rmcast_channel_t channel, char *name,
                        char *network, int port, char *interface, uint8_t direction);

/* Define the module */

orte_rmcast_module_t orte_rmcast_udp_module = {
    init,
    finalize,
    udp_send,
    udp_send_nb,
    udp_send_buffer,
    udp_send_buffer_nb,
    udp_recv,
    udp_recv_nb,
    udp_recv_buffer,
    udp_recv_buffer_nb,
    orte_rmcast_base_cancel_recv,
    open_channel,
    orte_rmcast_base_close_channel,
    orte_rmcast_base_query
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
    
    if (init_completed) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:udp: init called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the globals */
    OBJ_CONSTRUCT(&msg_log, opal_pointer_array_t);
    opal_pointer_array_init(&msg_log, 8, INT_MAX, 8);
    
    /* setup the respective public address channel */
    if (ORTE_PROC_IS_TOOL) {
        /* tools only open the sys channel */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_SYS_CHANNEL, "system",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        orte_rmcast_base.my_output_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
        orte_rmcast_base.my_input_channel = NULL;
    } else if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* daemons and hnp open the sys and data server channels */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_SYS_CHANNEL, "system",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }        
        orte_rmcast_base.my_output_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
        orte_rmcast_base.my_input_channel = NULL;
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_DATA_SERVER_CHANNEL, "data-server",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* open the error reporting channel */
         if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_ERROR_CHANNEL, "error",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else if (ORTE_PROC_IS_APP) {
        /* apps open the app public and data server channels */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_APP_PUBLIC_CHANNEL, "app-announce",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_DATA_SERVER_CHANNEL, "data-server",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* open the error reporting channel */
         if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_ERROR_CHANNEL, "error",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
         /* finally, if we are an app, setup our grp xmit/recv channels, if given */
        if (ORTE_PROC_IS_APP && NULL != orte_rmcast_base.my_group_name) {
            if (ORTE_SUCCESS != (rc = open_channel(orte_rmcast_base.my_group_number,
                                                   "recv",
                                                   NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.my_input_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
            if (ORTE_SUCCESS != (rc = open_channel(orte_rmcast_base.my_group_number+1,
                                                   "xmit",
                                                   NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.my_output_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
        }
    } else {
        opal_output(0, "rmcast:udp:init - unknown process type");
        return ORTE_ERR_SILENT;
    }
    init_completed = true;

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    rmcast_recv_log_t *log;
    int j;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:udp: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    for (j=0; j < msg_log.size; j++) {
        if (NULL != (log = opal_pointer_array_get_item(&msg_log, j))) {
            OBJ_RELEASE(log);
        }
    }
    OBJ_DESTRUCT(&msg_log);
    
    init_completed = false;
    return;
}

/* internal blocking send support */
static void internal_snd_cb(int status,
                            orte_rmcast_channel_t channel,
                            orte_rmcast_seq_t seq_num,
                            orte_rmcast_tag_t tag,
                            orte_process_name_t *sender,
                            struct iovec *msg, int count, void *cbdata)
{
    ((rmcast_base_send_t *)cbdata)->send_complete = true;
}

static void internal_snd_buf_cb(int status,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_seq_t seq_num,
                                orte_rmcast_tag_t tag,
                                orte_process_name_t *sender,
                                opal_buffer_t *buf, void *cbdata)
{
    ((rmcast_base_send_t *)cbdata)->send_complete = true;
}

static int queue_xmit(rmcast_base_send_t *snd,
                      orte_rmcast_channel_t channel)
{
    rmcast_base_channel_t *ch, *chptr;
    opal_list_item_t *item;
    
     OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: send of %d %s"
                         " called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         (int)channel));
    
   /* if we were asked to send this on our group output
     * channel, substitute it
     */
    if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        if (NULL == orte_rmcast_base.my_output_channel) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        ch = orte_rmcast_base.my_output_channel;
        goto process;
    } else if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        if (NULL == orte_rmcast_base.my_input_channel) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        ch = orte_rmcast_base.my_input_channel;
        goto process;
    }
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_base_channel_t*)item;
        if (channel == chptr->channel) {
            ch = chptr;
            break;
        }
    }
    if (NULL == ch) {
        /* didn't find it */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
process:    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: queue xmit of %d %s"
                         " called on multicast channel %03d.%03d.%03d.%03d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         OPAL_IF_FORMAT_ADDR(ch->network)));
    
    return xmit_data(ch, snd);
}

static int udp_send(orte_rmcast_channel_t channel,
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
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(snd->send_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int udp_send_nb(orte_rmcast_channel_t channel,
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
    snd->cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static int udp_send_buffer(orte_rmcast_channel_t channel,
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

    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(snd);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(snd->send_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int udp_send_buffer_nb(orte_rmcast_channel_t channel,
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
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(snd);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static int udp_recv(orte_process_name_t *name,
                    orte_rmcast_channel_t channel,
                    orte_rmcast_tag_t tag,
                    orte_rmcast_seq_t *seq_num,
                    struct iovec **msg, int *count)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    orte_rmcast_channel_t chan;

    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_input_channel->channel;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_output_channel->channel;
    } else {
        chan = channel;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_queue_recv(&recvptr, chan, tag,
                                                           ORTE_RMCAST_NON_PERSISTENT,
                                                           NULL, NULL, NULL, true))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    ORTE_PROGRESSED_WAIT(recvptr->recvd, 0, 1);
    
    /* xfer the data */
    if (NULL != name) {
        /* caller requested id of sender */
        name->jobid = recvptr->name.jobid;
        name->vpid = recvptr->name.vpid;
    }
    *seq_num = recvptr->seq_num;
    *msg = recvptr->iovec_array;
    *count = recvptr->iovec_count;
    
    /* remove the recv */
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_remove_item(&orte_rmcast_base.recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int udp_recv_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    orte_rmcast_channel_t chan;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: recv_nb called on channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));
    
    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_input_channel->channel;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_output_channel->channel;
    } else {
        chan = channel;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_queue_recv(NULL, chan, tag, flags,
                                                           cbfunc, NULL, cbdata, false))) {
        if (ORTE_EXISTS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
    
    return ORTE_SUCCESS;
}

static int udp_recv_buffer(orte_process_name_t *name,
                             orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                           orte_rmcast_seq_t *seq_num,
                             opal_buffer_t *buf)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    orte_rmcast_channel_t chan;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: recv_buffer called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));

    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_input_channel->channel;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_output_channel->channel;
    } else {
        chan = channel;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_queue_recv(&recvptr, chan, tag,
                                                           ORTE_RMCAST_NON_PERSISTENT,
                                                           NULL, NULL, NULL, true))) {
        ORTE_ERROR_LOG(ret);
        return ret;
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
    
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_remove_item(&orte_rmcast_base.recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    OBJ_RELEASE(recvptr);
    
    return ret;
}

static int udp_recv_buffer_nb(orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_rmcast_flag_t flags,
                                orte_rmcast_callback_buffer_fn_t cbfunc, void *cbdata)
{
    orte_rmcast_channel_t chan;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: recv_buffer_nb called on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
    
    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_input_channel->channel;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_output_channel->channel;
    } else {
        chan = channel;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_queue_recv(NULL, chan, tag, flags,
                                                           NULL, cbfunc, cbdata, false))) {
        if (ORTE_EXISTS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
    
    return ORTE_SUCCESS;
}

static int open_channel(orte_rmcast_channel_t channel, char *name,
                        char *network, int port, char *interface, uint8_t direction)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *nchan, *chan;
    uint32_t netaddr=0, netmask=0, intr=0;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s opening channel %d for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, name));

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
    OPAL_OUTPUT_VERBOSE((7, orte_rmcast_base.rmcast_output,
                         "%s open_channel: searching for %s:%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), name, channel));
                        
    chan = NULL;
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        nchan = (rmcast_base_channel_t*)item;
        
        OPAL_OUTPUT_VERBOSE((7, orte_rmcast_base.rmcast_output,
                             "%s open_channel: channel %s:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             nchan->name, channel));

        if (nchan->channel == channel ||
            0 == strcasecmp(nchan->name, name)) {
            /* check the network, if one was specified */
            if (0 != netaddr && netaddr != (nchan->network & netmask)) {
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
                             "%s rmcast:udp using existing channel %s:%d network %03d.%03d.%03d.%03d port %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             chan->name, chan->channel,
                             OPAL_IF_FORMAT_ADDR(chan->network),
                             (int)chan->port));
        
        if (ORTE_SUCCESS != (rc = setup_channel(chan, direction))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
    }
    
    /* we didn't find an existing match, so create a new channel */
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s creating new channel %d for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, name));

    chan = OBJ_NEW(rmcast_base_channel_t);
    chan->name = strdup(name);
    chan->channel = channel;
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
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_append(&orte_rmcast_base.channels, &chan->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp opening new channel %s:%d network %03d.%03d.%03d.%03d port %d for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         chan->name, chan->channel,
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

/****    LOCAL FUNCTIONS    ****/

static void process_recv(int fd, short event, void *cbdata)
{
    orte_mcast_msg_event_t *msg = (orte_mcast_msg_event_t*)cbdata;
    
    orte_rmcast_base_process_recv(msg);
    OBJ_RELEASE(msg);
    return;
}

static void recv_handler(int sd, short flags, void* cbdata)
{
    uint8_t *data;
    ssize_t sz;
    rmcast_base_channel_t *chan = (rmcast_base_channel_t*)cbdata;
    opal_buffer_t *buf;
    
    /* read the data */
    data = (uint8_t*)malloc(orte_rmcast_udp_sndbuf_size * sizeof(uint8_t));
    sz = read(sd, data, orte_rmcast_udp_sndbuf_size);
    
    if (sz <= 0) {
        /* this shouldn't happen - report the errno */
        opal_output(0, "%s Error on multicast recv socket event: %s(%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(errno), errno);
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp recvd %d bytes from channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)sz, (int)chan->channel));

    /* clear the way for the next message */
    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.load(buf, data, sz);
    ORTE_MULTICAST_MESSAGE_EVENT(buf, process_recv);
    
    return;
}

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction)
{
    int rc;
    int xmitsd, recvsd;
    
    if (0 <= chan->xmit && 0 <= chan->recv) {
        /* already setup */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s setup:channel %d already setup",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             chan->channel));
        return ORTE_SUCCESS;
    }
    
    /* setup the IPv4 addr info */
    chan->addr.sin_family = AF_INET;
    chan->addr.sin_addr.s_addr = htonl(chan->network);
    chan->addr.sin_port = htons(chan->port);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s setup:channel addr %03d.%03d.%03d.%03d port %d for %s:%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port,
                         (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));
    
    if (0 > chan->xmit && ORTE_RMCAST_XMIT & direction) {
        /* create a xmit socket */
        if (ORTE_SUCCESS != (rc = setup_socket(&xmitsd, chan, false))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        chan->xmit = xmitsd;
        chan->send_data = (uint8_t*)malloc(orte_rmcast_udp_sndbuf_size);
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
        
        opal_event_set(opal_event_base, &chan->recv_ev, chan->recv, OPAL_EV_READ|OPAL_EV_PERSIST, recv_handler, chan);
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
        opal_output(0, "rmcast:udp: unable to set the "
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
        /* set the recvbuf size */
        if ((setsockopt(target_sd, SOL_SOCKET, SO_RCVBUF,
                        &orte_rmcast_udp_rcvbuf_size, sizeof(orte_rmcast_udp_rcvbuf_size))) < 0) {
            opal_output(0, "%s rmcast:init: setsockopt() failed on SO_RCVBUF\n"
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
        /* set the sendbuf size */
        if ((setsockopt(target_sd, SOL_SOCKET, SO_SNDBUF,
                        &orte_rmcast_udp_sndbuf_size, sizeof(orte_rmcast_udp_sndbuf_size))) < 0) {
            opal_output(0, "%s rmcast:init: setsockopt() failed on SO_SNDBUF\n"
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

static int xmit_data(rmcast_base_channel_t *chan, rmcast_base_send_t *snd)
{
    char *bytes;
    int32_t sz;
    int rc;
    opal_buffer_t *buf;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s transmitting data for channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), chan->channel));

        /* setup the message for xmission */
        if (ORTE_SUCCESS != (rc = orte_rmcast_base_build_msg(chan, &buf, snd))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
#if 0
        /* store the working buf in the send ring buffer in case we
         * need to retransmit it later
         */
        log = OBJ_NEW(rmcast_send_log_t);
        log->channel = chan->channel;
        log->seq_num = chan->seq_num;
        opal_dss.copy_payload(log->buf, buf);
        if (NULL != (lg = (rmcast_send_log_t*)opal_ring_buffer_push(&chan->cache, log))) {
            /* release the old message */
            OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                                 "%s releasing message %d channel %d from log",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 lg->seq_num, lg->channel));
            OBJ_RELEASE(lg);
        }
#endif

        /* unload the working buf to obtain the payload */
        if (ORTE_SUCCESS != (rc = opal_dss.unload(buf, (void**)&bytes, &sz))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* done with the working buf */
        OBJ_RELEASE(buf);

        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:udp multicasting %d bytes to network %03d.%03d.%03d.%03d port %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sz,
                             OPAL_IF_FORMAT_ADDR(chan->network), (int)chan->port, (int)snd->tag));
                
        if (sz != (rc = sendto(chan->xmit, bytes, sz, 0,
                                     (struct sockaddr *)&(chan->addr), sizeof(struct sockaddr_in)))) {
            /* didn't get the message out */
            opal_output(0, "%s failed to send message to multicast network %03d.%03d.%03d.%03d on\n\terror %s(%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(chan->network),
                        strerror(errno), errno);
            rc = errno;
        } else {
            rc = ORTE_SUCCESS;
        }
        
        if (NULL != snd->buf) {
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_buffer) {
                snd->cbfunc_buffer(rc, chan->channel, chan->seq_num, snd->tag,
                                   ORTE_PROC_MY_NAME, snd->buf, snd->cbdata);
            }
        } else {
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_iovec) {
                snd->cbfunc_iovec(rc, chan->channel, chan->seq_num, snd->tag, ORTE_PROC_MY_NAME,
                                  snd->iovec_array, snd->iovec_count, snd->cbdata);
            }
        }

        /* roll to next message sequence number */
        ORTE_MULTICAST_NEXT_SEQUENCE_NUM(chan->seq_num);
CLEANUP:
        return rc;    
}
