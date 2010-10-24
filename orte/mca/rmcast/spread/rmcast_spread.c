/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "opal/types.h"

#include <string.h>
#include <sys/types.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sp.h>

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

#include "rmcast_spread.h"

#define SPREAD_NAME "4803"
/* LOCAL DATA */
static bool init_completed = false;
static opal_pointer_array_t msg_log;

static  char    private_group[MAX_GROUP_NAME];
static  mailbox Mbox;

/* LOCAL FUNCTIONS */
static void recv_handler(int sd, short flags, void* user);

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction, mailbox Mbox);

static void xmit_data(int sd, short flags, void* send_req);


/* API FUNCTIONS */
static int init(void);

static void finalize(void);

static int spread_send_buffer(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              opal_buffer_t *buf);

static int spread_send_buffer_nb(orte_rmcast_channel_t channel,
                                 orte_rmcast_tag_t tag,
                                 opal_buffer_t *buf,
                                 orte_rmcast_callback_buffer_fn_t cbfunc,
                                 void *cbdata);

static int spread_send(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       struct iovec *msg, int count);

static int spread_send_nb(orte_rmcast_channel_t channel,
                          orte_rmcast_tag_t tag,
                          struct iovec *msg, int count,
                          orte_rmcast_callback_fn_t cbfunc,
                          void *cbdata);

static int spread_recv_buffer(orte_process_name_t *sender,
                              orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              opal_buffer_t *buf);

static int spread_recv_buffer_nb(orte_rmcast_channel_t channel,
                                 orte_rmcast_tag_t tag,
                                 orte_rmcast_flag_t flags,
                                 orte_rmcast_callback_buffer_fn_t cbfunc,
                                 void *cbdata);

static int spread_recv(orte_process_name_t *sender,
                       orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       struct iovec **msg, int *count);

static int spread_recv_nb(orte_rmcast_channel_t channel,
                          orte_rmcast_tag_t tag,
                          orte_rmcast_flag_t flags,
                          orte_rmcast_callback_fn_t cbfunc,
                          void *cbdata);

static int open_channel(orte_rmcast_channel_t channel, char *name,
                        char *network, int port, char *interface, uint8_t direction);

/* Define the module */

orte_rmcast_module_t orte_rmcast_spread_module = {
    init,
    finalize,
    spread_send,
    spread_send_nb,
    spread_send_buffer,
    spread_send_buffer_nb,
    spread_recv,
    spread_recv_nb,
    spread_recv_buffer,
    spread_recv_buffer_nb,
    orte_rmcast_base_cancel_recv,
    open_channel,
    orte_rmcast_base_close_channel,
    orte_rmcast_base_query
};

static char* SP_error2str( int error)
{
	switch( error )
	{
		case ILLEGAL_SPREAD:
			return "SP_error: Illegal spread was provided";
			break;
		case COULD_NOT_CONNECT:
			return "SP_error: Could not connect. Is Spread running?";
			break;
		case REJECT_QUOTA:
			return "SP_error: Connection rejected, to many users";
			break;
		case REJECT_NO_NAME:
			return "SP_error: Connection rejected, no name was supplied";
			break;
		case REJECT_ILLEGAL_NAME:
			return "SP_error: Connection rejected, illegal name";
			break;
		case REJECT_NOT_UNIQUE:
			return "SP_error: Connection rejected, name not unique";
			break;
		case REJECT_VERSION:
			return "SP_error: Connection rejected, library does not fit daemon";
			break;
		case CONNECTION_CLOSED:
			return "SP_error: Connection closed by spread";
			break;
		case REJECT_AUTH:
			return "SP_error: Connection rejected, authentication failed";
			break;
		case ILLEGAL_SESSION:
			return "SP_error: Illegal session was supplied";
			break;
		case ILLEGAL_SERVICE:
			return "SP_error: Illegal service request";
			break;
		case ILLEGAL_MESSAGE:
			return "SP_error: Illegal message";
			break;
		case ILLEGAL_GROUP:
			return "SP_error: Illegal group";
			break;
		case BUFFER_TOO_SHORT:
			return "SP_error: The supplied buffer was too short";
			break;
		case GROUPS_TOO_SHORT:
			return "SP_error: The supplied groups list was too short";
			break;
		case MESSAGE_TOO_LONG:
			return "SP_error: (The message body + group names was too large to fit in a message";
			break;
		case NET_ERROR_ON_SESSION:
			return "SP_error: The network socket experienced an error. This Spread mailbox will no longer work until the connection is disconnected and then reconnected";
			break;
		default:
			return "SP_error: unrecognized error";
	}
}



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
    
    if ((rc = SP_connect(SPREAD_NAME, getlogin(), 0, 1, &Mbox, private_group)) < 0) {
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, 
                             "rmcast:spread: init SP_connect failed %s ",
                             SP_error2str(rc)));
        rc = ORTE_ERROR;
        return rc;
    }
    
    init_completed = true;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:spread: init called",
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
        orte_rmcast_base.my_group_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
    } else if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* daemons and hnp open the sys and data server channels */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_SYS_CHANNEL, "system",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }        
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_DATA_SERVER_CHANNEL, "data-server",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        orte_rmcast_base.my_group_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
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
        /* also setup our grp channel, if one was given */
        if (NULL != orte_rmcast_base.my_group_name) {
            if (ORTE_SUCCESS != (rc = open_channel(orte_rmcast_base.my_group_number,
                                                   orte_rmcast_base.my_group_name,
                                                   NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.my_group_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
        }
    } else {
        opal_output(0, "rmcast:spread:init - unknown process type");
        return ORTE_ERR_SILENT;
    }
    
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    rmcast_recv_log_t *log;
    int j;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:spread: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    for (j=0; j < msg_log.size; j++) {
        if (NULL != (log = opal_pointer_array_get_item(&msg_log, j))) {
            OBJ_RELEASE(log);
        }
    }
    OBJ_DESTRUCT(&msg_log);
    
    return;
}

static void internal_snd_cb(int status,
                            orte_rmcast_channel_t channel,
                            orte_rmcast_tag_t tag,
                            orte_process_name_t *sender,
                            struct iovec *msg, int count, void *cbdata)
{
    ((rmcast_base_send_t *)cbdata)->send_complete = true;
}

static void internal_snd_buf_cb(int status,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_tag_t tag,
                                orte_process_name_t *sender,
                                opal_buffer_t *buf, void *cbdata)
{
    ((rmcast_base_send_t *)cbdata)->send_complete = true;
}

static rmcast_base_channel_t *get_chan_from_name(char *name)
{
    rmcast_base_channel_t *ret = NULL;
    opal_list_item_t *item;
    
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        ret = (rmcast_base_channel_t*)item;
        if (!strcasecmp(name, ret->name)) {
            return ret;
        }
    }
    
    return NULL;
}

static int queue_xmit(rmcast_base_send_t *snd,
                      orte_rmcast_channel_t channel)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chptr, *ch;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: queue_xmit to %d:%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         channel, tag));
    
    /* if we were asked to send this on our group output
     * channel, substitute it
     */
    if (ORTE_RMCAST_GROUP_CHANNEL == channel) {
        if (NULL == orte_rmcast_base.my_group_channel) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        ch = orte_rmcast_base.my_group_channel;
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
                         "%s rmcast:spread: send of %d %s"
                         " called on multicast channel %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         ch->name));
    
    /* add it to this channel's pending sends */
    OPAL_THREAD_LOCK(&ch->send_lock);
    opal_list_append(&ch->pending_sends, &snd->item);
    
    /* do we need to start the send event? */
    if (!ch->sends_in_progress) {
        opal_event.add(&ch->send_ev, 0);
        ch->sends_in_progress = true;
    }
    OPAL_THREAD_UNLOCK(&ch->send_lock);
    
    return ORTE_SUCCESS;
}


static int spread_send(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       struct iovec *msg, int count)
{
    rmcast_base_send_t *snd;
    int ret = ORTE_ERROR;
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_base_send_t);
    snd->iovec_array = msg;
    snd->iovec_count = count;
    snd->tag = tag;
    snd->cbfunc_iovec = internal_snd_cb;
    snd->cbdata = snd;
    snd->send_complete = false;
    
    if ((snd->cbdata == NULL) || (ORTE_SUCCESS != (ret = queue_xmit(snd, channe)))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(snd->send_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int spread_send_nb(orte_rmcast_channel_t channel,
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

static int spread_send_buffer(orte_rmcast_channel_t channel,
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
    snd->send_complete = false;
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(snd);
        return ret;
    }
    
    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(snd->send_complete, 0, 1);
    return ORTE_SUCCESS;
}

static int spread_send_buffer_nb(orte_rmcast_channel_t channel,
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

static int spread_recv(orte_process_name_t *name,
                       orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       struct iovec **msg, int *count)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    orte_rmcast_channel_t chan;
    
    if (ORTE_RMCAST_GROUP_CHANNEL == channel) {
        chan = orte_rmcast_base.my_group_number;
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
    *msg = recvptr->iovec_array;
    *count = recvptr->iovec_count;
    
    /* remove the recv */
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_remove_item(&orte_rmcast_base.recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int spread_recv_nb(orte_rmcast_channel_t channel,
                          orte_rmcast_tag_t tag,
                          orte_rmcast_flag_t flags,
                          orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    orte_rmcast_channel_t chan;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: recv_nb called on channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));
    
    if (ORTE_RMCAST_GROUP_CHANNEL == channel) {
        chan = orte_rmcast_base.my_group_number;
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

static int spread_recv_buffer(orte_process_name_t *name,
                              orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              opal_buffer_t *buf)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    orte_rmcast_channel_t chan;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: recv_buffer called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));
    
    if (ORTE_RMCAST_GROUP_CHANNEL == channel) {
        chan = orte_rmcast_base.my_group_number;
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

static int spread_recv_buffer_nb(orte_rmcast_channel_t channel,
                                 orte_rmcast_tag_t tag,
                                 orte_rmcast_flag_t flags,
                                 orte_rmcast_callback_buffer_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:udp: recv_buffer_nb called on multicast channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, tag));
    
    if (ORTE_RMCAST_GROUP_CHANNEL == channel) {
        chan = orte_rmcast_base.my_group_number;
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
    int rc;
    
    /* 
     * We ignore the network and interface parameters for the 
     * Spread component.
     */
    
    /* see if this name has already been assigned a channel */
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
            chan = nchan;
            break;
        }
    }
    
    if (NULL != chan) {
        /* already exists - check that the requested
         * sockets are setup
         */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread using existing channel %s:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             chan->name, chan->channel));
        
        if (ORTE_SUCCESS != (rc = setup_channel(chan, direction, Mbox))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
    }
    
    /* we didn't find an existing match, so create a new channel */
    chan = OBJ_NEW(rmcast_base_channel_t);
    chan->name = strdup(name);
    chan->channel = channel;
    
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_append(&orte_rmcast_base.channels, &chan->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread opening new channel %s:%d for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         chan->name, chan->channel,
                         (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));
    
    if (ORTE_SUCCESS != (rc = setup_channel(chan, direction, Mbox))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction, mailbox Mbox)
{
    
    if (0 <= chan->xmit && 0 <= chan->recv) {
        /* already setup */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s setup:channel %d already setup",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             chan->channel));
        return ORTE_SUCCESS;
    }
    
    
    SP_join( Mbox, chan->name);
    
    if (0 > chan->xmit && ORTE_RMCAST_XMIT & direction) {
        
        chan->xmit = Mbox;
        chan->send_data = (uint8_t*)malloc(mca_rmcast_spread_component.max_msg_size);
        /* setup the event to xmit messages, but don't activate it */
        opal_event.set(&chan->send_ev, chan->xmit, OPAL_EV_WRITE, xmit_data, chan);        
    }
    
    if (0 > chan->recv && ORTE_RMCAST_RECV & direction) {
        
        chan->recv = Mbox;
        
        /* setup an event to catch messages */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s setup:channel activating recv event on fd %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),(int)chan->recv));
        
        opal_event.set(&chan->recv_ev, chan->recv, OPAL_EV_READ|OPAL_EV_PERSIST, recv_handler, chan);
        opal_event.add(&chan->recv_ev, 0);
    }
    
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
    opal_buffer_t *buf;
    int32_t tmp32;
    rmcast_send_log_t *log, *lg;
    
    OPAL_THREAD_LOCK(&chan->send_lock);
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s transmitting data for channel %d(%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), chan->channel, 
                         chan->name));
    
    while (NULL != (item = opal_list_remove_first(&chan->pending_sends))) {
        snd = (rmcast_base_send_t*)item;
        
        /* setup the message for xmission */
        if (ORTE_SUCCESS != (rc = orte_rmcast_base_build_msg(chan, &buf, snd))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
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
        
        /* unload the working buf to obtain the payload */
        if (ORTE_SUCCESS != (rc = opal_dss.unload(buf, (void**)&bytes, &sz))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* done with the working buf */
        OBJ_RELEASE(buf);
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread multicasting %d bytes to group %s tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), outbound,
                             chan->name, (int)snd->tag));
        
        if (0 > (rc = SP_multicast(chan->xmit, RELIABLE_MESS, chan->name, 0, sz, (const char *)bytes))) {
            /* didn't get the message out */
            opal_output(0, "%s failed to send message to spread group %s on\n\terror %s(%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), chan->name,
                        SP_error2str(rc), rc);
            rc = errno;
        }
        
        if (NULL != snd->buf) {
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_buffer) {
                snd->cbfunc_buffer(rc, chan->channel, snd->tag,
                                   ORTE_PROC_MY_NAME, snd->buf, snd->cbdata);
            }
        } else {
            /* call the cbfunc if required */
            if (NULL != snd->cbfunc_iovec) {
                snd->cbfunc_iovec(rc, chan->channel, snd->tag, ORTE_PROC_MY_NAME,
                                  snd->iovec_array, snd->iovec_count, snd->cbdata);
            }
        }
        
        /* roll to next message sequence number */
        ORTE_MULTICAST_NEXT_SEQUENCE_NUM(chan->seq_num);
    CLEANUP:        
        /* cleanup */
        OBJ_RELEASE(item);
    }
    
    /* cleanup */
    opal_event.del(&chan->send_ev);
    chan->sends_in_progress = false;
    
    OPAL_THREAD_UNLOCK(&chan->send_lock);
}


/****    LOCAL FUNCTIONS    ****/

static void process_recv(int fd, short event, void *cbdata)
{
    orte_mcast_msg_event_t *msg = (orte_mcast_msg_event_t*)cbdata;
    
    orte_rmcast_base_process_recv(msg);
    OBJ_RELEASE(msg);
    return;
}


static inline char * get_group_name(char groups[][MAX_GROUP_NAME], int indx)
{
    return groups[indx];
}

static void recv_handler(int sd, short flags, void* cbdata)
{
    uint8_t *data;
    int  sz;
    rmcast_base_channel_t *chan = (rmcast_base_channel_t*)cbdata;
    service srvc;
    char sender[MAX_GROUP_NAME];
    static void * groups;
    static int size_groups;
    int   num_groups, size_data;
    int16 mess_type;
    int endian_mismatch;
    opal_buffer_t *buf;
    
    if (!groups) {
        size_groups = 1;
        groups = malloc(size_groups*MAX_GROUP_NAME);
    }
    /* Read all available spread messages. */
    while (SP_poll(sd) > 0) {
        size_data = mca_rmcast_spread_component.max_msg_size;
        data = (uint8_t*)malloc(size_data * sizeof(uint8_t));
        
        srvc = 0;
        do {
            sz = SP_receive(sd, &srvc, sender, size_groups, &num_groups, groups, &mess_type, &endian_mismatch, size_data, (char *)data);
            if (sz < 0) {
                /* this shouldn't happen - report the errno */
                opal_output(0, "%s Error on multicast recv spread event: %s(%d:%d:%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), SP_error2str(sz), sz, num_groups, endian_mismatch);
                
                switch (sz) {
                        
                    case GROUPS_TOO_SHORT:
                        /*
                         * Number of groups required is "-num_groups" so we
                         * free the old groups array and malloc a new one of
                         * the right size (-num_groups)*MAX_GROUP_NAME.
                         */
                        size_groups = -num_groups;
                        free(groups);
                        groups = malloc(size_groups*MAX_GROUP_NAME);
                        break;
                    case BUFFER_TOO_SHORT:
                        /*
                         * Size of buffer required is "-endian_mismatch" so we
                         * free the old data array and malloc a new one of the
                         * right size (-endian_mismatch)*sizeof(uint8_t).
                         */
                        size_data = -endian_mismatch;
                        free(data);
                        data = (uint8_t*)malloc(size_data * sizeof(uint8_t));
                        if (!data) {
                            opal_output(0," %s Error in allocating data buffer for incoming message (%d)\n",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),size_data);
                            exit (-1);
                        }
                        break;
                    case ILLEGAL_SESSION:
                    case ILLEGAL_MESSAGE:
                    case CONNECTION_CLOSED:
                        exit(-1);
                }
            }
            
        } while (sz < 0);
        
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread recvd %d bytes from channel %d(%s)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)sz, num_groups, get_group_name(groups,0)));
        
        if (Is_regular_mess(srvc)) {
            int i;
            
            for (i=0;i<num_groups;i++) {
                chan = get_chan_from_name(get_group_name(groups,i));
                if (chan) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:spread recvd %d bytes from channel %d(%s)",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (int)sz, (int)chan->channel, chan->name));
                    
                    /* clear the way for the next message */
                    buf = OBJ_NEW(opal_buffer_t);
                    opal_dss.load(buf, data, sz);
                    ORTE_MULTICAST_MESSAGE_EVENT(buf, process_recv);
                } else {
                    /*
                     * We've just received a message on a channel whose name
                     * we don't recognize, so log a message and drop the
                     * message.
                     */
                    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                         "%s rmcast:spread recvd %d bytes from unknown channel named (%s)",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (int)sz, get_group_name(groups,i)));
                    free(data);
                }
            }
        } else {
            /*
             * We ignore all membership change messages for now.
             */
            free(data);
            
        }
    }
    return;
}
