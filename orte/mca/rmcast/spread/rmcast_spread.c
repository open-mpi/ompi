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
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_list_t channels;
static bool init_completed = false;
static orte_rmcast_channel_t next_channel;
static opal_pointer_array_t msg_log;

static  char    private_group[MAX_GROUP_NAME];
static  mailbox Mbox;

/* LOCAL FUNCTIONS */
static void recv_handler(int sd, short flags, void* user);

static int setup_channel(rmcast_base_channel_t *chan, uint8_t direction, mailbox Mbox);

static void xmit_data(int sd, short flags, void* send_req);


/* LOCAL STRUCTURE VALUES */
static rmcast_base_channel_t *my_group_channel=NULL;

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

static void cancel_recv(orte_rmcast_channel_t channel,
                        orte_rmcast_tag_t tag);

static int open_channel(orte_rmcast_channel_t *channel, char *name,
                        char *network, int port, char *interface, uint8_t direction);

static int close_channel(orte_rmcast_channel_t channel);

static orte_rmcast_channel_t query(void);

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
    cancel_recv,
    open_channel,
    close_channel,
    query
};

static void	SP_error2str( int error , char *error_str)
{
	switch( error )
	{
		case ILLEGAL_SPREAD:
			sprintf( error_str, "SP_error: (%d) Illegal spread was provided\n", error );
			break;
		case COULD_NOT_CONNECT:
			sprintf( error_str, "SP_error: (%d) Could not connect. Is Spread running?\n", error );
			break;
		case REJECT_QUOTA:
			sprintf( error_str, "SP_error: (%d) Connection rejected, to many users\n", error );
			break;
		case REJECT_NO_NAME:
			sprintf( error_str, "SP_error: (%d) Connection rejected, no name was supplied\n", error );
			break;
		case REJECT_ILLEGAL_NAME:
			sprintf( error_str, "SP_error: (%d) Connection rejected, illegal name\n", error );
			break;
		case REJECT_NOT_UNIQUE:
			sprintf( error_str, "SP_error: (%d) Connection rejected, name not unique\n", error );
			break;
		case REJECT_VERSION:
			sprintf( error_str, "SP_error: (%d) Connection rejected, library does not fit daemon\n", error );
			break;
		case CONNECTION_CLOSED:
			sprintf( error_str, "SP_error: (%d) Connection closed by spread\n", error );
			break;
		case REJECT_AUTH:
			sprintf( error_str, "SP_error: (%d) Connection rejected, authentication failed\n", error );
			break;
		case ILLEGAL_SESSION:
			sprintf( error_str, "SP_error: (%d) Illegal session was supplied\n", error );
			break;
		case ILLEGAL_SERVICE:
			sprintf( error_str, "SP_error: (%d) Illegal service request\n", error );
			break;
		case ILLEGAL_MESSAGE:
			sprintf( error_str, "SP_error: (%d) Illegal message\n", error );
			break;
		case ILLEGAL_GROUP:
			sprintf( error_str, "SP_error: (%d) Illegal group\n", error );
			break;
		case BUFFER_TOO_SHORT:
			sprintf( error_str, "SP_error: (%d) The supplied buffer was too short\n", error );
			break;
		case GROUPS_TOO_SHORT:
			sprintf( error_str, "SP_error: (%d) The supplied groups list was too short\n", error );
			break;
		case MESSAGE_TOO_LONG:
			sprintf( error_str, "SP_error: (%d) The message body + group names was too large to fit in a message\n", error );
			break;
		case NET_ERROR_ON_SESSION:
			sprintf( error_str, "SP_error: (%d) The network socket experienced an error. This Spread mailbox will no longer work until the connection is disconnected and then reconnected\n", error );
			break;
		default:
			sprintf( error_str, "SP_error: (%d) unrecognized error\n", error );
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
    orte_rmcast_channel_t channel;
    
    if (init_completed) {
        return ORTE_SUCCESS;
    }

    if ((rc = SP_connect(SPREAD_NAME, getlogin(), 0, 1, &Mbox, private_group)) < 0) {
      char error_string[1024];

      SP_error2str(rc, error_string);
      OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, 
			   "rmcast:spread: init SP_connect failed %s ",
			   error_string));
      rc = ORTE_ERROR;
      return rc;
    }

    init_completed = true;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:spread: init called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the globals */
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&recvs, opal_list_t);
    OBJ_CONSTRUCT(&channels, opal_list_t);
    next_channel = ORTE_RMCAST_DYNAMIC_CHANNELS;
    OBJ_CONSTRUCT(&msg_log, opal_pointer_array_t);
    opal_pointer_array_init(&msg_log, 8, INT_MAX, 8);
    
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
        /* setup our grp channel, if one was given */
        if (NULL != orte_rmcast_base.my_group_name) {
            channel = orte_rmcast_base.my_group_number;
            if (ORTE_SUCCESS != (rc = open_channel(&channel, orte_rmcast_base.my_group_name,
                                                   NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            my_group_channel = (rmcast_base_channel_t*)opal_list_get_last(&channels);
        }
    } else {
        opal_output(0, "rmcast:spread:init - unknown process type");
        return ORTE_ERR_SILENT;
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    rmcast_recv_log_t *log;
    int j;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output, "%s rmcast:spread: finalize called",
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
    for (j=0; j < msg_log.size; j++) {
        if (NULL != (log = opal_pointer_array_get_item(&msg_log, j))) {
            OBJ_RELEASE(log);
        }
    }
    OBJ_DESTRUCT(&msg_log);
    OPAL_THREAD_UNLOCK(&lock);
    
    OBJ_DESTRUCT(&lock);
    

    return;
}

static void iovec2scatter(scatter *out, struct iovec *msg, int count)
{
  int i;

  for (i=0;i<count;i++) {

    out->elements[i].buf = msg[i].iov_base;
    out->elements[i].len = msg[i].iov_len;
  }

  out->num_elements = count;
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
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag)
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
        opal_event_add(&ch->send_ev, 0);
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
    
    if ((snd->cbdata == NULL) || (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag)))) {
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
    
    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
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

    if (ORTE_SUCCESS != (ret = queue_xmit(snd, channel, tag))) {
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
                         "%s rmcast:spread: queue_recv called on spread channel %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ch->name, tag));
    
    if (!blocking) {
        /* do we already have a recv for this channel/tag/type? */
        OPAL_THREAD_LOCK(&lock);
        for (item = opal_list_get_first(&recvs);
             item != opal_list_get_end(&recvs);
             item = opal_list_get_next(item)) {
            rptr = (rmcast_base_recv_t*)item;
            if (channel != rptr->channel) {
                /* different channel */
                continue;
            }
            if (tag != rptr->tag) {
                /* different tag */
                continue;
            }
            if ((NULL != cbfunc_iovec && NULL != rptr->cbfunc_iovec) ||
                (NULL != cbfunc_buffer && NULL != rptr->cbfunc_buffer)) {
                /* matching type - recv already in place */
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:spread: matching recv already active on spread channel %s tag %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ch->name, tag));
                OPAL_THREAD_UNLOCK(&lock);
                return ORTE_EXISTS;
            }
        }
        OPAL_THREAD_UNLOCK(&lock);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: adding non-blocking recv on spread channel %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ch->name, tag));
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);

    return ORTE_SUCCESS;
}

static int spread_recv(orte_process_name_t *name,
                    orte_rmcast_channel_t channel,
                    orte_rmcast_tag_t tag,
                    struct iovec **msg, int *count)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->iovecs_requested = true;
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

static int spread_recv_nb(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       orte_rmcast_flag_t flags,
                       orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: recv_nb called on channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel));
    
    recvptr = OBJ_NEW(rmcast_base_recv_t);
    recvptr->iovecs_requested = true;
    recvptr->channel = channel;
    recvptr->tag = tag;
    recvptr->flags = flags;
    recvptr->cbfunc_iovec = cbfunc;
    recvptr->cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = queue_recv(recvptr, channel, tag, cbfunc, NULL, false))) {
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

static int spread_recv_buffer(orte_process_name_t *name,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           opal_buffer_t *buf)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread: recv_buffer called on multicast channel %d",
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
    int rc;
    
    /* 
     * We ignore the network and interface parameters for the 
     * Spread component.
     */
    
    /* see if this name has already been assigned a channel */
    OPAL_OUTPUT_VERBOSE((7, orte_rmcast_base.rmcast_output,
                         "%s open_channel: searching for %s:%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), name, *channel));
                        
    chan = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        nchan = (rmcast_base_channel_t*)item;
        
        OPAL_OUTPUT_VERBOSE((7, orte_rmcast_base.rmcast_output,
                             "%s open_channel: channel %s:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             nchan->name, *channel));

        if (nchan->channel == *channel ||
            0 == strcasecmp(nchan->name, name)) {

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
    /* if we were given a channel, then just use it */
    if (ORTE_RMCAST_INVALID_CHANNEL != *channel) {
        chan->channel = *channel;
    } else {
        chan->channel = next_channel++;
        *channel = chan->channel;
    }

    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&channels, &chan->item);
    OPAL_THREAD_UNLOCK(&lock);
    
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
        opal_event_set(&chan->send_ev, chan->xmit, OPAL_EV_WRITE, xmit_data, chan);        
    }
    
    if (0 > chan->recv && ORTE_RMCAST_RECV & direction) {

        chan->recv = Mbox;
        
        /* setup an event to catch messages */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s setup:channel activating recv event on fd %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),(int)chan->recv));
        
        opal_event_set(&chan->recv_ev, chan->recv, OPAL_EV_READ|OPAL_EV_PERSIST, recv_handler, chan);
        opal_event_add(&chan->recv_ev, 0);
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
    opal_buffer_t buf;
    int32_t tmp32;
    rmcast_send_log_t *log, *lg;
    
    OPAL_THREAD_LOCK(&chan->send_lock);
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s transmitting data for channel %d(%s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), chan->channel, 
			 chan->name));
    
    while (NULL != (item = opal_list_remove_first(&chan->pending_sends))) {
        snd = (rmcast_base_send_t*)item;
        
        /* setup a tmp buffer for a working area */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);

        /* start the send data area with our header */
        ORTE_MULTICAST_MESSAGE_HDR_HTON(chan->send_data, snd->tag, chan->seq_num);
        
        /* are we sending a buffer? */
        if (NULL == snd->buf) {
            /* flag the buffer as containing iovecs */
            flag = 0;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_INT8))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }            
            
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s packing %d iovecs",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 snd->iovec_count));
            
            /* pack the number of iovecs */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &snd->iovec_count, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            
            /* pack each iovec into a buffer in prep for sending
             * so we can recreate the array at the other end
             */
            for (sz=0; sz < snd->iovec_count; sz++) {
                /* pack the size */
                tmp32 = snd->iovec_array[sz].iov_len;
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s packing %d bytes for iovec %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     tmp32, sz));
                
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tmp32, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                if (0 < tmp32) {
                    /* pack the bytes */
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, snd->iovec_array[sz].iov_base, tmp32, OPAL_UINT8))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                }
            }
        } else {
            /* flag it as being a buffer */
            flag = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_INT8))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s copying payload", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* copy the payload */
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, snd->buf))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
        
        /* store the working buf in the send ring buffer in case we
         * need to retransmit it later
         */
        log = OBJ_NEW(rmcast_send_log_t);
        log->channel = chan->channel;
        log->seq_num = chan->seq_num;
        opal_dss.copy_payload(log->buf, &buf);
        if (NULL != (lg = (rmcast_send_log_t*)opal_ring_buffer_push(&chan->cache, log))) {
            /* release the old message */
            OPAL_OUTPUT_VERBOSE((5, orte_rmcast_base.rmcast_output,
                                 "%s releasing message %d channel %d from log",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 lg->seq_num, lg->channel));
            OBJ_RELEASE(lg);
        }
        
        /* unload the working buf to obtain the payload */
        if (ORTE_SUCCESS != (rc = opal_dss.unload(&buf, (void**)&bytes, &sz))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* done with the working buf */
        OBJ_DESTRUCT(&buf);

        /* add the payload, up to the limit */
        ORTE_MULTICAST_LOAD_MESSAGE(chan->send_data, bytes, sz,
                                    mca_rmcast_spread_component.max_msg_size,
                                    &outbound);
        
        if (outbound < 0) {
            /* message was too large */
            opal_output(0, "%s message to multicast network %03d.%03d.%03d.%03d failed - size %d was too large (limit: %d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(chan->network),
                        -1*outbound, mca_rmcast_spread_component.max_msg_size);
            if (1 == flag) {
                /* reload into original buffer */
                if (ORTE_SUCCESS != (rc = opal_dss.load(snd->buf, (void*)bytes, sz))) {
                    ORTE_ERROR_LOG(rc);
                }
            }
            /* cleanup */
            goto CLEANUP;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread multicasting %d bytes to group %s tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), outbound,
                             chan->name, (int)snd->tag));
                
        if (outbound != (rc = SP_multicast(chan->xmit, RELIABLE_MESS, chan->name, 0, outbound, (const char *)chan->send_data))) {
            /* didn't get the message out */
            opal_output(0, "%s failed to send message to spread group %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), chan->name);
            /* cleanup */
            goto CLEANUP;
        }
        
        if (1 == flag) {
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
    opal_event_del(&chan->send_ev);
    chan->sends_in_progress = false;

    OPAL_THREAD_UNLOCK(&chan->send_lock);
}


/****    LOCAL FUNCTIONS    ****/

static void process_recv(int fd, short event, void *cbdata)
{
    orte_mcast_msg_event_t *msg = (orte_mcast_msg_event_t*)cbdata;
    rmcast_base_channel_t *chan = msg->channel;
    opal_list_item_t *item;
    rmcast_base_recv_t *ptr;
    orte_process_name_t name;
    orte_rmcast_tag_t tag;
    opal_buffer_t buf;
    int8_t flag;
    struct iovec *iovec_array=NULL;
    int32_t iovec_count=0, i, sz, n;
    opal_buffer_t *recvd_buf=NULL;
    int rc;
    orte_rmcast_seq_t recvd_seq_num;
    rmcast_recv_log_t *log, *lg;

    /* extract the header */
    ORTE_MULTICAST_MESSAGE_HDR_NTOH(msg->data, &name, tag, recvd_seq_num);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:spread:recv sender: %s tag: %d seq_num: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), (int)tag, recvd_seq_num));
    
    /* if this message is from myself, ignore it */
    if (name.jobid == ORTE_PROC_MY_NAME->jobid && name.vpid == ORTE_PROC_MY_NAME->vpid) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread:recv sent from myself: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name)));
        goto cleanup;
    }
    
    /* if this message is from a different job family, ignore it unless
     * it is on the system channel. We ignore these messages to avoid
     * confusion between different jobs since we all may be sharing
     * multicast channels. The system channel is left open to support
     * cross-job communications via the HNP.
     */
    if (ORTE_JOB_FAMILY(name.jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* if the channel is other than the system channel, ignore it */
        if (ORTE_RMCAST_SYS_CHANNEL != chan->channel) {
            OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:spread:recv from a different job family: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&name)));
            goto cleanup;
        }
        /* if I am other than the HNP or a tool, ignore it */
        if (!ORTE_PROC_IS_HNP && !ORTE_PROC_IS_TOOL) {
            OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:spread:recv from a different job family: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&name)));
            goto cleanup;
        }
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
    
    /* find the recv for this channel, tag, and type */
    for (item = opal_list_get_first(&recvs);
         item != opal_list_get_end(&recvs);
         item = opal_list_get_next(item)) {
        ptr = (rmcast_base_recv_t*)item;
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread:recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if (chan->channel != ptr->channel) {
            continue;
        }
        
        if (tag != ptr->tag && ORTE_RMCAST_TAG_WILDCARD != ptr->tag) {
            continue;
        }
        
        if (0 == flag && !ptr->iovecs_requested) {
            /* it's an iovec and this recv is for buffers */
            continue;
        }
        
        if (1 == flag && ptr->iovecs_requested) {
            /* it's a buffer and this recv is for iovecs */
            continue;
        }
        
        /* we have a recv - unpack the data */
        if (0 == flag) {
            /* get the number of iovecs in the buffer */
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
                iovec_array[i].iov_base = NULL;
                iovec_array[i].iov_len = sz;
                if (0 < sz) {
                    /* allocate the space */
                    iovec_array[i].iov_base = (uint8_t*)malloc(sz);
                    /* unpack the data */
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, iovec_array[i].iov_base, &sz, OPAL_UINT8))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }                    
                }
            }
        } else {
            /* buffer was included */
            recvd_buf = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(recvd_buf, &buf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }                    
        }
        
        /* if the sender's vpid is invalid, then this is a request for
         * assignment of a name - so don't log the message
         */
        if (ORTE_VPID_INVALID == name.vpid) {
            goto MATCH;
        }
        
        /* look up the message log for this sender */
        log = NULL;
        for (n=0; n < msg_log.size;  n++) {
            if (NULL == (lg = (rmcast_recv_log_t*)opal_pointer_array_get_item(&msg_log, n))) {
                continue;
            }
            if ((name.jobid == lg->name.jobid && name.vpid == lg->name.vpid) &&
                chan->channel == lg->channel) {
                log = lg;
                break;
            }
        }
        if (NULL == log) {
            /* new sender - create a log */
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:spread:recv creating new msg log for %s channel %d seq# %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&name), (int)msg->channel->channel, recvd_seq_num));
            log = OBJ_NEW(rmcast_recv_log_t);
            log->name.jobid = name.jobid;
            log->name.vpid = name.vpid;
            log->channel = chan->channel;
            log->seq_num = recvd_seq_num;
            opal_pointer_array_add(&msg_log, log);
            goto MATCH;
        }
        
        if (recvd_seq_num < log->seq_num) {
            /* this must be a repeat of an earlier message - ignore it */
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:spread:recv recvd repeat msg %d (log at %d) from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 recvd_seq_num, log->seq_num, ORTE_NAME_PRINT(&name)));
            goto cleanup;
        }
        
        if (log->seq_num != (recvd_seq_num-1)) {
            /* this message out of sequence - tell
             * the sender the last number we got
             */
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:spread:recv msg %d is out of sequence (log at %d) from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 recvd_seq_num, log->seq_num, ORTE_NAME_PRINT(&name)));
            /* ignore this message */
            goto cleanup;
        }
        
        /* update the seq number */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread:recv update msg log to %d from %s:%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             recvd_seq_num, ORTE_NAME_PRINT(&name), log->channel));
        log->seq_num = recvd_seq_num;
        
    MATCH:
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:spread:recv delivering message to channel %d tag %d",
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
                /* copy the sender's name */
                ptr->name.jobid = name.jobid;
                ptr->name.vpid = name.vpid;
                /* flag it as recvd to release blocking recv */
                ptr->recvd = true;
            }
        } else {
            if (NULL != ptr->cbfunc_buffer) {
                ptr->cbfunc_buffer(ORTE_SUCCESS, ptr->channel, tag,
                                   &name, recvd_buf, ptr->cbdata);
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
                /* copy the sender's name */
                ptr->name.jobid = name.jobid;
                ptr->name.vpid = name.vpid;
                /* flag it as recvd to release blocking recv */
                ptr->recvd = true;
            }
        }
        /* we are done - only one recv can match */
        break;
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
    service srvc;
    char sender[MAX_GROUP_NAME], groups[2][MAX_GROUP_NAME];
    int num_groups;
    int16 mess_type;
    int endian_mismatch;

    /* Read all available spread messages. */
    while (SP_poll(sd) > 0) {

      data = (uint8_t*)malloc(mca_rmcast_spread_component.max_msg_size * sizeof(uint8_t));

      srvc = 0;      
      sz = SP_receive(sd, &srvc, sender, 2, &num_groups, groups, &mess_type, &endian_mismatch, mca_rmcast_spread_component.max_msg_size, (char *)data);
      
      if (sz <= 0) {
	char error_string[1024];

	SP_error2str(sz, error_string);
        /* this shouldn't happen - report the errno */
        opal_output(0, "%s Error on multicast recv spread event: %s(%d:%d:%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), error_string, sz, num_groups, endian_mismatch);
        return;
      }

      OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
			   "%s rmcast:spread recvd %d bytes from channel %d(%s)",
			   ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
			   (int)sz, num_groups, groups[0]));

      if (Is_regular_mess(srvc)) {
	int i;

	for (i=0;i<num_groups;i++) {
	  chan = get_chan_from_name(groups[i]);
	  if (chan) {
	    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
				 "%s rmcast:spread recvd %d bytes from channel %d(%s)",
				 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				 (int)sz, (int)chan->channel, chan->name));
	    
	    /* clear the way for the next message */
	    ORTE_MULTICAST_MESSAGE_EVENT(data, sz, chan, process_recv);
	  } else {
	    /*
	     * We've just received a message on a channel whose name
	     * we don't recognize, so log a message and drop the
	     * message.
	     */
	    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
				 "%s rmcast:spread recvd %d bytes from unknown channel named (%s)",
				 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				 (int)sz, groups[i]));
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
