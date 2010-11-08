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
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/rmcast/base/private.h"
#include "orte/mca/rmcast/base/base.h"
#include "rmcast_tcp.h"

/* LOCAL DATA */
static bool init_completed = false;
static orte_job_t *daemons=NULL;

/* LOCAL FUNCTIONS */
static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);

static void relay_handler(int status, orte_process_name_t* sender,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata);
static void relay(int fd, short event, void *cbdata);

static int send_data(rmcast_base_send_t *snd, orte_rmcast_channel_t channel);

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
                           orte_rmcast_seq_t *seq_num,
                           opal_buffer_t *buf);

static int tcp_recv_buffer_nb(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              orte_rmcast_flag_t flags,
                              orte_rmcast_callback_buffer_fn_t cbfunc,
                              void *cbdata);

static int tcp_recv(orte_process_name_t *sender,
                    orte_rmcast_channel_t channel,
                    orte_rmcast_tag_t tag,
                    orte_rmcast_seq_t *seq_num,
                    struct iovec **msg, int *count);

static int tcp_recv_nb(orte_rmcast_channel_t channel,
                       orte_rmcast_tag_t tag,
                       orte_rmcast_flag_t flags,
                       orte_rmcast_callback_fn_t cbfunc,
                       void *cbdata);

static int open_channel(orte_rmcast_channel_t channel, char *name,
                        char *network, int port, char *interface, uint8_t direction);

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
    init_completed = true;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: init called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
       /* activate a recv to catch relays */
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_MULTICAST_RELAY,
                                                          ORTE_RML_PERSISTENT,
                                                          relay_handler,
                                                          NULL))) {
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
                                                   "recv", NULL, -1, NULL, ORTE_RMCAST_RECV))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.my_input_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
            if (ORTE_SUCCESS != (rc = open_channel(orte_rmcast_base.my_group_number+1,
                                                   "xmit", NULL, -1, NULL, ORTE_RMCAST_XMIT))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.my_output_channel = (rmcast_base_channel_t*)opal_list_get_last(&orte_rmcast_base.channels);
        }
    } else {
        opal_output(0, "rmcast:tcp:init - unknown process type");
        return ORTE_ERR_SILENT;
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
    
    /* start the processing thread */
    if (ORTE_SUCCESS != (rc = orte_rmcast_base_start_threads(false, true))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* now activate the non-blocking recv so we catch messages */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_MULTICAST,
                                                      ORTE_RML_PERSISTENT,
                                                      recv_handler,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_MULTICAST);
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_MULTICAST_RELAY);
    }

    /* stop the processing thread */
    orte_rmcast_base_stop_threads();

    return;
}

/* internal blocking send support */
static bool send_complete, send_buf_complete;

static void internal_snd_cb(int status,
                            orte_rmcast_channel_t channel,
                            orte_rmcast_seq_t seq_num,
                            orte_rmcast_tag_t tag,
                            orte_process_name_t *sender,
                            struct iovec *msg, int count, void *cbdata)
{
    send_complete = true;
}

static void internal_snd_buf_cb(int status,
                                orte_rmcast_channel_t channel,
                                orte_rmcast_seq_t seq_num,
                                orte_rmcast_tag_t tag,
                                orte_process_name_t *sender,
                                opal_buffer_t *buf, void *cbdata)
{
    send_buf_complete = true;
}

static int send_data(rmcast_base_send_t *snd,
                     orte_rmcast_channel_t channel)
{
    opal_list_item_t *item;
    orte_proc_t *proc;
    orte_odls_child_t *child;
    int rc, v;
    opal_buffer_t *buf;
    rmcast_base_channel_t *ch;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: send of %d %s"
                         " called on multicast channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == snd->iovec_array) ? (int)snd->buf->bytes_used : (int)snd->iovec_count,
                         (NULL == snd->iovec_array) ? "bytes" : "iovecs",
                         (int)channel));
    
    /* setup the message for xmission */
    if (ORTE_SUCCESS != (rc = orte_rmcast_base_queue_xmit(snd, channel, &buf, &ch))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    OPAL_THREAD_LOCK(&ch->send_lock);

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp multicasting %d bytes to channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)buf->bytes_used,
                         (int)ch->channel, (int)snd->tag));
    
    if (ORTE_PROC_IS_HNP) {
        /* if we don't already have it, get the daemon object */
        if (NULL == daemons) {
            daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        }
        /* send it to each daemon */
        for (v=1; v < daemons->procs->size; v++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v))) {
                continue;
            }
            if (NULL == proc->rml_uri) {
                /* not ready yet - don't know contact info */
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s rmcast:tcp dont have path to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name)));
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp sending to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            if (0 > (rc = orte_rml.send_buffer(&proc->name, buf, ORTE_RML_TAG_MULTICAST, 0))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        /* send the message to my children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (NULL == child->rml_uri) {
                /* race condition - hasn't reported in yet */
                continue;
            }
            if (0 > (rc = orte_rml.send_buffer(child->name, buf, ORTE_RML_TAG_MULTICAST, 0))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        rc = ORTE_SUCCESS;
    } else {
        /* if I am a daemon, I need to relay this to my children first */
        if (ORTE_PROC_IS_DAEMON) {
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (NULL == child->rml_uri) {
                    /* race condition */
                    continue;
                }
                OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                     "%s relaying multicast to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                if (0 > (rc = orte_rml.send_buffer(child->name, buf, ORTE_RML_TAG_MULTICAST, 0))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
        }

        /* send it to the HNP */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:tcp sending multicast to HNP %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));        
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_MULTICAST_RELAY, 0))) {
            ORTE_ERROR_LOG(rc);
            /* didn't get the message out */
            opal_output(0, "%s failed to send message to multicast channel %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)ch->channel);
            goto cleanup;
        }
        rc = ORTE_SUCCESS;
    }

    if (NULL != snd->buf) {
        /* call the cbfunc if required */
        if (NULL != snd->cbfunc_buffer) {
            snd->cbfunc_buffer(rc, channel, ch->seq_num, snd->tag,
                               ORTE_PROC_MY_NAME,
                               snd->buf, snd->cbdata);
        }
    } else {
        /* call the cbfunc if required */
        if (NULL != snd->cbfunc_iovec) {
            snd->cbfunc_iovec(rc, channel, ch->seq_num, snd->tag,
                              ORTE_PROC_MY_NAME,
                              snd->iovec_array, snd->iovec_count, snd->cbdata);
        }
    }
    
    /* roll to next message sequence number */
    ORTE_MULTICAST_NEXT_SEQUENCE_NUM(ch->seq_num);
    
 cleanup:
    OBJ_RELEASE(buf);

    OPAL_THREAD_UNLOCK(&ch->send_lock);
    
    return rc;
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
    
    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
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
    snd.cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
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

    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
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
    snd.cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&snd);
        return ret;
    }
    OBJ_DESTRUCT(&snd);

    return ORTE_SUCCESS;
}

static int tcp_recv(orte_process_name_t *name,
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
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
    opal_list_remove_item(&orte_rmcast_base.recvs, &recvptr->item);
    ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int tcp_recv_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    int ret;
    orte_rmcast_channel_t chan;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_nb called on channel %d",
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
        if (ORTE_EXISTS == ret) {
            ret = ORTE_SUCCESS;
        } else {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;
}

static int tcp_recv_buffer(orte_process_name_t *name,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           orte_rmcast_seq_t *seq_num,
                           opal_buffer_t *buf)
{
    rmcast_base_recv_t *recvptr;
    int ret;
    orte_rmcast_channel_t chan;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_buffer called on multicast channel %d",
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
    
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.recv_process_ctl);
    opal_list_remove_item(&orte_rmcast_base.recvs, &recvptr->item);
    ORTE_RELEASE_THREAD(&orte_rmcast_base.recv_process_ctl);
    OBJ_RELEASE(recvptr);
    
    return ret;
}

static int tcp_recv_buffer_nb(orte_rmcast_channel_t channel,
                              orte_rmcast_tag_t tag,
                              orte_rmcast_flag_t flags,
                              orte_rmcast_callback_buffer_fn_t cbfunc, void *cbdata)
{
    int ret;
    orte_rmcast_channel_t chan;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: recv_buffer_nb called on multicast channel %d tag %d",
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

/* for the tcp module, we will be using the RML to "fake" a
 * multicast in combination with the grpcomm "xcast" interface.
 * We cannot control the network and interface in this
 * combination as it gets auto-picked well before us, so we
 * ignore that info here
 */
static int open_channel(orte_rmcast_channel_t channel, char *name,
                        char *network, int port, char *interface, uint8_t direction)
{
    opal_list_item_t *item;
    rmcast_base_channel_t *chan;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s opening channel %d for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, name));

    /* see if this name has already been assigned a channel on the specified network */
    for (item = opal_list_get_first(&orte_rmcast_base.channels);
         item != opal_list_get_end(&orte_rmcast_base.channels);
         item = opal_list_get_next(item)) {
        chan = (rmcast_base_channel_t*)item;
        
        if (0 == strcasecmp(chan->name, name)) {
            /* check the channel, if one was given */
            if (ORTE_RMCAST_INVALID_CHANNEL != channel) {
                if (ORTE_RMCAST_INVALID_CHANNEL == chan->channel) {
                    chan->channel = channel;
                } else if (chan->channel != channel) {
                    /* another channel for this name */
                    goto newchan;
                }
            }
            /* all setup - nothing to do */
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp using existing channel",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return ORTE_SUCCESS;
        }
    }
    
 newchan:
    /* we didn't find an existing match, so create a new channel */
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s creating new channel %d for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), channel, name));

    chan = OBJ_NEW(rmcast_base_channel_t);
    chan->name = strdup(name);
    chan->channel = channel;
    /* add to list of known channels */
    OPAL_THREAD_LOCK(&orte_rmcast_base.lock);
    opal_list_append(&orte_rmcast_base.channels, &chan->item);
    OPAL_THREAD_UNLOCK(&orte_rmcast_base.lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp opening new channel for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));

    return ORTE_SUCCESS;
}


/****    LOCAL FUNCTIONS    ****/
static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    uint8_t *data;
    int32_t siz;
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp recvd multicast msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* clear the way for the next message */
    opal_dss.unload(buffer, (void**)&data, &siz);
    ORTE_MULTICAST_MESSAGE_EVENT(data, siz);
    
    return;
}

static void relay(int fd, short event, void *cbdata)
{
    orte_message_event_t *msg = (orte_message_event_t*)cbdata;
    orte_proc_t *proc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    int rc, v;
    uint8_t *data;
    int32_t siz;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp relaying multicast msg from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg->sender)));
    
    /* if we don't already have it, get the daemon object */
    if (NULL == daemons) {
        daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    }
    /* send it to each daemon other than the one that sent it to me */
    for (v=1; v < daemons->procs->size; v++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v))) {
            continue;
        }
        /* if this message came from a daemon, then we don't want
         * to send it back to the same one as it will enter an
         * infinite loop
         */
        if (ORTE_PROC_MY_NAME->jobid == msg->sender.jobid &&
            proc->name.vpid == msg->sender.vpid) {
            continue;
        }
        if (NULL == proc->rml_uri) {
            /* race condition */
            continue;
        }
        if (0 > (rc = orte_rml.send_buffer(&proc->name, msg->buffer, ORTE_RML_TAG_MULTICAST, 0))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* send the message to my children */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (NULL == child->rml_uri) {
            /* race condition */
            OPAL_OUTPUT_VERBOSE((7, orte_rmcast_base.rmcast_output,
                                 "%s child %s has not checked in",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            continue;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s relaying multicast msg from %s to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&msg->sender),
                             ORTE_NAME_PRINT(child->name)));
        if (0 > (rc = orte_rml.send_buffer(child->name, msg->buffer, ORTE_RML_TAG_MULTICAST, 0))) {
            ORTE_ERROR_LOG(rc);
        }
    }

    /* now process it myself */
    opal_dss.unload(msg->buffer, (void**)&data, &siz);
    ORTE_MULTICAST_MESSAGE_EVENT(data, siz);
    /* protect the buffer */
    OBJ_RELEASE(msg);
}

static void relay_handler(int status, orte_process_name_t* sender,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)
{
    /* if the message is from myself, ignore it */
    if (sender->jobid == ORTE_PROC_MY_NAME->jobid &&
        sender->vpid == ORTE_PROC_MY_NAME->vpid) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp relay multicast msg from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* clear the way for the next message */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, relay);
    
    return;
}
