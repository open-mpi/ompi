/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/threads/threads.h"

#include "orte/mca/rmcast/base/private.h"
#include "orte/mca/rmcast/base/base.h"
#include "rmcast_tcp.h"

/* LOCAL DATA */
static bool init_completed = false;
static orte_job_t *daemons=NULL;
static bool comm_enabled = false;
static orte_thread_ctl_t ctl;
static opal_list_t tools;

/* LOCAL FUNCTIONS */
static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);

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

static void enable_comm(void);

static void disable_comm(void);

static void process_msg(orte_rmcast_msg_t *msg);

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
    orte_rmcast_base_query,
    enable_comm,
    disable_comm,
    process_msg
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
    
    /* setup local ctl */
    OBJ_CONSTRUCT(&ctl, orte_thread_ctl_t);
    OBJ_CONSTRUCT(&tools, opal_list_t);

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
    } else if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_SCHEDULER) {
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
        /* open the app public channel so we can hear app announcements and commands */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_APP_PUBLIC_CHANNEL, "app-announce",
                                               NULL, -1, NULL, ORTE_RMCAST_BIDIR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* open the heartbeat channel */
        if (ORTE_SUCCESS != (rc = open_channel(ORTE_RMCAST_HEARTBEAT_CHANNEL, "heartbeat",
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
       /* finally, setup our grp xmit/recv channels, if given */
        if (NULL != orte_rmcast_base.my_group_name) {
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
    if (ORTE_SUCCESS != (rc = orte_rmcast_base_start_threads())) {
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
    
    comm_enabled = true;
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp: finalize called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* stop the chatter */
    comm_enabled = false;

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_MULTICAST);

    /* stop the processing thread */
    orte_rmcast_base_stop_threads();

    while (NULL != (item = opal_list_remove_first(&tools))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&tools);

    OBJ_DESTRUCT(&ctl);
    return;
}

static void enable_comm(void)
{
    ORTE_ACQUIRE_THREAD(&ctl);
    orte_rmcast_base_start_threads();
    comm_enabled = true;
    ORTE_RELEASE_THREAD(&ctl);
}

static void disable_comm(void)
{
    ORTE_ACQUIRE_THREAD(&ctl);
    comm_enabled = false;
    orte_rmcast_base_stop_threads();
    ORTE_RELEASE_THREAD(&ctl);
}

static void cbfunc(int status,
                   struct orte_process_name_t* peer,
                   struct opal_buffer_t* buffer,
                   orte_rml_tag_t tag,
                   void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int send_data(rmcast_base_send_t *snd,
                     orte_rmcast_channel_t channel)
{
    opal_list_item_t *item, *next;
    orte_proc_t *proc;
    orte_odls_child_t *child;
    int rc, v;
    opal_buffer_t *buf;
    rmcast_base_channel_t *ch;
    orte_namelist_t *tool;

    if (!comm_enabled) {
        return ORTE_ERR_COMM_DISABLED;
    }

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
        ORTE_RELEASE_THREAD(&ctl);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp multicasting %d bytes to channel %d tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)buf->bytes_used,
                         (int)ch->channel, (int)snd->tag));
    
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* if we are a daemon, then we have to send it to the HNP
         * for relay to all other daemons - we cannot send it
         * ourselves as, at startup, we won't know who else is
         * out there until -after- a startup handshake is
         * exchanged via multicast
         */
        if (ORTE_PROC_IS_DAEMON) {
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp sending to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
            /* ignore errors */
            OBJ_RETAIN(buf);
            if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                    ORTE_ERROR_LOG(rc);
                }
                rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
            }
        } else {            
            /* if we don't already have it, get the daemon object */
            if (NULL == daemons) {
                daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            }
            /* send it to each daemon other than myself */
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
                /* ignore errors */
                OBJ_RETAIN(buf);
                if (0 > (rc = orte_rml.send_buffer_nb(&proc->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                    if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                        ORTE_ERROR_LOG(rc);
                    }
                    rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
                }

                /* now send it to all attached tools */
                item = opal_list_get_first(&tools);
                while (item != opal_list_get_end(&tools)) {
                    tool = (orte_namelist_t*)item;
                    next = opal_list_get_next(item);
                    OBJ_RETAIN(buf);
                    if (0 > (rc = orte_rml.send_buffer_nb(&tool->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                        if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                            ORTE_ERROR_LOG(rc);
                        }
                        opal_list_remove_item(&tools, item);
                        OBJ_RELEASE(item);
                        OBJ_RELEASE(buf);
                        rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
                    }
                    item = next;
                }
            }
        }
        /* send the message to my children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (!child->alive) {
                continue;
            }
            if (NULL == child->rml_uri) {
                /* race condition - hasn't reported in yet */
                continue;
            }
            /* ignore errors */
            OBJ_RETAIN(buf);
            if (0 > (rc = orte_rml.send_buffer_nb(child->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                    ORTE_ERROR_LOG(rc);
                }
                rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
            }
        }
        rc = ORTE_SUCCESS;
    } else {
        /* I am a tool or an app - send it to my HNP for relay */
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:tcp sending multicast to HNP %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
        OBJ_RETAIN(buf);
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
            orte_errmgr.abort(rc, "%s Failed to send message to multicast channel %d",
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
    
 cleanup:
    OBJ_RELEASE(buf);
    
    return rc;
}

static int tcp_send(orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      struct iovec *msg, int count)
{
    rmcast_base_send_t snd;
    int ret;
    
    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.iovec_array = msg;
    snd.iovec_count = count;
    snd.tag = tag;

    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
        ORTE_ERROR_LOG(ret);
    }
    
    /* carefully cleanup */
    snd.iovec_array = NULL;
    snd.iovec_count = 0;
    OBJ_DESTRUCT(&snd);

    ORTE_RELEASE_THREAD(&ctl);
    return ret;
}

static int tcp_send_nb(orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         struct iovec *msg, int count,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata)
{
    int ret;
    rmcast_base_send_t snd;
    
    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.iovec_array = msg;
    snd.iovec_count = count;
    snd.tag = tag;
    snd.cbfunc_iovec = cbfunc;
    snd.cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
        ORTE_ERROR_LOG(ret);
    }

    /* carefully cleanup */
    snd.iovec_array = NULL;
    snd.iovec_count = 0;
    OBJ_DESTRUCT(&snd);

    ORTE_RELEASE_THREAD(&ctl);
    return ret;
}

static int tcp_send_buffer(orte_rmcast_channel_t channel,
                             orte_rmcast_tag_t tag,
                             opal_buffer_t *buf)
{
    int ret;
    rmcast_base_send_t snd;
    
    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.buf = buf;
    snd.tag = tag;

    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
        ORTE_ERROR_LOG(ret);
    }
    
    /* carefully cleanup */
    snd.buf = NULL;
    OBJ_DESTRUCT(&snd);

    ORTE_RELEASE_THREAD(&ctl);
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
    
    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

    /* queue it to be sent - preserves order! */
    OBJ_CONSTRUCT(&snd, rmcast_base_send_t);
    snd.buf = buf;
    snd.tag = tag;
    snd.cbfunc_buffer = cbfunc;
    snd.cbdata = cbdata;
    
    if (ORTE_SUCCESS != (ret = send_data(&snd, channel))) {
        ORTE_ERROR_LOG(ret);
    }

    /* carefully cleanup */
    snd.buf = NULL;
    OBJ_DESTRUCT(&snd);

    ORTE_RELEASE_THREAD(&ctl);
    return ret;
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

    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

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
    ORTE_RELEASE_THREAD(&ctl);
    
    recvptr->ctl.active = true;
    ORTE_ACQUIRE_THREAD(&recvptr->ctl);
    
    /* xfer the data */
    if (NULL != name) {
        /* caller requested id of sender */
        name->jobid = recvptr->name.jobid;
        name->vpid = recvptr->name.vpid;
        ORTE_EPOCH_SET(name->epoch,recvptr->name.epoch);
    }
    *seq_num = recvptr->seq_num;
    *msg = recvptr->iovec_array;
    *count = recvptr->iovec_count;
    
    /* remove the recv */
    recvptr->iovec_array = NULL;
    recvptr->iovec_count = 0;
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
    
    ORTE_ACQUIRE_THREAD(&ctl);

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
    
    ORTE_RELEASE_THREAD(&ctl);
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

    ORTE_ACQUIRE_THREAD(&ctl);

    if (!comm_enabled) {
        ORTE_RELEASE_THREAD(&ctl);
        return ORTE_ERR_COMM_DISABLED;
    }

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
        ORTE_RELEASE_THREAD(&ctl);
        return ret;
    }
    ORTE_RELEASE_THREAD(&ctl);
    
    recvptr->ctl.active = true;
    ORTE_ACQUIRE_THREAD(&recvptr->ctl);
    
    /* xfer the data */
    if (NULL != name) {
        /* caller requested id of sender */
        name->jobid = recvptr->name.jobid;
        name->vpid = recvptr->name.vpid;
        ORTE_EPOCH_SET(name->epoch,recvptr->name.epoch);
    }
    *seq_num = recvptr->seq_num;
    if (ORTE_SUCCESS != (ret = opal_dss.copy_payload(buf, recvptr->buf))) {
        ORTE_ERROR_LOG(ret);
    }
    /* release the recv */
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
    
    ORTE_ACQUIRE_THREAD(&ctl);

    if (ORTE_RMCAST_GROUP_INPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_input_channel->channel;
    } else if (ORTE_RMCAST_GROUP_OUTPUT_CHANNEL == channel) {
        chan = orte_rmcast_base.my_output_channel->channel;
    } else {
        chan = channel;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_queue_recv(NULL, chan, tag, flags,
                                                           NULL, cbfunc, cbdata, false))) {
        if (ORTE_EXISTS == ret) {
            ret = ORTE_SUCCESS;
        } else {
            ORTE_ERROR_LOG(ret);
        }
    }
    ORTE_RELEASE_THREAD(&ctl);
   
    return ret;
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
    ORTE_ACQUIRE_THREAD(&orte_rmcast_base.main_ctl);
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
            ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
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
    opal_list_append(&orte_rmcast_base.channels, &chan->item);
    ORTE_RELEASE_THREAD(&orte_rmcast_base.main_ctl);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp opening new channel for%s%s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (ORTE_RMCAST_RECV & direction) ? " RECV" : " ",
                         (ORTE_RMCAST_XMIT & direction) ? " XMIT" : " "));

    return ORTE_SUCCESS;
}


static void process_msg(orte_rmcast_msg_t *msg)
{
    int rc;
    opal_list_item_t *item, *next;
    int v;
    orte_proc_t *proc;
    orte_odls_child_t *child;
    opal_buffer_t *buf;
    orte_namelist_t *tool;

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp processing message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg->sender)));

    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(buf, msg->buf);

    if (ORTE_PROC_IS_HNP) {
        /* if this message came from a different job family, then we have
         * to track the sender so we can relay mcast messages to them as
         * they won't be a member of the daemon job
         */
        if (ORTE_JOB_FAMILY(msg->sender.jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            tool = OBJ_NEW(orte_namelist_t);
            tool->name.jobid = msg->sender.jobid;
            tool->name.vpid = msg->sender.vpid;
            opal_list_append(&tools, &tool->item);
        }

        /* if we don't already have it, get the daemon object */
        if (NULL == daemons) {
            daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        }
        /* relay msg to each daemon excluding myself and whomever sent this to me */
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
            if (msg->sender.jobid == proc->name.jobid &&
                msg->sender.vpid == proc->name.vpid) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s rmcast:tcp relaying msg to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            /* ignore errors */
            OBJ_RETAIN(buf);
            if (0 > (rc = orte_rml.send_buffer_nb(&proc->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                    ORTE_ERROR_LOG(rc);
                }
                rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
            }
            /* now send it to all attached tools except whomever sent it to me, if applicable */
            item = opal_list_get_first(&tools);
            while (item != opal_list_get_end(&tools)) {
                tool = (orte_namelist_t*)item;
                next = opal_list_get_next(item);
                if (msg->sender.jobid == tool->name.jobid &&
                    msg->sender.vpid == tool->name.vpid) {
                    item = next;
                    continue;
                }
                OBJ_RETAIN(buf);
                if (0 > (rc = orte_rml.send_buffer_nb(&tool->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                    if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                        ORTE_ERROR_LOG(rc);
                    }
                    opal_list_remove_item(&tools, item);
                    OBJ_RELEASE(item);
                    OBJ_RELEASE(buf);
                    rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
                }
                item = next;
            }
        }
    }

    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* need to relay this to my children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (!child->alive) {
                continue;
            }
            if (NULL == child->rml_uri) {
                /* race condition */
                continue;
            }
            if (msg->sender.jobid == child->name->jobid &&
                msg->sender.vpid == child->name->vpid) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                                 "%s relaying multicast to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));

            /* ignore errors */
            OBJ_RETAIN(buf);
            if (0 > (rc = orte_rml.send_buffer_nb(child->name, buf, ORTE_RML_TAG_MULTICAST, 0, cbfunc, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc && ORTE_ERR_UNREACH != rc) {
                    ORTE_ERROR_LOG(rc);
                }
                rc = ORTE_SUCCESS;  /* don't confuse up-stream client */
            }
        }
    }
    OBJ_RELEASE(buf);

    /* now process it myself - this releases the msg */
    orte_rmcast_base_process_msg(msg);
}

    /****    LOCAL FUNCTIONS    ****/
static void recv_handler(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    if (!comm_enabled) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:tcp recvd multicast msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* clear the way for the next message */
    ORTE_MULTICAST_MESSAGE_EVENT(sender, buffer);
    
    return;
}
