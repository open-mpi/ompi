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

#if !ORTE_DISABLE_FULL_SUPPORT

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/threads/threads.h"

#include "orte/mca/rmcast/base/private.h"

#endif

#include "orte/mca/rmcast/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmcast/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_rmcast_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Global variables
 */
orte_rmcast_module_t orte_rmcast = {
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
orte_rmcast_base_t orte_rmcast_base;
static bool opened=false;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmcast_base_open(void)
{
    int value, i;
    char *tmp, **nets=NULL, *ptr;
    int idx, lb;
    struct sockaddr_in inaddr;
    uint32_t addr, netaddr, netmask;
    bool assigned;
    int rc;
    
    if (opened) {
        /* ensure we don't go through here twice */
        return ORTE_SUCCESS;
    }
    opened = true;
    orte_rmcast_base.opened = true;
    
    /* ensure all global values are initialized */
    OBJ_CONSTRUCT(&orte_rmcast_base.main_ctl, orte_thread_ctl_t);
    OBJ_CONSTRUCT(&orte_rmcast_base.recvs, opal_list_t);
    OBJ_CONSTRUCT(&orte_rmcast_base.channels, opal_list_t);

    OBJ_CONSTRUCT(&orte_rmcast_base.recv_thread, opal_thread_t);
    OBJ_CONSTRUCT(&orte_rmcast_base.recv_ctl, orte_thread_ctl_t);
    OBJ_CONSTRUCT(&orte_rmcast_base.recv_process, opal_thread_t);
    OBJ_CONSTRUCT(&orte_rmcast_base.recv_process_ctl, orte_thread_ctl_t);

    OBJ_CONSTRUCT(&orte_rmcast_base.msg_logs, opal_list_t);
    orte_rmcast_base.unreliable_xport = false;

    orte_rmcast_base.xmit_network = 0;
    orte_rmcast_base.my_group_name = NULL;
    orte_rmcast_base.my_group_number = 0;
    orte_rmcast_base.interface = 0;
    orte_rmcast_base.ports.start = NULL;
    orte_rmcast_base.ports.end = NULL;
    orte_rmcast_base.my_output_channel = NULL;
    orte_rmcast_base.my_input_channel = NULL;

    /* public multicast channel for this job */
    mca_base_param_reg_string_name("rmcast", "base_multicast_network",
                                "Network to use for multicast xmissions [link (default) | site | org | global | tuple-addr]",
                                false, false, "link", &tmp);
    rc = ORTE_ERR_SILENT;
    if (0 == strcasecmp(tmp, "site")) {
        rc = opal_iftupletoaddr("239.255.0.150", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "org")) {
        rc = opal_iftupletoaddr("239.192.0.150", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "global")) {
        rc = opal_iftupletoaddr("224.0.1.150", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "link")) {
        /* default to link */
        rc = opal_iftupletoaddr("224.0.0.150", &orte_rmcast_base.xmit_network, NULL);
    } else if (NULL != strchr(tmp, '.')) {
        /* must have been given an actual network address */
        rc = opal_iftupletoaddr(tmp, &orte_rmcast_base.xmit_network, NULL);
    }
    free(tmp);
    
    if (ORTE_SUCCESS != rc) {
        orte_show_help("help-rmcast-base.txt", "unrecognized-network", true, tmp);
        return ORTE_ERR_SILENT;
    }

    /* channel offset */
    mca_base_param_reg_string_name("rmcast", "base_group",
                                   "Multicast group of this process (name:number)",
                                   false, false, NULL, &tmp);
    /* parse the value */
    if (NULL != tmp) {
        if (NULL == (ptr = strrchr(tmp, ':'))) {
            orte_show_help("help-rmcast-base.txt", "value-out-of-range", true,
                           tmp, "string-name:number");
            return ORTE_ERR_SILENT;
        }
        *ptr = '\0';
        orte_rmcast_base.my_group_name = strdup(tmp);
        ptr++;
        value = strtoul(ptr, NULL, 10);
        if (value < 2 || value > 255) {
            orte_show_help("help-rmcast-base.txt", "value-out-of-range", true,
                           ptr, "2-255");
            return ORTE_ERR_SILENT;
        }
        orte_rmcast_base.my_group_number = value;
        free(tmp);
    } else {
        /* since nothing was given, use our local jobid */
        orte_rmcast_base.my_group_name = strdup(ORTE_LOCAL_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        orte_rmcast_base.my_group_number = ORTE_RMCAST_DYNAMIC_CHANNELS + ORTE_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid);
    }


    /* multicast interfaces */
    mca_base_param_reg_string_name("rmcast", "base_if_include",
                                   "Comma-separated list of interfaces (given in IP form) to use for multicast messages",
                                   false, false, NULL, &tmp);
    /* if nothing was provided, default to first non-loopback interface */
    lb = -1;
    if (NULL == tmp) {
        idx = opal_ifbegin();
        while (0 < idx) {
            /* ignore the loopback interface */
            if (opal_ifisloopback(idx)) {
                /* save the loopback index */
                lb = idx;
                /* look at next one */
                idx = opal_ifnext(idx);
                continue;
            }
            if (ORTE_SUCCESS != (rc = opal_ifindextoaddr(idx, (struct sockaddr*)&inaddr, sizeof(inaddr)))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.interface = ntohl(inaddr.sin_addr.s_addr);
            break;
        }
        if (idx < 0) {
            /* if we didn't at least find a loopback, punt */
            if (lb < 0) {
                orte_show_help("help-rmcast-base.txt", "no-avail-interfaces", true);
                return ORTE_ERR_SILENT;
            }
            /* use the loopback device */
            if (ORTE_SUCCESS != (rc = opal_ifindextoaddr(lb, (struct sockaddr*)&inaddr, sizeof(inaddr)))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rmcast_base.interface = ntohl(inaddr.sin_addr.s_addr);
        }
    } else {
        /* separate the list */
        nets = opal_argv_split(tmp, ',');
        free(tmp);
        idx = -1;
        assigned = false;
        for (i=0; NULL != nets[i] && !assigned; i++) {
            if (ORTE_SUCCESS != (rc = opal_iftupletoaddr(nets[i], &netaddr, &netmask))) {
                orte_show_help("help-rmcast-base.txt", "invalid-net-mask", true, nets[i], ORTE_ERROR_NAME(rc));
                return ORTE_ERR_SILENT;
            }
            /* search for a matching interface - take the first one within the returned scope */
            idx = opal_ifbegin();
            while (0 < idx) {
                /* ignore the loopback interface */
                if (opal_ifisloopback(idx)) {
                    idx = opal_ifnext(idx);
                    continue;
                }
                if (ORTE_SUCCESS != (rc = opal_ifindextoaddr(idx, (struct sockaddr*)&inaddr, sizeof(inaddr)))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                addr = ntohl(inaddr.sin_addr.s_addr);
                if (netaddr == (addr & netmask)) {
                    orte_rmcast_base.interface = ntohl(inaddr.sin_addr.s_addr);
                    assigned = true;
                    break;
                }
                idx = opal_ifnext(idx);
           }
        }
        opal_argv_free(nets);
        if (idx < 0) {
            orte_show_help("help-rmcast-base.txt", "no-avail-interfaces", true);
            return ORTE_ERR_SILENT;
        }
    }

    /* range of available ports */
    mca_base_param_reg_string_name("rmcast", "base_multicast_ports",
                                "Ports available for multicast channels (default: 6900-7155)",
                                false, false, "6900-7154", &tmp);
    orte_util_get_ranges(tmp, &orte_rmcast_base.ports.start, &orte_rmcast_base.ports.end);
    
    /* send cache size */
    mca_base_param_reg_int_name("rmcast", "base_cache_size",
                                "Number of messages to be held in send cache (default: 16)",
                                   false, false, 16, &orte_rmcast_base.cache_size);

    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_rmcast_base.rmcast_output = opal_output_open(NULL);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rmcast", orte_rmcast_base.rmcast_output,
                                 mca_rmcast_base_static_components, 
                                 &orte_rmcast_base.rmcast_opened, true)) {
        return ORTE_ERROR;
    }

    /* All done */
    return ORTE_SUCCESS;
}

/****    CLASS INSTANCES    ****/
static void send_construct(rmcast_base_send_t *ptr)
{
    ptr->retransmit = false;
    ptr->iovec_array = NULL;
    ptr->iovec_count = 0;
    ptr->buf = NULL;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->cbfunc_iovec = NULL;
    ptr->cbfunc_buffer = NULL;
    ptr->cbdata = NULL;
    OBJ_CONSTRUCT(&ptr->ctl, orte_thread_ctl_t);
}
static void send_destruct(rmcast_base_send_t *ptr)
{
    OBJ_DESTRUCT(&ptr->ctl);
}
OBJ_CLASS_INSTANCE(rmcast_base_send_t,
                   opal_list_item_t,
                   send_construct,
                   send_destruct);

static void recv_construct(rmcast_base_recv_t *ptr)
{
    ptr->name.jobid = ORTE_JOBID_INVALID;
    ptr->name.vpid = ORTE_VPID_INVALID;
    ptr->channel = ORTE_RMCAST_INVALID_CHANNEL;
    OBJ_CONSTRUCT(&ptr->ctl, orte_thread_ctl_t);
    ptr->seq_num = ORTE_RMCAST_SEQ_INVALID;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->flags = ORTE_RMCAST_NON_PERSISTENT;  /* default */
    ptr->iovec_array = NULL;
    ptr->iovec_count = 0;
    ptr->buf = NULL;
    ptr->cbfunc_buffer = NULL;
    ptr->cbfunc_iovec = NULL;
    ptr->cbdata = NULL;
}
static void recv_destruct(rmcast_base_recv_t *ptr)
{
    int i;

    OBJ_DESTRUCT(&ptr->ctl);
    if (NULL != ptr->iovec_array) {
        for (i=0; i < ptr->iovec_count; i++) {
            if (NULL != ptr->iovec_array[i].iov_base) {
                free(ptr->iovec_array[i].iov_base);
            }
        }
        free(ptr->iovec_array);
    }
    if (NULL != ptr->buf) {
        OBJ_RELEASE(ptr->buf);
    }
}
OBJ_CLASS_INSTANCE(rmcast_base_recv_t,
                   opal_list_item_t,
                   recv_construct,
                   recv_destruct);

static void channel_construct(rmcast_base_channel_t *ptr)
{
    ptr->name = NULL;
    ptr->channel = ORTE_RMCAST_INVALID_CHANNEL;
    ptr->network = 0;
    ptr->port = 0;
    ptr->interface = 0;
    ptr->xmit = -1;
    ptr->restart = true;
    ptr->seq_num = ORTE_RMCAST_SEQ_INVALID;
    ptr->recv = -1;
    memset(&ptr->addr, 0, sizeof(ptr->addr));
    memset(&ptr->recv_ev, 0, sizeof(opal_event_t));
    OBJ_CONSTRUCT(&ptr->cache, opal_ring_buffer_t);
    opal_ring_buffer_init(&ptr->cache, orte_rmcast_base.cache_size);
}
static void channel_destruct(rmcast_base_channel_t *ptr)
{
    rmcast_send_log_t *rb;
    
    /* cleanup the recv side */
    if (0 < ptr->recv) {
        opal_event_del(&ptr->recv_ev);
        CLOSE_THE_SOCKET(ptr->recv);
    }
    /* release the channel name */
    if (NULL != ptr->name) {
        free(ptr->name);
    }
    /* clear the cache */
    while (NULL != (rb = (rmcast_send_log_t*)opal_ring_buffer_pop(&ptr->cache))) {
        OBJ_RELEASE(rb);
    }
    OBJ_DESTRUCT(&ptr->cache);
}
OBJ_CLASS_INSTANCE(rmcast_base_channel_t,
                   opal_list_item_t,
                   channel_construct,
                   channel_destruct);

static void trk_construct(rmcast_seq_tracker_t *ptr)
{
    ptr->channel = ORTE_RMCAST_INVALID_CHANNEL;
    ptr->seq_num = ORTE_RMCAST_SEQ_INVALID;
}
OBJ_CLASS_INSTANCE(rmcast_seq_tracker_t,
                   opal_list_item_t,
                   trk_construct, NULL);

static void recvlog_construct(rmcast_recv_log_t *ptr)
{
    ptr->name.jobid = ORTE_JOBID_INVALID;
    ptr->name.vpid = ORTE_VPID_INVALID;
    OBJ_CONSTRUCT(&ptr->last_msg, opal_list_t);
}
static void recvlog_destruct(rmcast_recv_log_t *ptr)
{
    opal_list_item_t *item;

    ptr->name.jobid = ORTE_JOBID_INVALID;
    ptr->name.vpid = ORTE_VPID_INVALID;
    while (NULL != (item = opal_list_remove_first(&ptr->last_msg))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->last_msg);
}
OBJ_CLASS_INSTANCE(rmcast_recv_log_t,
                   opal_list_item_t,
                   recvlog_construct,
                   recvlog_destruct);

static void sendlog_construct(rmcast_send_log_t *ptr)
{
    ptr->channel = ORTE_RMCAST_INVALID_CHANNEL;
    ptr->seq_num = ORTE_RMCAST_SEQ_INVALID;
    ptr->buf = OBJ_NEW(opal_buffer_t);
}
static void sendlog_destruct(rmcast_send_log_t *ptr)
{
    if (NULL != ptr->buf) {
        OBJ_RELEASE(ptr->buf);
    }
}
OBJ_CLASS_INSTANCE(rmcast_send_log_t,
                   opal_object_t,
                   sendlog_construct,
                   sendlog_destruct);

static void msg_construct(orte_rmcast_msg_t *ptr)
{
    ptr->buf = OBJ_NEW(opal_buffer_t);
}
static void msg_destruct(orte_rmcast_msg_t *ptr)
{
    if (NULL != ptr->buf) {
        OBJ_RELEASE(ptr->buf);
    }
}
OBJ_CLASS_INSTANCE(orte_rmcast_msg_t,
                   opal_object_t,
                   msg_construct,
                   msg_destruct);

#endif /* ORTE_DISABLE_FULL_SUPPORT */
