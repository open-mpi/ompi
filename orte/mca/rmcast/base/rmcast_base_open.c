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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

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
    NULL
};
orte_rmcast_base_t orte_rmcast_base;

static bool opened = false;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmcast_base_open(void)
{
    int value, pval, i;
    char *tmp, **nets=NULL, **ports=NULL, *ptr;
    int idx;
    struct sockaddr_in inaddr;
    uint32_t addr, netaddr, netmask;
    bool assigned;
    int rc;
    
    if (opened) {
        /* ensure we don't go through here twice */
        return ORTE_SUCCESS;
    }
    opened = true;
    
    /* ensure all global values are initialized */
    orte_rmcast_base.xmit_network = 0;
    orte_rmcast_base.my_group_name = NULL;
    orte_rmcast_base.my_group_number = 0;
    orte_rmcast_base.interface = 0;
    for (i=0; i < 255; i++) {
        orte_rmcast_base.ports[i] = 0;
    }
    
    /* public multicast channel for this job */
    mca_base_param_reg_string_name("rmcast", "base_multicast_network",
                                "Network to use for multicast xmissions [link (default) | site | org | global | tuple-addr]",
                                false, false, "link", &tmp);
    rc = ORTE_ERR_SILENT;
    if (0 == strcasecmp(tmp, "site")) {
        rc = opal_iftupletoaddr("239.255.0.0", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "org")) {
        rc = opal_iftupletoaddr("239.192.0.0", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "global")) {
        rc = opal_iftupletoaddr("224.0.1.0", &orte_rmcast_base.xmit_network, NULL);
    } else if (0 == strcasecmp(tmp, "link")) {
        /* default to link */
        rc = opal_iftupletoaddr("224.0.0.0", &orte_rmcast_base.xmit_network, NULL);
    } else if (NULL != strchr(tmp, '.')) {
        /* must have been given an actual network address */
        rc = opal_iftupletoaddr(tmp, &orte_rmcast_base.xmit_network, NULL);
    }
    
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
    }

    /* multicast interfaces */
    mca_base_param_reg_string_name("rmcast", "base_if_include",
                                   "Comma-separated list of interfaces (given in IP form) to use for multicast messages",
                                   false, false, NULL, &tmp);
    /* if nothing was provided, default to first non-loopback interface */
    if (NULL == tmp) {
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
            orte_rmcast_base.interface = ntohl(inaddr.sin_addr.s_addr);
            break;
        }
        if (idx < 0) {
            orte_show_help("help-rmcast-base.txt", "no-avail-interfaces", true);
            return ORTE_ERR_SILENT;
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
    ports = NULL;
    orte_util_parse_range_options(tmp, &ports);
    if (255 < opal_argv_count(ports)) {
        orte_show_help("help-rmcast-base.txt", "too-many-values", true,
                       "ports", tmp, opal_argv_count(ports), "255");
        free(tmp);
        opal_argv_free(nets);
        return ORTE_ERR_SILENT;
    }
    for (i=0; i < opal_argv_count(ports); i++) {
        pval = strtoul(ports[i], NULL, 10);
        if (pval >= UINT16_MAX) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(tmp);
            opal_argv_free(ports);
            return ORTE_ERR_BAD_PARAM;
        }
        orte_rmcast_base.ports[i] = pval;
    }
    free(tmp);
    opal_argv_free(ports);
    
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
static void mcast_event_constructor(orte_mcast_msg_event_t *ev)
{
    ev->ev = (opal_event_t*)malloc(sizeof(opal_event_t));
    ev->data = NULL;
}
static void mcast_event_destructor(orte_mcast_msg_event_t *ev)
{
    if (NULL != ev->ev) { 
        free(ev->ev); 
    } 
    if (NULL != ev->data) {
        free(ev->data);
    }
}
OBJ_CLASS_INSTANCE(orte_mcast_msg_event_t, 
                   opal_object_t, 
                   mcast_event_constructor, 
                   mcast_event_destructor); 

static void send_construct(rmcast_base_send_t *ptr)
{
    ptr->data = NULL;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->cbfunc = NULL;
    ptr->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(rmcast_base_send_t,
                   opal_list_item_t,
                   send_construct,
                   NULL);

static void recv_construct(rmcast_base_recv_t *ptr)
{
    ptr->channel = ORTE_RMCAST_INVALID_CHANNEL;
    ptr->recvd = false;
    ptr->data = NULL;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->flags = ORTE_RMCAST_NON_PERSISTENT;  /* default */
    ptr->cbfunc = NULL;
    ptr->cbdata = NULL;
}
static void recv_destruct(rmcast_base_recv_t *ptr)
{
    if (NULL != ptr->data) {
        OBJ_RELEASE(ptr->data);
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
    ptr->recv = -1;
    memset(&ptr->addr, 0, sizeof(ptr->addr));
    OBJ_CONSTRUCT(&ptr->send_lock, opal_mutex_t);
    ptr->sends_in_progress = false;
    OBJ_CONSTRUCT(&ptr->pending_sends, opal_list_t);
    ptr->send_data = NULL;
}
static void channel_destruct(rmcast_base_channel_t *ptr)
{
    /* cleanup the recv side */
    if (0 < ptr->recv) {
        opal_event_del(&ptr->recv_ev);
        CLOSE_THE_SOCKET(ptr->recv);
    }
    /* attempt to xmit any pending sends */
    /* cleanup the xmit side */
    if (0 < ptr->xmit) {
        opal_event_del(&ptr->send_ev);
        CLOSE_THE_SOCKET(ptr->xmit);
    }
    OBJ_DESTRUCT(&ptr->send_lock);
    /* release the channel name */
    if (NULL != ptr->name) {
        free(ptr->name);
    }
    if (NULL != ptr->send_data) {
        free(ptr->send_data);
    }
}
OBJ_CLASS_INSTANCE(rmcast_base_channel_t,
                   opal_list_item_t,
                   channel_construct,
                   channel_destruct);

#endif /* ORTE_DISABLE_FULL_SUPPORT */
