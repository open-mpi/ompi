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

#include "orte/mca/rmcast/base/base.h"
#include "rmcast_basic.h"

/* LOCAL DATA */
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_list_t channels;
static opal_list_t networks;
static bool init_completed = false;
static uint8_t next_octet1 = 0;
static uint8_t next_octet2 = 0;
static uint8_t next_octet3 = 0;
static unsigned int next_channel = 0;

/* LOCAL FUNCTIONS */
#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)

static void recv_handler(int sd, short flags, void* user);

static int setup_socket(int *sd, uint32_t chan, uint16_t port, bool bindsocket);

static void xmit_data(int sd, short flags, void* send_req);

static uint32_t parse_network(char *network);

/*
 * Data structure for tracking networks and their channels
 */
typedef struct {
    opal_list_item_t item;
    uint32_t network;
    uint8_t next_freq;
} rmcast_basic_network_t;
static void newtork_construct(rmcast_basic_network_t *ptr)
{
    ptr->network = 0;
    ptr->next_freq = 0;
}
OBJ_CLASS_INSTANCE(rmcast_basic_network_t,
                   opal_list_item_t,
                   newtork_construct,
                   NULL);

/*
 * Data structure for tracking assigned channels
 */
typedef struct {
    opal_list_item_t item;
    char *name;
    unsigned int channel;
    uint32_t full_addr;
    uint16_t port;
    uint8_t freq;
    int xmit;
    int recv;
    struct sockaddr_in addr;
    opal_event_t send_ev;
    opal_mutex_t send_lock;
    bool sends_in_progress;
    opal_list_t pending_sends;
    uint8_t *send_data;
    opal_event_t recv_ev;
    uint8_t *recvd_data;
} rmcast_basic_channel_t;

static void channel_construct(rmcast_basic_channel_t *ptr)
{
    ptr->name = NULL;
    ptr->channel = 0;
    ptr->full_addr = 0;
    ptr->port = 0;
    ptr->freq = 0;
    ptr->xmit = -1;
    ptr->recv = -1;
    memset(&ptr->addr, 0, sizeof(ptr->addr));
    OBJ_CONSTRUCT(&ptr->send_lock, opal_mutex_t);
    ptr->sends_in_progress = false;
    OBJ_CONSTRUCT(&ptr->pending_sends, opal_list_t);
    ptr->send_data = NULL;
    ptr->recvd_data = NULL;
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&channels, &ptr->item);
    OPAL_THREAD_UNLOCK(&lock);
}
static void channel_destruct(rmcast_basic_channel_t *ptr)
{
    /* cleanup the recv side */
    opal_event_del(&ptr->recv_ev);
    if (0 < ptr->recv) {
        CLOSE_THE_SOCKET(ptr->recv);
    }
    if (NULL != ptr->recvd_data) {
        free(ptr->recvd_data);
    }
    /* attempt to xmit any pending sends */
    /* cleanup the xmit side */
    opal_event_del(&ptr->send_ev);
    if (0 < ptr->xmit) {
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
OBJ_CLASS_INSTANCE(rmcast_basic_channel_t,
                   opal_list_item_t,
                   channel_construct,
                   channel_destruct);

/*
 * Data structure for tracking registered non-blocking recvs
 */
typedef struct {
    opal_list_item_t item;
    uint32_t channel;
    bool recvd;
    opal_buffer_t *data;
    orte_rmcast_tag_t tag;
    orte_rmcast_flag_t flags;
    orte_rmcast_callback_fn_t cbfunc;
    void *cbdata;
} rmcast_basic_recv_t;

static void recv_construct(rmcast_basic_recv_t *ptr)
{
    ptr->channel = 0;
    ptr->recvd = false;
    ptr->data = NULL;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->flags = ORTE_RMCAST_NON_PERSISTENT;  /* default */
    ptr->cbfunc = NULL;
    ptr->cbdata = NULL;
    OPAL_THREAD_LOCK(&lock);
    opal_list_append(&recvs, &ptr->item);
    OPAL_THREAD_UNLOCK(&lock);
}
static void recv_destruct(rmcast_basic_recv_t *ptr)
{
    if (NULL != ptr->data) {
        OBJ_RELEASE(ptr->data);
    }
}
OBJ_CLASS_INSTANCE(rmcast_basic_recv_t,
                   opal_list_item_t,
                   recv_construct,
                   recv_destruct);

/*
 * Data structure for tracking pending sends
 */
typedef struct {
    opal_list_item_t item;
    bool send_complete;
    opal_buffer_t *data;
    orte_rmcast_tag_t tag;
    orte_rmcast_callback_fn_t cbfunc;
    void *cbdata;
} rmcast_basic_send_t;

static void send_construct(rmcast_basic_send_t *ptr)
{
    ptr->send_complete = false;
    ptr->data = NULL;
    ptr->tag = ORTE_RMCAST_TAG_INVALID;
    ptr->cbfunc = NULL;
    ptr->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(rmcast_basic_send_t,
                   opal_list_item_t,
                   send_construct,
                   NULL);


/* API FUNCTIONS */
static int init(void);

static void finalize(void);

static int basic_send(unsigned int channel,
                      orte_rmcast_tag_t tag,
                      opal_buffer_t *buf);

static int basic_send_nb(unsigned int channel,
                         orte_rmcast_tag_t tag,
                         opal_buffer_t *buf,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata);

static int basic_recv(unsigned int channel,
                      orte_rmcast_tag_t tag,
                      opal_buffer_t *buf);

static int basic_recv_nb(unsigned int channel,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata);

static void cancel_recv(unsigned int channel,
                        orte_rmcast_tag_t tag);

static unsigned int open_channel(char *name, char *network, uint8_t direction);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the HNP. Remote
 * procs interact with the HNP's IOF via the HNP's receive function,
 * which operates independently and is in the rmcast_basic_receive.c file
 */

orte_rmcast_module_t orte_rmcast_basic_module = {
    init,
    finalize,
    basic_send,
    basic_send_nb,
    basic_recv,
    basic_recv_nb,
    cancel_recv,
    open_channel
};

static int init(void)
{
    int xmitsd, recvsd;
    rmcast_basic_channel_t *chan;
    rmcast_basic_network_t *net;
    uint8_t freq;
    char *name;
    int rc;
    uint16_t port;
    
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
    OBJ_CONSTRUCT(&networks, opal_list_t);

    /* set the last octets to point at the beginning of the
     * specified octet ranges
     */
    next_octet1 = orte_rmcast_base.octet1[0];
    next_octet2 = orte_rmcast_base.octet2[0];
    next_octet3 = orte_rmcast_base.octet3[0];
    
    /* form our base-level network */
    net = OBJ_NEW(rmcast_basic_network_t);
    net->network = OPAL_IF_ASSEMBLE_NETWORK(next_octet1, next_octet2, next_octet3);
    
    /* select our frequency and port */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_TOOL) {
        freq = ORTE_RMCAST_SYS_ADDR + orte_rmcast_base.channel_offset;
        name = "system";
    } else if (ORTE_PROC_IS_APP) {
        freq = ORTE_RMCAST_APP_PUBLIC_ADDR + orte_rmcast_base.channel_offset;
        name = "app-public";
    } else {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    port = orte_rmcast_base.ports[freq-orte_rmcast_base.channel_offset-1];
    
    /* setup the next freq for this network */
    net->next_freq = freq + 1;
    
    /* add this channel to our list */
    chan = OBJ_NEW(rmcast_basic_channel_t);
    chan->name = strdup(name);
    chan->full_addr = net->network + freq;
    chan->port = port;
    chan->channel = next_channel++;

    /* setup the IPv4 addr info */
    chan->addr.sin_family = AF_INET;
    chan->addr.sin_addr.s_addr = htonl(chan->full_addr);
    chan->addr.sin_port = htons(chan->port);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "addr %03d.%03d.%03d.%03d port %d freq %d offset %d ports %d",
                         OPAL_IF_FORMAT_ADDR(chan->full_addr), (int)port,
                         (int)freq, (int)orte_rmcast_base.channel_offset,
                         (int)orte_rmcast_base.ports[0]));
    
    /* create a xmit socket */
    if (ORTE_SUCCESS != (rc = setup_socket(&xmitsd, chan->full_addr, port, false))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    chan->xmit = xmitsd;
    chan->send_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);

    /* create a recv socket */
    if (ORTE_SUCCESS != (rc = setup_socket(&recvsd, chan->full_addr, port, true))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    chan->recv = recvsd;
    chan->recvd_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);

    /* setup an event to catch messages */
    opal_event_set(&chan->recv_ev, chan->recv, OPAL_EV_READ|OPAL_EV_PERSIST, recv_handler, chan);
    opal_event_add(&chan->recv_ev, 0);
    
    /* setup the event to xmit messages, but don't activate it */
    opal_event_set(&chan->send_ev, chan->xmit, OPAL_EV_WRITE, xmit_data, chan);
    
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
    while (NULL != (item = opal_list_remove_first(&networks))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&networks);
    OPAL_THREAD_UNLOCK(&lock);

    OBJ_DESTRUCT(&lock);
    
    return;
}

/* internal blocking send support */
static void internal_snd_cb(int channel, opal_buffer_t *buf, void *cbdata)
{
    rmcast_basic_send_t *snd = (rmcast_basic_send_t*)cbdata;
    
    snd->send_complete = true;
}

static int basic_send(unsigned int channel,
                      orte_rmcast_tag_t tag,
                      opal_buffer_t *buf)
{
    opal_list_item_t *item;
    rmcast_basic_channel_t *chptr, *ch;
    rmcast_basic_send_t *snd;
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_basic_channel_t*)item;
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
                         "%s rmcast:basic: send of %lu bytes"
                         " called on multicast channel %03d.%03d.%03d.%03d %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (unsigned long)buf->bytes_used,
                         OPAL_IF_FORMAT_ADDR(ch->full_addr), ch->full_addr));
    
    /* check the msg size to ensure it isn't too big */
    if (buf->bytes_used > (ORTE_RMCAST_BASIC_MAX_MSG_SIZE-10)) {
        orte_show_help("help-orte-rmcast-basic.txt",
                       "orte-rmcast-basic:msg-too-large", true,
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       OPAL_IF_FORMAT_ADDR(ch->full_addr), tag,
                       buf->bytes_used,
                       ORTE_RMCAST_BASIC_MAX_MSG_SIZE-10);
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_basic_send_t);
    snd->data = buf;
    snd->tag = tag;
    snd->cbfunc = internal_snd_cb;
    snd->cbdata = snd;
    
    /* add it to this channel's pending sends */
    OPAL_THREAD_LOCK(&ch->send_lock);
    opal_list_append(&ch->pending_sends, &snd->item);
    
    /* do we need to start the send event? */
    if (!ch->sends_in_progress) {
        opal_event_add(&ch->send_ev, 0);
        ch->sends_in_progress = true;
    }
    OPAL_THREAD_UNLOCK(&ch->send_lock);

    /* now wait for the send to complete */
    ORTE_PROGRESSED_WAIT(snd->send_complete, 0, 1);
    
    return ORTE_SUCCESS;
}

static int basic_send_nb(unsigned int channel,
                         orte_rmcast_tag_t tag,
                         opal_buffer_t *buf,
                         orte_rmcast_callback_fn_t cbfunc,
                         void *cbdata)
{
    opal_list_item_t *item;
    rmcast_basic_channel_t *chptr, *ch;
    rmcast_basic_send_t *snd;
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_basic_channel_t*)item;
        if (channel == chptr->channel) {
            ch = chptr;
            break;
        }
    }
    if (NULL == ch) {
        /* didn't find it */
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((0, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic: send_nb of %lu bytes"
                         " called on multicast channel %03d.%03d.%03d.%03d %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (unsigned long)buf->bytes_used,
                         OPAL_IF_FORMAT_ADDR(ch->full_addr), ch->full_addr));
    
    if (buf->bytes_used > (ORTE_RMCAST_BASIC_MAX_MSG_SIZE-10)) {
        orte_show_help("help-orte-rmcast-basic.txt",
                       "orte-rmcast-basic:msg-too-large", true,
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       OPAL_IF_FORMAT_ADDR(ch->full_addr), tag,
                       buf->bytes_used,
                       ORTE_RMCAST_BASIC_MAX_MSG_SIZE-10);
        return ORTE_ERR_NOT_SUPPORTED;
    }

    /* queue it to be sent - preserves order! */
    snd = OBJ_NEW(rmcast_basic_send_t);
    snd->data = buf;
    snd->tag = tag;
    snd->cbfunc = cbfunc;
    snd->cbdata = cbdata;
    
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

static int basic_recv(unsigned int channel,
                      orte_rmcast_tag_t tag,
                      opal_buffer_t *buf)
{
    opal_list_item_t *item;
    rmcast_basic_recv_t *recvptr;
    rmcast_basic_channel_t *ch, *chptr;
    
    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_basic_channel_t*)item;
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
                         "%s rmcast:basic: recv called on multicast channel %03d.%03d.%03d.%03d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(ch->full_addr)));
    
    recvptr = OBJ_NEW(rmcast_basic_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    
    ORTE_PROGRESSED_WAIT(recvptr->recvd, 0, 1);
    
    opal_dss.copy_payload(buf, recvptr->data);
    OPAL_THREAD_LOCK(&lock);
    opal_list_remove_item(&recvs, &recvptr->item);
    OPAL_THREAD_UNLOCK(&lock);
    OBJ_RELEASE(recvptr);
    
    return ORTE_SUCCESS;
}

static int basic_recv_nb(unsigned int channel,
                         orte_rmcast_flag_t flags,
                         orte_rmcast_tag_t tag,
                         orte_rmcast_callback_fn_t cbfunc, void *cbdata)
{
    opal_list_item_t *item;
    rmcast_basic_recv_t *recvptr;
    rmcast_basic_channel_t *ch, *chptr;

    /* find the channel */
    ch = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        chptr = (rmcast_basic_channel_t*)item;
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
                         "%s rmcast:basic: recv_nb called on multicast channel %03d.%03d.%03d.%03d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_IF_FORMAT_ADDR(ch->full_addr)));
    
    recvptr = OBJ_NEW(rmcast_basic_recv_t);
    recvptr->channel = channel;
    recvptr->tag = tag;
    recvptr->flags = flags;
    recvptr->cbfunc = cbfunc;
    recvptr->cbdata = cbdata;
    
    return ORTE_SUCCESS;
}

static void cancel_recv(unsigned int channel,
                        orte_rmcast_tag_t tag)
{
    opal_list_item_t *item, *next;
    rmcast_basic_recv_t *ptr;
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&recvs);
    while (item != opal_list_get_end(&recvs)) {
        next = opal_list_get_next(item);
        
        ptr = (rmcast_basic_recv_t*)item;
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

static unsigned int open_channel(char *name, char *network, uint8_t direction)
{
    opal_list_item_t *item;
    rmcast_basic_channel_t *nchan, *chan;
    rmcast_basic_network_t *net, *netitem;
    uint32_t netaddr;
    int rc;
    int xmitsd, recvsd;
    
    /* see if this name has already been assigned a channel */
    chan = NULL;
    for (item = opal_list_get_first(&channels);
         item != opal_list_get_end(&channels);
         item = opal_list_get_next(item)) {
        nchan = (rmcast_basic_channel_t*)item;
        
        if (0 == strcasecmp(nchan->name, name)) {
            chan = nchan;
            break;
        }
    }
    
    if (NULL != chan) {
        /* already exists - check that the requested
         * socket is setup
         */
        if (0 > chan->xmit && ORTE_RMCAST_XMIT & direction) {
            setup_socket(&chan->xmit, chan->full_addr, chan->port, false);
        }
        if (0 > chan->recv && ORTE_RMCAST_RECV & direction) {
            setup_socket(&chan->recv, chan->full_addr, chan->port, true);
            /* setup an event to catch messages */
            opal_event_set(&chan->recv_ev, chan->recv, OPAL_EV_READ, recv_handler, chan);
            opal_event_add(&chan->recv_ev, 0);
        }
        return chan->channel;
    }
    
    /* the named channel doesn't exist - did they give
     * us a specific network to use?
     */
    if (NULL != network) {
        net = NULL;
        netaddr = parse_network(network);
        for (item = opal_list_get_first(&networks);
             item != opal_list_get_end(&networks);
             item = opal_list_get_next(item)) {
            netitem = (rmcast_basic_network_t*)item;
            
            if (netaddr == netitem->network) {
                net = netitem;
                break;
            }
        }
        if (NULL == net) {
            /* new network - create it */
            net = OBJ_NEW(rmcast_basic_network_t);
            net->network = OPAL_IF_ASSEMBLE_NETWORK(next_octet1, next_octet2, next_octet3);
            net->next_freq = orte_rmcast_base.channel_offset + 1;
        }
        /* assign next freq to the next available channel */
        chan = OBJ_NEW(rmcast_basic_channel_t);
        chan->name = strdup(name);
        chan->full_addr = net->network + net->next_freq;
        chan->port = orte_rmcast_base.ports[net->next_freq-orte_rmcast_base.channel_offset-1];
        chan->channel = next_channel++;
        /* setup the IPv4 addr info */
        chan->addr.sin_family = AF_INET;
        chan->addr.sin_addr.s_addr = htonl(chan->full_addr);
        chan->addr.sin_port = htons(chan->port);
        if (ORTE_RMCAST_XMIT & direction) {
            /* create a xmit socket */
            if (ORTE_SUCCESS != (rc = setup_socket(&xmitsd, chan->full_addr, chan->port, false))) {
                ORTE_ERROR_LOG(rc);
                return 0;
            }
            chan->xmit = xmitsd;
            chan->send_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);
        }
        if (ORTE_RMCAST_RECV & direction) {
            /* create a recv socket */
            if (ORTE_SUCCESS != (rc = setup_socket(&recvsd, chan->full_addr, chan->port, true))) {
                ORTE_ERROR_LOG(rc);
                return 0;
            }
            chan->recv = recvsd;
            chan->recvd_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);
        }
        /* change to the next freq */
        net->next_freq++;
        return chan->channel;
    }
    
    /* if we get here, then we couldn't find a channel of the given name
     * AND we were not given a network address to use. In this case, use
     * the next available network/freq
     *
     * RHC: for now, we are not going to worry about balancing loads
     * across available networks. We are just going to use the next
     * network with an available freq
     */
    
    net = NULL;
    for (item = opal_list_get_first(&networks);
         item != opal_list_get_end(&networks);
         item = opal_list_get_next(item)) {
        netitem = (rmcast_basic_network_t*)item;
        
        if (netitem->next_freq < 255) {
            net = netitem;
            break;
        }
    }
    if (NULL == net) {
        /* we are hosed */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return 255;
    }
    
    chan = OBJ_NEW(rmcast_basic_channel_t);  /* puts it on list */
    chan->name = strdup(name);
    chan->full_addr = net->network + net->next_freq;
    chan->port = orte_rmcast_base.ports[net->next_freq-orte_rmcast_base.channel_offset-1];
    chan->channel = next_channel++;
    
    /* setup the IPv4 addr info */
    chan->addr.sin_family = AF_INET;
    chan->addr.sin_addr.s_addr = htonl(chan->full_addr);
    chan->addr.sin_port = htons(chan->port);
    if (ORTE_RMCAST_XMIT & direction) {
        /* create a xmit socket */
        if (ORTE_SUCCESS != (rc = setup_socket(&xmitsd, chan->full_addr, chan->port, false))) {
            ORTE_ERROR_LOG(rc);
            return 255;
        }
        chan->xmit = xmitsd;
        chan->send_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);
    }
    if (ORTE_RMCAST_RECV & direction) {
        /* create a recv socket */
        if (ORTE_SUCCESS != (rc = setup_socket(&recvsd, chan->full_addr, chan->port, true))) {
            ORTE_ERROR_LOG(rc);
            return 255;
        }
        chan->recv = recvsd;
        chan->recvd_data = (uint8_t*)malloc(mca_rmcast_basic_component.max_msg_size);
    }
    /* change to the next freq */
    net->next_freq++;

    return chan->channel;
}

static void recv_handler(int sd, short flags, void* cbdata)
{
    opal_list_item_t *item, *next;
    rmcast_basic_channel_t *chan = (rmcast_basic_channel_t*)cbdata;
    rmcast_basic_recv_t *ptr;
    uint8_t *payload;
    ssize_t sz;
    uint32_t tmp;
    orte_process_name_t name;
    uint16_t tmp16;
    orte_rmcast_tag_t tag;
    
    /* read the data */
    sz = read(sd, chan->recvd_data, mca_rmcast_basic_component.max_msg_size);
    
    OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic recvd %d bytes on channel %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)sz, (int)chan->channel));

    /* extract the name and convert it to host order */
    memcpy(&tmp, &chan->recvd_data[0], 4);
    name.jobid = ntohl(tmp);
    memcpy(&tmp, &chan->recvd_data[4], 4);
    name.vpid = ntohl(tmp);
    
    OPAL_OUTPUT_VERBOSE((4, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic:recv sender: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name)));
    
    /* if this message is from myself, ignore it */
    if (name.jobid == ORTE_PROC_MY_NAME->jobid &&
        name.vpid == ORTE_PROC_MY_NAME->vpid) {
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic:recv sent to myself!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }
    
    /* extract the target tag */
    memcpy(&tmp16, &chan->recvd_data[8], 2);
    tag = ntohs(tmp16);
    
    OPAL_OUTPUT_VERBOSE((4, orte_rmcast_base.rmcast_output,
                         "%s rmcast:basic:recv got tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)tag));
    
    /* find all recv's for this channel and tag */
    item = opal_list_get_first(&recvs);
    while (item != opal_list_get_end(&recvs)) {
        next = opal_list_get_next(item);
        ptr = (rmcast_basic_recv_t*)item;
        
        OPAL_OUTPUT_VERBOSE((10, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic:recv checking channel %d tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)ptr->channel, (int)ptr->tag));
        
        if (chan->channel == ptr->channel &&
            tag == ptr->tag) {
            /* data must be placed in malloc'd area for buffer */
            payload = (uint8_t*)malloc(sz-10);
            memcpy(payload, &chan->recvd_data[10], sz-10);
            
            /* create a buffer for the data */
            ptr->data = OBJ_NEW(opal_buffer_t);
            
            /* load the data into the buffer */
            opal_dss.load(ptr->data, payload, sz-10);
            
            if (NULL != ptr->cbfunc) {
                ptr->cbfunc(ptr->channel, ptr->data, ptr->cbdata);
                /* if it isn't persistent, remove it */
                if (!(ORTE_RMCAST_PERSISTENT & ptr->flags)) {
                    OPAL_THREAD_LOCK(&lock);
                    opal_list_remove_item(&recvs, &ptr->item);
                    OPAL_THREAD_UNLOCK(&lock);
                    OBJ_RELEASE(ptr);
                }
            } else {
                /* flag it as recvd to release blocking recv */
                ptr->recvd = true;
            }
        }
        /* move along list */
        item = next;
    }
    return;
}

static int setup_socket(int *sd, uint32_t chan, uint16_t port, bool bindsocket)
{
    uint8_t ttl = 1;
    struct sockaddr_in inaddr;
    struct ip_mreq req;
    int addrlen;
    int target_sd;
    int flags;

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
        opal_output(0,"rmcast:init: socketopt() failed: %s (%d)", 
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

    /* Bind the socket if requested */
    if (bindsocket) {
        memset(&inaddr, 0, sizeof(inaddr));
        inaddr.sin_family = AF_INET;
        inaddr.sin_addr.s_addr = htonl(chan);
        inaddr.sin_port = htons(port);
        addrlen = sizeof(struct sockaddr_in);
        
        /* bind the socket */
        if (bind(target_sd, (struct sockaddr*)&inaddr, addrlen) < 0) {
            opal_output(0, "%s rmcast:init: bind() failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        strerror(opal_socket_errno), opal_socket_errno);
            CLOSE_THE_SOCKET(target_sd);
            return ORTE_ERROR;
        }
    }
    
    /* set membership to "any" */
    memset(&req, 0, sizeof (req));
    req.imr_multiaddr.s_addr = htonl(chan);
    req.imr_interface.s_addr = htonl(INADDR_ANY);
    
    if ((setsockopt(target_sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, 
                    (void *)&req, sizeof (req))) < 0) {
        opal_output(0, "%s rmcast:init: setsockopt() failed: %s (%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    strerror(opal_socket_errno), opal_socket_errno);
        CLOSE_THE_SOCKET(target_sd);
        return ORTE_ERROR;
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
    rmcast_basic_channel_t *chan = (rmcast_basic_channel_t*)send_req;
    rmcast_basic_send_t *snd;
    opal_list_item_t *item;
    char *bytes;
    int32_t sz;
    int rc;
    uint16_t tmp;
    uint32_t nm;

    OPAL_THREAD_LOCK(&chan->send_lock);
    while (NULL != (item = opal_list_remove_first(&chan->pending_sends))) {
        snd = (rmcast_basic_send_t*)item;
        
        /* extract the payload */
        opal_dss.unload(snd->data, (void**)&bytes, &sz);
        
        /* start the send data area with our name in network-byte-order */
        nm = htonl(ORTE_PROC_MY_NAME->jobid);
        memcpy(&chan->send_data[0], &nm, 4);
        nm = htonl(ORTE_PROC_MY_NAME->vpid);
        memcpy(&chan->send_data[4], &nm, 4);
        
        /* add the tag data, also converted */
        tmp = htons(snd->tag);
        memcpy(&chan->send_data[8], &tmp, 2);
        
        /* add the payload, up to the limit */
        memcpy(&chan->send_data[10], bytes, sz);
        
        OPAL_OUTPUT_VERBOSE((2, orte_rmcast_base.rmcast_output,
                             "%s rmcast:basic sending %d bytes to tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)(sz+10), (int)snd->tag));
        
        if ((sz+10) != (rc = sendto(chan->xmit, chan->send_data, sz+10, 0,
                    (struct sockaddr *)&(chan->addr), sizeof(struct sockaddr_in)))) {
            /* didn't get the message out */
            opal_output(0, "%s failed to send message - size %d may be too large (limit: %d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (int)(sz+10), ORTE_RMCAST_BASIC_MAX_MSG_SIZE);
            /* reload into original buffer */
            opal_dss.load(snd->data, (void*)bytes, sz);
            /* cleanup */
            OBJ_RELEASE(item);
            continue;
       }

        /* reload into original buffer */
        opal_dss.load(snd->data, (void*)bytes, sz);
        
        /* call the cbfunc if required */
        if (NULL != snd->cbfunc) {
            snd->cbfunc(chan->channel, snd->data, snd->cbdata);
        }
        /* cleanup */
        OBJ_RELEASE(item);
    }
    
    /* cleanup */
    opal_event_del(&chan->send_ev);
    chan->sends_in_progress = false;

    OPAL_THREAD_UNLOCK(&chan->send_lock);
}

static uint32_t parse_network(char *network)
{
    char **octets=NULL;
    uint32_t net, oct1, oct2, oct3;
    
    /* the network will be provided as a set of dot-separated
     * octets, so split at those points
     */
    octets = opal_argv_split(network, '.');
    oct1 = strtoul(octets[0], NULL, 10);
    oct2 = strtoul(octets[1], NULL, 10);
    oct3 = strtoul(octets[2], NULL, 10);
    net = ((oct1 >> 24) & 0x000000FF) |
          ((oct2 >> 16) & 0x000000FF) |
          ((oct3 >>  8) & 0x000000FF);
    opal_argv_free(octets);
    return net;
}
