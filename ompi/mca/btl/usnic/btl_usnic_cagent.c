/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include "opal_stdint.h"
#include "opal/threads/mutex.h"
#include "opal/mca/event/event.h"
#include "opal/util/show_help.h"
#include "opal/types.h"
#include "opal/util/output.h"

#include "ompi/mca/rte/rte.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/constants.h"

#include "btl_usnic.h"
#include "btl_usnic_connectivity.h"

/**************************************************************************
 * Agent data and methods
 **************************************************************************/

/*
 * Data structure used to proxy messages between the RTE thread and
 * the agent thread.
 */
typedef struct {
    opal_buffer_t *buffer;
    ompi_process_name_t sender;
    opal_event_t event;
} agent_buf_name_t;

/*
 * Long enough to hold "xxx.xxx.xxx.xxx/xx"
 */
#define IPV4ADDRLEN 20

/*
 * Long enough to hold "xx:xx:xx:xx:xx:xx"
 */
#define MACLEN 18

/*
 * Holds all the information about a UDP port that the agent thread is
 * listening on (for incoming PINGs and ACKs).
 */
typedef struct {
    opal_list_item_t super;

    /* Data from the LISTEN command message */
    uint32_t ipv4_addr;
    uint32_t cidrmask;
    char ipv4_addr_str[IPV4STRADDRLEN];
    uint32_t mtu;
    char *nodename;
    char *if_name;
    char *usnic_name;
    char mac_str[MACSTRLEN];

    /* File descriptor, UDP port, buffer to receive messages, and event */
    int fd;
    uint32_t udp_port;
    uint8_t *buffer;
    opal_event_t event;
    ompi_btl_usnic_module_t *module;
} agent_udp_port_listener_t;

OBJ_CLASS_DECLARATION(agent_udp_port_listener_t);

typedef enum {
    AGENT_MSG_TYPE_PING = 17,
    AGENT_MSG_TYPE_ACK
} agent_udp_message_type_t;

/*
 * Ping and ACK messages
 */
typedef struct {
    uint8_t message_type;

    /* The sender's IP address and port (i.e., where the ACK can be
       sent).  This is actually redundant with the sockaddr that we
       get from recvfrom(), but that's ok -- it provides a sanity
       check.  */
    uint32_t src_ipv4_addr;
    uint32_t src_udp_port;

    /* If this is a PING, the message should be this size.
       If this is an ACK, we are ACKing a ping of this size. */
    uint32_t size;
} agent_udp_message_t;

typedef struct {
    opal_list_item_t super;

    /* Data from the PING command message */
    uint32_t src_ipv4_addr; /* in network byte order */
    uint32_t src_udp_port;
    agent_udp_port_listener_t *listener;
    uint32_t dest_ipv4_addr; /* in network byte order */
    uint32_t dest_cidrmask;
    uint32_t dest_udp_port;
    struct sockaddr_in dest_sockaddr;
    char *dest_nodename;
    uint8_t dest_mac[6];

    /* The sizes and corresponding buffers of the PING messages that
       we'll send, and whether each of those PING messages have been
       ACKed yet */
#define NUM_PING_SIZES 2
    size_t sizes[NUM_PING_SIZES];
    uint8_t *buffers[NUM_PING_SIZES];
    bool acked[NUM_PING_SIZES];

    /* Number of times we've sent this ping */
    int num_sends;

    /* Timer used to re-send the PING, and whether the timer is active
       or not */
    opal_event_t timer;
    bool timer_active;
} agent_ping_t;

OBJ_CLASS_DECLARATION(agent_ping_t);


/**************************************************************************
 * Utility functions, constructors, destructors
 **************************************************************************/

static void port_listener_zero(agent_udp_port_listener_t *obj)
{
    obj->ipv4_addr =
        obj->cidrmask =
        obj->mtu = 0;
    obj->nodename =
        obj->if_name =
        obj->usnic_name = NULL;
    memset(obj->ipv4_addr_str, 0, sizeof(obj->ipv4_addr_str));
    memset(obj->mac_str, 0, sizeof(obj->mac_str));

    obj->fd = -1;
    obj->udp_port = -1;
    obj->buffer = NULL;
}

static void port_listener_constructor(agent_udp_port_listener_t *obj)
{
    port_listener_zero(obj);
}

static void port_listener_destructor(agent_udp_port_listener_t *obj)
{
    if (-1 != obj->fd) {
        close(obj->fd);
    }
    if (NULL != obj->nodename) {
        free(obj->nodename);
    }
    if (NULL != obj->if_name) {
        free(obj->if_name);
    }
    if (NULL != obj->usnic_name) {
        free(obj->usnic_name);
    }
    if (NULL != obj->buffer) {
        free(obj->buffer);
    }

    port_listener_zero(obj);
}

OBJ_CLASS_INSTANCE(agent_udp_port_listener_t,
                   opal_list_item_t,
                   port_listener_constructor,
                   port_listener_destructor);

static void agent_ping_result_zero(agent_ping_t *obj)
{
    obj->src_ipv4_addr = 0;
    obj->src_udp_port = 0;
    obj->listener = NULL;
    obj->dest_ipv4_addr = 0;
    obj->dest_udp_port = 0;
    obj->num_sends = 0;
    obj->timer_active = false;

    for (int i = 0; i < NUM_PING_SIZES; ++i) {
        obj->sizes[i] = 0;
        obj->buffers[i] = NULL;
        obj->acked[i] = false;
    }
}

static void agent_ping_result_constructor(agent_ping_t *obj)
{
    agent_ping_result_zero(obj);
}

static void agent_ping_result_destructor(agent_ping_t *obj)
{
    for (int i = 0; i < NUM_PING_SIZES; ++i) {
        if (NULL != obj->buffers[i]) {
            free(obj->buffers[i]);
        }
    }
    if (obj->timer_active) {
        opal_event_del(&obj->timer);
    }

    agent_ping_result_zero(obj);
}

OBJ_CLASS_INSTANCE(agent_ping_t,
                   opal_list_item_t,
                   agent_ping_result_constructor,
                   agent_ping_result_destructor);

/*
 * Wrapper around sendto() loop
 */
static void agent_sendto(int fd, char *buffer, ssize_t numbytes,
                         struct sockaddr *addr)
{
    ssize_t rc;
    while (1) {
        rc = sendto(fd, buffer, numbytes, 0, addr, sizeof(*addr));
        /* Note that since this is UDP, so we don't need to check
           for 0 < rc < ap->sizes[i] */
        if (rc == numbytes) {
            return;
        } else if (rc < 0) {
            if (errno == EAGAIN || errno == EINTR) {
                continue;
            }

            ABORT("Unexpected sendto() error");
            /* Will not return */
        }

        /* We should never get here, but just in case we do, sleep a
           little, just so we don't hammer the CPU */
        usleep(1);
    }

    /* Will not get here */
}

/**************************************************************************
 * All of the following functions run in RTE thread
 **************************************************************************/

/* This variable belongs to the agent thread, but is safe to access
   from the RTE thread (because of its own internal locking) */
static opal_event_base_t *evbase;

/* Note that to avoid locking of the agent thread data structures, the
   ORTE thread routines avoid reading/writing those data structures.
   Instead, when the ORTE routines receive commands from the client
   thread, they basically save the message and queue up an event to
   run in the agent thread. */

/* Need to forward declare these functions; they are referenced as
   function pointers in the RTE thread functions */
static void agent_thread_cmd_listen(int fd, short flags, void *context);
static void agent_thread_cmd_ping(int fd, short flags, void *context);

/*
 * Save the message info and queue it up in an event to run in the
 * agent thread.
 */
static void agent_queue_thread_cmd(opal_buffer_t *buffer,
                                   ompi_process_name_t *sender,
                                   opal_event_cbfunc_t callback)
{
    agent_buf_name_t *abn = malloc(sizeof(agent_buf_name_t));
    if (NULL == abn) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }

    /* Copy the buffer so that we can have it after the RML receive
       callback returns (have pending question in to Ralph as to why
       this is necessary, vs. just OBJ_RETAIN'ing the buffer).  Note
       that copy_payload copies *from the current buffer position*, so
       we don't need to re-unpack the command from the new buffer. */
    abn->buffer = OBJ_NEW(opal_buffer_t);
    if (NULL == abn->buffer) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }
    opal_dss.copy_payload(abn->buffer, buffer);

    abn->sender = *sender;

    /* Queue up an immediately-active event in the agent thread */
    opal_event_set(evbase, &abn->event, -1, OPAL_EV_WRITE, callback, abn);
    opal_event_active(&abn->event, OPAL_EV_WRITE, 1);
}

/*
 * Called when we get an incoming RML message
 */
static void agent_rml_receive(int status, ompi_process_name_t* sender,
                              opal_buffer_t *buffer,
                              orte_rml_tag_t tag, void *cbdata)
{
    int32_t command;

    /* Unpack and invoke the command */
    UNPACK_INT32(buffer, command);
    assert(CONNECTIVITY_AGENT_CMD_LISTEN == command ||
           CONNECTIVITY_AGENT_CMD_PING == command);

    switch(command) {
    case CONNECTIVITY_AGENT_CMD_LISTEN:
        agent_queue_thread_cmd(buffer, sender, agent_thread_cmd_listen);
        break;
    case CONNECTIVITY_AGENT_CMD_PING:
        agent_queue_thread_cmd(buffer, sender, agent_thread_cmd_ping);
        break;
    default:
        ABORT("Unexpected connectivity agent command");
        break;
    }
}

/**************************************************************************
 * All of the following functions run in agent thread
 **************************************************************************/

static struct timeval ack_timeout;
static opal_list_t listeners;
/* JMS The pings_pending and ping_results should probably both be hash
   tables for more efficient lookups */
static opal_list_t pings_pending;
static opal_list_t ping_results;
static volatile bool agent_thread_time_to_exit = false;

/*
 * A dummy function invoked in an event just for the purposes of
 * waking up the agent main thread (in case it was blocked in the
 * event loop with no other events to wake it up).
 */
static void agent_thread_noop(int fd, short flags, void *context)
{
    /* Intentionally a no op */
}

/*
 * Check to ensure that we expected to receive a ping from this sender
 * on the interface in which it was received (i.e., did the usnic
 * module corresponding to the received interface choose to pair
 * itself with the sender's interface).  If not, discard it.
 *
 * Note that there may be a race condition here.  We may get a ping
 * before we've setup endpoints on the module in question.  It's no
 * problem -- if we don't find it, we'll drop the PING and let the
 * sender try again later.
 */
static bool agent_thread_is_ping_expected(ompi_btl_usnic_module_t *module,
                                          uint32_t src_ipv4_addr)
{
    bool found = false;
    opal_list_item_t *item;

    /* If we have a NULL value for the module, it means that the MPI
       process that is the agent hasn't submitted the LISTEN command
       yet (which can happen for a fast sender / slow receiver).  So
       just return "ping is not [yet] expected". */
    if (NULL == module) {
        return false;
    }

    opal_mutex_lock(&module->all_endpoints_lock);
    if (module->all_endpoints_constructed) {
        OPAL_LIST_FOREACH(item, &module->all_endpoints, opal_list_item_t) {
            ompi_btl_usnic_endpoint_t *ep;
            ep = container_of(item, ompi_btl_usnic_endpoint_t,
                              endpoint_endpoint_li);
            if (src_ipv4_addr == ep->endpoint_remote_addr.ipv4_addr) {
                found = true;
                break;
            }
        }
    }
    opal_mutex_unlock(&module->all_endpoints_lock);

    return found;
}

/*
 * Handle an incoming PING message (send an ACK)
 */
static void agent_thread_handle_ping(agent_udp_port_listener_t *listener,
                                     ssize_t numbytes, struct sockaddr *from)
{
    /* If the size we received isn't equal to what the sender says it
       sent, do the simple thing: just don't send an ACK */
    agent_udp_message_t *msg = (agent_udp_message_t*) listener->buffer;
    struct sockaddr_in *src_addr_in = (struct sockaddr_in*) from;
    if (msg->size != numbytes) {
        char str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &src_addr_in->sin_addr, str, sizeof(str));

        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity got bad ping: %d bytes from %s, expected %d (discarded)",
                            (int) numbytes, str, (int) msg->size);
        return;
    }

    /* Ensure that the sender sent the ping from the IP address that
       they think they sent it from.  If they didn't, then drop it
       (i.e., it's a bad ping because the sender sent it from an
       unexpected interface).  This should probably never happen, but
       it's a good failsafe for unexpected scenarios. */
    char msg_ipv4_addr_str[IPV4STRADDRLEN];
    char real_ipv4_addr_str[IPV4STRADDRLEN];

    ompi_btl_usnic_snprintf_ipv4_addr(msg_ipv4_addr_str,
                                      sizeof(msg_ipv4_addr_str),
                                      msg->src_ipv4_addr, 0);
    ompi_btl_usnic_snprintf_ipv4_addr(real_ipv4_addr_str,
                                      sizeof(real_ipv4_addr_str),
                                      src_addr_in->sin_addr.s_addr, 0);

    if (msg->src_ipv4_addr != src_addr_in->sin_addr.s_addr) {
        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity got bad ping (from unexpected address: %s != %s, discarded)",
                            msg_ipv4_addr_str, real_ipv4_addr_str);
        return;
    }

    /* Finally, check that the ping is from an interface that the
       module expects */
    if (!agent_thread_is_ping_expected(listener->module,
                                       src_addr_in->sin_addr.s_addr)) {
        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity got bad ping (from unexpected address: listener %s not paired with peer interface %s, discarded)",
                            listener->ipv4_addr_str,
                            real_ipv4_addr_str);
        return;
    }

    /* Ok, this is a good ping.  Send the ACK back */

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity got PING (size=%ld) from %s; sending ACK",
                        numbytes, real_ipv4_addr_str);

    /* Send back an ACK.  No need to allocate a new buffer; just
       re-use the same buffer we just got.  Note that msg->size is
       already set.. */
    msg->message_type = AGENT_MSG_TYPE_ACK;
    msg->src_ipv4_addr = listener->ipv4_addr;
    msg->src_udp_port = listener->udp_port;

    agent_sendto(listener->fd, (char*) listener->buffer, sizeof(*msg), from);
}

/*
 * Handle an incoming ACK message
 */
static void agent_thread_handle_ack(agent_udp_port_listener_t *listener,
                                    ssize_t numbytes, struct sockaddr *from)
{
    char str[INET_ADDRSTRLEN];
    struct sockaddr_in *src_addr_in = (struct sockaddr_in*) from;
    inet_ntop(AF_INET, &src_addr_in->sin_addr, str, sizeof(str));

    /* If we got a wonky ACK message that is the wrong length, just
       return */
    agent_udp_message_t *msg = (agent_udp_message_t*) listener->buffer;
    if (numbytes != sizeof(*msg)) {
        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity got bad ACK: %d bytes from %s, expected %d (discarded)",
                            (int) numbytes, str, (int) sizeof(*msg));
        return;
    }

    /* Find the pending ping request that this ACK is for */
    agent_ping_t *ap;
    OPAL_LIST_FOREACH(ap, &pings_pending, agent_ping_t) {
        if (ap->dest_ipv4_addr == msg->src_ipv4_addr &&
            ap->dest_udp_port == msg->src_udp_port) {
            /* Found it -- indicate that it has been acked */
            for (int i = 0; i < NUM_PING_SIZES; ++i) {
                if (ap->sizes[i] == msg->size) {
                    ap->acked[i] = true;
                    return;
                }
            }
        }
    }

    /* If we didn't find the matching ping for this ACK, then just
       discard it */
    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity got unexpected ACK: %d bytes from %s (discarded)",
                        (int) numbytes, str);
}

/*
 * Receive a message from the listening UDP socket
 */
static void agent_thread_receive_ping(int fd, short flags, void *context)
{
    agent_udp_port_listener_t *listener =
        (agent_udp_port_listener_t *) context;
    assert(NULL != listener);

    /* Receive the message */
    ssize_t numbytes;
    struct sockaddr src_addr;
    struct sockaddr_in *src_addr_in = (struct sockaddr_in*) &src_addr;
    socklen_t addrlen = sizeof(src_addr);

    while (1) {
        numbytes = recvfrom(listener->fd, listener->buffer, listener->mtu, 0,
                            &src_addr, &addrlen);
        if (numbytes > 0) {
            break;
        } else if (numbytes < 0) {
            if (errno == EAGAIN || errno == EINTR) {
                continue;
            }

            ABORT("Unexpected error from recvfrom");
            /* Will not return */
        }
    }

    char str[INET_ADDRSTRLEN];
    agent_udp_message_t *msg;
    msg = (agent_udp_message_t *) listener->buffer;
    switch (msg->message_type) {
    case AGENT_MSG_TYPE_PING:
        agent_thread_handle_ping(listener, numbytes, &src_addr);
        break;
    case AGENT_MSG_TYPE_ACK:
        agent_thread_handle_ack(listener, numbytes, &src_addr);
        break;
    default:
        /* Ignore unknown pings */
        inet_ntop(AF_INET, &src_addr_in->sin_addr, str, sizeof(str));
        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity agent received unknown message: %d bytes from %s",
                            (int) numbytes, str);
        break;
    }
}

static agent_udp_port_listener_t *
agent_thread_find_listener(uint32_t ipv4_addr, uint32_t *udp_port)
{
    agent_udp_port_listener_t *listener;
    OPAL_LIST_FOREACH(listener, &listeners, agent_udp_port_listener_t) {
        if (listener->ipv4_addr == ipv4_addr) {
            *udp_port = listener->udp_port;
            return listener;
        }
    }

    return NULL;
}

/*
 * Send an RML reply back from the LISTEN command: send back the IP
 * address and UDP port that we're listening on.
 */
static int agent_thread_cmd_listen_reply(ompi_process_name_t *dest,
                                         uint64_t addr, int32_t udp_port)
{
    opal_buffer_t *msg;
    msg = OBJ_NEW(opal_buffer_t);
    if (NULL == msg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    PACK_INT32(msg, CONNECTIVITY_AGENT_CMD_LISTEN);
    PACK_UINT32(msg, addr);
    PACK_UINT32(msg, udp_port);

    int ret;
    ret = ompi_rte_send_buffer_nb(dest, msg,
                                  OMPI_RML_TAG_USNIC_CONNECTIVITY_REPLY,
                                  ompi_rte_send_cbfunc, NULL);
    if (OMPI_SUCCESS != ret) {
        OMPI_ERROR_LOG(ret);
        OBJ_RELEASE(msg);
        return ret;
    }

    return OMPI_SUCCESS;
}

/*
 * The RTE thread will queue up an event to call this function when it
 * receives a LISTEN command RML message.
 */
static void agent_thread_cmd_listen(int fd, short flags, void *context)
{
    agent_buf_name_t *abn = (agent_buf_name_t*) context;
    opal_buffer_t *buffer = abn->buffer;
    ompi_process_name_t *sender = &abn->sender;

    int ret;
    uint64_t module64;
    UNPACK_UINT64(buffer, module64);

    uint32_t ipv4_addr, cidrmask;
    UNPACK_UINT32(buffer, ipv4_addr);
    UNPACK_UINT32(buffer, cidrmask);

    /* If we're already listening on this address, send the UDP port
       back to the client. */
    uint32_t udp_port;
    agent_udp_port_listener_t *listener;
    listener = agent_thread_find_listener(ipv4_addr, &udp_port);
    if (NULL != listener) {
        /* If we get a non-NULL "module" pointer value from the
           client, it means that this client is the same process as
           this agent, and we should save this pointer value (all
           non-agent MPI procs will send NULL as their "module"
           pointer value -- i.e., some non-agent MPI proc was the
           first one to send the LISTEN command). */
        if (NULL == listener->module) {
            listener->module = (ompi_btl_usnic_module_t*)module64;
        }
        agent_thread_cmd_listen_reply(sender, ipv4_addr, udp_port);

        OBJ_RELEASE(buffer);
        free(abn);

        return;
    }

    /* We're not listening on this address already, so create a
       listener entry */
    listener = OBJ_NEW(agent_udp_port_listener_t);
    if (NULL == listener) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }

    /* Unpack the rest of the message */
    UNPACK_UINT32(buffer, listener->mtu);
    UNPACK_STRING(buffer, listener->nodename);
    UNPACK_STRING(buffer, listener->if_name);
    UNPACK_STRING(buffer, listener->usnic_name);
    uint8_t mac[6];
    UNPACK_BYTES(buffer, mac, sizeof(mac));

    listener->ipv4_addr = ipv4_addr;
    listener->cidrmask = cidrmask;
    listener->module = (ompi_btl_usnic_module_t*) module64;

    /* We're now done with the RTE buffer */
    OBJ_RELEASE(buffer);
    buffer = NULL;

    /* Fill in the ipv4_addr_str and mac_str.  Since we don't have the
       IPv4 address in sockaddr_in form, it's not worth using
       inet_ntop() */
    ompi_btl_usnic_snprintf_ipv4_addr(listener->ipv4_addr_str,
                                      sizeof(listener->ipv4_addr_str),
                                      ipv4_addr, cidrmask);
    ompi_btl_usnic_sprintf_mac(listener->mac_str, mac);

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity agent listening on %s, (%s/%s)",
                        listener->ipv4_addr_str,
                        listener->usnic_name, listener->if_name);

    listener->buffer = malloc(listener->mtu);
    if (NULL == listener->buffer) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }

    /* Create the listening socket */
    listener->fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (listener->fd < 0) {
        OMPI_ERROR_LOG(listener->fd);
        ABORT("Could not open listening socket");
        /* Will not return */
    }

    /* Bind it to the designated interface */
    struct sockaddr_in inaddr;
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = ipv4_addr;
    inaddr.sin_port = htons(0);

    ret = bind(listener->fd, (struct sockaddr*) &inaddr, sizeof(inaddr));
    if (ret < 0) {
        OMPI_ERROR_LOG(ret);
        ABORT("Could not bind listening socket");
        /* Will not return */
    }

    /* Find out the port we got */
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);
    ret = getsockname(listener->fd, (struct sockaddr*) &inaddr, &addrlen);
    if (ret < 0) {
        OMPI_ERROR_LOG(ret);
        ABORT("Could not get UDP port number from listening socket");
        /* Will not return */
    }
    listener->udp_port = ntohs(inaddr.sin_port);

    /* Set the "don't fragment" bit on outgoing frames because we
       want MTU-sized messages to get through successfully to the
       peer, or fail if they have to fragment because of an MTU
       mismatch somewhere enroute */
    int val = IP_PMTUDISC_DO;
    ret = setsockopt(listener->fd, IPPROTO_IP, IP_MTU_DISCOVER,
                     &val, sizeof(val));
    if (0 != ret) {
        OMPI_ERROR_LOG(ret);
        ABORT("Unable to set \"do not fragment\" on UDP socket");
        /* Will not return */
    }

    /* Set the send and receive buffer sizes to our MTU size */
    int temp;
    temp = (int) listener->mtu;
    if ((ret = setsockopt(listener->fd, SOL_SOCKET, SO_RCVBUF,
                          &temp, sizeof(temp))) < 0 ||
        (ret = setsockopt(listener->fd, SOL_SOCKET, SO_SNDBUF,
                          &temp, sizeof(temp))) < 0) {
        OMPI_ERROR_LOG(ret);
        ABORT("Could not set socket buffer sizes");
        /* Will not return */
    }

    /* Create a listening event */
    opal_event_set(evbase, &listener->event, listener->fd,
                   OPAL_EV_READ | OPAL_EV_PERSIST,
                   agent_thread_receive_ping, listener);
    opal_event_add(&listener->event, 0);

    /* Save this listener on the list of listeners */
    opal_list_append(&listeners, &listener->super);

    /* Return the port number to the sender */
    ret = agent_thread_cmd_listen_reply(sender, ipv4_addr, listener->udp_port);

    /* All done! */
    free(abn);
    return;
}

/*
 * Send a ping
 */
static void agent_thread_send_ping(int fd, short flags, void *context)
{
    agent_ping_t *ap = (agent_ping_t*) context;
    ap->timer_active = false;

    char dest_ipv4_addr_str[IPV4STRADDRLEN];
    ompi_btl_usnic_snprintf_ipv4_addr(dest_ipv4_addr_str,
                                      sizeof(dest_ipv4_addr_str),
                                      ap->dest_ipv4_addr, ap->dest_cidrmask);

    /* If we got all the ACKs for this ping, then move this ping from
       the "pending" list to the "results" list.  We can also free the
       buffers associated with this ping result, just to save some
       space in the long run.  */
    if (ap->acked[0] && ap->acked[1]) {
        opal_list_remove_item(&pings_pending, &ap->super);
        opal_list_append(&ping_results, &ap->super);

        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity GOOD between %s <--> %s",
                            ap->listener->ipv4_addr_str,
                            dest_ipv4_addr_str);

        for (int i = 0; i < 2; ++i) {
            if (NULL != ap->buffers[i]) {
                free(ap->buffers[i]);
                ap->buffers[i] = NULL;
            }
        }

        return;
    }

    /* If we've resent too many times, then just abort */
    if (ap->num_sends > mca_btl_usnic_component.connectivity_num_retries) {
        char *topic;
        if (ap->acked[0] && !ap->acked[1]) {
            topic = "connectivity error: small ok, large bad";
        } else if (!ap->acked[0] && ap->acked[1]) {
            topic = "connectivity error: small bad, large ok";
        } else {
            topic = "connectivity error: small bad, large bad";
        }

        char mac_str[MACSTRLEN], ipv4_addr_str[IPV4STRADDRLEN];
        ompi_btl_usnic_snprintf_ipv4_addr(ipv4_addr_str, sizeof(ipv4_addr_str),
                                          ap->dest_ipv4_addr,
                                          ap->dest_cidrmask);
        ompi_btl_usnic_sprintf_mac(mac_str, ap->dest_mac);
        opal_show_help("help-mpi-btl-usnic.txt", topic, true,
                       ompi_process_info.nodename,
                       ap->listener->ipv4_addr_str,
                       ap->listener->usnic_name,
                       ap->listener->if_name,
                       ap->listener->mac_str,
                       ap->dest_nodename,
                       ipv4_addr_str,
                       mac_str,
                       ap->sizes[0],
                       ap->sizes[1]);
        ompi_btl_usnic_exit();
        /* Will not return */
    }

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity pinging %s (%s) from %s (%s/%s)",
                        dest_ipv4_addr_str, ap->dest_nodename,
                        ap->listener->ipv4_addr_str,
                        ap->listener->if_name, ap->listener->usnic_name);

    /* Send the ping messages to the peer */
    for (int i = 0; i < NUM_PING_SIZES; ++i) {
        agent_sendto(ap->listener->fd, (char*) ap->buffers[i], ap->sizes[i],
                     (struct sockaddr*) &ap->dest_sockaddr);
    }

    /* Set a timer to check if these pings are ACKed */
    opal_event_set(evbase, &ap->timer,
                   -1, 0, agent_thread_send_ping, ap);
    opal_event_add(&ap->timer, &ack_timeout);
    ap->timer_active = true;

    /* Count how many times we've done this */
    ++ap->num_sends;
}

/*
 * The RTE thread will queue up an event to call this function when it
 * receives a PING command RML message.
 */
static void agent_thread_cmd_ping(int fd, short flags, void *context)
{
    agent_buf_name_t *abn = (agent_buf_name_t*) context;
    opal_buffer_t *buffer = abn->buffer;

    uint32_t src_ipv4_addr, src_udp_port;
    uint32_t dest_ipv4_addr, dest_cidrmask, dest_udp_port, mtu;
    uint8_t dest_mac[6];
    char *dest_nodename;
    UNPACK_UINT32(buffer, src_ipv4_addr);
    UNPACK_UINT32(buffer, src_udp_port);
    UNPACK_UINT32(buffer, dest_ipv4_addr);
    UNPACK_UINT32(buffer, dest_cidrmask);
    UNPACK_UINT32(buffer, dest_udp_port);
    UNPACK_BYTES(buffer, dest_mac, 6);
    UNPACK_UINT32(buffer, mtu);
    UNPACK_STRING(buffer, dest_nodename);

    /* We're now done with the original RML message buffer */
    OBJ_RELEASE(buffer);
    free(abn);

    /* Have we already pinged this IP address / port? */
    agent_ping_t *ap;
    OPAL_LIST_FOREACH(ap, &ping_results, agent_ping_t) {
        if (ap->dest_ipv4_addr == dest_ipv4_addr &&
            ap->dest_udp_port == dest_udp_port) {
            /* We already have results from pinging this IP address /
               port, so there's no need for further action */
            return;
        }
    }

    /* Are we in the middle of pinging this IP address / port? */
    OPAL_LIST_FOREACH(ap, &pings_pending, agent_ping_t) {
        if (ap->dest_ipv4_addr == dest_ipv4_addr &&
            ap->dest_udp_port == dest_udp_port) {
            /* We're already in the middle of pinging this IP address
               / port, so there's no need for further action */
            return;
        }
    }

    /* This is a new ping request.  Find the listener with this source
       ipv4 address */
    bool found = false;
    agent_udp_port_listener_t *listener;
    OPAL_LIST_FOREACH(listener, &listeners, agent_udp_port_listener_t) {
        if (listener->ipv4_addr == src_ipv4_addr) {
            found = true;
            break;
        }
    }
    if (!found) {
        ABORT("Could not ping listener for ping request");
        /* Will not return */
    }

    /* This is a new ping request; track it */
    ap = OBJ_NEW(agent_ping_t);
    if (NULL == ap) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }
    ap->src_ipv4_addr = src_ipv4_addr;
    ap->src_udp_port = src_udp_port;
    ap->listener = listener;
    ap->dest_ipv4_addr = dest_ipv4_addr;
    ap->dest_cidrmask = dest_cidrmask;
    ap->dest_udp_port = dest_udp_port;
    ap->dest_sockaddr.sin_family = AF_INET;
    ap->dest_sockaddr.sin_addr.s_addr = dest_ipv4_addr;
    ap->dest_sockaddr.sin_port = htons(dest_udp_port);
    memcpy(ap->dest_mac, dest_mac, 6);
    ap->dest_nodename = dest_nodename;

    /* The first message we send will be "short" (a simple control
       message); the second will be "long" (i.e., caller-specified
       length) */
    ap->sizes[0] = sizeof(agent_udp_message_t);

    /* Note that the MTU is the max Ethernet frame payload.  So from
       that MTU, we have to subtract off the max IP header (e.g., if
       all IP options are enabled, which is 60 bytes), and then also
       subtract off the UDP header (which is 8 bytes).  So we need to
       subtract off 68 bytes from the MTU, and that's the largest ping
       payload we can send. */
    ap->sizes[1] = mtu - 68;

    /* Allocate a buffer for each size.  Make sure the smallest size
       is at least sizeof(agent_udp_message_t). */
    agent_udp_message_t *msg;
    for (size_t i = 0; i < NUM_PING_SIZES; ++i) {
        ap->buffers[i] = calloc(1, ap->sizes[i]);
        if (NULL == ap->buffers[i]) {
            OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
            ABORT("Out of memory");
            /* Will not return */
        }

        /* Fill in the message with return addressing information */
        msg = (agent_udp_message_t*) ap->buffers[i];
        msg->message_type = AGENT_MSG_TYPE_PING;
        msg->src_ipv4_addr = ap->src_ipv4_addr;
        msg->src_udp_port = ap->src_udp_port;
        msg->size = ap->sizes[i];
    }

    /* Save this ping request on the "pending" list */
    opal_list_append(&pings_pending, &ap->super);

    /* Send the ping */
    agent_thread_send_ping(0, 0, ap);
}

/*
 * Agent progress thread main entry point
 */
static void *agent_thread_main(opal_object_t *obj)
{
    while (!agent_thread_time_to_exit) {
        opal_event_loop(evbase, OPAL_EVLOOP_ONCE);
    }

    return NULL;
}

/**************************************************************************
 * All of the following functions run in the main application thread
 **************************************************************************/

static bool agent_initialized = false;
static opal_thread_t agent_thread;

/*
 * Setup the agent and start its event loop running in a dedicated
 * thread
 */
int ompi_btl_usnic_connectivity_agent_init(void)
{
    /* Only do this initialization if I am the agent (the agent is
       local rank 0) */
    if (ompi_process_info.my_local_rank != 0) {
        return OMPI_SUCCESS;
    }
    if (agent_initialized) {
        return OMPI_SUCCESS;
    }

    /* Create the event base */
    evbase = opal_event_base_create();

    /* Make a struct timeval for use with timer events.  Note that the
       MCA param is expressed in terms of *milli*seconds, but the
       timeval timeout is expressed in terms of *micro*seconds. */
    ack_timeout.tv_sec =
        mca_btl_usnic_component.connectivity_ack_timeout / 1000;
    ack_timeout.tv_usec =
        1000 * (mca_btl_usnic_component.connectivity_ack_timeout % 1000);

    /* Create lists */
    OBJ_CONSTRUCT(&listeners, opal_list_t);
    OBJ_CONSTRUCT(&pings_pending, opal_list_t);
    OBJ_CONSTRUCT(&ping_results, opal_list_t);

    /********************************************************************
     * Once all of the above is setup, launch the RML receives and
     * start the event loop.
     ********************************************************************/

    /* Setup the RML receive */
    ompi_rte_recv_buffer_nb(OMPI_NAME_WILDCARD,
                            OMPI_RML_TAG_USNIC_CONNECTIVITY,
                            OMPI_RML_PERSISTENT,
                            agent_rml_receive, NULL);

    /* Spawn the agent thread event loop */
    OBJ_CONSTRUCT(&agent_thread, opal_thread_t);
    agent_thread.t_run = agent_thread_main;
    agent_thread.t_arg = NULL;
    int ret;
    ret = opal_thread_start(&agent_thread);
    if (OPAL_SUCCESS != ret) {
        OMPI_ERROR_LOG(ret);
        ABORT("Failed to start usNIC agent thread");
        /* Will not return */
    }

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity agent initialized");
    agent_initialized = true;
    return OMPI_SUCCESS;
}

/*
 * Shut down the agent
 */
int ompi_btl_usnic_connectivity_agent_finalize(void)
{
    agent_initialized = false;

    /* Only do this if I have the agent running */
    if (NULL == evbase) {
        return OMPI_SUCCESS;
    }

    /* Cancel the RML receive */
    ompi_rte_recv_cancel(OMPI_NAME_WILDCARD, OMPI_RML_TAG_USNIC_CONNECTIVITY);

    /* Shut down the event loop.  Send it a no-op event so that it
       wakes up and exits the loop. */
    opal_event_t ev;
    agent_thread_time_to_exit = true;
    opal_event_set(evbase, &ev, -1, OPAL_EV_WRITE, agent_thread_noop, NULL);
    opal_event_active(&ev, OPAL_EV_WRITE, 1);
    opal_thread_join(&agent_thread, NULL);

    /* Shut down all active listeners */
    agent_udp_port_listener_t *listener, *lnext;
    OPAL_LIST_FOREACH_SAFE(listener, lnext, &listeners,
                           agent_udp_port_listener_t) {
        opal_event_del(&listener->event);
        close(listener->fd);
        opal_list_remove_item(&listeners, &listener->super);
        OBJ_RELEASE(listener);
    }

    /* Destroy the pending pings and ping results */
    agent_ping_t *request, *pnext;
    OPAL_LIST_FOREACH_SAFE(request, pnext, &pings_pending, agent_ping_t) {
        opal_list_remove_item(&pings_pending, &request->super);
        OBJ_RELEASE(request);
    }

    OPAL_LIST_FOREACH_SAFE(request, pnext, &ping_results, agent_ping_t) {
        opal_list_remove_item(&ping_results, &request->super);
        OBJ_RELEASE(request);
    }

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity client finalized");
    return OMPI_SUCCESS;
}
