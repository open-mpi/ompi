/*
 * Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <alloca.h>

#include "opal_stdint.h"
#include "opal/threads/mutex.h"
#include "opal/mca/event/event.h"
#include "opal/util/show_help.h"
#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/util/fd.h"

#include "btl_usnic.h"
#include "btl_usnic_connectivity.h"

/**************************************************************************
 * Agent data and methods
 **************************************************************************/

/*
 * Local variables
 */
static int ipc_accept_fd = -1;
static char *ipc_filename = NULL;
static opal_event_t ipc_event;
static struct timeval ack_timeout;
static opal_list_t udp_port_listeners;
static opal_list_t ipc_listeners;
/* JMS The pings_pending and ping_results should probably both be hash
   tables for more efficient lookups */
static opal_list_t pings_pending;
static opal_list_t ping_results;
static volatile bool agent_thread_time_to_exit = false;
static opal_event_base_t *evbase = NULL;


/*
 * Holds all the information about a UDP port that the agent thread is
 * listening on (for incoming PINGs and ACKs).
 */
typedef struct {
    opal_list_item_t super;

    /* Data from the LISTEN command message */
    uint32_t ipv4_addr;
    uint32_t netmask;
    char ipv4_addr_str[IPV4STRADDRLEN];
    uint32_t max_msg_size;
    char *nodename;
    char *usnic_name;

    /* File descriptor, UDP port, buffer to receive messages, and event */
    int fd;
    uint32_t udp_port;
    uint8_t *buffer;
    opal_event_t event;
    bool active;
    opal_btl_usnic_module_t *module;
} agent_udp_port_listener_t;

OBJ_CLASS_DECLARATION(agent_udp_port_listener_t);

/*
 * Holds information for a local IPC socket fd (i.e., a connection
 * from a local process to this agent).
 */
typedef struct {
    opal_list_item_t super;

    int client_fd;
    opal_event_t event;
    bool active;
} agent_ipc_listener_t;

OBJ_CLASS_DECLARATION(agent_ipc_listener_t);

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
    uint32_t dest_netmask;
    uint32_t dest_udp_port;
    struct sockaddr_in dest_sockaddr;
    char *dest_nodename;

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

static void udp_port_listener_zero(agent_udp_port_listener_t *obj)
{
    obj->ipv4_addr =
        obj->netmask =
        obj->max_msg_size = 0;
    obj->nodename =
        obj->usnic_name = NULL;
    memset(obj->ipv4_addr_str, 0, sizeof(obj->ipv4_addr_str));

    obj->fd = -1;
    obj->udp_port = -1;
    obj->buffer = NULL;

    obj->active = false;
}

static void udp_port_listener_constructor(agent_udp_port_listener_t *obj)
{
    udp_port_listener_zero(obj);
}

static void udp_port_listener_destructor(agent_udp_port_listener_t *obj)
{
    /* Find any pings that are pending on this listener and delete
       them */
    agent_ping_t *ap, *apnext;
    OPAL_LIST_FOREACH_SAFE(ap, apnext, &pings_pending, agent_ping_t) {
        if (ap->src_ipv4_addr == obj->ipv4_addr) {
            opal_list_remove_item(&pings_pending, &ap->super);
            OBJ_RELEASE(ap);
        }
    }

    if (-1 != obj->fd) {
        close(obj->fd);
    }
    if (NULL != obj->nodename) {
        free(obj->nodename);
    }
    if (NULL != obj->usnic_name) {
        free(obj->usnic_name);
    }
    if (NULL != obj->buffer) {
        free(obj->buffer);
    }

    /* If the "active" flag is set, then the event is active and the
       item is on the ipc_listeners list */
    if (obj->active) {
        opal_event_del(&obj->event);
        opal_list_remove_item(&udp_port_listeners, &obj->super);
    }

    udp_port_listener_zero(obj);
}

OBJ_CLASS_INSTANCE(agent_udp_port_listener_t,
                   opal_list_item_t,
                   udp_port_listener_constructor,
                   udp_port_listener_destructor);

static void ipc_listener_zero(agent_ipc_listener_t *obj)
{
    obj->client_fd = -1;
    obj->active = false;
}

static void ipc_listener_constructor(agent_ipc_listener_t *obj)
{
    ipc_listener_zero(obj);
}

static void ipc_listener_destructor(agent_ipc_listener_t *obj)
{
    if (-1 != obj->client_fd) {
        close(obj->client_fd);
    }

    /* If the "active" flag is set, then the event is active and the
       item is on the ipc_listeners list */
    if (obj->active) {
        opal_event_del(&obj->event);
        opal_list_remove_item(&ipc_listeners, &obj->super);
    }

    ipc_listener_zero(obj);
}

OBJ_CLASS_INSTANCE(agent_ipc_listener_t,
                   opal_list_item_t,
                   ipc_listener_constructor,
                   ipc_listener_destructor);

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
           for 0 < rc < numbytes */
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
 * All of the following functions run in agent thread
 **************************************************************************/

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
static bool agent_thread_is_ping_expected(opal_btl_usnic_module_t *module,
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
            opal_btl_usnic_endpoint_t *ep;
            ep = container_of(item, opal_btl_usnic_endpoint_t,
                              endpoint_endpoint_li);
            if (src_ipv4_addr == ep->endpoint_remote_modex.ipv4_addr) {
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

    opal_btl_usnic_snprintf_ipv4_addr(msg_ipv4_addr_str,
                                      sizeof(msg_ipv4_addr_str),
                                      msg->src_ipv4_addr, 0);
    opal_btl_usnic_snprintf_ipv4_addr(real_ipv4_addr_str,
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
                        numbytes, msg_ipv4_addr_str);

    /* Send back an ACK.  No need to allocate a new buffer; just
       re-use the same buffer we just got.  Note that msg->size is
       already set. */
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
        numbytes = recvfrom(listener->fd, listener->buffer, listener->max_msg_size, 0,
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
    OPAL_LIST_FOREACH(listener, &udp_port_listeners, agent_udp_port_listener_t) {
        if (listener->ipv4_addr == ipv4_addr) {
            *udp_port = listener->udp_port;
            return listener;
        }
    }

    return NULL;
}

/*
 * Send reply back from the LISTEN command: send back the IP address
 * and UDP port that we're listening on.
 */
static int agent_thread_cmd_listen_reply(int fd,
                                         uint32_t addr, int32_t udp_port)
{
    int ret;

    opal_btl_usnic_connectivity_cmd_listen_reply_t cmd = {
        .cmd = CONNECTIVITY_AGENT_CMD_LISTEN,
        .ipv4_addr = addr,
        .udp_port = udp_port
    };

    ret = opal_fd_write(fd, sizeof(cmd), &cmd);
    if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("usnic connectivity agent IPC write failed");
        /* Will not return */
    }

    return OPAL_SUCCESS;
}

/*
 * Receive and process the rest of a LISTEN command from a local IPC
 * client.
 */
static void agent_thread_cmd_listen(agent_ipc_listener_t *ipc_listener)
{
    /* Read the rest of the LISTEN command from the IPC socket */
    int ret;
    opal_btl_usnic_connectivity_cmd_listen_t cmd;
    ret = opal_fd_read(ipc_listener->client_fd, sizeof(cmd), &cmd);
    if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("usnic connectivity agent IPC LISTEN read failed");
        /* Will not return */
    }

    /* If we're already listening on this address, send the UDP port
       back to the client. */
    uint32_t udp_port;
    agent_udp_port_listener_t *udp_listener;
    udp_listener = agent_thread_find_listener(cmd.ipv4_addr, &udp_port);
    if (NULL != udp_listener) {
        /* If we get a non-NULL "module" pointer value from the
           client, it means that this client is the same process as
           this agent, and we should save this pointer value (all
           non-agent MPI procs will send NULL as their "module"
           pointer value -- i.e., some non-agent MPI proc was the
           first one to send the LISTEN command). */
        if (NULL == udp_listener->module) {
            udp_listener->module = cmd.module;
        }
        agent_thread_cmd_listen_reply(ipc_listener->client_fd,
                                      cmd.ipv4_addr, udp_port);
        return;
    }

    /* We're not listening on this interface already, so create a
       UDP port listener entry */
    udp_listener = OBJ_NEW(agent_udp_port_listener_t);
    if (NULL == udp_listener) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }

    udp_listener->module = cmd.module;
    udp_listener->max_msg_size = cmd.max_msg_size;
    udp_listener->ipv4_addr = cmd.ipv4_addr;
    udp_listener->netmask = cmd.netmask;
    udp_listener->usnic_name = strdup(cmd.usnic_name);

    /* Fill in the ipv4_addr_str.  Since we don't have the IPv4
       address in sockaddr_in form, it's not worth using
       inet_ntop() */
    opal_btl_usnic_snprintf_ipv4_addr(udp_listener->ipv4_addr_str,
                                      sizeof(udp_listener->ipv4_addr_str),
                                      cmd.ipv4_addr, cmd.netmask);

    udp_listener->buffer = malloc(udp_listener->max_msg_size);
    if (NULL == udp_listener->buffer) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }

    /* Create the listening socket */
    udp_listener->fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (udp_listener->fd < 0) {
        OPAL_ERROR_LOG(udp_listener->fd);
        ABORT("Could not open listening socket");
        /* Will not return */
    }

    /* Bind it to the designated interface */
    struct sockaddr_in inaddr;
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = cmd.ipv4_addr;
    inaddr.sin_port = htons(0);

    ret = bind(udp_listener->fd, (struct sockaddr*) &inaddr, sizeof(inaddr));
    if (ret < 0) {
        OPAL_ERROR_LOG(ret);
        ABORT("Could not bind listening socket");
        /* Will not return */
    }

    /* Find out the port we got */
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);
    ret = getsockname(udp_listener->fd, (struct sockaddr*) &inaddr, &addrlen);
    if (ret < 0) {
        OPAL_ERROR_LOG(ret);
        ABORT("Could not get UDP port number from listening socket");
        /* Will not return */
    }
    udp_listener->udp_port = ntohs(inaddr.sin_port);

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity agent listening on %s:%d, (%s)",
                        udp_listener->ipv4_addr_str,
                        udp_listener->udp_port,
                        udp_listener->usnic_name);

    /* Set the "don't fragment" bit on outgoing frames because we
       want MTU-sized messages to get through successfully to the
       peer, or fail if they have to fragment because of an MTU
       mismatch somewhere enroute */
    int val = IP_PMTUDISC_DO;
    ret = setsockopt(udp_listener->fd, IPPROTO_IP, IP_MTU_DISCOVER,
                     &val, sizeof(val));
    if (0 != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("Unable to set \"do not fragment\" on UDP socket");
        /* Will not return */
    }

    /* Set the send and receive buffer sizes to our MTU size */
    int temp;
    temp = (int) udp_listener->max_msg_size;
    if ((ret = setsockopt(udp_listener->fd, SOL_SOCKET, SO_RCVBUF,
                          &temp, sizeof(temp))) < 0 ||
        (ret = setsockopt(udp_listener->fd, SOL_SOCKET, SO_SNDBUF,
                          &temp, sizeof(temp))) < 0) {
        OPAL_ERROR_LOG(ret);
        ABORT("Could not set socket buffer sizes");
        /* Will not return */
    }

    /* Create a listening event */
    opal_event_set(evbase, &udp_listener->event, udp_listener->fd,
                   OPAL_EV_READ | OPAL_EV_PERSIST,
                   agent_thread_receive_ping, udp_listener);
    opal_event_add(&udp_listener->event, 0);

    /* Save this listener on the list of udp_port_listeners */
    opal_list_append(&udp_port_listeners, &udp_listener->super);

    udp_listener->active = true;

    /* Return the port number to the sender */
    ret = agent_thread_cmd_listen_reply(ipc_listener->client_fd,
                                        cmd.ipv4_addr, udp_listener->udp_port);

    /* All done! */
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
    opal_btl_usnic_snprintf_ipv4_addr(dest_ipv4_addr_str,
                                      sizeof(dest_ipv4_addr_str),
                                      ap->dest_ipv4_addr, ap->dest_netmask);

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
            // For the show_help topic checker script
            // SHOW_HELP:"help-mpi-btl-usnic.txt","connectivity error: small ok, large bad"
            topic = "connectivity error: small ok, large bad";
        } else if (!ap->acked[0] && ap->acked[1]) {
            // For the show_help topic checker script
            // SHOW_HELP:"help-mpi-btl-usnic.txt","connectivity error: small bad, large ok"
            topic = "connectivity error: small bad, large ok";
        } else {
            // For the show_help topic checker script
            // SHOW_HELP:"help-mpi-btl-usnic.txt","connectivity error: small bad, large bad"
            topic = "connectivity error: small bad, large bad";
        }

        char ipv4_addr_str[IPV4STRADDRLEN];
        opal_btl_usnic_snprintf_ipv4_addr(ipv4_addr_str, sizeof(ipv4_addr_str),
                                          ap->dest_ipv4_addr,
                                          ap->dest_netmask);
        opal_show_help("help-mpi-btl-usnic.txt", topic, true,
                       opal_process_info.nodename,
                       ap->listener->ipv4_addr_str,
                       ap->listener->usnic_name,
                       ap->dest_nodename,
                       ipv4_addr_str,
                       ap->sizes[0],
                       ap->sizes[1]);
        opal_btl_usnic_exit(NULL);
        /* Will not return */
    }

    time_t t = time(NULL);
    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity pinging %s:%d (%s) from %s (%s) at %s",
                        dest_ipv4_addr_str,
                        ntohs(ap->dest_sockaddr.sin_port),
                        ap->dest_nodename,
                        ap->listener->ipv4_addr_str,
                        ap->listener->usnic_name,
                        ctime(&t));

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
 * Receive and process the rest of a PING command from a local IPC
 * client.
 */
static void agent_thread_cmd_ping(agent_ipc_listener_t *ipc_listener)
{
    /* Read the rest of the PING command from the IPC socket */
    int ret;
    opal_btl_usnic_connectivity_cmd_ping_t cmd;
    ret = opal_fd_read(ipc_listener->client_fd, sizeof(cmd), &cmd);
    if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("usnic connectivity agent IPC PING read failed");
        /* Will not return */
    }

    /* Have we already pinged this IP address / port? */
    agent_ping_t *ap;
    OPAL_LIST_FOREACH(ap, &ping_results, agent_ping_t) {
        if (ap->dest_ipv4_addr == cmd.dest_ipv4_addr &&
            ap->dest_udp_port == cmd.dest_udp_port) {
            /* We already have results from pinging this IP address /
               port, so there's no need for further action */
            return;
        }
    }

    /* Are we in the middle of pinging this IP address / port? */
    OPAL_LIST_FOREACH(ap, &pings_pending, agent_ping_t) {
        if (ap->dest_ipv4_addr == cmd.dest_ipv4_addr &&
            ap->dest_udp_port == cmd.dest_udp_port) {
            /* We're already in the middle of pinging this IP address
               / port, so there's no need for further action */
            return;
        }
    }

    /* This is a new ping request.  Find the listener with this source
       ipv4 address */
    bool found = false;
    agent_udp_port_listener_t *udp_listener;
    OPAL_LIST_FOREACH(udp_listener, &udp_port_listeners,
                      agent_udp_port_listener_t) {
        if (udp_listener->ipv4_addr == cmd.src_ipv4_addr) {
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
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }
    ap->src_ipv4_addr = cmd.src_ipv4_addr;
    ap->src_udp_port = cmd.src_udp_port;
    ap->listener = udp_listener;
    ap->dest_ipv4_addr = cmd.dest_ipv4_addr;
    ap->dest_netmask = cmd.dest_netmask;
    ap->dest_udp_port = cmd.dest_udp_port;
    ap->dest_sockaddr.sin_family = AF_INET;
    ap->dest_sockaddr.sin_addr.s_addr = cmd.dest_ipv4_addr;
    ap->dest_sockaddr.sin_port = htons(cmd.dest_udp_port);
    ap->dest_nodename = strdup(cmd.dest_nodename);

    /* The first message we send will be "short" (a simple control
       message); the second will be "long" (i.e., caller-specified
       length) */
    ap->sizes[0] = sizeof(agent_udp_message_t);

    /* Note that the MTU is the max Ethernet frame payload.  So from
       that MTU, we have to subtract off the max IP header (e.g., if
       all IP options are enabled, which is 60 bytes), and then also
       subtract off the UDP header (which is 8 bytes).  So we need to
       subtract off 68 bytes from the MTU, and that's the largest ping
       payload we can send.
       max_msg_size allows for minimal UDP header, be more conservative */
    ap->sizes[1] = cmd.max_msg_size - (68 - 42);

    /* Allocate a buffer for each size.  Make sure the smallest size
       is at least sizeof(agent_udp_message_t). */
    agent_udp_message_t *msg;
    for (size_t i = 0; i < NUM_PING_SIZES; ++i) {
        ap->buffers[i] = calloc(1, ap->sizes[i]);
        if (NULL == ap->buffers[i]) {
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
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
 * Receive and process the rest of an UNLISTEN command from a local IPC
 * client.
 */
static void agent_thread_cmd_unlisten(agent_ipc_listener_t *ipc_listener)
{
    /* Read the rest of the UNLISTEN command from the IPC socket */
    int ret;
    opal_btl_usnic_connectivity_cmd_unlisten_t cmd;
    ret = opal_fd_read(ipc_listener->client_fd, sizeof(cmd), &cmd);
    if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("usnic connectivity agent IPC UNLISTEN read failed");
        /* Will not return */
    }

    /* If we are listening on this address (and we should be), then
       stop listening on it. */
    uint32_t udp_port;
    agent_udp_port_listener_t *udp_listener;
    udp_listener = agent_thread_find_listener(cmd.ipv4_addr, &udp_port);
    if (NULL != udp_listener) {
        OBJ_RELEASE(udp_listener);
    }

    /* All done! */
    return;
}

/*
 * Called when we get an incoming IPC message
 */
static void agent_thread_ipc_receive(int fd, short flags, void *context)
{
    int32_t command;
    agent_ipc_listener_t *ipc_listener = (agent_ipc_listener_t*) context;

    /* Read the command */
    command = -1;
    int ret = opal_fd_read(fd, sizeof(command), &command);
    if (OPAL_ERR_TIMEOUT == ret) {
        /* We get OPAL_ERR_TIMEOUT if the remote side hung up */
        OBJ_RELEASE(ipc_listener);
        return;
    } else if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("usnic connectivity agent IPC command read failed");
        /* Will not return */
    }

    assert(CONNECTIVITY_AGENT_CMD_LISTEN == command ||
           CONNECTIVITY_AGENT_CMD_PING == command ||
           CONNECTIVITY_AGENT_CMD_UNLISTEN == command);

    switch (command) {
    case CONNECTIVITY_AGENT_CMD_LISTEN:
        agent_thread_cmd_listen(ipc_listener);
        break;
    case CONNECTIVITY_AGENT_CMD_PING:
        agent_thread_cmd_ping(ipc_listener);
        break;
    case CONNECTIVITY_AGENT_CMD_UNLISTEN:
        agent_thread_cmd_unlisten(ipc_listener);
        break;
    default:
        ABORT("Unexpected connectivity agent command");
        break;
    }
}

/*
 * We got a new connection on the IPC named socket.  Add it to the
 * event base.
 */
static void agent_thread_accept(int fd, short flags, void *context)
{
    struct sockaddr addr;
    socklen_t len;
    agent_ipc_listener_t *listener = NULL;

    len = sizeof(addr);
    int client_fd = accept(fd, &addr, &len);
    if (client_fd < 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("accept() failed");
        /* Will not return */
    }

    /* If we got a good client, verify that it sent the magic token */
    int tlen = strlen(CONNECTIVITY_MAGIC_TOKEN);
    char *msg = alloca(tlen + 1);
    if (NULL == msg) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        ABORT("Out of memory");
        /* Will not return */
    }
    if (OPAL_SUCCESS != opal_fd_read(client_fd, tlen, msg)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity agent IPC read failed");
        /* Will not return */
    }
    if (0 != memcmp(msg, CONNECTIVITY_MAGIC_TOKEN, tlen)) {
        opal_output_verbose(20, USNIC_OUT,
                            "usNIC connectivity got bad IPC client (wrong magic token); disconnected");
        close(client_fd);
        return;
    }

    /* Make a listener object for this peer */
    listener = OBJ_NEW(agent_ipc_listener_t);
    listener->client_fd = client_fd;

    /* Write back the magic token to ACK that we got the peer's
       magic token and all is kosher */
    if (OPAL_SUCCESS != opal_fd_write(client_fd, tlen,
                                      CONNECTIVITY_MAGIC_TOKEN)) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        ABORT("usnic connectivity agent IPC read failed");
        /* Will not return */
    }

    /* Add this IPC listener to the event base */
    opal_event_set(evbase, &listener->event, client_fd,
                   OPAL_EV_READ | OPAL_EV_PERSIST,
                   agent_thread_ipc_receive, listener);
    opal_event_add(&listener->event, 0);

    /* Save this listener on the list of ipc_listeners */
    opal_list_append(&ipc_listeners, &listener->super);

    listener->active = true;

    return;
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
int opal_btl_usnic_connectivity_agent_init(void)
{
    /* Only do this initialization if I am the agent (the agent is
       local rank 0) */
    if (opal_process_info.my_local_rank != 0) {
        return OPAL_SUCCESS;
    }
    if (agent_initialized) {
        return OPAL_SUCCESS;
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
    OBJ_CONSTRUCT(&udp_port_listeners, opal_list_t);
    OBJ_CONSTRUCT(&ipc_listeners, opal_list_t);
    OBJ_CONSTRUCT(&pings_pending, opal_list_t);
    OBJ_CONSTRUCT(&ping_results, opal_list_t);

    /********************************************************************
     * Once all of the above is setup, create the unix domain socket
     * and start the event loop.
     ********************************************************************/

    /* Create the unix domain socket in the job session directory */
    ipc_accept_fd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (ipc_accept_fd < 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("socket() failed");
        /* Will not return */
    }

    asprintf(&ipc_filename, "%s/%s",
             opal_process_info.job_session_dir, CONNECTIVITY_SOCK_NAME);
    if (NULL == ipc_filename) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("Out of memory");
        /* Will not return */
    }
    unlink(ipc_filename);

    struct sockaddr_un address;
    assert(strlen(ipc_filename) < sizeof(address.sun_path));

    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    strncpy(address.sun_path, ipc_filename, sizeof(address.sun_path) - 1);

    if (bind(ipc_accept_fd, (struct sockaddr *) &address,
             sizeof(struct sockaddr_un)) != 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("bind() failed");
        /* Will not return */
    }

    /* Give an arbitrarily large backlog number so that connecting
       clients will never be backlogged (note for Future Jeff: please
       don't laugh at Past Jeff if 256 has become a trivially small
       number of on-server procs in a single job). */
    if (listen(ipc_accept_fd, 256) != 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("listen() failed");
        /* Will not return */
    }

    /* Add the socket to the event base */
    opal_event_set(evbase, &ipc_event, ipc_accept_fd,
                   OPAL_EV_READ | OPAL_EV_PERSIST,
                   agent_thread_accept, NULL);
    opal_event_add(&ipc_event, 0);

    /* Spawn the agent thread event loop */
    OBJ_CONSTRUCT(&agent_thread, opal_thread_t);
    agent_thread.t_run = agent_thread_main;
    agent_thread.t_arg = NULL;
    int ret;
    ret = opal_thread_start(&agent_thread);
    if (OPAL_SUCCESS != ret) {
        OPAL_ERROR_LOG(ret);
        ABORT("Failed to start usNIC agent thread");
        /* Will not return */
    }

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity agent initialized");
    agent_initialized = true;
    return OPAL_SUCCESS;
}

/*
 * Shut down the agent
 */
int opal_btl_usnic_connectivity_agent_finalize(void)
{
    agent_initialized = false;

    /* Only do this if I have the agent running */
    if (NULL == evbase) {
        return OPAL_SUCCESS;
    }

    /* Shut down the event loop.  Send it a no-op event so that it
       wakes up and exits the loop. */
    opal_event_t ev;
    agent_thread_time_to_exit = true;
    opal_event_set(evbase, &ev, -1, OPAL_EV_WRITE, agent_thread_noop, NULL);
    opal_event_active(&ev, OPAL_EV_WRITE, 1);
    opal_thread_join(&agent_thread, NULL);

    /* Shut down all active udp_port_listeners */
    agent_udp_port_listener_t *udp_listener, *ulnext;
    OPAL_LIST_FOREACH_SAFE(udp_listener, ulnext, &udp_port_listeners,
                           agent_udp_port_listener_t) {
        OBJ_RELEASE(udp_listener);
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

    /* Shut down all active ipc_listeners */
    agent_ipc_listener_t *ipc_listener, *inext;
    OPAL_LIST_FOREACH_SAFE(ipc_listener, inext, &ipc_listeners,
                           agent_ipc_listener_t) {
        OBJ_RELEASE(ipc_listener);
    }

    /* Close the local IPC socket and remove the file */
    if (ipc_accept_fd != -1) {
        close(ipc_accept_fd);
        ipc_accept_fd = -1;
    }
    if (NULL != ipc_filename) {
        unlink(ipc_filename);
        free(ipc_filename);
        ipc_filename = NULL;
    }

    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity client finalized");
    return OPAL_SUCCESS;
}
