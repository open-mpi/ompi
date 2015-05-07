/*
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_USNIC_CONNECTIVITY_H
#define OPAL_BTL_USNIC_CONNECTIVITY_H

#include "opal_config.h"

#include "opal/util/show_help.h"

#if BTL_IN_OPAL
#include "opal/util/proc.h"
#else
#include "ompi/proc/proc.h"
#endif

#include "btl_usnic_compat.h"
#include "btl_usnic_module.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_util.h"


/**
 * Agent-based service to verify UDP connectivity between two peers.
 *
 * Generally, it is a client-server pattern with three entities
 * involved:
 *
 * 1. Agent thread: running in MPI process local rank 0
 * 2. Client: running in the main application thread in every MPI process
 * 3. RTE thread: running in every MPI process
 *
 * If enabled (via MCA param), the usnic module_init() will setup the
 * client (and server on local rank 0).  For each usnic module, Each
 * client will send a request to the server (via local Unix domain
 * socket) asking it to listen on its usnic interface.  The agent will
 * discard duplicates and setup a single UDP socket listener on the
 * eth interface corresponding to each requested usnic interface.  The
 * agent returns the listening UDP port number to the client, and each
 * client puts this UDP port number in their modex information.
 *
 * At the first send to a given MPI process peer, the client will send
 * another request to the server asking it to verify connectivity to
 * the peer (supplying the peer's UDP listener port number from the
 * peer's modex info).  Again, the agent will discard duplicates -- it
 * will only verify connectivity to each peer's *server* once.  The
 * agent will send a short UDP message and a long UDP message
 * (basically, the MTU-68 bytes -- see comment in btl_usnic_cagent.c
 * for the reasons why) to the listening peer UDP port.
 *
 * When the peer agent gets PING messages, it sends short ACK control
 * messages back to the sending agent.  When the sending agent gets
 * all ACKs back from the peer, it rules that connectivity is GOOD and
 * no further action is taken.  If the sending agent doesn't get one
 * or both ACKs back in a timely fashion, it re-sends the PING(s) that
 * wasn't(weren't) ACKed.  Eventually if the sending agent re-sends
 * too many times and does not get an ACK back, it gives up, displays
 * and error, and aborts the MPI job.
 *
 * Note that the client/server interaction is intentionally quite
 * primitive:
 *
 * 1. Client requests agent to listen on interface X.  Server responds
 * with UDP port number of listener.
 *
 * 2. Client requests ping check to peer Y.  Client does not wait for
 * the answer; the agent either verifies the connectivity successfully
 * or aborts the job.
 *
 * All client/agent communication is via blocking calls to a local
 * Unix domain socket.
 *
 * As mentioned above, the agent is smart about discarding duplicate
 * ping requests from clients.  Since a single agent serves all MPI
 * processes on a given server, this cuts down on a lot of PING
 * traffic.
 */

/*
 * Forward declaration
 */
struct opal_btl_usnic_module_t;

/** @internal
 * This macro just makes the macros below a little easier to read.
 */
#define ABORT(msg) opal_btl_usnic_util_abort((msg), __FILE__, __LINE__)

/**
 * Local IPC socket message types.  This value is either sent or
 * packed as the first field in each message to identify its type.
 * Use a non-zero value as the first enum just as defensive
 * programming (i.e., it's a slightly lower chance that an
 * uninitialized message type would randomly match these values).
 */
enum {
    CONNECTIVITY_AGENT_CMD_LISTEN = 17,
    CONNECTIVITY_AGENT_CMD_PING,
    CONNECTIVITY_AGENT_CMD_UNLISTEN,
    CONNECTIVITY_AGENT_CMD_MAX
};

#define CONNECTIVITY_NODENAME_LEN 128
#define CONNECTIVITY_IFNAME_LEN 32

/*
 * Unix domain socket name
 */
#define CONNECTIVITY_SOCK_NAME "btl-usnic-cagent-socket"

/*
 * Magic token to ensure that client/server recognize each other
 */
#define CONNECTIVITY_MAGIC_TOKEN "-*-I am usNIC; hear me roar-*-"

/*
 * Fields for the LISTEN command.  This struct is sent down the IPC
 * socket from the cclient to the cagent.
 */
typedef struct {
    void *module;
    uint32_t ipv4_addr;
    uint32_t netmask;
    uint32_t max_msg_size;
    char nodename[CONNECTIVITY_NODENAME_LEN];
    char usnic_name[CONNECTIVITY_IFNAME_LEN];
} opal_btl_usnic_connectivity_cmd_listen_t;

/*
 * Fields for the UNLISTEN command.  This struct is sent down the IPC
 * socket from the cclient to the cagent.
 */
typedef struct {
    uint32_t ipv4_addr;
} opal_btl_usnic_connectivity_cmd_unlisten_t;

/*
 * Command+fields for the reply to the LISTEN command.  This struct is
 * sent down the IPC socket from the cagent to the cclient.
 */
typedef struct {
    int32_t cmd;
    uint32_t ipv4_addr;
    uint32_t udp_port;
} opal_btl_usnic_connectivity_cmd_listen_reply_t;

/*
 * Fields for the PING command.  This struct is sent down the IPC
 * socket from the cclient to the cagent.
 */
typedef struct {
    uint32_t src_ipv4_addr;
    uint32_t src_udp_port;
    uint32_t dest_ipv4_addr;
    uint32_t dest_netmask;
    uint32_t dest_udp_port;
    uint32_t max_msg_size;
    char dest_nodename[CONNECTIVITY_NODENAME_LEN];
} opal_btl_usnic_connectivity_cmd_ping_t;

/**
 * Startup the connectivity client.
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int opal_btl_usnic_connectivity_client_init(void);

/**
 * Tell the agent to establsh a listening port on the given IP
 * address.
 *
 * @params[in] module The module that is requesting the listen.
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * The module contains the local interface addressing information,
 * which tells the agent on which interface to listen.
 *
 * This routine will request the new listen from the agent, and wait
 * for the agent to reply with the UDP port that is being used/was
 * created.  The UDP listening port will then be stuffed in
 * module->local_modex.connectivity_udp_port (i.e., data that will be
 * sent in the modex).
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int opal_btl_usnic_connectivity_listen(struct opal_btl_usnic_module_t *module);

/**
 * Tell the agent to ping a specific IP address and UDP port number
 * with a specific message size.
 *
 * @param[in] src_ipv4_addr The source module IPv4 address
 * @param[in] src_port The source module listening UDP port
 * @param[in] dest_ipv4_addr The destination IPv4 address
 * @param[in] dest_netmask The destination netmask
 * @param[in] dest_port The destination UDP port
 * @param[in] dest_nodename The destination server name
 * @param[in] max_msg_size The max ping message size to send
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * Note that several of the above parameters are only passed so that
 * they can be used in a complete/helpful error message, if necessary.
 *
 * This function does not wait for a reply from the agent; it assumes
 * the agent will successfully ping the remote peer or will abort the
 * MPI job if the pinging fails.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int opal_btl_usnic_connectivity_ping(uint32_t src_ipv4_addr, int src_port,
                                     uint32_t dest_ipv4_addr,
                                     uint32_t dest_netmask, int dest_port,
                                     char *dest_nodename,
                                     size_t max_msg_size);

/**
 * Tell the agent to stop listening on the given IP address.
 *
 * @params[in] module The module that is requesting the unlisten.
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * The module contains the local interface addressing information,
 * which tells the agent on which interface to stop listening.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int opal_btl_usnic_connectivity_unlisten(struct opal_btl_usnic_module_t *module);

/**
 * Shut down the connectivity service client.
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int opal_btl_usnic_connectivity_client_finalize(void);

/**
 * Startup the connectivity agent.
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * This function will be a no-op if this process is not the local rank
 * 0.
 */
int opal_btl_usnic_connectivity_agent_init(void);

/**
 * Shut down the connectivity agent
 *
 * @returns OPAL_SUCCESS or an OPAL error code.
 *
 * This function will be a no-op if this process is not the local rank
 * 0.
 */
int opal_btl_usnic_connectivity_agent_finalize(void);


/**
 * Helper function invoked in the BTL that will invoke a ping, if the
 * ping hasn't already been invoked.
 */
static inline void
opal_btl_usnic_check_connectivity(opal_btl_usnic_module_t *module,
                                  opal_btl_usnic_endpoint_t *endpoint)
{
    if (OPAL_LIKELY(mca_btl_usnic_component.connectivity_enabled) &&
        OPAL_UNLIKELY(!endpoint->endpoint_connectivity_checked)) {
        opal_btl_usnic_connectivity_ping(module->local_modex.ipv4_addr,
                                         module->local_modex.connectivity_udp_port,
                                         endpoint->endpoint_remote_modex.ipv4_addr,
                                         endpoint->endpoint_remote_modex.netmask,
                                         endpoint->endpoint_remote_modex.connectivity_udp_port,
                                         opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                                         endpoint->endpoint_remote_modex.max_msg_size);
        endpoint->endpoint_connectivity_checked = true;
    }
}

#endif /* OPAL_BTL_USNIC_CONNECITIVITY_H */
