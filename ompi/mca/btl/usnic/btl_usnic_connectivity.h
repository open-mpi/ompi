/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_BTL_USNIC_CONNECTIVITY_H
#define OMPI_BTL_USNIC_CONNECTIVITY_H

#include "ompi_config.h"

#include "opal/util/show_help.h"

#include "ompi/runtime/mpiruntime.h"

#include "btl_usnic_util.h"
#include "btl_usnic_proc.h"


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
 * client will RML send a request to the server asking it to listen on
 * its usnic interface.  The agent will discard duplicates and setup a
 * single UDP socket listener on the eth interface corresponding to
 * each requested usnic interface.  The agent returns the listening
 * UDP port number to the client, and each client puts this UDP port
 * number in their modex information.
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
 * All client/agent communication is via the RML.
 *
 * As mentioned above, the agent is smart about discarding duplicate
 * ping requests from clients.  Since a single agent serves all MPI
 * processes on a given server, this cuts down on a lot of PING
 * traffic.
 */

/*
 * Forward declaration
 */
struct ompi_btl_usnic_module_t;

/** @internal
 * This macro just makes the macros below a little easier to read.
 */
#define ABORT(msg) ompi_btl_usnic_util_abort((msg), __FILE__, __LINE__, 1)

/**
 * Helper macro to pack binary bytes into RML message buffers.
 *
 * Note that this macro will call ompi_btl_usnic_util_abort()
 * if an error occurs.
 */
#define PACK_BYTES(buffer, value, num_bytes)                            \
    do {                                                                \
        int ret;                                                        \
        ret = opal_dss.pack((buffer), (value), (num_bytes), OPAL_BYTE); \
        if (OPAL_SUCCESS != ret) {                                      \
            OMPI_ERROR_LOG(ret);                                        \
            ABORT("Could not pack");                                    \
        }                                                               \
    } while(0)

/**
 * @internal
 *
 * Back-end macro for PACK_STRING, PACK_INT32, and PACK_UINT32.
 *
 * Note that this macro will call ompi_btl_usnic_util_abort()
 * if an error occurs.
 */
#define PACK_(buffer, type, opal_type, value)                           \
    do {                                                                \
        int ret;                                                        \
        type temp = (type) (value);                                     \
        if (OPAL_SUCCESS !=                                             \
            (ret = opal_dss.pack((buffer), &temp, 1, opal_type))) {     \
            OMPI_ERROR_LOG(ret);                                        \
            ABORT("Could not pack");                                    \
        }                                                               \
    } while(0)

/**
 * Helper macro the pack a string into RML message buffers.
 */
#define PACK_STRING(buffer, value) PACK_(buffer, char *, OPAL_STRING, value)
/**
 * Helper macro the pack an int32_ into RML message buffers.
 */
#define PACK_INT32(buffer, value)  PACK_(buffer, int32_t, OPAL_INT32, value)
/**
 * Helper macro the pack a uint32_t into RML message buffers.
 */
#define PACK_UINT32(buffer, value) PACK_(buffer, uint32_t, OPAL_UINT32, value)
/**
 * Helper macro the pack a uint64_t into RML message buffers.
 */
#define PACK_UINT64(buffer, value) PACK_(buffer, uint64_t, OPAL_UINT64, value)

/**
 * Helper macro to unpack binary bytes from RML message buffers.
 *
 * Note that all of these macros will call ompi_btl_usnic_util_abort() if
 * an error occurs.
 */
#define UNPACK_BYTES(buffer, value, num_bytes)                          \
    do {                                                                \
        int ret_value, n = (num_bytes);                                 \
        ret_value = opal_dss.unpack((buffer), value, &n, OPAL_BYTE);    \
        if (OPAL_SUCCESS != ret_value) {                                \
            OMPI_ERROR_LOG(ret_value);                                  \
            ABORT("Could not unpack");                                  \
        }                                                               \
    } while(0)

/**
 * @internal
 *
 * Back-end macro for UNPACK_STRING, UNPACK_INT32, and UNPACK_UINT32
 *
 * Note that this macro will call ompi_btl_usnic_util_abort() if an error
 * occurs.
 *
 * Also note that we use a temp variable of the correct type because
 * some of the values passed in to this macro are volatile, and the
 * call to opal_dss.unpack() will discard that volatile qualifier.
 */
#define UNPACK(buffer, type, opal_unpack_type, value)                   \
    do {                                                                \
        int ret_value, n = 1;                                           \
        type temp;                                                      \
        value = (type) 0;                                               \
        ret_value = opal_dss.unpack((buffer), &temp, &n, opal_unpack_type); \
        if (OPAL_SUCCESS != ret_value) {                                \
            OMPI_ERROR_LOG(ret_value);                                  \
            ABORT("Could not unpack");                                  \
        } else {                                                        \
            value = (type) temp;                                        \
        }                                                               \
    } while(0)

/**
 * Helper macro to unpack a string from RML message buffers.
 */
#define UNPACK_STRING(buffer, value)                            \
    UNPACK(buffer, char *, OPAL_STRING, value);
/**
 * Helper macro to unpack an int32_t from RML message buffers.
 */
#define UNPACK_INT32(buffer, value)                             \
    UNPACK(buffer, int32_t, OPAL_INT32, value);
/**
 * Helper macro to unpack a uint32_t from RML message buffers.
 */
#define UNPACK_UINT32(buffer, value)                            \
    UNPACK(buffer, uint32_t, OPAL_UINT32, value)
/**
 * Helper macro to unpack a uint64_t from RML message buffers.
 */
#define UNPACK_UINT64(buffer, value)                            \
    UNPACK(buffer, uint64_t, OPAL_UINT64, value)

/**
 * RML message types.  This value is packed as the first field in each
 * RML message to identify its type.  Use a non-zero value as the
 * first enum just as defensive programming (i.e., it's a slightly
 * lower chance that an uninitialized message type would randomly
 * match these values).
 */
enum {
    CONNECTIVITY_AGENT_CMD_LISTEN = 17,
    CONNECTIVITY_AGENT_CMD_PING,
    CONNECTIVITY_AGENT_CMD_MAX
};

/**
 * Startup the connectivity client.
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int ompi_btl_usnic_connectivity_client_init(void);

/**
 * Tell the agent to establsh a listening port on the given IP
 * address.
 *
 * @params[in] module The module that is requesting the listen.
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
 *
 * The module contains the local interface addressing information,
 * which tells the agent one which interface to listen.
 *
 * This routine will request the new listen from the agent, and wait
 * for the agent to reply with the UDP port that is being used/was
 * created.  The UDP listening port will then be stuffed in
 * module->local_addr.connectivity_udp_port (i.e., data that will be
 * sent in the modex).
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int ompi_btl_usnic_connectivity_listen(struct ompi_btl_usnic_module_t *module);

/**
 * Tell the agent to ping a specific IP address and UDP port number
 * with a specific message size.
 *
 * @param[in] src_ipv4_addr The source module IPv4 address
 * @param[in] src_port The source module listening UDP port
 * @param[in] dest_ipv4_addr The destination IPv4 address
 * @param[in] dest_cidrmask The destination CIDR mask
 * @param[in] dest_port The destination UDP port
 * @param[in] dest_mac The destination MAC address
 * @param[in] dest_nodename The destination server name
 * @param[in] mtu The max ping message size to send
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
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
int ompi_btl_usnic_connectivity_ping(uint32_t src_ipv4_addr, int src_port,
                                     uint32_t dest_ipv4_addr,
                                     uint32_t dest_cidrmask, int dest_port,
                                     uint8_t *dest_mac, char *dest_nodename,
                                     size_t mtu);

/**
 * Shut down the connectivity service client.
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
 *
 * It is safe to call this function even if the connectivity check is
 * disabled; it will be a no-op in this case.
 */
int ompi_btl_usnic_connectivity_client_finalize(void);

/**
 * Startup the connectivity agent.
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
 *
 * This function will be a no-op if this process is not the local rank
 * 0.
 */
int ompi_btl_usnic_connectivity_agent_init(void);

/**
 * Shut down the connectivity agent
 *
 * @returns OMPI_SUCCESS or an OMPI error code.
 *
 * This function will be a no-op if this process is not the local rank
 * 0.
 */
int ompi_btl_usnic_connectivity_agent_finalize(void);


/**
 * Helper function invoked in the BTL that will invoke a ping, if the
 * ping hasn't already been invoked.
 */
static inline void
ompi_btl_usnic_check_connectivity(ompi_btl_usnic_module_t *module,
				  ompi_btl_usnic_endpoint_t *endpoint)
{
    if (OPAL_LIKELY(mca_btl_usnic_component.connectivity_enabled) &&
        OPAL_UNLIKELY(!endpoint->endpoint_connectivity_checked)) {
        ompi_btl_usnic_connectivity_ping(module->local_addr.ipv4_addr,
                                         module->local_addr.connectivity_udp_port,
                                         endpoint->endpoint_remote_addr.ipv4_addr,
                                         endpoint->endpoint_remote_addr.cidrmask,
                                         endpoint->endpoint_remote_addr.connectivity_udp_port,
                                         endpoint->endpoint_remote_addr.mac,
					 endpoint->endpoint_proc->proc_ompi->proc_hostname,
                                         endpoint->endpoint_remote_addr.mtu);
        endpoint->endpoint_connectivity_checked = true;
    }
}

#endif /* OMPI_BTL_USNIC_CONNECITIVITY_H */
