/*
 * Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <assert.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <time.h>

#include "opal_stdint.h"
#include "opal/threads/mutex.h"
#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/util/fd.h"

#include "btl_usnic.h"
#include "btl_usnic_module.h"
#include "btl_usnic_connectivity.h"

/**************************************************************************
 * Client-side data and methods
 **************************************************************************/

static bool initialized = false;
static int agent_fd = -1;


/*
 * Startup the agent and share our MCA param values with the it.
 */
int opal_btl_usnic_connectivity_client_init(void)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OPAL_SUCCESS;
    }
    assert(!initialized);

    /* Open local IPC socket to the agent */
    agent_fd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (agent_fd < 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("socket() failed");
        /* Will not return */
    }

    char *ipc_filename = NULL;
    asprintf(&ipc_filename, "%s/%s",
             opal_process_info.job_session_dir, CONNECTIVITY_SOCK_NAME);
    if (NULL == ipc_filename) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("Out of memory");
        /* Will not return */
    }
#if !defined(NDEBUG)
    struct sockaddr_un sun;
    assert(strlen(ipc_filename) <= sizeof(sun.sun_path));
#endif

    /* Wait for the agent to create its socket.  Timeout after 10
       seconds if we don't find the socket. */
    struct stat sbuf;
    time_t start = time(NULL);
    while (1) {
        int ret = stat(ipc_filename, &sbuf);
        if (0 == ret) {
            break;
        } else if (ENOENT != errno) {
            /* If the error wasn't "file not found", then something
               else Bad happened */
            OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
            ABORT("stat() failed");
            /* Will not return */
        }

        /* If the named socket wasn't there yet, then give the agent a
           little time to establish it */
        usleep(1);

        if (time(NULL) - start > 10) {
            ABORT("connectivity client timeout waiting for server socket to appear");
            /* Will not return */
        }
    }

    /* Connect */
    struct sockaddr_un address;
    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    strncpy(address.sun_path, ipc_filename, sizeof(address.sun_path) - 1);

    if (0 != connect(agent_fd, (struct sockaddr*) &address, sizeof(address))) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("connect() failed");
        /* Will not return */
    }

    /* Send the magic token */
    int tlen = strlen(CONNECTIVITY_MAGIC_TOKEN);
    if (OPAL_SUCCESS != opal_fd_write(agent_fd, tlen,
                                      CONNECTIVITY_MAGIC_TOKEN)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC connect write failed");
        /* Will not return */
    }

    /* Receive a magic token back */
    char *ack = alloca(tlen + 1);
    if (NULL == ack) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("Out of memory");
        /* Will not return */
    }
    if (OPAL_SUCCESS != opal_fd_read(agent_fd, tlen, ack)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC connect read failed");
        /* Will not return */
    }
    if (memcmp(ack, CONNECTIVITY_MAGIC_TOKEN, tlen) != 0) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client got wrong token back from agent");
        /* Will not return */
    }

    /* All done */
    initialized = true;
    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity client initialized");
    return OPAL_SUCCESS;
}


/*
 * Send a listen command to the agent
 */
int opal_btl_usnic_connectivity_listen(opal_btl_usnic_module_t *module)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        module->local_modex.connectivity_udp_port = 0;
        return OPAL_SUCCESS;
    }

    /* Send the LISTEN command */
    int id = CONNECTIVITY_AGENT_CMD_LISTEN;
    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(id), &id)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    /* Send the LISTEN command parameters */
    opal_btl_usnic_connectivity_cmd_listen_t cmd = {
        .module = NULL,
        .ipv4_addr = module->local_modex.ipv4_addr,
        .netmask = module->local_modex.netmask,
        .max_msg_size = module->local_modex.max_msg_size
    };
    /* Only the MPI process who is also the agent will send the
       pointer value (it doesn't make sense otherwise) */
    if (0 == opal_process_info.my_local_rank) {
        cmd.module = module;
    }

    /* Ensure to NULL-terminate the passed strings */
    strncpy(cmd.nodename, opal_process_info.nodename,
            CONNECTIVITY_NODENAME_LEN - 1);
    strncpy(cmd.usnic_name, module->fabric_info->fabric_attr->name,
            CONNECTIVITY_IFNAME_LEN - 1);

    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(cmd), &cmd)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    /* Wait for the reply with the UDP port */
    opal_btl_usnic_connectivity_cmd_listen_reply_t reply;
    memset(&reply, 0, sizeof(reply));
    if (OPAL_SUCCESS != opal_fd_read(agent_fd, sizeof(reply), &reply)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC read failed");
        /* Will not return */
    }

    /* Get the UDP port number that was received */
    assert(CONNECTIVITY_AGENT_CMD_LISTEN == reply.cmd);
    module->local_modex.connectivity_udp_port = reply.udp_port;

    return OPAL_SUCCESS;
}


int opal_btl_usnic_connectivity_ping(uint32_t src_ipv4_addr, int src_port,
                                     uint32_t dest_ipv4_addr,
                                     uint32_t dest_netmask, int dest_port,
                                     char *dest_nodename,
                                     size_t max_msg_size)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OPAL_SUCCESS;
    }

    /* Send the PING command */
    int id = CONNECTIVITY_AGENT_CMD_PING;
    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(id), &id)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    /* Send the PING command parameters */
    opal_btl_usnic_connectivity_cmd_ping_t cmd = {
        .src_ipv4_addr = src_ipv4_addr,
        .src_udp_port = src_port,
        .dest_ipv4_addr = dest_ipv4_addr,
        .dest_netmask = dest_netmask,
        .dest_udp_port = dest_port,
        .max_msg_size = max_msg_size
    };
    /* Ensure to NULL-terminate the passed string */
    strncpy(cmd.dest_nodename, dest_nodename, CONNECTIVITY_NODENAME_LEN - 1);

    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(cmd), &cmd)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    return OPAL_SUCCESS;
}


/*
 * Send an unlisten command to the agent
 */
int opal_btl_usnic_connectivity_unlisten(opal_btl_usnic_module_t *module)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OPAL_SUCCESS;
    }
    /* Only the MPI process who is also the agent will send the
       UNLISTEN command */
    if (0 != opal_process_info.my_local_rank) {
        return OPAL_SUCCESS;
    }

    /* Send the UNLISTEN command */
    int id = CONNECTIVITY_AGENT_CMD_UNLISTEN;
    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(id), &id)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    /* Send the UNLISTEN command parameters */
    opal_btl_usnic_connectivity_cmd_unlisten_t cmd = {
        .ipv4_addr = module->local_modex.ipv4_addr,
    };

    if (OPAL_SUCCESS != opal_fd_write(agent_fd, sizeof(cmd), &cmd)) {
        OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
        ABORT("usnic connectivity client IPC write failed");
        /* Will not return */
    }

    return OPAL_SUCCESS;
}


/*
 * Shut down the connectivity client
 */
int opal_btl_usnic_connectivity_client_finalize(void)
{
    /* Make it safe to finalize, even if we weren't initialized */
    if (!initialized) {
        return OPAL_SUCCESS;
    }

    close(agent_fd);
    agent_fd = -1;

    initialized = false;
    return OPAL_SUCCESS;
}
