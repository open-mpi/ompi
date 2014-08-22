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
#include <string.h>
#include <unistd.h>

#include "opal_stdint.h"
#include "opal/threads/mutex.h"
#include "opal/mca/event/event.h"
#include "opal/mca/db/db.h"
#include "opal/util/output.h"

#include "ompi/proc/proc.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/constants.h"

#include "btl_usnic.h"
#include "btl_usnic_module.h"
#include "btl_usnic_connectivity.h"

/**************************************************************************
 * Client-side data and methods
 **************************************************************************/

static bool initialized = false;
static ompi_process_name_t agent_name;

typedef struct {
    uint32_t addr;
    uint32_t udp_port;
    bool receive_done;
} client_rml_receive_data_t;


/*
 * Receive replies from the agent
 */
static void client_rml_receive(int status, ompi_process_name_t* sender,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tag, void *cbdata)
{
    int32_t command;
    volatile client_rml_receive_data_t *cddr =
        (client_rml_receive_data_t*) cbdata;

    /* What command is this a reply for? */
    UNPACK_INT32(buffer, command);
    assert(command == CONNECTIVITY_AGENT_CMD_LISTEN);

    UNPACK_UINT32(buffer, cddr->addr);
    UNPACK_UINT32(buffer, cddr->udp_port);

    /* Tell the main thread that the reply is done */
    opal_atomic_mb();
    cddr->receive_done = true;
}


/*
 * Startup the agent and share our MCA param values with the it.
 */
int ompi_btl_usnic_connectivity_client_init(void)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OMPI_SUCCESS;
    }

    assert(!initialized);

    /* Get the name of the agent */
    int ret;
    ompi_process_name_t *ptr;
    ptr = &agent_name;
    ret = ompi_rte_db_fetch(ompi_proc_local_proc, OPAL_DB_LOCALLDR, (void**) &ptr, OPAL_ID_T);
    if (OMPI_SUCCESS != ret) {
        OMPI_ERROR_LOG(ret);
        BTL_ERROR(("usNIC connectivity client unable to db_fetch local leader"));
        return ret;
    }

    initialized = true;
    opal_output_verbose(20, USNIC_OUT,
                        "usNIC connectivity client initialized");
    return OMPI_SUCCESS;
}


/*
 * Send a listen command to the agent
 */
int ompi_btl_usnic_connectivity_listen(ompi_btl_usnic_module_t *module)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OMPI_SUCCESS;
    }

    opal_buffer_t *msg;
    msg = OBJ_NEW(opal_buffer_t);
    if (NULL == msg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Send the LISTEN command.  Include enough information for the
       agent to be able to print a show_help() message, if
       necessary. */
    PACK_INT32(msg, CONNECTIVITY_AGENT_CMD_LISTEN);
    /* Only the MPI process who is also the agent will send the
       pointer value (it doesn't make sense otherwise) */
    if (0 == ompi_process_info.my_local_rank) {
        PACK_UINT64(msg, (uint64_t) module);
    } else {
        PACK_UINT64(msg, (uint64_t) NULL);
    }
    PACK_UINT32(msg, module->local_addr.ipv4_addr);
    PACK_UINT32(msg, module->local_addr.cidrmask);
    PACK_UINT32(msg, module->local_addr.mtu);
    PACK_STRING(msg, ompi_process_info.nodename);
    PACK_STRING(msg, module->if_name);
    PACK_STRING(msg, ibv_get_device_name(module->device));
    PACK_BYTES(msg, module->local_addr.mac, 6);

    /* Post a receive for the agent to reply with the UDP port to me */
    volatile client_rml_receive_data_t data;
    data.receive_done = false;
    ompi_rte_recv_buffer_nb(OMPI_NAME_WILDCARD,
                            OMPI_RML_TAG_USNIC_CONNECTIVITY_REPLY,
                            0,
                            client_rml_receive, (void*) &data);

    /* Send it to the agent */
    int ret;
    ret = ompi_rte_send_buffer_nb(&agent_name, msg,
                                  OMPI_RML_TAG_USNIC_CONNECTIVITY,
                                  ompi_rte_send_cbfunc, NULL);
    if (OMPI_SUCCESS != ret) {
        OMPI_ERROR_LOG(ret);
        OBJ_RELEASE(msg);
        return ret;
    }

    /* Wait for the reply */
    while (!data.receive_done) {
        /* Sleep to let the RTE progress thread run */
        usleep(1);
    }

    /* Get the UDP port number that was received */
    opal_atomic_mb();
    module->local_addr.connectivity_udp_port = data.udp_port;

    return OMPI_SUCCESS;
}


int ompi_btl_usnic_connectivity_ping(uint32_t src_ipv4_addr, int src_port,
                                     uint32_t dest_ipv4_addr,
                                     uint32_t dest_cidrmask, int dest_port,
                                     uint8_t dest_mac[6], char *dest_nodename,
                                     size_t mtu)
{
    /* If connectivity checking is not enabled, do nothing */
    if (!mca_btl_usnic_component.connectivity_enabled) {
        return OMPI_SUCCESS;
    }

    opal_buffer_t *msg;
    msg = OBJ_NEW(opal_buffer_t);
    if (NULL == msg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    PACK_INT32(msg, CONNECTIVITY_AGENT_CMD_PING);
    PACK_UINT32(msg, src_ipv4_addr);
    PACK_UINT32(msg, src_port);
    PACK_UINT32(msg, dest_ipv4_addr);
    PACK_UINT32(msg, dest_cidrmask);
    PACK_UINT32(msg, dest_port);
    PACK_BYTES(msg, dest_mac, 6);
    PACK_UINT32(msg, mtu);
    PACK_STRING(msg, dest_nodename);

    /* Send it to the agent */
    int ret;
    ret = ompi_rte_send_buffer_nb(OMPI_PROC_MY_NAME, msg,
                                  OMPI_RML_TAG_USNIC_CONNECTIVITY,
                                  ompi_rte_send_cbfunc, NULL);
    if (OMPI_SUCCESS != ret) {
        OMPI_ERROR_LOG(ret);
        OBJ_RELEASE(msg);
        return ret;
    }

    return OMPI_SUCCESS;
}


/*
 * Shut down the connectivity client
 */
int ompi_btl_usnic_connectivity_client_finalize(void)
{
    /* Make it safe to finalize, even if we weren't initialized */
    if (!initialized) {
        return OMPI_SUCCESS;
    }

    initialized = false;
    return OMPI_SUCCESS;
}
