/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>

#include "opal_stdint.h"
#include "opal/util/arch.h"
#include "opal/util/show_help.h"

#include "ompi/mca/rte/rte.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/constants.h"

#include "btl_usnic.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_util.h"


static void proc_construct(ompi_btl_usnic_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_modex = NULL;
    proc->proc_modex_count = 0;
    proc->proc_modex_claimed = NULL;
    proc->proc_endpoints = NULL;
    proc->proc_endpoint_count = 0;

    /* add to list of all proc instance */
    opal_list_append(&mca_btl_usnic_component.usnic_procs, &proc->super);
}


static void proc_destruct(ompi_btl_usnic_proc_t* proc)
{
    /* remove from list of all proc instances */
    opal_list_remove_item(&mca_btl_usnic_component.usnic_procs, &proc->super);

    /* release resources */
    if (NULL != proc->proc_modex) {
        free(proc->proc_modex);
        proc->proc_modex = NULL;
    }

    if (NULL != proc->proc_modex_claimed) {
        free(proc->proc_modex_claimed);
        proc->proc_modex_claimed = NULL;
    }

    /* Release all endpoints associated with this proc */
    if (NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
        proc->proc_endpoints = NULL;
    }
}


OBJ_CLASS_INSTANCE(ompi_btl_usnic_proc_t,
                   opal_list_item_t, 
                   proc_construct,
                   proc_destruct);

/*
 * Look for an existing usnic process instance based on the
 * associated ompi_proc_t instance.
 */
ompi_btl_usnic_proc_t *
ompi_btl_usnic_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    ompi_btl_usnic_proc_t* usnic_proc;

    for (usnic_proc = (ompi_btl_usnic_proc_t*)
             opal_list_get_first(&mca_btl_usnic_component.usnic_procs);
         usnic_proc != (ompi_btl_usnic_proc_t*)
             opal_list_get_end(&mca_btl_usnic_component.usnic_procs);
         usnic_proc  = (ompi_btl_usnic_proc_t*)
             opal_list_get_next(usnic_proc)) {
        if (usnic_proc->proc_ompi == ompi_proc) {
            return usnic_proc;
        }
    }

    return NULL;
}


/*
 * Look for an existing usnic proc based on a hashed RTE process
 * name.
 */
ompi_btl_usnic_endpoint_t *
ompi_btl_usnic_proc_lookup_endpoint(ompi_btl_usnic_module_t *receiver,
                                    uint64_t sender_hashed_rte_name)
{
    size_t i;
    uint32_t mynet, peernet;
    ompi_btl_usnic_proc_t *proc;
    ompi_btl_usnic_endpoint_t *endpoint;
    
    for (proc = (ompi_btl_usnic_proc_t*)
             opal_list_get_first(&mca_btl_usnic_component.usnic_procs);
         proc != (ompi_btl_usnic_proc_t*)
             opal_list_get_end(&mca_btl_usnic_component.usnic_procs);
         proc  = (ompi_btl_usnic_proc_t*)
             opal_list_get_next(proc)) {
        if (ompi_rte_hash_name(&proc->proc_ompi->proc_name) ==
            sender_hashed_rte_name) {
            break;
        }
    }

    /* If we didn't find the sending proc (!), return NULL */
    if (opal_list_get_end(&mca_btl_usnic_component.usnic_procs) == 
        (opal_list_item_t*) proc) {
        return NULL;
    }

    /* Look through all the endpoints on sender's proc and find one
       that we can reach.  For the moment, do the same test as in
       match_modex: check to see if we have compatible IPv4
       networks. */
    mynet = ompi_btl_usnic_get_ipv4_subnet(receiver->if_ipv4_addr,
                                           receiver->if_cidrmask);

    for (i = 0; i < proc->proc_endpoint_count; ++i) {
        endpoint = proc->proc_endpoints[i];
        peernet = ompi_btl_usnic_get_ipv4_subnet(endpoint->endpoint_remote_addr.ipv4_addr,
                                                 endpoint->endpoint_remote_addr.cidrmask);

        /* If we match, we're done */
        if (mynet == peernet) {
            return endpoint;
        }
    }

    /* Didn't find it */
    return NULL;
}


/*
 * Create an ompi_btl_usnic_proc_t and initialize it with modex info
 * and an empty array of endpoints.
 */
static ompi_btl_usnic_proc_t *create_proc(ompi_proc_t *ompi_proc)
{
    ompi_btl_usnic_proc_t *proc = NULL;
    size_t size;
    int rc;

    /* Create the proc if it doesn't already exist */
    proc = OBJ_NEW(ompi_btl_usnic_proc_t);
    if (NULL == proc) {
        return NULL;
    }

    /* Initialize number of peers */
    proc->proc_endpoint_count = 0;
    proc->proc_ompi = ompi_proc;

    /* query for the peer address info */
    rc = ompi_modex_recv(&mca_btl_usnic_component.super.btl_version,
                         ompi_proc, (void*)&proc->proc_modex,
                         &size);

    if (OMPI_SUCCESS != rc) {
        opal_show_help("help-mpi-btl-usnic.txt", "internal error during init",
                       true,
                       ompi_process_info.nodename,
                       "<none>", 0,
                       "ompi_modex_recv() failed", __FILE__, __LINE__,
                       opal_strerror(rc));
        OBJ_RELEASE(proc);
        return NULL;
    }

    if ((size % sizeof(ompi_btl_usnic_addr_t)) != 0) {
        char msg[1024];

        snprintf(msg, sizeof(msg), 
                 "sizeof(modex for peer %s data) == %d, expected multiple of %d",
                 OMPI_NAME_PRINT(&ompi_proc->proc_name),
                 (int) size, (int) sizeof(ompi_btl_usnic_addr_t));
        opal_show_help("help-mpi-btl-usnic.txt", "internal error during init",
                       true,
                       ompi_process_info.nodename,
                       "<none>", 0,
                       "invalid modex data", __FILE__, __LINE__,
                       msg);

        OBJ_RELEASE(proc);
        return NULL;
    }

    proc->proc_modex_count = size / sizeof(ompi_btl_usnic_addr_t);
    if (0 == proc->proc_modex_count) {
        proc->proc_endpoints = NULL;
        OBJ_RELEASE(proc);
        return NULL;
    }

    proc->proc_modex_claimed = (bool*) 
        calloc(proc->proc_modex_count, sizeof(bool));
    if (NULL == proc->proc_modex_claimed) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(proc);
        return NULL;
    }

    proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        calloc(proc->proc_modex_count, sizeof(mca_btl_base_endpoint_t*));
    if (NULL == proc->proc_endpoints) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(proc);
        return NULL;
    }

    return proc;
}

/*
 * For a specific module, see if this proc has matching address/modex
 * info.  If so, create an endpoint and return it.
 */
static int match_modex(ompi_btl_usnic_module_t *module,
                       ompi_btl_usnic_proc_t *proc)
{
    size_t i;

    /* Each module can claim an address in the proc's modex info that
       no other local module is using.  See if we can find an unused
       address that's on this module's subnet. */
    for (i = 0; i < proc->proc_modex_count; ++i) {
        if (!proc->proc_modex_claimed[i]) {
            char my_ip_string[32], peer_ip_string[32];
            uint32_t mynet, peernet;

            inet_ntop(AF_INET, &module->if_ipv4_addr,
                      my_ip_string, sizeof(my_ip_string));
            inet_ntop(AF_INET, &proc->proc_modex[i].ipv4_addr,
                      peer_ip_string, sizeof(peer_ip_string));
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic:match_modex: checking my IP address/subnet (%s/%d) vs. peer (%s/%d)",
                                my_ip_string, module->if_cidrmask,
                                peer_ip_string, proc->proc_modex[i].cidrmask);

            /* JMS For the moment, do an abbreviated comparison.  Just
               compare the CIDR-masked IP address to see if they're on
               the same network.  If so, we're good.  Need to
               eventually replace this with the same type of IP
               address matching that is in the TCP BTL (probably want
               to move that routine down to opal/util/if.c...?) */
            mynet   = ompi_btl_usnic_get_ipv4_subnet(module->if_ipv4_addr,
                                                     module->if_cidrmask);
            peernet = ompi_btl_usnic_get_ipv4_subnet(proc->proc_modex[i].ipv4_addr,
                                                     proc->proc_modex[i].cidrmask);

            /* If we match, we're done */
            if (mynet == peernet) {
                opal_output_verbose(5, USNIC_OUT, "btl:usnic:match_modex: IP networks match -- yay!");
                break;
            }
            else {
                opal_output_verbose(5, USNIC_OUT, "btl:usnic:match_modex: IP networks DO NOT match -- mynet=%#x peernet=%#x",
                                    mynet, peernet);
            }
        }
    }

    /* Return -1 on failure */
    if (i >= proc->proc_modex_count) {
        return -1;
    }

    /* If MTU does not match, throw an error */
    if (proc->proc_modex[i].mtu != module->if_mtu) {
        opal_show_help("help-mpi-btl-usnic.txt", "MTU mismatch",
                       true,
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->port_num,
                       module->if_mtu,
                       (NULL == proc->proc_ompi->proc_hostname) ?
                       "unknown" : proc->proc_ompi->proc_hostname,
                       proc->proc_modex[i].mtu);
        return -1;
    } 

    return i;
}
/*
 * Create an endpoint and claim the matched modex slot
 */
int
ompi_btl_usnic_create_endpoint(ompi_btl_usnic_module_t *module,
                ompi_btl_usnic_proc_t *proc,
                ompi_btl_usnic_endpoint_t **endpoint_o)
{
    int modex_index;
    struct ibv_ah_attr ah_attr;
    ompi_btl_usnic_endpoint_t *endpoint;

    /* look for matching modex info */
    modex_index = match_modex(module, proc);
    if (modex_index < 0) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:create_endpoint: did not find usnic modex info for peer %s",
                            OMPI_NAME_PRINT(&proc->proc_ompi->proc_name));
        return OMPI_ERR_NOT_FOUND;
    }

    endpoint = OBJ_NEW(ompi_btl_usnic_endpoint_t);
    if (NULL == endpoint) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Initalize the endpoint */
    endpoint->endpoint_module = module;
    endpoint->endpoint_remote_addr = proc->proc_modex[modex_index];

    /* Initialize endpoint sequence number info */
    endpoint->endpoint_next_seq_to_send = module->local_addr.isn;
    endpoint->endpoint_ack_seq_rcvd = endpoint->endpoint_next_seq_to_send - 1;
    endpoint->endpoint_next_contig_seq_to_recv =
        endpoint->endpoint_remote_addr.isn;
    endpoint->endpoint_highest_seq_rcvd =
        endpoint->endpoint_next_contig_seq_to_recv - 1;
    endpoint->endpoint_rfstart = WINDOW_SIZE_MOD(endpoint->endpoint_next_contig_seq_to_recv);

    /* Create the address handle on this endpoint from the modex info.
       memset to both silence valgrind warnings (since the attr struct
       ends up getting written down an fd to the kernel) and actually
       zero out all the fields that we don't care about / want to be
       logically false. */
    memset(&ah_attr, 0, sizeof(ah_attr));
    ah_attr.is_global = 1;
    ah_attr.port_num = 1;
    ah_attr.grh.dgid = endpoint->endpoint_remote_addr.gid;

    endpoint->endpoint_remote_ah = ibv_create_ah(module->pd, &ah_attr);
    if (NULL == endpoint->endpoint_remote_ah) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true,
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->port_num,
                       "ibv_create_ah()", __FILE__, __LINE__,
                       "Failed to create an address handle");
        OBJ_RELEASE(endpoint);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Now claim that modex slot */
    proc->proc_modex_claimed[modex_index] = true;

    /* Save the endpoint on this proc's array of endpoints */
    proc->proc_endpoints[proc->proc_endpoint_count] = endpoint;
    endpoint->endpoint_proc_index = proc->proc_endpoint_count;
    endpoint->endpoint_proc = proc;
    ++proc->proc_endpoint_count;
    OBJ_RETAIN(proc);

    /* also add endpoint to module's list of endpoints */
    opal_list_append(&(module->all_endpoints),
            &(endpoint->endpoint_endpoint_li));

    *endpoint_o = endpoint;
    return OMPI_SUCCESS;
}

/*
 * If we haven't done so already, receive the modex info for the
 * specified ompi_proc.  Search that proc's modex info; if we can find
 * matching address info, then create an endpoint.
 *
 * If we don't find a match, it's not an error: just return "not
 * found".
 *
 * There is a one-to-one correspondence between a ompi_proc_t and a
 * ompi_btl_usnic_proc_t instance.  We cache additional data on the
 * ompi_btl_usnic_proc_t: specifically, the list of
 * ompi_btl_usnic_endpoint_t instances, and published addresses/modex
 * info.
 */
int ompi_btl_usnic_proc_match(ompi_proc_t *ompi_proc,
                              ompi_btl_usnic_module_t *module,
                              ompi_btl_usnic_proc_t **proc)
{
    /* Check if we have already created a proc structure for this peer
       ompi process */
    *proc = ompi_btl_usnic_proc_lookup_ompi(ompi_proc);
    if (*proc != NULL) {
        OBJ_RETAIN(*proc);
    } else {
        /* If not, go make one */
        *proc = create_proc(ompi_proc);
        if (NULL == *proc) {
            return OMPI_ERR_NOT_FOUND;
        }
    }
    return OMPI_SUCCESS;
}
