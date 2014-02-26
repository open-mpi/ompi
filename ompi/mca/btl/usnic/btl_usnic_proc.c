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
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
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

/* larger weight values are more desirable (i.e., worth, not cost) */
enum conn_weight {
    WEIGHT_UNREACHABLE = -1,
    WEIGHT_DIFF_NET = 0,
    WEIGHT_SAME_NET = 1
};

static void proc_construct(ompi_btl_usnic_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_modex = NULL;
    proc->proc_modex_count = 0;
    proc->proc_modex_claimed = NULL;
    proc->proc_endpoints = NULL;
    proc->proc_endpoint_count = 0;
    proc->proc_ep_weights = NULL;

    /* add to list of all proc instance */
    opal_list_append(&mca_btl_usnic_component.usnic_procs, &proc->super);
}


static void proc_destruct(ompi_btl_usnic_proc_t* proc)
{
    uint32_t i;

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

    if (NULL != proc->proc_ep_weights) {
        for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
            free(proc->proc_ep_weights[i]);
            proc->proc_ep_weights[i] = NULL;
        }
        free(proc->proc_ep_weights);
        proc->proc_ep_weights = NULL;
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
    ompi_btl_usnic_proc_t *proc;
    ompi_btl_usnic_endpoint_t *endpoint;
    opal_list_item_t *item;

    MSGDEBUG1_OUT("lookup_endpoint: recvmodule=%p sendhash=0x%" PRIx64,
                  (void *)receiver, sender_hashed_rte_name);

    for (item = opal_list_get_first(&receiver->all_endpoints);
         item != opal_list_get_end(&receiver->all_endpoints);
         item = opal_list_get_next(item)) {
        endpoint = container_of(item, ompi_btl_usnic_endpoint_t,
                                endpoint_endpoint_li);
        proc = endpoint->endpoint_proc;
        if (ompi_rte_hash_name(&proc->proc_ompi->proc_name) ==
            sender_hashed_rte_name) {
            MSGDEBUG1_OUT("lookup_endpoint: matched endpoint=%p",
                          (void *)endpoint);
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

/* Compare the addresses of the local interface corresponding to module and the
 * remote interface corresponding to proc_modex_addr.  Returns a weight value. */
static enum conn_weight compute_weight(
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_addr_t *proc_modex_addr)
{
    char my_ip_string[INET_ADDRSTRLEN], peer_ip_string[INET_ADDRSTRLEN];
    uint32_t mynet, peernet;

    inet_ntop(AF_INET, &module->if_ipv4_addr,
              my_ip_string, sizeof(my_ip_string));
    inet_ntop(AF_INET, &proc_modex_addr->ipv4_addr,
              peer_ip_string, sizeof(peer_ip_string));

    /* Just compare the CIDR-masked IP address to see if they're on
       the same network.  If so, we're good. */
    mynet   = ompi_btl_usnic_get_ipv4_subnet(module->if_ipv4_addr,
                                             module->if_cidrmask);
    peernet = ompi_btl_usnic_get_ipv4_subnet(proc_modex_addr->ipv4_addr,
                                             proc_modex_addr->cidrmask);

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic:%s: checking my IP address/subnet (%s/%d) vs. peer (%s/%d): %s",
                        __func__, my_ip_string, module->if_cidrmask,
                        peer_ip_string, proc_modex_addr->cidrmask,
                        (mynet == peernet ? "match" : "DO NOT match"));

    if (mynet == peernet) {
        return WEIGHT_SAME_NET;
    } else {
        return WEIGHT_DIFF_NET;
    }
}

/*
 * For a specific module, see if this proc has matching address/modex
 * info.  If so, create an endpoint and return it.
 */
static int match_modex(ompi_btl_usnic_module_t *module,
                       ompi_btl_usnic_proc_t *proc)
{
    size_t i, j;
    int8_t **weights;
    uint32_t num_modules;
    int modex_index = -1;

    num_modules = mca_btl_usnic_component.num_modules;

    opal_output_verbose(20, USNIC_OUT, "btl:usnic:%s: module=%p proc=%p with dimensions %d x %d",
                        __func__, (void *)module, (void *)proc,
                        num_modules, (int)proc->proc_modex_count);

    /* We compute an interface match-up weights table once for each
     * (module,proc) pair and cache it in the proc.  Store per-proc instead of
     * per-module, since MPI dynamic process routines can add procs but not new
     * modules. */
    if (NULL == proc->proc_ep_weights) {
        proc->proc_ep_weights = malloc(num_modules *
                                       sizeof(*proc->proc_ep_weights));
        if (NULL == proc->proc_ep_weights) {
            OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
            return -1;
        }
        proc->proc_ep_max_weight = WEIGHT_UNREACHABLE;

        weights = proc->proc_ep_weights;

        for (i = 0; i < num_modules; ++i) {
            weights[i] = malloc(proc->proc_modex_count * sizeof(*weights[i]));
            if (NULL == weights[i]) {
                OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);

                /* free everything allocated so far */
                for (j = 0; j < i; ++j) {
                    free(proc->proc_ep_weights[j]);
                }
                free(proc->proc_ep_weights);
                return -1;
            }

            /* compute all weights */
            for (j = 0; j < proc->proc_modex_count; ++j) {
                weights[i][j] = compute_weight(mca_btl_usnic_component.usnic_active_modules[i],
                                               &proc->proc_modex[j]);
                if (!mca_btl_usnic_component.use_udp &&
                    WEIGHT_DIFF_NET == weights[i][j]) {
                    /* UDP is required for routability */
                    weights[i][j] = WEIGHT_UNREACHABLE;
                }
                if (weights[i][j] > proc->proc_ep_max_weight) {
                    proc->proc_ep_max_weight = weights[i][j];
                }
            }
        }
    }

    if (WEIGHT_UNREACHABLE == proc->proc_ep_max_weight) {
        opal_output_verbose(5, USNIC_OUT, "btl:usnic:%s: unable to find any valid interface pairs for proc %s",
                            __func__, OMPI_NAME_PRINT(&proc->proc_ompi->proc_name));
        return -1;
    }

    weights = proc->proc_ep_weights;

    /* Each module can claim an address in the proc's modex info that no other
     * local module is using.  Take the first maximal interface pairing where
     * the remote interface is not yet claimed.  If unclaimed remote interfaces
     * remain but their pairings are non-maximal, they will not be used.
     *
     * This code relies on the order of modules on a local side matching the
     * order of the modex entries that we send around, otherwise both sides may
     * not agree on a bidirectional connection.  It also assumes that add_procs
     * will be invoked on the local modules in that same order, for the same
     * reason.  If those assumptions do not hold, we will need to canonicalize
     * this match ordering somehow, probably by (jobid,vpid) pair or by the
     * interface MAC or IP address. */
    for (i = 0; i < num_modules; ++i) {
        if (mca_btl_usnic_component.usnic_active_modules[i] == module) {
            for (j = 0; j < proc->proc_modex_count; ++j) {
                if (!proc->proc_modex_claimed[j] &&
                    weights[i][j] == proc->proc_ep_max_weight) {
                    opal_output_verbose(5, USNIC_OUT,
                                        "module[%d] (%p) claiming endpoint[%d] on proc %p",
                                        (int)i, (void *)module, (int)j,
                                        (void *)proc);
                    modex_index = j;
                    break;
                }
            }
        }
    }

    /* If MTU does not match, throw an error */
    if (modex_index >= 0 &&
        proc->proc_modex[modex_index].mtu != module->if_mtu) {
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

    return modex_index;
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
    MSGDEBUG1_OUT("create_endpoint: module=%p claimed endpoint=%p on proc=%p (hash=0x%" PRIx64 ")\n",
                  (void *)module, (void *)endpoint, (void *)proc,
                  ompi_rte_hash_name(&proc->proc_ompi->proc_name));

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
 * This routine transfers ownership of an object reference to the caller, who
 * is eventually responsible for transferring or releasing that reference.
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
