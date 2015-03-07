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
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <netinet/in.h>

#include "opal_config.h"

#include "opal_stdint.h"
#include "opal/util/arch.h"
#include "opal/util/show_help.h"
#include "opal/constants.h"

#include "btl_usnic_compat.h"
#include "btl_usnic.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_util.h"
#include "btl_usnic_graph.h"

/* larger weight values are more desirable (i.e., worth, not cost) */
enum {
    WEIGHT_UNREACHABLE = -1
};

/* Helper macros for "match_modex" and friends for translating between array
 * indices and vertex IDs.  Module vertices always come first in the graph,
 * followed by proc (endpoint) vertices. */
#define PROC_VERTEX(modex_idx) (mca_btl_usnic_component.num_modules + modex_idx)
#define MODULE_VERTEX(module_idx) (module_idx)
#define PROC_INDEX(proc_vertex) ((proc_vertex) - mca_btl_usnic_component.num_modules)
#define MODULE_INDEX(module_vertex) (module_vertex)

static void proc_construct(opal_btl_usnic_proc_t* proc)
{
    proc->proc_opal = 0;
    proc->proc_modex = NULL;
    proc->proc_modex_count = 0;
    proc->proc_modex_claimed = NULL;
    proc->proc_endpoints = NULL;
    proc->proc_endpoint_count = 0;
    proc->proc_ep_match_table = NULL;
    proc->proc_match_exists = false;

    /* add to list of all proc instance */
    opal_list_append(&mca_btl_usnic_component.usnic_procs, &proc->super);
}


static void proc_destruct(opal_btl_usnic_proc_t* proc)
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

    if (NULL != proc->proc_ep_match_table) {
        free(proc->proc_ep_match_table);
        proc->proc_ep_match_table = NULL;
    }

    /* Release all endpoints associated with this proc */
    if (NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
        proc->proc_endpoints = NULL;
    }
}


OBJ_CLASS_INSTANCE(opal_btl_usnic_proc_t,
                   opal_list_item_t,
                   proc_construct,
                   proc_destruct);

/*
 * Look for an existing usnic process instance based on the
 * associated opal_proc_t instance.
 */
opal_btl_usnic_proc_t *
opal_btl_usnic_proc_lookup_ompi(opal_proc_t* opal_proc)
{
    opal_btl_usnic_proc_t* usnic_proc;

    for (usnic_proc = (opal_btl_usnic_proc_t*)
             opal_list_get_first(&mca_btl_usnic_component.usnic_procs);
         usnic_proc != (opal_btl_usnic_proc_t*)
             opal_list_get_end(&mca_btl_usnic_component.usnic_procs);
         usnic_proc  = (opal_btl_usnic_proc_t*)
             opal_list_get_next(usnic_proc)) {
        if (usnic_proc->proc_opal == opal_proc) {
            return usnic_proc;
        }
    }

    return NULL;
}


/*
 * Look for an existing usnic proc based on a hashed RTE process
 * name.
 */
opal_btl_usnic_endpoint_t *
opal_btl_usnic_proc_lookup_endpoint(opal_btl_usnic_module_t *receiver,
                                    uint64_t sender_proc_name)
{
    opal_btl_usnic_proc_t *proc;
    opal_btl_usnic_endpoint_t *endpoint;
    opal_list_item_t *item;

    MSGDEBUG1_OUT("lookup_endpoint: recvmodule=%p sendhash=0x%" PRIx64,
                  (void *)receiver, sender_proc_name);

    opal_mutex_lock(&receiver->all_endpoints_lock);
    for (item = opal_list_get_first(&receiver->all_endpoints);
         item != opal_list_get_end(&receiver->all_endpoints);
         item = opal_list_get_next(item)) {
        endpoint = container_of(item, opal_btl_usnic_endpoint_t,
                                endpoint_endpoint_li);
        proc = endpoint->endpoint_proc;
        /* Note that this works today because opal_proc_t->proc_name
           is unique across the universe.  George is potentially
           working to give handles instead of proc names, and then
           have a function pointer to perform comparisons.  This would
           be bad here in the critical path, though... */
        if (usnic_compat_rte_hash_name(&(proc->proc_opal->proc_name)) ==
            sender_proc_name) {
            MSGDEBUG1_OUT("lookup_endpoint: matched endpoint=%p",
                          (void *)endpoint);
            opal_mutex_unlock(&receiver->all_endpoints_lock);
            return endpoint;
        }
    }
    opal_mutex_unlock(&receiver->all_endpoints_lock);

    /* Didn't find it */
    return NULL;
}

/*
 * Create an opal_btl_usnic_proc_t and initialize it with modex info
 * and an empty array of endpoints.
 *
 * Returns OPAL_ERR_UNREACH if we can't reach the peer (i.e., we can't
 * find their modex data).
 */
static int create_proc(opal_proc_t *opal_proc,
                       opal_btl_usnic_proc_t **usnic_proc)
{
    opal_btl_usnic_proc_t *proc = NULL;
    size_t size;
    int rc;

    *usnic_proc = NULL;

    /* Create the proc if it doesn't already exist */
    proc = OBJ_NEW(opal_btl_usnic_proc_t);
    if (NULL == proc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Initialize number of peers */
    proc->proc_endpoint_count = 0;
    proc->proc_opal = opal_proc;

    /* query for the peer address info */
    usnic_compat_modex_recv(&rc, &mca_btl_usnic_component.super.btl_version,
                            opal_proc, &proc->proc_modex, &size);

    /* If this proc simply doesn't have this key, then they're not
       running the usnic BTL -- just ignore them.  Otherwise, show an
       error message. */
    if (OPAL_ERR_NOT_FOUND == rc) {
        OBJ_RELEASE(proc);
        return OPAL_ERR_UNREACH;
    } else if (OPAL_SUCCESS != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       "<none>", "<none>",
                       "opal_modex_recv() failed", __FILE__, __LINE__,
                       opal_strerror(rc));
        OBJ_RELEASE(proc);
        return OPAL_ERROR;
    }

    if ((size % sizeof(opal_btl_usnic_modex_t)) != 0) {
        char msg[1024];

        snprintf(msg, sizeof(msg),
                 "sizeof(modex for peer %s data) == %d, expected multiple of %d",
                 usnic_compat_proc_name_print(&opal_proc->proc_name),
                 (int) size, (int) sizeof(opal_btl_usnic_modex_t));
        opal_show_help("help-mpi-btl-usnic.txt", "internal error during init",
                       true,
                       opal_process_info.nodename,
                       "<none>", 0,
                       "invalid modex data", __FILE__, __LINE__,
                       msg);

        OBJ_RELEASE(proc);
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    /* See if the peer has the same underlying wire protocol as me.
       If not, then print an error and ignore this peer. */
// RFXXX - things are weird when i force this to fail
    if (mca_btl_usnic_component.transport_protocol !=
        proc->proc_modex->protocol) {
        uint64_t proto;
        char protostr[32];
        proto = mca_btl_usnic_component.transport_protocol;
        memset(protostr, 0, sizeof(protostr));
        strncpy(protostr, fi_tostr(&proto, FI_TYPE_PROTOCOL),
                sizeof(protostr) - 1);
        proto = proc->proc_modex->protocol;
        opal_show_help("help-mpi-btl-usnic.txt",
                       "transport mismatch",
                       true,
                       opal_process_info.nodename,
                       protostr,
                       "peer",
                       fi_tostr(&proto, FI_TYPE_PROTOCOL));

        OBJ_RELEASE(proc);
        return OPAL_ERR_UNREACH;
    }

    proc->proc_modex_count = size / sizeof(opal_btl_usnic_modex_t);
    if (0 == proc->proc_modex_count) {
        proc->proc_endpoints = NULL;
        OBJ_RELEASE(proc);
        return OPAL_ERR_UNREACH;
    }

    proc->proc_modex_claimed = (bool*)
        calloc(proc->proc_modex_count, sizeof(bool));
    if (NULL == proc->proc_modex_claimed) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(proc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        calloc(proc->proc_modex_count, sizeof(mca_btl_base_endpoint_t*));
    if (NULL == proc->proc_endpoints) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(proc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    *usnic_proc = proc;
    return OPAL_SUCCESS;
}

/* Compare the addresses of the local interface corresponding to module and the
 * remote interface corresponding to proc_modex_addr.  Returns a weight value
 * (higher values indicate more desirable connections). */
static uint64_t compute_weight(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_modex_t *proc_modex_addr)
{
    char my_ip_string[INET_ADDRSTRLEN], peer_ip_string[INET_ADDRSTRLEN];
    struct sockaddr_in sin;
    struct sockaddr_in *sinp;
    struct fi_usnic_info *uip;
    uint32_t mynet, peernet;
    int err;
    int metric;
    uint32_t min_link_speed_gbps;

    uip = &module->usnic_info;
    sinp = module->fabric_info->src_addr;
    inet_ntop(AF_INET, &sinp->sin_addr,
              my_ip_string, sizeof(my_ip_string));
    inet_ntop(AF_INET, &proc_modex_addr->ipv4_addr,
              peer_ip_string, sizeof(peer_ip_string));

    /* Just compare the CIDR-masked IP address to see if they're on
       the same network.  If so, we're good. */
    mynet = sinp->sin_addr.s_addr & uip->ui.v1.ui_netmask_be;
    peernet = proc_modex_addr->ipv4_addr & proc_modex_addr->netmask;
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic:%s: checking my IP address/subnet (%s/%d) vs. peer (%s/%d): %s",
                        __func__, my_ip_string,
                        usnic_netmask_to_cidrlen(uip->ui.v1.ui_netmask_be),
                        peer_ip_string,
                        usnic_netmask_to_cidrlen(proc_modex_addr->netmask),
                        (mynet == peernet ? "match" : "DO NOT match"));

    min_link_speed_gbps = MIN(module->super.btl_bandwidth,
                              proc_modex_addr->link_speed_mbps) / 1000;

    /* Returned metric is:
     *    0 - same VLAN
     *    1..MAXINT - relative distance metric
     *    -1 - unreachable
     */
    metric = 0;
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = proc_modex_addr->ipv4_addr;
    err = module->usnic_av_ops->get_distance(module->av, &sin, &metric);
    if (0 != err || (0 == err && -1 == metric)) {
        return 0; /* no connectivity */
    }
    else {
        /* Format in binary    MSB                             LSB
         * most sig. 32-bits:  00000000 0000000A BBBBBBBB 00000001
         * least sig. 32-bits: CCCCCCCC CCCCCCCC CCCCCCCC CCCCCCCC
         *
         * A = 1 iff same subnet
         * B = min link speed (in Gbps) between iface pair
         * C = metric from routing table
         *
         * That is, this prioritizes interfaces in the same subnet first,
         * followed by having the same link speed.  The extra literal "1" is in
         * there to help prioritize over any zero-cost links that might
         * otherwise make their way into the graph.  It is not strictly
         * necessary and could be eliminated if the extra byte is needed.
         *
         * TODO add an MCA parameter to optionally swap the offsets of A and
         * B, thereby prioritizing link speed over same subnet reachability.
         */
        /* FIXME how can we check that the metric is the same before we have
         * communication with this host?  Mismatched metrics could cause the
         * remote peer to make a different pairing decision... */
        if (min_link_speed_gbps > 0xff) {
            opal_output_verbose(20, USNIC_OUT, "clamping min_link_speed_gbps=%u to 255",
                                min_link_speed_gbps);
            min_link_speed_gbps = 0xff;
        }
        return ((uint64_t)(mynet == peernet) << 48) |
               ((uint64_t)(min_link_speed_gbps & 0xff) << 40) |
               ((uint64_t)0x1 << 32) |
               (/*metric=*/0);
    }
}

/* Populate the given proc's match table from an array of (u,v) edge pairs.
 *
 * (DJG: this unfortunately knows a bit too much about the internals of
 * "match_modex")
 */
static void edge_pairs_to_match_table(
    opal_btl_usnic_proc_t *proc,
    bool proc_is_left,
    int nme,
    int *me)
{
    int i;
    int left, right;
    int module_idx, proc_idx;
    int num_modules;

    num_modules = (int)mca_btl_usnic_component.num_modules;

    assert(nme >= 0);
    for (i = 0; i < nme; ++i) {
        left  = me[2*i+0];
        right = me[2*i+1];

        if (proc_is_left) {
            proc_idx = PROC_INDEX(left);
            module_idx = MODULE_INDEX(right);
        } else {
            module_idx = MODULE_INDEX(left);
            proc_idx = PROC_INDEX(right);
        }
        assert(module_idx >= 0 && module_idx < num_modules);
        assert(proc_idx >= 0 && proc_idx < (int)proc->proc_modex_count);
        proc->proc_ep_match_table[module_idx] = proc_idx;
        proc->proc_match_exists = true;
    }

    /* emit match summary for debugging purposes */
    for (i = 0; i < num_modules; ++i) {
        if (-1 != proc->proc_ep_match_table[i]) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic:%s: module[%d] (%p) should claim endpoint[%d] on proc %p",
                                __func__, i,
                                (void *)mca_btl_usnic_component.usnic_active_modules[i],
                                proc->proc_ep_match_table[i], (void *)proc);
        } else {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic:%s: module[%d] (%p) will NOT claim an endpoint on proc %p",
                                __func__, i,
                                (void *)mca_btl_usnic_component.usnic_active_modules[i],
                                (void *)proc);
        }
    }
}

/**
 * Constructs an interface graph from all local modules and the given proc's
 * remote interfaces.  The resulting vertices will always have the module
 * vertices appear before the proc vertices.
 */
static int create_proc_module_graph(
    opal_btl_usnic_proc_t *proc,
    bool proc_is_left,
    opal_btl_usnic_graph_t **g_out)
{
    int err;
    int i, j;
    int u, v;
    int num_modules;
    opal_btl_usnic_graph_t *g = NULL;

    if (NULL == g_out) {
        return OPAL_ERR_BAD_PARAM;
    }
    *g_out = NULL;

    num_modules = (int)mca_btl_usnic_component.num_modules;

    /* Construct a bipartite graph with remote interfaces on the one side and
     * local interfaces (modules) on the other. */
    err = opal_btl_usnic_gr_create(NULL, NULL, &g);
    if (OPAL_SUCCESS != err) {
        OPAL_ERROR_LOG(err);
        goto out;
    }

    /* create vertices for each interface (local and remote) */
    for (i = 0; i < num_modules; ++i) {
        int idx = -1;
        err = opal_btl_usnic_gr_add_vertex(g,
                                           mca_btl_usnic_component.usnic_active_modules[i],
                                           &idx);
        if (OPAL_SUCCESS != err) {
            OPAL_ERROR_LOG(err);
            goto out_free_graph;
        }
        assert(idx == MODULE_VERTEX(i));
    }
    for (i = 0; i < (int)proc->proc_modex_count; ++i) {
        int idx = -1;
        err = opal_btl_usnic_gr_add_vertex(g, &proc->proc_modex[i], &idx);
        if (OPAL_SUCCESS != err) {
            OPAL_ERROR_LOG(err);
            goto out_free_graph;
        }
        assert(idx == (int)PROC_VERTEX(i));
    }

    /* now add edges between interfaces that can communicate */
    for (i = 0; i < num_modules; ++i) {
        for (j = 0; j < (int)proc->proc_modex_count; ++j) {
            int64_t weight, cost;

            /* assumption: compute_weight returns the same weight on the
             * remote process with these arguments (effectively) transposed */
            weight = compute_weight(mca_btl_usnic_component.usnic_active_modules[i],
                                    &proc->proc_modex[j]);

            opal_output_verbose(20, USNIC_OUT,
                                "btl:usnic:%s: weight=0x%016" PRIx64 " for edge module[%d] (%p) <--> endpoint[%d] on proc %p",
                                __func__,
                                weight, i,
                                (void *)mca_btl_usnic_component.usnic_active_modules[i],
                                j, (void *)proc);

            if (WEIGHT_UNREACHABLE == weight) {
                continue;
            } else {
                /* the graph code optimizes for minimum *cost*, but we have
                 * been computing weights (negative costs) */
                cost = -weight;
            }
            assert(INT64_MAX != cost);
            assert(INT64_MIN != cost);

            if (proc_is_left) {
                u = PROC_VERTEX(j);
                v = MODULE_VERTEX(i);
            } else {
                u = MODULE_VERTEX(i);
                v = PROC_VERTEX(j);
            }
            opal_output_verbose(20, USNIC_OUT,
                                "btl:usnic:%s: adding edge (%d,%d) with cost=%" PRIi64 " for edge module[%d] <--> endpoint[%d]",
                                __func__, u, v, cost, i, j);
            err = opal_btl_usnic_gr_add_edge(g, u, v, cost,
                                             /*capacity=*/1,
                                             /*e_data=*/NULL);
            if (OPAL_SUCCESS != err) {
                OPAL_ERROR_LOG(err);
                goto out_free_graph;
            }
        }
    }

    *g_out = g;
    return OPAL_SUCCESS;

out_free_graph:
    opal_btl_usnic_gr_free(g);
out:
    return err;
}

/*
 * For a specific module, see if this proc has matching address/modex
 * info.  If so, create an endpoint and return it.
 *
 * Implementation note: This code relies on the order of modules on a local
 * side matching the order of the modex entries that we send around, otherwise
 * both sides may not agree on a bidirectional connection.  It also assumes
 * that add_procs will be invoked on the local modules in that same order, for
 * the same reason.  If those assumptions do not hold, we will need to
 * canonicalize this match ordering somehow, probably by (jobid,vpid) pair or
 * by the interface MAC or IP address.
 */
static int match_modex(opal_btl_usnic_module_t *module,
                       opal_btl_usnic_proc_t *proc,
                       int *index_out)
{
    int err = OPAL_SUCCESS;
    size_t i;
    uint32_t num_modules;
    opal_btl_usnic_graph_t *g = NULL;
    bool proc_is_left;

    if (NULL == index_out) {
        return OPAL_ERR_BAD_PARAM;
    }
    *index_out = -1;

    num_modules = mca_btl_usnic_component.num_modules;

    opal_output_verbose(20, USNIC_OUT, "btl:usnic:%s: module=%p proc=%p with dimensions %d x %d",
                        __func__, (void *)module, (void *)proc,
                        num_modules, (int)proc->proc_modex_count);

    /* We compute an interface match-up table once for each (module,proc) pair
     * and cache it in the proc.  Store per-proc instead of per-module, since
     * MPI dynamic process routines can add procs but not new modules. */
    if (NULL == proc->proc_ep_match_table) {
        proc->proc_ep_match_table = malloc(num_modules *
                                       sizeof(*proc->proc_ep_match_table));
        if (NULL == proc->proc_ep_match_table) {
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* initialize to "no matches" */
        for (i = 0; i < num_modules; ++i) {
            proc->proc_ep_match_table[i] = -1;
        }

        /* For graphs where all edges are equal (and even for some other
         * graphs), two peers making matching calculations with "mirror image"
         * graphs might not end up with the same matching.  Ensure that both
         * sides are always setting up the exact same graph by always putting
         * the process with the lower (jobid,vpid) on the "left".
         */
#if 0
        proc_is_left = (proc->proc_opal->proc_name <
                        opal_proc_local_get()->proc_name);
#else
        proc_is_left =
            usnic_compat_proc_name_compare(proc->proc_opal->proc_name,
                                           opal_proc_local_get()->proc_name);
#endif

        err = create_proc_module_graph(proc, proc_is_left, &g);
        if (OPAL_SUCCESS != err) {
            goto out_free_table;
        }

        int nme = 0;
        int *me = NULL;
        err = opal_btl_usnic_solve_bipartite_assignment(g, &nme, &me);
        if (OPAL_SUCCESS != err) {
            OPAL_ERROR_LOG(err);
            goto out_free_graph;
        }

        edge_pairs_to_match_table(proc, proc_is_left, nme, me);
        free(me);

        err = opal_btl_usnic_gr_free(g);
        if (OPAL_SUCCESS != err) {
            OPAL_ERROR_LOG(err);
            return err;
        }
    }


    if (!proc->proc_match_exists) {
        opal_output_verbose(5, USNIC_OUT, "btl:usnic:%s: unable to find any valid interface pairs for proc %s",
                            __func__,
                            usnic_compat_proc_name_print(&proc->proc_opal->proc_name));
        return OPAL_ERR_NOT_FOUND;
    }

    /* assuming no strange failure cases, this should always be present */
    if (NULL != proc->proc_ep_match_table && proc->proc_match_exists) {
        for (i = 0; i < num_modules; ++i) {
            if (module == mca_btl_usnic_component.usnic_active_modules[i]) {
                *index_out = proc->proc_ep_match_table[i];
                break;
            }
        }
    }

    /* If MTU does not match, throw an error */
    /* TODO with UDP, do we still want to enforce this restriction or just take
     * the min of the two MTUs?  Another choice is to disqualify this pairing
     * before running the matching algorithm on it. */
    if (*index_out >= 0 &&
        proc->proc_modex[*index_out].max_msg_size !=
        (uint16_t) module->fabric_info->ep_attr->max_msg_size) {
        opal_show_help("help-mpi-btl-usnic.txt", "MTU mismatch",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       module->fabric_info->ep_attr->max_msg_size,
                       (NULL == proc->proc_opal->proc_hostname) ?
                       "unknown" : proc->proc_opal->proc_hostname,
                       proc->proc_modex[*index_out].max_msg_size);
        *index_out = -1;
        return OPAL_ERR_UNREACH;
    }

    return (*index_out == -1 ? OPAL_ERR_NOT_FOUND : OPAL_SUCCESS);

out_free_graph:
    opal_btl_usnic_gr_free(g);
out_free_table:
    free(proc->proc_ep_match_table);
    proc->proc_ep_match_table = NULL;
    proc->proc_match_exists = false;
    return err;
}

/*
 * Initiate the process to create a USD dest.
 * It will be polled for completion later.
 */
static int start_av_insert(opal_btl_usnic_module_t *module,
                                  opal_btl_usnic_endpoint_t *endpoint,
                                  int channel)
{
    int ret;
    opal_btl_usnic_modex_t *modex = &endpoint->endpoint_remote_modex;
    opal_btl_usnic_addr_context_t *context;
    struct sockaddr_in sin;

    context = calloc(1, sizeof(*context));
    context->endpoint = endpoint;
    context->channel_id = channel;

    char str[IPV4STRADDRLEN];
    opal_btl_usnic_snprintf_ipv4_addr(str, sizeof(str), modex->ipv4_addr,
                                      modex->netmask);
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic:start_av_insert: to channel %d at %s:%d",
                        channel, str, modex->ports[channel]);

    /* build remote address */
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(modex->ports[channel]);
    sin.sin_addr.s_addr = modex->ipv4_addr;

    ret = fi_av_insert(module->av, &sin, 1,
            &endpoint->endpoint_remote_addrs[channel], 0, context);
    /* Did an error occur? */
    if (1 != ret) {
        opal_show_help("help-mpi-btl-usnic.txt", "libfabric API failed",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_av_insert()", __FILE__, __LINE__,
                       ret,
                       "Failed to initiate AV insert");
        free(context);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/*
 * Create an endpoint and claim the matched modex slot
 */
int
opal_btl_usnic_create_endpoint(opal_btl_usnic_module_t *module,
                opal_btl_usnic_proc_t *proc,
                opal_btl_usnic_endpoint_t **endpoint_o)
{
    int rc;
    int modex_index;
    opal_btl_usnic_endpoint_t *endpoint;

    /* look for matching modex info */
    rc = match_modex(module, proc, &modex_index);
    if (OPAL_SUCCESS != rc) {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic:create_endpoint: did not match usnic modex info for peer %s",
                            usnic_compat_proc_name_print(&proc->proc_opal->proc_name));
        return rc;
    }

    endpoint = OBJ_NEW(opal_btl_usnic_endpoint_t);
    if (NULL == endpoint) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Initalize the endpoint */
    endpoint->endpoint_module = module;
    assert(modex_index >= 0 && modex_index < (int)proc->proc_modex_count);
    endpoint->endpoint_remote_modex = proc->proc_modex[modex_index];

    /* Start creating destinations; one for each channel.  These
       progress in the background.a */
    for (int i = 0; i < USNIC_NUM_CHANNELS; ++i)  {
        rc = start_av_insert(module, endpoint, i);
        if (OPAL_SUCCESS != rc) {
            OBJ_RELEASE(endpoint);
            return rc;
        }
    }

    /* Initialize endpoint sequence number info */
    endpoint->endpoint_next_seq_to_send = module->local_modex.isn;
    endpoint->endpoint_ack_seq_rcvd = endpoint->endpoint_next_seq_to_send - 1;
    endpoint->endpoint_next_contig_seq_to_recv =
        endpoint->endpoint_remote_modex.isn;
    endpoint->endpoint_highest_seq_rcvd =
        endpoint->endpoint_next_contig_seq_to_recv - 1;
    endpoint->endpoint_rfstart = WINDOW_SIZE_MOD(endpoint->endpoint_next_contig_seq_to_recv);

    /* Now claim that modex slot */
    proc->proc_modex_claimed[modex_index] = true;
    MSGDEBUG1_OUT("create_endpoint: module=%p claimed endpoint=%p on proc=%p (hash=0x%" PRIx64 ")\n",
                  (void *)module, (void *)endpoint, (void *)proc,
                  proc->proc_opal->proc_name);

    /* Save the endpoint on this proc's array of endpoints */
    proc->proc_endpoints[proc->proc_endpoint_count] = endpoint;
    endpoint->endpoint_proc_index = proc->proc_endpoint_count;
    endpoint->endpoint_proc = proc;
    ++proc->proc_endpoint_count;
    OBJ_RETAIN(proc);

    /* also add endpoint to module's list of endpoints (done here and
       not in the endpoint constructor because we aren't able to pass
       the module as a constructor argument -- doh!). */
    opal_mutex_lock(&module->all_endpoints_lock);
    opal_list_append(&(module->all_endpoints),
            &(endpoint->endpoint_endpoint_li));
    endpoint->endpoint_on_all_endpoints = true;
    opal_mutex_unlock(&module->all_endpoints_lock);

    *endpoint_o = endpoint;
    return OPAL_SUCCESS;
}

/*
 * If we haven't done so already, receive the modex info for the
 * specified opal_proc.  Search that proc's modex info; if we can find
 * matching address info, then create an endpoint.
 *
 * If we don't find a match, it's not an error: just return "not
 * found".
 *
 * This routine transfers ownership of an object reference to the caller, who
 * is eventually responsible for transferring or releasing that reference.
 *
 * There is a one-to-one correspondence between a opal_proc_t and a
 * opal_btl_usnic_proc_t instance.  We cache additional data on the
 * opal_btl_usnic_proc_t: specifically, the list of
 * opal_btl_usnic_endpoint_t instances, and published addresses/modex
 * info.
 */
int opal_btl_usnic_proc_match(opal_proc_t *opal_proc,
                              opal_btl_usnic_module_t *module,
                              opal_btl_usnic_proc_t **proc)
{
    /* Check if we have already created a proc structure for this peer
       ompi process */
    *proc = opal_btl_usnic_proc_lookup_ompi(opal_proc);
    if (*proc != NULL) {
        OBJ_RETAIN(*proc);
        return OPAL_SUCCESS;
    } else {
        /* If not, go make one */
        return create_proc(opal_proc, proc);
    }
}
