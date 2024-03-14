/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2010 Oracle and/or its affiliates.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018-2019 Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif

#include "opal/class/opal_hash_table.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/reachable/base/base.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/util/bipartite_graph.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"

#include "btl_tcp.h"
#include "btl_tcp_proc.h"

static void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t *proc);
static void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t *proc);

OBJ_CLASS_INSTANCE(mca_btl_tcp_proc_t, opal_list_item_t, mca_btl_tcp_proc_construct,
                   mca_btl_tcp_proc_destruct);

void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t *tcp_proc)
{
    tcp_proc->proc_opal = NULL;
    tcp_proc->proc_addrs = NULL;
    tcp_proc->proc_addr_count = 0;
    tcp_proc->proc_endpoints = NULL;
    tcp_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&tcp_proc->proc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&tcp_proc->btl_index_to_endpoint, opal_hash_table_t);
    opal_hash_table_init(&tcp_proc->btl_index_to_endpoint, mca_btl_tcp_component.tcp_num_btls);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t *tcp_proc)
{
    if (NULL != tcp_proc->proc_opal) {
        /* remove from list of all proc instances */
        OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
        opal_proc_table_remove_value(&mca_btl_tcp_component.tcp_procs,
                                     tcp_proc->proc_opal->proc_name);
        OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
        OBJ_RELEASE(tcp_proc->proc_opal);
        tcp_proc->proc_opal = NULL;
    }
    /* release resources */
    if (NULL != tcp_proc->proc_endpoints) {
        free(tcp_proc->proc_endpoints);
    }
    if (NULL != tcp_proc->proc_addrs) {
        free(tcp_proc->proc_addrs);
    }
    OBJ_DESTRUCT(&tcp_proc->btl_index_to_endpoint);
    OBJ_DESTRUCT(&tcp_proc->proc_lock);
}

static inline int mca_btl_tcp_proc_is_proc_left(opal_process_name_t a, opal_process_name_t b)
{
    if (a.jobid != b.jobid) {
        return (a.jobid < b.jobid);
    } else {
        return (a.vpid < b.vpid);
    }
}

#define MCA_BTL_TCP_PROC_LOCAL_VERTEX(index)  (index)
#define MCA_BTL_TCP_PROC_REMOTE_VERTEX(index) (index + mca_btl_tcp_component.tcp_num_btls)

/* This function builds a graph to match local and remote interfaces
 * together. It also populates the remote proc object.
 *
 * @param btl_proc (IN)           Remote proc information
 * @param remote_addrs (IN)       List of addresses from remote interfaces
 * @param local_proc_is_left (IN) Boolean indicator. If true, we set local process
 *                                interfaces to be on the left side of the graph.
 *                                If false, we set remote process interfaces to
 *                                be on the left side of the graph.
 * @param graph_out (OUT)         Constructed and populated bipartite interface
 *                                graph with vertices as interfaces and negative
 *                                reachability weights as costs for the edges.
 * @return                        OPAL error code or success
 *
 * The vertices of this graph are the local and remote interfaces. Edges in
 * this graph are connections between the interfaces. Costs are computed as
 * negative weight which is calculated using the reachability framework.
 *
 * In order to mirror inputs on both the local and remote side when solving
 * interface matching from both sides, we require local_proc_is_left to
 * indicate whether the local interfaces should be on the left of the graph
 * or not.
 *
 * The remote list and proc_addrs are assembled and populated here so that
 * we can ensure that the vertex ordering matches the proc_addr ordering.
 * This allows us to pass the correct pointers to the vertex data for storage.
 *
 */
static int mca_btl_tcp_proc_create_interface_graph(mca_btl_tcp_proc_t *btl_proc,
                                                   mca_btl_tcp_modex_addr_t *remote_addrs,
                                                   int local_proc_is_left,
                                                   opal_bp_graph_t **graph_out)
{
    opal_bp_graph_t *graph = NULL;
    opal_reachable_t *results = NULL;
    opal_list_t *local_list = &mca_btl_tcp_component.local_ifs;
    opal_list_t *remote_list;
    int rc, v_index, x, y, cost, u, v, num_edges = 0;
    size_t i;

    remote_list = OBJ_NEW(opal_list_t);
    if (NULL == remote_list) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    /* the modex and proc structures differ slightly, so copy the
       fields needed in the proc version */
    for (i = 0; i < btl_proc->proc_addr_count; i++) {
        /* Construct opal_if_t objects for the remote interfaces */
        opal_if_t *interface = OBJ_NEW(opal_if_t);
        if (NULL == interface) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            goto out;
        }

        if (MCA_BTL_TCP_AF_INET == remote_addrs[i].addr_family) {
            memcpy(&btl_proc->proc_addrs[i].addr_union.addr_inet, remote_addrs[i].addr,
                   sizeof(struct in_addr));
            btl_proc->proc_addrs[i].addr_family = AF_INET;

            memcpy(&((struct sockaddr_in *) &(interface->if_addr))->sin_addr, remote_addrs[i].addr,
                   sizeof(struct in_addr));
            ((struct sockaddr *) &(interface->if_addr))->sa_family = AF_INET;
            interface->af_family = AF_INET;
        } else if (MCA_BTL_TCP_AF_INET6 == remote_addrs[i].addr_family) {
#if OPAL_ENABLE_IPV6
            memcpy(&btl_proc->proc_addrs[i].addr_union.addr_inet6, remote_addrs[i].addr,
                   sizeof(struct in6_addr));
            btl_proc->proc_addrs[i].addr_family = AF_INET6;

            memcpy(&((struct sockaddr_in6 *) &(interface->if_addr))->sin6_addr,
                   remote_addrs[i].addr, sizeof(struct in6_addr));
            ((struct sockaddr *) &(interface->if_addr))->sa_family = AF_INET6;
            interface->af_family = AF_INET6;
#else
            rc = OPAL_ERR_NOT_SUPPORTED;
            OBJ_RELEASE(interface);
            goto out;
#endif
        } else {
            BTL_ERROR(("Unexpected address family %d", (int) remote_addrs[i].addr_family));
            rc = OPAL_ERR_BAD_PARAM;
            OBJ_RELEASE(interface);
            goto out;
        }

        btl_proc->proc_addrs[i].addr_port = remote_addrs[i].addr_port;
        btl_proc->proc_addrs[i].addr_ifkindex = remote_addrs[i].addr_ifkindex;

        interface->if_mask = remote_addrs[i].addr_mask;
        interface->if_bandwidth = remote_addrs[i].addr_bandwidth;

        opal_list_append(remote_list, &(interface->super));
    }

    rc = opal_bp_graph_create(NULL, NULL, &graph);
    if (OPAL_SUCCESS != rc) {
        goto out;
    }
    results = opal_reachable.reachable(local_list, remote_list);
    if (NULL == results) {
        rc = OPAL_ERROR;
        goto err_graph;
    }

    /* Add vertices for each local node. These will store the btl index */
    for (x = 0; x < results->num_local; x++) {
        rc = opal_bp_graph_add_vertex(graph, &mca_btl_tcp_component.tcp_btls[x]->btl_index,
                                      &v_index);
        if (OPAL_SUCCESS != rc) {
            goto err_graph;
        }
    }

    /* Add vertices for each remote node. These will store remote interface information */
    for (y = 0; y < results->num_remote; y++) {
        rc = opal_bp_graph_add_vertex(graph, &btl_proc->proc_addrs[y], &v_index);
        if (OPAL_SUCCESS != rc) {
            goto err_graph;
        }
    }

    /* Add edges */
    for (x = 0; x < results->num_local; x++) {
        for (y = 0; y < results->num_remote; y++) {
            /* The bipartite assignment solver will optimize a graph for
             * least cost. Since weights vary from 0 as no connection and
             * higher weights as better connections (multiplied by some other
             * factors), higher weight is better. Thus, to achieve least cost,
             * we set cost as negative weight.
             */
            cost = -results->weights[x][y];
            /* Skip edges with no connections */
            if (0 == cost) {
                continue;
            }
            if (local_proc_is_left) {
                u = MCA_BTL_TCP_PROC_LOCAL_VERTEX(x);
                v = MCA_BTL_TCP_PROC_REMOTE_VERTEX(y);
            } else {
                u = MCA_BTL_TCP_PROC_REMOTE_VERTEX(y);
                v = MCA_BTL_TCP_PROC_LOCAL_VERTEX(x);
            }
            rc = opal_bp_graph_add_edge(graph, u, v, cost, 1, NULL);
            if (OPAL_SUCCESS != rc) {
                goto err_graph;
            }
            num_edges++;
        }
    }

    if (0 == num_edges) {
        BTL_ERROR(("Unable to find reachable pairing between local and remote interfaces"));
        rc = OPAL_ERR_UNREACH;
    }

    *graph_out = graph;
    goto out;

err_graph:
    if (NULL != graph) {
        opal_bp_graph_free(graph);
    }
out:
    if (NULL != results) {
        free(results);
    }
    if (NULL != remote_list) {
        OBJ_RELEASE(remote_list);
    }
    return rc;
}

/* We store the matched interface data by using the btl_index as the key and
 * a pointer to a mca_btl_tcp_addr_t struct.
 */
static int mca_btl_tcp_proc_store_matched_interfaces(mca_btl_tcp_proc_t *btl_proc,
                                                     int local_proc_is_left, opal_bp_graph_t *graph,
                                                     int num_matched, int *matched_edges)
{
    int rc = OPAL_SUCCESS;
    int i, left, right;
    uint32_t *local_index;
    struct mca_btl_tcp_addr_t *remote_addr;

    for (i = 0; i < num_matched; i++) {
        left = matched_edges[2 * i + 0];
        right = matched_edges[2 * i + 1];
        if (local_proc_is_left) {
            rc = opal_bp_graph_get_vertex_data(graph, left, (void *) &local_index);
            if (OPAL_SUCCESS != rc) {
                goto out;
            }
            rc = opal_bp_graph_get_vertex_data(graph, right, (void *) &remote_addr);
            if (OPAL_SUCCESS != rc) {
                goto out;
            }
        } else {
            rc = opal_bp_graph_get_vertex_data(graph, right, (void *) &local_index);
            if (OPAL_SUCCESS != rc) {
                goto out;
            }
            rc = opal_bp_graph_get_vertex_data(graph, left, (void *) &remote_addr);
            if (OPAL_SUCCESS != rc) {
                goto out;
            }
        }
        opal_hash_table_set_value_uint32(&btl_proc->btl_index_to_endpoint, *local_index,
                                         (void *) remote_addr);
    }
out:
    return rc;
}

static int mca_btl_tcp_proc_handle_modex_addresses(mca_btl_tcp_proc_t *btl_proc,
                                                   mca_btl_tcp_modex_addr_t *remote_addrs,
                                                   int local_proc_is_left)
{
    opal_bp_graph_t *graph = NULL;
    int rc = OPAL_SUCCESS;
    int num_matched = 0;
    int *matched_edges = NULL;

    rc = mca_btl_tcp_proc_create_interface_graph(btl_proc, remote_addrs, local_proc_is_left,
                                                 &graph);
    if (rc) {
        goto cleanup;
    }

    rc = opal_bp_graph_solve_bipartite_assignment(graph, &num_matched, &matched_edges);
    if (rc) {
        goto cleanup;
    }

    rc = mca_btl_tcp_proc_store_matched_interfaces(btl_proc, local_proc_is_left, graph, num_matched,
                                                   matched_edges);

cleanup:
    if (NULL != graph) {
        opal_bp_graph_free(graph);
    }
    free(matched_edges);
    return rc;
}

/*
 * Create a TCP process structure. There is a one-to-one correspondence
 * between a opal_proc_t and a mca_btl_tcp_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_tcp_endpoint_t instances,
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_tcp_proc_t *mca_btl_tcp_proc_create(opal_proc_t *proc)
{
    mca_btl_tcp_proc_t *btl_proc;
    int rc, local_proc_is_left;
    mca_btl_tcp_modex_addr_t *remote_addrs = NULL;
    size_t size;

    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    rc = opal_proc_table_get_value(&mca_btl_tcp_component.tcp_procs, proc->proc_name,
                                   (void **) &btl_proc);
    if (OPAL_SUCCESS == rc) {
        OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
        return btl_proc;
    }

    /* proc was not found, so create one */
    btl_proc = OBJ_NEW(mca_btl_tcp_proc_t);
    if (NULL == btl_proc) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* Retain the proc, but don't store the ref into the btl_proc just yet. This
     * provides a way to release the btl_proc in case of failure without having to
     * unlock the mutex.
     */
    OBJ_RETAIN(proc);

    /* lookup tcp parameters exported by this proc */
    OPAL_MODEX_RECV(rc, &mca_btl_tcp_component.super.btl_version, &proc->proc_name,
                    (uint8_t **) &remote_addrs, &size);
    if (OPAL_SUCCESS != rc) {
        if (OPAL_ERR_NOT_FOUND != rc) {
            BTL_ERROR(("opal_modex_recv: failed with return value=%d", rc));
        }
        goto cleanup;
    }

    if (0 != (size % sizeof(mca_btl_tcp_modex_addr_t))) {
        BTL_ERROR(("opal_modex_recv: invalid size %lu: btl-size: %lu\n", (unsigned long) size,
                   (unsigned long) sizeof(mca_btl_tcp_modex_addr_t)));
        rc = OPAL_ERROR;
        goto cleanup;
    }

    btl_proc->proc_addr_count = size / sizeof(mca_btl_tcp_modex_addr_t);
    btl_proc->proc_addrs = malloc(btl_proc->proc_addr_count * sizeof(mca_btl_tcp_addr_t));
    if (NULL == btl_proc->proc_addrs) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* When solving for bipartite assignment, a graph with equal weights
     * can provide different outputs depending on the input parameters.
     * Thus two processes can construct different interface matchings.
     * To avoid this case, we put the process with the lower jobid on the
     * left or if they are equal, we use the lower vpid on the left.
     *
     * The concept of mirroring the local and remote sides is borrowed
     * from the usnic btl implementation of its bipartite assignment solver.
     */
    local_proc_is_left = mca_btl_tcp_proc_is_proc_left(proc->proc_name,
                                                       opal_proc_local_get()->proc_name);
    rc = mca_btl_tcp_proc_handle_modex_addresses(btl_proc, remote_addrs, local_proc_is_left);

    if (OPAL_SUCCESS != rc) {
        goto cleanup;
    }

    /* allocate space for endpoint array - one for each exported address */
    btl_proc->proc_endpoints = (mca_btl_base_endpoint_t **) malloc(
        (1 + btl_proc->proc_addr_count) * sizeof(mca_btl_base_endpoint_t *));
    if (NULL == btl_proc->proc_endpoints) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

cleanup:
    if (OPAL_SUCCESS == rc) {
        btl_proc->proc_opal = proc; /* link with the proc */
        /* add to hash table of all proc instance. */
        opal_proc_table_set_value(&mca_btl_tcp_component.tcp_procs, proc->proc_name, btl_proc);
    } else {
        if (btl_proc) {
            OBJ_RELEASE(btl_proc); /* release the local proc */
            OBJ_RELEASE(proc);     /* and the ref on the OMPI proc */
            btl_proc = NULL;
        }
    }

    if (NULL != remote_addrs) {
        free(remote_addrs);
    }

    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    return btl_proc;
}

/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign
 * it an address.
 */
int mca_btl_tcp_proc_insert(mca_btl_tcp_proc_t *btl_proc, mca_btl_base_endpoint_t *btl_endpoint)
{
    mca_btl_tcp_module_t *tcp_btl = btl_endpoint->endpoint_btl;
    mca_btl_tcp_addr_t *remote_addr;
    int rc = OPAL_SUCCESS;

    rc = opal_hash_table_get_value_uint32(&btl_proc->btl_index_to_endpoint, tcp_btl->btl_index,
                                          (void **) &remote_addr);
    if (OPAL_SUCCESS != rc) {
        if (9 < opal_output_get_verbosity(opal_btl_base_framework.framework_output)) {
            char *proc_hostname = opal_get_proc_hostname(btl_proc->proc_opal);
            opal_output(0, "btl:tcp: host %s, process %s UNREACHABLE", proc_hostname,
                        OPAL_NAME_PRINT(btl_proc->proc_opal->proc_name));
            free(proc_hostname);
        }
        goto out;
    }
    btl_endpoint->endpoint_addr = remote_addr;

#ifndef WORDS_BIGENDIAN
    /* if we are little endian and our peer is not so lucky, then we
       need to put all information sent to him in big endian (aka
       Network Byte Order) and expect all information received to
       be in NBO.  Since big endian machines always send and receive
       in NBO, we don't care so much about that case. */
    if (btl_proc->proc_opal->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        btl_endpoint->endpoint_nbo = true;
    }
#endif

    /* insert into endpoint array */
    btl_endpoint->endpoint_proc = btl_proc;
    btl_proc->proc_endpoints[btl_proc->proc_endpoint_count++] = btl_endpoint;

out:
    return rc;
}

/*
 * Remove an endpoint from the proc array and indicate the address is
 * no longer in use.
 */

int mca_btl_tcp_proc_remove(mca_btl_tcp_proc_t *btl_proc, mca_btl_base_endpoint_t *btl_endpoint)
{
    size_t i;
    if (NULL != btl_proc) {
        OPAL_THREAD_LOCK(&btl_proc->proc_lock);
        for (i = 0; i < btl_proc->proc_endpoint_count; i++) {
            if (btl_proc->proc_endpoints[i] == btl_endpoint) {
                memmove(btl_proc->proc_endpoints + i, btl_proc->proc_endpoints + i + 1,
                        (btl_proc->proc_endpoint_count - i - 1)
                            * sizeof(mca_btl_base_endpoint_t *));
                if (--btl_proc->proc_endpoint_count == 0) {
                    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
                    OBJ_RELEASE(btl_proc);
                    return OPAL_SUCCESS;
                }
                break;
            }
        }
        OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    }
    return OPAL_SUCCESS;
}

/*
 * Look for an existing TCP process instance based on the globally unique
 * process identifier.
 */
mca_btl_tcp_proc_t *mca_btl_tcp_proc_lookup(const opal_process_name_t *name)
{
    mca_btl_tcp_proc_t *proc = NULL;

    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    opal_proc_table_get_value(&mca_btl_tcp_component.tcp_procs, *name, (void **) &proc);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
    if (OPAL_UNLIKELY(NULL == proc)) {
        mca_btl_base_endpoint_t *endpoint;
        opal_proc_t *opal_proc;

        BTL_VERBOSE(("adding tcp proc for peer {%s}", OPAL_NAME_PRINT(*name)));

        opal_proc = opal_proc_for_name(*name);
        if (NULL == opal_proc) {
            return NULL;
        }

        /* try adding this proc to each btl until */
        for (uint32_t i = 0; i < mca_btl_tcp_component.tcp_num_btls; ++i) {
            endpoint = NULL;
            (void) mca_btl_tcp_add_procs(&mca_btl_tcp_component.tcp_btls[i]->super, 1, &opal_proc,
                                         &endpoint, NULL);
            if (NULL != endpoint && NULL == proc) {
                /* construct all the endpoints and get the proc */
                proc = endpoint->endpoint_proc;
            }
        }
    }

    return proc;
}

/*
 * loop through all available BTLs for one matching the source address
 * of the request.
 */
void mca_btl_tcp_proc_accept(mca_btl_tcp_proc_t *btl_proc, struct sockaddr *addr, int sd)
{
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    int found_match = 0;
    mca_btl_base_endpoint_t *match_btl_endpoint;

    for (size_t i = 0; i < btl_proc->proc_endpoint_count; i++) {
        mca_btl_base_endpoint_t *btl_endpoint = btl_proc->proc_endpoints[i];
        /* We are not here to make a decision about what is good socket
         * and what is not. We simply check that this socket fit the endpoint
         * end we prepare for the real decision function mca_btl_tcp_endpoint_accept. */
        if (btl_endpoint->endpoint_addr->addr_family != addr->sa_family) {
            continue;
        }
        switch (addr->sa_family) {
        case AF_INET:
            if (memcmp(&btl_endpoint->endpoint_addr->addr_union.addr_inet,
                       &(((struct sockaddr_in *) addr)->sin_addr), sizeof(struct in_addr))) {
                char tmp[2][16];
                opal_output_verbose(20, opal_btl_base_framework.framework_output,
                                    "btl: tcp: Match incoming connection from %s %s with locally "
                                    "known IP %s failed (iface %d/%d)!\n",
                                    OPAL_NAME_PRINT(btl_proc->proc_opal->proc_name),
                                    inet_ntop(AF_INET,
                                              (void *) &((struct sockaddr_in *) addr)->sin_addr,
                                              tmp[0], 16),
                                    inet_ntop(AF_INET,
                                              (void *) (struct in_addr *) &btl_endpoint
                                                  ->endpoint_addr->addr_union.addr_inet,
                                              tmp[1], 16),
                                    (int) i, (int) btl_proc->proc_endpoint_count);
                continue;
            } else if (btl_endpoint->endpoint_state != MCA_BTL_TCP_CLOSED) {
                found_match = 1;
                match_btl_endpoint = btl_endpoint;
                continue;
            }
            break;
#if OPAL_ENABLE_IPV6
        case AF_INET6:
            if (memcmp(&btl_endpoint->endpoint_addr->addr_union.addr_inet,
                       &(((struct sockaddr_in6 *) addr)->sin6_addr), sizeof(struct in6_addr))) {
                char tmp[2][INET6_ADDRSTRLEN];
                opal_output_verbose(20, opal_btl_base_framework.framework_output,
                                    "btl: tcp: Match incoming connection from %s %s with locally "
                                    "known IP %s failed (iface %d/%d)!\n",
                                    OPAL_NAME_PRINT(btl_proc->proc_opal->proc_name),
                                    inet_ntop(AF_INET6,
                                              (void *) &((struct sockaddr_in6 *) addr)->sin6_addr,
                                              tmp[0], INET6_ADDRSTRLEN),
                                    inet_ntop(AF_INET6,
                                              (void *) (struct in6_addr *) &btl_endpoint
                                                  ->endpoint_addr->addr_union.addr_inet,
                                              tmp[1], INET6_ADDRSTRLEN),
                                    (int) i, (int) btl_proc->proc_endpoint_count);
                continue;
            } else if (btl_endpoint->endpoint_state != MCA_BTL_TCP_CLOSED) {
                found_match = 1;
                match_btl_endpoint = btl_endpoint;
                continue;
            }
            break;
#endif
        default:;
        }

        /* Set state to CONNECTING to ensure that subsequent connections do not attempt to re-use
         * endpoint in the num_links > 1 case*/
        btl_endpoint->endpoint_state = MCA_BTL_TCP_CONNECTING;
        (void) mca_btl_tcp_endpoint_accept(btl_endpoint, addr, sd);
        OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
        return;
    }
    /* In this case the connection was inbound to an address exported, but was not in a CLOSED
     * state. mca_btl_tcp_endpoint_accept() has logic to deal with the race condition that has
     * likely caused this scenario, so call it here.*/
    if (found_match) {
        (void) mca_btl_tcp_endpoint_accept(match_btl_endpoint, addr, sd);
        OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
        return;
    }
    /* No further use of this socket. Close it */
    CLOSE_THE_SOCKET(sd);
    {
        char *addr_str = NULL, *tmp;
        char ip[128];
        ip[sizeof(ip) - 1] = '\0';

        for (size_t i = 0; i < btl_proc->proc_endpoint_count; i++) {
            mca_btl_base_endpoint_t *btl_endpoint = btl_proc->proc_endpoints[i];
            if (btl_endpoint->endpoint_addr->addr_family != addr->sa_family) {
                continue;
            }
            inet_ntop(btl_endpoint->endpoint_addr->addr_family,
                      (void *) &(btl_endpoint->endpoint_addr->addr_union.addr_inet), ip,
                      sizeof(ip) - 1);
            if (NULL == addr_str) {
                opal_asprintf(&tmp, "\n\t%s", ip);
            } else {
                opal_asprintf(&tmp, "%s\n\t%s", addr_str, ip);
                free(addr_str);
            }
            addr_str = tmp;
        }
        tmp = opal_get_proc_hostname(btl_proc->proc_opal);
        opal_show_help("help-mpi-btl-tcp.txt", "dropped inbound connection", true,
                       opal_process_info.nodename, getpid(), tmp,
                       OPAL_NAME_PRINT(btl_proc->proc_opal->proc_name),
                       opal_net_get_hostname((struct sockaddr *) addr),
                       btl_proc->proc_endpoint_count, (NULL == addr_str) ? "NONE" : addr_str);
        free(tmp);
        if (NULL != addr_str) {
            free(addr_str);
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
}

/*
 * convert internal data structure (mca_btl_tcp_addr_t) to sockaddr_storage
 *
 */
bool mca_btl_tcp_proc_tosocks(mca_btl_tcp_addr_t *proc_addr, struct sockaddr_storage *output)
{
    memset(output, 0, sizeof(*output));
    switch (proc_addr->addr_family) {
    case AF_INET:
        output->ss_family = AF_INET;
        memcpy(&((struct sockaddr_in *) output)->sin_addr, &proc_addr->addr_union.addr_inet,
               sizeof(struct in_addr));
        ((struct sockaddr_in *) output)->sin_port = proc_addr->addr_port;
        break;
#if OPAL_ENABLE_IPV6
    case AF_INET6: {
        struct sockaddr_in6 *inaddr = (struct sockaddr_in6 *) output;
        output->ss_family = AF_INET6;
        memcpy(&inaddr->sin6_addr, &proc_addr->addr_union.addr_inet6,
               sizeof(proc_addr->addr_union.addr_inet6));
        inaddr->sin6_port = proc_addr->addr_port;
        inaddr->sin6_scope_id = 0;
        inaddr->sin6_flowinfo = 0;
    } break;
#endif
    default:
        opal_output(0, "mca_btl_tcp_proc: unknown af_family received: %d\n",
                    proc_addr->addr_family);
        return false;
    }
    return true;
}
