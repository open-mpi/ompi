/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "src/include/constants.h"
#include "src/include/types.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_MATH_H
#    include <math.h>
#endif

#ifndef MIN
#    define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#include "reachable_weighted.h"
#include "src/mca/prtereachable/base/base.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_string_copy.h"

static int weighted_init(void);
static int weighted_fini(void);
static prte_reachable_t *weighted_reachable(pmix_list_t *local_ifs, pmix_list_t *remote_ifs);

static int get_weights(pmix_pif_t *local_if, pmix_pif_t *remote_if);
static int calculate_weight(int bandwidth_local, int bandwidth_remote, int connection_quality);

/*
 * Describes the quality of a possible connection between a local and
 * a remote network interface.  Highest connection quality is assigned
 * to connections between interfaces on same network.  This is because
 * same network implies a single hop to destination.  Public addresses
 * are preferred over private addresses.  This is all guessing,
 * because we don't know actual network topology.
 */
enum connection_quality {
    CQ_NO_CONNECTION = 0,
    CQ_PRIVATE_DIFFERENT_NETWORK = 50,
    CQ_PRIVATE_SAME_NETWORK = 80,
    CQ_PUBLIC_DIFFERENT_NETWORK = 90,
    CQ_PUBLIC_SAME_NETWORK = 100
};

const prte_reachable_base_module_t prte_prtereachable_weighted_module = {weighted_init,
                                                                         weighted_fini,
                                                                         weighted_reachable};

// local variables
static int init_cntr = 0;

static int weighted_init(void)
{
    ++init_cntr;

    return PRTE_SUCCESS;
}

static int weighted_fini(void)
{
    --init_cntr;

    return PRTE_SUCCESS;
}

static prte_reachable_t *weighted_reachable(pmix_list_t *local_ifs, pmix_list_t *remote_ifs)
{
    prte_reachable_t *reachable_results = NULL;
    int i, j;
    pmix_pif_t *local_iter, *remote_iter;

    reachable_results = prte_reachable_allocate(pmix_list_get_size(local_ifs),
                                                pmix_list_get_size(remote_ifs));
    if (NULL == reachable_results) {
        return NULL;
    }

    i = 0;
    PMIX_LIST_FOREACH(local_iter, local_ifs, pmix_pif_t)
    {
        j = 0;
        PMIX_LIST_FOREACH(remote_iter, remote_ifs, pmix_pif_t)
        {
            reachable_results->weights[i][j] = get_weights(local_iter, remote_iter);
            j++;
        }
        i++;
    }

    return reachable_results;
}

static int get_weights(pmix_pif_t *local_if, pmix_pif_t *remote_if)
{
    char str_local[128], str_remote[128], *conn_type;
    struct sockaddr *local_sockaddr, *remote_sockaddr;
    struct sockaddr_storage laddr, raddr;
    int weight;

    local_sockaddr = (struct sockaddr *) &local_if->if_addr;
    remote_sockaddr = (struct sockaddr *) &remote_if->if_addr;

    /* pmix_net_get_hostname returns a static buffer.  Great for
       single address printfs, need to copy in this case */
    pmix_string_copy(str_local, pmix_net_get_hostname(local_sockaddr), sizeof(str_local));
    str_local[sizeof(str_local) - 1] = '\0';
    pmix_string_copy(str_remote, pmix_net_get_hostname(remote_sockaddr), sizeof(str_remote));
    str_remote[sizeof(str_remote) - 1] = '\0';

    /*  initially, assume no connection is possible */
    weight = calculate_weight(0, 0, CQ_NO_CONNECTION);

    if (AF_INET == local_sockaddr->sa_family && AF_INET == remote_sockaddr->sa_family) {
        memset(&laddr, 0, sizeof(laddr));
        memcpy(&laddr, local_sockaddr, sizeof(struct sockaddr));
        memset(&raddr, 0, sizeof(raddr));
        memcpy(&raddr, remote_sockaddr, sizeof(struct sockaddr));

        if (pmix_net_addr_isipv4public(local_sockaddr)
            && pmix_net_addr_isipv4public(remote_sockaddr)) {
            if (pmix_net_samenetwork(&laddr, &raddr, local_if->if_mask)) {
                conn_type = "IPv4 PUBLIC SAME NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PUBLIC_SAME_NETWORK);
            } else {
                conn_type = "IPv4 PUBLIC DIFFERENT NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PUBLIC_DIFFERENT_NETWORK);
            }
        } else if (!pmix_net_addr_isipv4public(local_sockaddr)
                   && !pmix_net_addr_isipv4public(remote_sockaddr)) {
            if (pmix_net_samenetwork(&laddr, &raddr, local_if->if_mask)) {
                conn_type = "IPv4 PRIVATE SAME NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PRIVATE_SAME_NETWORK);
            } else {
                conn_type = "IPv4 PRIVATE DIFFERENT NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PRIVATE_DIFFERENT_NETWORK);
            }
        } else {
            /* one private, one public address.  likely not a match. */
            conn_type = "IPv4 NO CONNECTION";
            weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                      CQ_NO_CONNECTION);
        }

#if PRTE_ENABLE_IPV6
    } else if (AF_INET6 == local_sockaddr->sa_family && AF_INET6 == remote_sockaddr->sa_family) {
        memset(&laddr, 0, sizeof(laddr));
        memcpy(&laddr, local_sockaddr, sizeof(struct sockaddr));
        memset(&raddr, 0, sizeof(raddr));
        memcpy(&raddr, remote_sockaddr, sizeof(struct sockaddr));
        if (pmix_net_addr_isipv6linklocal(local_sockaddr)
            && pmix_net_addr_isipv6linklocal(remote_sockaddr)) {
            /* we can't actually tell if link local addresses are on
             * the same network or not with the weighted component.
             * Assume they are on the same network, so that they'll be
             * most likely to be paired together, breaking the fewest
             * number of connections.
             *
             * There used to be a comment in this code (and one in the
             * BTL TCP code as well) that the prte_if code doesn't
             * pass link-local addresses through.  However, this is
             * demonstratably not true on Linux, where link-local
             * interfaces are created.  Since it's easy to handle
             * either case, do so.
             */
            conn_type = "IPv6 LINK-LOCAL SAME NETWORK";
            weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                      CQ_PRIVATE_SAME_NETWORK);
        } else if (!pmix_net_addr_isipv6linklocal(local_sockaddr)
                   && !pmix_net_addr_isipv6linklocal(remote_sockaddr)) {
            if (pmix_net_samenetwork(&laddr, &raddr, local_if->if_mask)) {
                conn_type = "IPv6 PUBLIC SAME NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PUBLIC_SAME_NETWORK);
            } else {
                conn_type = "IPv6 PUBLIC DIFFERENT NETWORK";
                weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                          CQ_PUBLIC_DIFFERENT_NETWORK);
            }
        } else {
            /* one link-local, one public address.  likely not a match. */
            conn_type = "IPv6 NO CONNECTION";
            weight = calculate_weight(local_if->if_bandwidth, remote_if->if_bandwidth,
                                      CQ_NO_CONNECTION);
        }
#endif /* #if PRTE_ENABLE_IPV6 */

    } else {
        /* we don't have an address family match, so assume no
           connection */
        conn_type = "Address type mismatch";
        weight = calculate_weight(0, 0, CQ_NO_CONNECTION);
    }

    pmix_output_verbose(20, prte_prtereachable_base_framework.framework_output,
                        "reachable:weighted: path from %s to %s: %s", str_local, str_remote,
                        conn_type);

    return weight;
}

/*
 * Weights determined by bandwidth between
 * interfaces (limited by lower bandwidth
 * interface).  A penalty is added to minimize
 * the discrepancy in bandwidth.  This helps
 * prevent pairing of fast and slow interfaces
 *
 * Formula: connection_quality * (min(a,b) + 1/(1 + |a-b|))
 *
 * Examples: a     b     f(a,b)
 *           0     0     1
 *           0     1     0.5
 *           1     1     2
 *           1     2     1.5
 *           1     3     1.33
 *           1     10    1.1
 *           10    10    11
 *           10    14    10.2
 *           11    14    11.25
 *           11    15    11.2
 *
 * NOTE: connection_quality of 1 is assumed for examples.
 * In reality, since we're using integers, we need
 * connection_quality to be large enough
 * to capture decimals
 */
static int calculate_weight(int bandwidth_local, int bandwidth_remote, int connection_quality)
{
    int weight = connection_quality
                 * (MIN(bandwidth_local, bandwidth_remote)
                    + 1.0 / (1.0 + (double) abs(bandwidth_local - bandwidth_remote)));
    return weight;
}
