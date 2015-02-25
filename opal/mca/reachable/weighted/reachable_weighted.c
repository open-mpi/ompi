/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/if/if.h"

#include "opal/mca/reachable/base/base.h"
#include "reachable_weighted.h"

static int weighted_init(void);
static int weighted_fini(void);
static opal_if_t* weighted_reachable(opal_list_t *local_if,
                                     opal_list_t *remote_if);

/*
 * describes the quality of a possible connection between a local and
 * a remote network interface
 */
enum connection_quality {
    CQ_NO_CONNECTION,
    CQ_PRIVATE_DIFFERENT_NETWORK,
    CQ_PRIVATE_SAME_NETWORK,
    CQ_PUBLIC_DIFFERENT_NETWORK,
    CQ_PUBLIC_SAME_NETWORK
};


const opal_reachable_base_module_t opal_reachable_weighted_module = {
    weighted_init,
    weighted_fini,
    weighted_reachable
};

// local variables
static int init_cntr = 0;

static int weighted_init(void)
{
    ++init_cntr;

    return OPAL_SUCCESS;
}

static int weighted_fini(void)
{
    --init_cntr;

    return OPAL_SUCCESS;
}

static opal_if_t* weighted_reachable(opal_list_t *local_if,
                                     opal_list_t *remote_if)
{
    size_t perm_size, num_local_interfaces, num_peer_interfaces;
    enum connection_quality **weights;

    /*
     * assign weights to each possible pair of interfaces
     */
    num_local_interfaces = opal_list_get_size(local_if);
    num_peer_interfaces = opal_list_get_size(remote_if);

    perm_size = num_local_interfaces;
    if (num_peer_interfaces > perm_size) {
        perm_size = num_peer_interfaces;
    }

    weights = (enum connection_quality**)malloc(perm_size * sizeof(enum connection_quality*));

    best_addr = (mca_btl_tcp_addr_t ***) malloc(perm_size
                                                * sizeof(mca_btl_tcp_addr_t **));
    for(i = 0; i < perm_size; ++i) {
        weights[i] = (enum connection_quality*) malloc(perm_size * sizeof(enum connection_quality));
        memset(weights[i], 0, perm_size * sizeof(enum connection_quality));

        best_addr[i] = (mca_btl_tcp_addr_t **) malloc(perm_size * sizeof(mca_btl_tcp_addr_t *));
        memset(best_addr[i], 0, perm_size * sizeof(mca_btl_tcp_addr_t *));
    }

    for(i=0; i<num_local_interfaces; ++i) {
        for(j=0; j<num_peer_interfaces; ++j) {

            /*  initially, assume no connection is possible */
            weights[i][j] = CQ_NO_CONNECTION;

            /* check state of ipv4 address pair */
            if (NULL != local_interfaces[i]->ipv4_address &&
                NULL != peer_interfaces[j]->ipv4_address) {

                /*  check for loopback */
                if ((opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv4_address)
                     && !opal_net_islocalhost((struct sockaddr *)peer_interfaces[j]->ipv4_address))
                    || (opal_net_islocalhost((struct sockaddr *)peer_interfaces[j]->ipv4_address)
                        && !opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv4_address))
                    || (opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv4_address)
                        && !opal_ifislocal(proc_hostname))) {

                    /* No connection is possible on these interfaces */

                    /*  check for RFC1918 */
                } else if(opal_net_addr_isipv4public((struct sockaddr*) local_interfaces[i]->ipv4_address)
                          && opal_net_addr_isipv4public((struct sockaddr*)
                                                        peer_interfaces[j]->ipv4_address)) {
                    if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv4_address,
                                            (struct sockaddr*) peer_interfaces[j]->ipv4_address,
                                            local_interfaces[i]->ipv4_netmask)) {
                        weights[i][j] = CQ_PUBLIC_SAME_NETWORK;
                    } else {
                        weights[i][j] = CQ_PUBLIC_DIFFERENT_NETWORK;
                    }
                    best_addr[i][j] = peer_interfaces[j]->ipv4_endpoint_addr;
                    continue;
                } else {
                    if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv4_address,
                                            (struct sockaddr*) peer_interfaces[j]->ipv4_address,
                                            local_interfaces[i]->ipv4_netmask)) {
                        weights[i][j] = CQ_PRIVATE_SAME_NETWORK;
                    } else {
                        weights[i][j] = CQ_PRIVATE_DIFFERENT_NETWORK;
                    }
                    best_addr[i][j] = peer_interfaces[j]->ipv4_endpoint_addr;
                }
            }

            /* check state of ipv6 address pair - ipv6 is always public,
             * since link-local addresses are skipped in opal_ifinit()
             */
            if(NULL != local_interfaces[i]->ipv6_address &&
               NULL != peer_interfaces[j]->ipv6_address) {

                /*  check for loopback */
                if ((opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv6_address)
                     && !opal_net_islocalhost((struct sockaddr *)peer_interfaces[j]->ipv6_address))
                    || (opal_net_islocalhost((struct sockaddr *)peer_interfaces[j]->ipv6_address)
                        && !opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv6_address))
                    || (opal_net_islocalhost((struct sockaddr *)local_interfaces[i]->ipv6_address)
                        && !opal_ifislocal(proc_hostname))) {

                    /* No connection is possible on these interfaces */

                } else if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv6_address,
                                               (struct sockaddr*) peer_interfaces[j]->ipv6_address,
                                               local_interfaces[i]->ipv6_netmask)) {
                    weights[i][j] = CQ_PUBLIC_SAME_NETWORK;
                } else {
                    weights[i][j] = CQ_PUBLIC_DIFFERENT_NETWORK;
                }
                best_addr[i][j] = peer_interfaces[j]->ipv6_endpoint_addr;
            }

        } /* for each peer interface */
    } /* for each local interface */

    /*
     * determine the size of the set to permute (max number of
     * interfaces
     */

    best_assignment = (unsigned int *) malloc (perm_size * sizeof(int));

    a = (int *) malloc(perm_size * sizeof(int));
    if (NULL == a) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Can only find the best set of connections when the number of
     * interfaces is not too big.  When it gets larger, we fall back
     * to a simpler and faster (and not as optimal) algorithm.
     * See ticket https://svn.open-mpi.org/trac/ompi/ticket/2031
     * for more details about this issue.  */
    if (perm_size <= MAX_PERMUTATION_INTERFACES) {
        memset(a, 0, perm_size * sizeof(int));
        max_assignment_cardinality = -1;
        max_assignment_weight = -1;
        visit(0, -1, perm_size, a);

        rc = OPAL_ERR_UNREACH;
        for(i = 0; i < perm_size; ++i) {
            if(best_assignment[i] > num_peer_interfaces
               || weights[i][best_assignment[i]] == CQ_NO_CONNECTION
               || peer_interfaces[best_assignment[i]]->inuse
               || NULL == peer_interfaces[best_assignment[i]]) {
                continue;
            }
            peer_interfaces[best_assignment[i]]->inuse++;
            btl_endpoint->endpoint_addr = best_addr[i][best_assignment[i]];
            btl_endpoint->endpoint_addr->addr_inuse++;
            rc = OPAL_SUCCESS;
            break;
        }
    } else {
        enum mca_btl_tcp_connection_quality max;
        int i_max = 0, j_max = 0;
        /* Find the best connection that is not in use.  Save away
         * the indices of the best location. */
        max = CQ_NO_CONNECTION;
        for(i=0; i<num_local_interfaces; ++i) {
            for(j=0; j<num_peer_interfaces; ++j) {
                if (!peer_interfaces[j]->inuse) {
                    if (weights[i][j] > max) {
                        max = weights[i][j];
                        i_max = i;
                        j_max = j;
                    }
                }
            }
        }
        /* Now see if there is a some type of connection available. */
        rc = OPAL_ERR_UNREACH;
        if (CQ_NO_CONNECTION != max) {
            peer_interfaces[j_max]->inuse++;
            btl_endpoint->endpoint_addr = best_addr[i_max][j_max];
            btl_endpoint->endpoint_addr->addr_inuse++;
            rc = OPAL_SUCCESS;
        }
    }

    for(i = 0; i < perm_size; ++i) {
        free(weights[i]);
        free(best_addr[i]);
    }

    for(i = 0; i < num_peer_interfaces; ++i) {
        if(NULL != peer_interfaces[i]->ipv4_address) {
            free(peer_interfaces[i]->ipv4_address);
        }
        if(NULL != peer_interfaces[i]->ipv6_address) {
            free(peer_interfaces[i]->ipv6_address);
        }
        free(peer_interfaces[i]);
    }
    free(peer_interfaces);
    peer_interfaces = NULL;
    max_peer_interfaces = 0;

    for(i = 0; i < num_local_interfaces; ++i) {
        if(NULL != local_interfaces[i]->ipv4_address) {
            free(local_interfaces[i]->ipv4_address);
        }
        if(NULL != local_interfaces[i]->ipv6_address) {
            free(local_interfaces[i]->ipv6_address);
        }
        free(local_interfaces[i]);
    }
    free(local_interfaces);
    local_interfaces = NULL;
    max_local_interfaces = 0;

    free(weights);
    free(best_addr);
    free(best_assignment);
    free(a);
    return false;
}
