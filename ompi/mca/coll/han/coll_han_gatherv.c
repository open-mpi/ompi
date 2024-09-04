/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han.h"
#include "coll_han_trigger.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

/*
 * @file
 *
 * This files contains the hierarchical implementations of gatherv.
 * Only work with regular situation (each node has equal number of processes).
 */

/*
 * Implement hierarchical Gatherv to optimize large-scale communications where multiple nodes and
 * multiple processes per node send non-zero sized messages to the root, i.e. high incast.
 *
 * In Gatherv, only the root(receiver) process has the information of the amount of data, i.e.
 * datatype and count, from each sender process. Therefore node leaders need an additional step to
 * collect the expected data from its local peers. In summary, the steps are:
 * 1. Root
 *      a. Receive data from local peers (Low Gatherv)
 *      b. Receive data from other node leaders (Up Gatherv)
 *      c. If necessary reorder data from node leaders(see discussion below)
 * 2. Root's local peers
 *      a. Send data to root. (Low Gatherv)
 * 3. Node leaders:
 *      a. Collect the data transfer sizes(in bytes) from local peers (Low Gather)
 *      b. Receive data from local peers (Low Gatherv)
 *      c. Send data to the root (Up Gatherv)
 * 4. Node followers:
 *      a. Send the data transfer size(in bytes) to the node leader (Low Gather)
 *      b. Send data to the node leader (Low Gatherv)
 *
 * Note on reodering:
 * In Up Gatherv, data from each node is stored in a contiguous buffer sorted by the sender's
 * local rank, and MUST be reordered according to the root's displacement requirement on the output
 * buffer. Concretely, reordering can avoided if and only if both of following conditions are met:
 * 1. Data from processes on each node, other than the root's node, are placed in the output buffer
 *    in the same **order** as their local ranks. Note, it is possible to receive the data in the
 *    correct order even if the process are NOT mapped by core.
 * 2. No **gap** exists between data from the same node, other than the root's node, in the output
 *    buffer - it is ok if data from different nodes has gap.
 */
int mca_coll_han_gatherv_intra(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                               void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t displs,
                               struct ompi_datatype_t *rdtype, int root,
                               struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    int w_rank, w_size;              /* information about the global communicator */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    int err, *vranks, low_rank, low_size, up_rank, up_size, *topo;
    size_t *low_rcounts = NULL;
    ptrdiff_t *low_displs = NULL;
    ompi_count_array_t low_rcounts_desc;
    ompi_disp_array_t low_displs_desc;

    /* Create the subcommunicators */
    err = mca_coll_han_comm_create(comm, han_module);
    if (OMPI_SUCCESS != err) {
        OPAL_OUTPUT_VERBOSE(
            (30, mca_coll_han_component.han_output,
             "han cannot handle gatherv with this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_gatherv(sbuf, scount, sdtype, rbuf, rcounts, displs, rdtype,
                                            root, comm, han_module->previous_gatherv_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to determine if han can
     * be used */
    topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle gatherv with this communicator (imbalance). Fall "
                             "back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_UNINSTALL_COLL_API(comm, han_module, gatherv);
        return han_module->previous_gatherv(sbuf, scount, sdtype, rbuf, rcounts, displs, rdtype,
                                            root, comm, han_module->previous_gatherv_module);
    }

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    /* create the subcommunicators */
    ompi_communicator_t *low_comm
        = han_module->cached_low_comms[mca_coll_han_component.han_gatherv_low_module];
    ompi_communicator_t *up_comm
        = han_module->cached_up_comms[mca_coll_han_component.han_gatherv_up_module];

    /* Get the 'virtual ranks' mapping corresponding to the communicators */
    vranks = han_module->cached_vranks;
    /* information about sub-communicators */
    low_rank = ompi_comm_rank(low_comm);
    low_size = ompi_comm_size(low_comm);
    up_rank = ompi_comm_rank(up_comm);
    up_size = ompi_comm_size(up_comm);
    /* Get root ranks for low and up comms */
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);

    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: Han Gatherv root %d root_low_rank %d root_up_rank %d\n", w_rank,
                         root, root_low_rank, root_up_rank));

    err = OMPI_SUCCESS;
    /* #################### Root ########################### */
    if (root == w_rank) {
        int need_bounce_buf = 0;
        size_t total_up_rcounts = 0;
        ptrdiff_t *up_displs = NULL;
        size_t *up_rcounts = NULL, *up_peer_ub = NULL;
        ompi_count_array_t up_rcounts_desc;
        ompi_disp_array_t up_displs_desc;
        char *bounce_buf = NULL;

        low_rcounts = malloc(low_size * sizeof(size_t));
        low_displs = malloc(low_size * sizeof(ptrdiff_t));
        if (!low_rcounts || !low_displs) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto root_out;
        }

        int low_peer, up_peer, w_peer;
        for (w_peer = 0; w_peer < w_size; ++w_peer) {
            mca_coll_han_get_ranks(vranks, w_peer, low_size, &low_peer, &up_peer);
            if (root_up_rank != up_peer) {
                /* Not a local peer */
                continue;
            }
            low_displs[low_peer] = ompi_disp_array_get(displs, w_peer);
            low_rcounts[low_peer] = ompi_count_array_get(rcounts, w_peer);
        }

        /* Low Gatherv */
        OMPI_COUNT_ARRAY_INIT(&low_rcounts_desc, low_rcounts);
        OMPI_DISP_ARRAY_INIT(&low_displs_desc, low_displs);
        low_comm->c_coll->coll_gatherv(sbuf, scount, sdtype, rbuf, low_rcounts_desc,
                                       low_displs_desc, rdtype, root_low_rank, low_comm,
                                       low_comm->c_coll->coll_gatherv_module);

        char *tmp_rbuf = rbuf;

        up_rcounts = calloc(up_size, sizeof(size_t));
        up_displs = malloc(up_size * sizeof(ptrdiff_t));
        up_peer_ub = calloc(up_size, sizeof(size_t));
        if (!up_rcounts || !up_displs || !up_peer_ub) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto root_out;
        }

        for (up_peer = 0; up_peer < up_size; ++up_peer) {
            up_displs[up_peer] = PTRDIFF_MAX;
        }

        /* Calculate recv counts for the inter-node gatherv - no need to gather
         * from self again because the data is already in place */
        for (w_peer = 0; w_peer < w_size; ++w_peer) {
            mca_coll_han_get_ranks(vranks, w_peer, low_size, NULL, &up_peer);

            if (!need_bounce_buf && root_up_rank != up_peer && 0 < ompi_count_array_get(rcounts, w_peer) && 0 < w_peer
                && ompi_disp_array_get(displs, w_peer) < ompi_disp_array_get(displs, w_peer - 1)) {
                /* Data is not placed in the rank order so reordering is needed */
                need_bounce_buf = 1;
            }

            if (root_up_rank == up_peer) {
                /* No need to gather data on the same node again */
                continue;
            }

            up_peer_ub[up_peer] = 0 < ompi_count_array_get(rcounts, w_peer)
                                          && ompi_disp_array_get(displs, w_peer) + ompi_count_array_get(rcounts, w_peer) > up_peer_ub[up_peer]
                                      ? ompi_disp_array_get(displs, w_peer) + ompi_count_array_get(rcounts, w_peer)
                                      : up_peer_ub[up_peer];

            up_rcounts[up_peer] += ompi_count_array_get(rcounts, w_peer);
            total_up_rcounts += ompi_count_array_get(rcounts, w_peer);

            /* Optimize for the happy path */
            up_displs[up_peer] = 0 < ompi_count_array_get(rcounts, w_peer) && ompi_disp_array_get(displs, w_peer) < up_displs[up_peer]
                                     ? ompi_disp_array_get(displs, w_peer)
                                     : up_displs[up_peer];
        }

        /* If the data is not placed contiguously on recv buf, then we will need temp buf to store
         * the gap data and recover it later */
        for (up_peer = 0; up_peer < up_size; ++up_peer) {
            if (root_up_rank == up_peer) {
                continue;
            }
            if (!need_bounce_buf && 0 < up_rcounts[up_peer]
                && up_rcounts[up_peer] < up_peer_ub[up_peer] - up_displs[up_peer]) {
                need_bounce_buf = 1;
                break;
            }
        }

        if (need_bounce_buf) {
            ptrdiff_t rsize, rgap;
            rsize = opal_datatype_span(&rdtype->super, total_up_rcounts, &rgap);
            bounce_buf = malloc(rsize);
            if (!bounce_buf) {
                err = OMPI_ERR_OUT_OF_RESOURCE;
                goto root_out;
            }

            /* Calculate displacements for the inter-node gatherv */
            for (up_peer = 0; up_peer < up_size; ++up_peer) {
                up_displs[up_peer] = 0 < up_peer ? up_displs[up_peer - 1] + up_rcounts[up_peer - 1]
                                                 : 0;
            }

            tmp_rbuf = bounce_buf - rgap;
        }

        /* Up Gatherv */
        OMPI_COUNT_ARRAY_INIT(&up_rcounts_desc, up_rcounts);
        OMPI_DISP_ARRAY_INIT(&up_displs_desc, up_displs);
        up_comm->c_coll->coll_gatherv(sbuf, 0, sdtype, tmp_rbuf, up_rcounts_desc, up_displs_desc, rdtype,
                                      root_up_rank, up_comm, up_comm->c_coll->coll_gatherv_module);

        /* Use a temp buffer to reorder the output buffer if needed */
        if (need_bounce_buf) {
            ptrdiff_t offset = 0, rdext;
            ompi_datatype_type_extent(rdtype, &rdext);

            for (int i = 0; i < w_size; ++i) {
                up_peer = topo[2 * i];
                if (root_up_rank == up_peer) {
                    continue;
                }

                w_peer = topo[2 * i + 1];

                ompi_datatype_copy_content_same_ddt(rdtype, ompi_count_array_get(rcounts, w_peer),
                                                    (char *) rbuf + ompi_disp_array_get(displs, w_peer) * rdext,
                                                    bounce_buf + offset);
                offset += rdext * ompi_count_array_get(rcounts, w_peer);
            }
        }

    root_out:
        if (low_displs) {
            free(low_displs);
        }
        if (low_rcounts) {
            free(low_rcounts);
        }
        if (up_displs) {
            free(up_displs);
        }
        if (up_rcounts) {
            free(up_rcounts);
        }
        if (up_peer_ub) {
            free(up_peer_ub);
        }
        if (bounce_buf) {
            free(bounce_buf);
        }

        return err;
    }

    /* #################### Root's local peers ########################### */
    if (root_up_rank == up_rank) {
        /* Low Gatherv */
        low_comm->c_coll->coll_gatherv(sbuf, scount, sdtype, NULL, 0, 0, NULL, root_low_rank,
                                       low_comm, low_comm->c_coll->coll_gatherv_module);
        return OMPI_SUCCESS;
    }

    size_t sdsize = 0;
    size_t send_size = 0;

    ompi_datatype_type_size(sdtype, &sdsize);
    send_size = sdsize * scount;

    /* #################### Other node followers ########################### */
    if (root_low_rank != low_rank) {
        /* Low Gather - Gather each local peer's send data size */
        low_comm->c_coll->coll_gather((const void *) &send_size, sizeof(size_t), MPI_BYTE, NULL,
                                      sizeof(size_t), MPI_BYTE, root_low_rank, low_comm,
                                      low_comm->c_coll->coll_gather_module);
        /* Low Gatherv */
        low_comm->c_coll->coll_gatherv(sbuf, scount, sdtype, NULL, 0, 0, NULL, root_low_rank,
                                       low_comm, low_comm->c_coll->coll_gatherv_module);
        return OMPI_SUCCESS;
    }

    /* #################### Node leaders ########################### */

    char *tmp_buf = NULL;

    /* Allocate a temporary array to gather the data size, i.e. data type size x count,
     * in bytes from local peers */
    low_rcounts = malloc(low_size * sizeof(size_t));
    if (!low_rcounts) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto node_leader_out;
    }

    /* Low Gather -  Gather local peers' send data sizes */
    low_comm->c_coll->coll_gather((const void *) &send_size, sizeof(size_t), MPI_BYTE,
                                  (void *) low_rcounts, sizeof(size_t), MPI_BYTE, root_low_rank, low_comm,
                                  low_comm->c_coll->coll_gather_module);

    low_displs = malloc(low_size * sizeof(ptrdiff_t));
    if (!low_displs) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto node_leader_out;
    }

    size_t total_rsize = 0;
    for (int i = 0; i < low_size; ++i) {
        low_displs[i] = i > 0 ? low_displs[i - 1] + low_rcounts[i - 1] : 0;
        total_rsize += low_rcounts[i];
    }

    tmp_buf = (char *) malloc(total_rsize); /* tmp_buf is still valid if total_rsize is 0 */
    if (!tmp_buf) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto node_leader_out;
    }

    /* Low Gatherv */
    OMPI_COUNT_ARRAY_INIT(&low_rcounts_desc, low_rcounts);
    OMPI_DISP_ARRAY_INIT(&low_displs_desc, low_displs);
    low_comm->c_coll->coll_gatherv(sbuf, scount, sdtype, (void *) tmp_buf,
                                   low_rcounts_desc, low_displs_desc,
                                   MPI_BYTE, root_low_rank, low_comm,
                                   low_comm->c_coll->coll_gatherv_module);

    /* Up Gatherv */
    up_comm->c_coll->coll_gatherv(tmp_buf, total_rsize, MPI_BYTE, NULL, 0, 0, NULL,
                                  root_up_rank, up_comm, up_comm->c_coll->coll_gatherv_module);

node_leader_out:
    if (low_rcounts) {
        free(low_rcounts);
    }
    if (low_displs) {
        free(low_displs);
    }
    if (tmp_buf) {
        free(tmp_buf);
    }

    return err;
}
