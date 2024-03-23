/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All rights reserved.
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
 * This files contains the hierarchical implementations of scatterv.
 * Only work with regular situation (each node has equal number of processes).
 */

/*
 * Implement hierarchical Scatterv to optimize large-scale communications where root sends
 * non-zero sized messages to multiple nodes and multiple processes per node, i.e. high incast.
 *
 * In Scatterv, only the root(sender) process has the information of the amount of data, i.e.
 * datatype and count, to each receiver process. Therefore node leaders need an additional step to
 * collect the expected data from its local peers. In summary, the steps are:
 * 1. Root:
 *      a. If necessary, reorder and sort data (See discussion below)
 *      b. Send data to other node leaders (Up Iscatterv)
 *      c. Send data to local peers (Low Scatterv)
 * 2. Root's local peers:
 *      a. Receive data from root. (Low Scatterv)
 * 3. Node leaders:
 *      a. Collect the data transfer sizes(in bytes) from local peers (Low Gather)
 *      b. Receive data from the root (Up Iscatterv)
 *      c. Send data to local peers (Low Scatterv)
 * 4. Node followers:
 *      a. Send the data transfer size(in bytes) to the node leader (Low Gather)
 *      b. Receive data from the node leader (Low Scatterv)
 *
 * Note on reordering:
 * In Up Iscatterv, reordering the send buffer can be avoided if and only if both of following
 * conditions are met:
 * 1. The data for each node is sorted in the same order as peer local ranks. Note, it is possible
 *    to send the data in the correct order even if the process are NOT mapped by core.
 * 2. In the send buffer, other than the root's node, data destined to the same node are continuous
 *    - it is ok if data to different nodes has gap.
 */
int mca_coll_han_scatterv_intra(const void *sbuf, const int *scounts, const int *displs,
                                struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
                                struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    int w_rank, w_size;              /* information about the global communicator */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    int err, *vranks, low_rank, low_size, up_rank, up_size, *topo;
    int *low_scounts = NULL, *low_displs = NULL;
    ompi_request_t *iscatterv_req = NULL;

    /* Create the subcommunicators */
    err = mca_coll_han_comm_create(comm, han_module);
    if (OMPI_SUCCESS != err) {
        OPAL_OUTPUT_VERBOSE((
            30, mca_coll_han_component.han_output,
            "han cannot handle scatterv with this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return han_module->previous_scatterv(sbuf, scounts, displs, sdtype, rbuf, rcount, rdtype,
                                             root, comm, han_module->previous_scatterv_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to determine if han can
     * be used */
    topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle scatterv with this communicator (imbalance). Fall "
                             "back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, scatterv);
        return han_module->previous_scatterv(sbuf, scounts, displs, sdtype, rbuf, rcount, rdtype,
                                             root, comm, han_module->previous_scatterv_module);
    }

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    /* create the subcommunicators */
    ompi_communicator_t *low_comm
        = han_module->cached_low_comms[mca_coll_han_component.han_scatterv_low_module];
    ompi_communicator_t *up_comm
        = han_module->cached_up_comms[mca_coll_han_component.han_scatterv_up_module];

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
                         "[%d]: Han scatterv root %d root_low_rank %d root_up_rank %d\n", w_rank,
                         root, root_low_rank, root_up_rank));

    err = OMPI_SUCCESS;
    /* #################### Root ########################### */
    if (root == w_rank) {
        int low_peer, up_peer, w_peer;
        int need_bounce_buf = 0, total_up_scounts = 0, *up_displs = NULL, *up_scounts = NULL,
            *up_peer_lb = NULL, *up_peer_ub = NULL;
        char *reorder_sbuf = (char *) sbuf, *bounce_buf = NULL;
        size_t sdsize;

        low_scounts = malloc(low_size * sizeof(int));
        low_displs = malloc(low_size * sizeof(int));
        if (!low_scounts || !low_displs) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto root_out;
        }

        for (w_peer = 0; w_peer < w_size; ++w_peer) {
            mca_coll_han_get_ranks(vranks, w_peer, low_size, &low_peer, &up_peer);
            if (root_up_rank != up_peer) {
                /* Not a local peer */
                continue;
            }
            low_displs[low_peer] = displs[w_peer];
            low_scounts[low_peer] = scounts[w_peer];
        }

        ompi_datatype_type_size(sdtype, &sdsize);

        up_scounts = calloc(up_size, sizeof(int));
        up_displs = malloc(up_size * sizeof(int));
        up_peer_ub = calloc(up_size, sizeof(int));
        if (!up_scounts || !up_displs || !up_peer_ub) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto root_out;
        }

        for (up_peer = 0; up_peer < up_size; ++up_peer) {
            up_displs[up_peer] = INT_MAX;
        }

        /* Calculate send counts for the inter-node scatterv */
        for (w_peer = 0; w_peer < w_size; ++w_peer) {
            mca_coll_han_get_ranks(vranks, w_peer, low_size, NULL, &up_peer);

            if (!need_bounce_buf && root_up_rank != up_peer && 0 < scounts[w_peer] && 0 < w_peer
                && displs[w_peer] < displs[w_peer - 1]) {
                /* Data is not placed in the rank order so reordering is needed */
                need_bounce_buf = 1;
            }

            if (root_up_rank == up_peer) {
                /* No need to scatter data on the same node again */
                continue;
            }

            up_peer_ub[up_peer] = 0 < scounts[w_peer]
                                          && displs[w_peer] + scounts[w_peer] > up_peer_ub[up_peer]
                                      ? displs[w_peer] + scounts[w_peer]
                                      : up_peer_ub[up_peer];

            up_scounts[up_peer] += scounts[w_peer];
            total_up_scounts += scounts[w_peer];

            /* Optimize for the happy path */
            up_displs[up_peer] = 0 < scounts[w_peer] && displs[w_peer] < up_displs[up_peer]
                                     ? displs[w_peer]
                                     : up_displs[up_peer];
        }

        /* If the data is not placed contiguously on send buf without overlaping, then we need a
         * temp buf without gaps */
        for (up_peer = 0; up_peer < up_size; ++up_peer) {
            if (root_up_rank == up_peer) {
                continue;
            }
            if (!need_bounce_buf && 0 < up_scounts[up_peer]
                && up_scounts[up_peer] != up_peer_ub[up_peer] - up_displs[up_peer]) {
                need_bounce_buf = 1;
                break;
            }
        }

        if (need_bounce_buf) {
            bounce_buf = malloc(sdsize * total_up_scounts);
            if (!bounce_buf) {
                err = OMPI_ERR_OUT_OF_RESOURCE;
                goto root_out;
            }

            /* Calculate displacements for the inter-node scatterv */
            for (up_peer = 0; up_peer < up_size; ++up_peer) {
                up_displs[up_peer] = 0 < up_peer ? up_displs[up_peer - 1] + up_scounts[up_peer - 1]
                                                 : 0;
            }

            /* Use a temp buffer to reorder the send buffer if needed */
            ptrdiff_t offset = 0;

            for (int i = 0; i < w_size; ++i) {
                up_peer = topo[2 * i];
                if (root_up_rank == up_peer) {
                    continue;
                }

                w_peer = topo[2 * i + 1];

                ompi_datatype_copy_content_same_ddt(sdtype, (size_t) scounts[w_peer],
                                                    bounce_buf + offset,
                                                    (char *) sbuf
                                                        + (size_t) displs[w_peer] * sdsize);
                offset += sdsize * (size_t) scounts[w_peer];
            }

            reorder_sbuf = bounce_buf;
        }

        /* Up Iscatterv */
        up_comm->c_coll->coll_iscatterv((const char *) reorder_sbuf, up_scounts, up_displs, sdtype,
                                        rbuf, rcount, rdtype, root_up_rank, up_comm, &iscatterv_req,
                                        up_comm->c_coll->coll_iscatterv_module);

        /* Low Scatterv */
        low_comm->c_coll->coll_scatterv(sbuf, low_scounts, low_displs, sdtype, rbuf, rcount, rdtype,
                                        root_low_rank, low_comm,
                                        low_comm->c_coll->coll_scatterv_module);

        ompi_request_wait(&iscatterv_req, MPI_STATUS_IGNORE);

    root_out:
        if (low_displs) {
            free(low_displs);
        }
        if (low_scounts) {
            free(low_scounts);
        }
        if (up_displs) {
            free(up_displs);
        }
        if (up_scounts) {
            free(up_scounts);
        }
        if (up_peer_lb) {
            free(up_peer_lb);
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
        /* Low Scatterv */
        low_comm->c_coll->coll_scatterv(NULL, NULL, NULL, NULL, rbuf, rcount, rdtype, root_low_rank,
                                        low_comm, low_comm->c_coll->coll_scatterv_module);
        return OMPI_SUCCESS;
    }

    size_t rdsize = 0;
    uint64_t receive_size = 0;

    ompi_datatype_type_size(rdtype, &rdsize);
    receive_size = (uint64_t) rdsize * (uint64_t) rcount;

    /* #################### Other node followers ########################### */
    if (root_low_rank != low_rank) {
        /* Low Gather - Gather each local peer's receive data size */
        low_comm->c_coll->coll_gather((const void *) &receive_size, 1, MPI_UINT64_T, NULL, 1,
                                      MPI_UINT64_T, root_low_rank, low_comm,
                                      low_comm->c_coll->coll_gather_module);
        /* Low Scatterv */
        low_comm->c_coll->coll_scatterv(NULL, NULL, NULL, NULL, rbuf, rcount, rdtype, root_low_rank,
                                        low_comm, low_comm->c_coll->coll_scatterv_module);
        return OMPI_SUCCESS;
    }

    /* #################### Node leaders ########################### */

    uint64_t *low_data_size = NULL;
    char *tmp_buf = NULL;
    ompi_datatype_t *temptype = MPI_BYTE;

    /* Allocate a temporary array to gather the data size, i.e. data type size x count,
     * in bytes from local peers */
    low_data_size = malloc(low_size * sizeof(uint64_t));
    if (!low_data_size) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto node_leader_out;
    }

    /* Low Gather -  Gather local peers' receive data sizes */
    low_comm->c_coll->coll_gather((const void *) &receive_size, 1, MPI_UINT64_T,
                                  (void *) low_data_size, 1, MPI_UINT64_T, root_low_rank, low_comm,
                                  low_comm->c_coll->coll_gather_module);

    /* Determine if we need to create a custom datatype instead of MPI_BYTE,
     * to avoid count(type int) overflow
     * TODO: Remove this logic once we adopt large-count, i.e. count will become 64-bit.
     */
    int total_up_scount = 0;
    size_t rsize = 0, datatype_size = 1, max_data_size = 0;
    for (int i = 0; i < low_size; ++i) {
        rsize += (size_t) low_data_size[i];
        max_data_size = (size_t) low_data_size[i] > max_data_size ? (size_t) low_data_size[i]
                                                                  : max_data_size;
    }

    if (max_data_size > (size_t) INT_MAX) {
        datatype_size = coll_han_utils_gcd(low_data_size, low_size);
    }

    low_scounts = malloc(low_size * sizeof(int));
    low_displs = malloc(low_size * sizeof(int));
    tmp_buf = (char *) malloc(rsize); /* tmp_buf is still valid if rsize is 0 */
    if (!tmp_buf || !low_scounts || !low_displs) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto node_leader_out;
    }

    for (int i = 0; i < low_size; ++i) {
        low_scounts[i] = (int) ((size_t) low_data_size[i] / datatype_size);
        low_displs[i] = i > 0 ? low_displs[i - 1] + low_scounts[i - 1] : 0;
        total_up_scount += low_scounts[i];
    }

    if (1 < datatype_size) {
        coll_han_utils_create_contiguous_datatype(datatype_size, MPI_BYTE, &temptype);
        ompi_datatype_commit(&temptype);
    }

    /* Up Iscatterv */
    up_comm->c_coll->coll_iscatterv(NULL, NULL, NULL, NULL, (void *) tmp_buf, total_up_scount,
                                    temptype, root_up_rank, up_comm, &iscatterv_req,
                                    up_comm->c_coll->coll_iscatterv_module);

    ompi_request_wait(&iscatterv_req, MPI_STATUS_IGNORE);

    /* Low Scatterv */
    low_comm->c_coll->coll_scatterv((void *) tmp_buf, low_scounts, low_displs, temptype, rbuf,
                                    rcount, rdtype, root_low_rank, low_comm,
                                    low_comm->c_coll->coll_scatterv_module);

node_leader_out:
    if (low_scounts) {
        free(low_scounts);
    }
    if (low_displs) {
        free(low_displs);
    }
    if (low_data_size) {
        free(low_data_size);
    }
    if (tmp_buf) {
        free(tmp_buf);
    }
    if (MPI_BYTE != temptype) {
        ompi_datatype_destroy(&temptype);
    }

    return err;
}
