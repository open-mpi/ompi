/*
 * Copyright (c) 2018-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * This file provides information about current run rank mapping in the shape
 * of a integer array where each rank will provides a set of contiguous integer :
 * its rank and its location at the different topological levels (from the
 * highest to the lowest).
 * At the end, the order for these data chunks uses the topological level as keys:
 * the ranks are sorted first by the top level, then by the next level, ... etc.
 *
 * Warning: this is not for the faint of heart -- don't even bother
 * reading this source code if you don't have a strong understanding
 * of nested data structures and pointer math (remember that
 * associativity and order of C operations is *critical* in terms of
 * pointer math!).
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */


#include "mpi.h"
#include "coll_han.h"


#if OPAL_ENABLE_DEBUG
static void
mca_coll_han_topo_print(int *topo,
                        struct ompi_communicator_t *comm,
                        int num_topo_level)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    if (rank == 0) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: Han topo: ", rank));
        for( int i = 0; i < size*num_topo_level; i++ ) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "%d ", topo[i]));
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "\n"));
    }
}
#endif  /* OPAL_ENABLE_DEBUG */

/**
 * Topology initialization phase
 * Called each time a collective that needs buffer reordering is called
 *
 * @param num_topo_level (IN)   Number of the topological levels
 */
int*
mca_coll_han_topo_init(struct ompi_communicator_t *comm,
                       mca_coll_han_module_t *han_module,
                       int num_topo_level)
{
    if ( NULL != han_module->cached_topo ) {
        return han_module->cached_topo;
    }

    ompi_communicator_t *up_comm, *low_comm;
    ompi_request_t *request = MPI_REQUEST_NULL;
    int *my_low_rank_map = NULL;
    int *ranks_map = NULL;

    int size = ompi_comm_size(comm);

    if (NULL != han_module->cached_up_comms) {
        up_comm  = han_module->cached_up_comms[0];
        low_comm = han_module->cached_low_comms[0];
    } else {
        up_comm  = han_module->sub_comm[INTER_NODE];
        low_comm = han_module->sub_comm[INTRA_NODE];
    }
    assert(up_comm != NULL && low_comm != NULL);

    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);

    int *topo = (int *)malloc(sizeof(int) * size * num_topo_level);
    int is_imbalanced = 1;
    int ranks_non_consecutive = 0;

    /* node leaders translate the node-local ranks to global ranks and check whether they are placed consecutively */
    if (0 == low_rank) {
        my_low_rank_map = malloc(sizeof(int)*low_size);
        for (int i = 0; i < low_size; ++i) {
            topo[i] = i;
        }
        ompi_group_translate_ranks(low_comm->c_local_group, low_size, topo,
                                   comm->c_local_group, my_low_rank_map);
        /* check if ranks are consecutive */
        int rank = my_low_rank_map[0] + 1;
        for (int i = 1; i < low_size; ++i, ++rank) {
            if (my_low_rank_map[i] != rank) {
                ranks_non_consecutive = 1;
                break;
            }
        }

        int reduce_vals[] = {ranks_non_consecutive, low_size, -low_size};

        up_comm->c_coll->coll_allreduce(MPI_IN_PLACE, &reduce_vals, 3,
                                        MPI_INT, MPI_MAX, up_comm,
                                        up_comm->c_coll->coll_allreduce_module);

        /* is the distribution of processes balanced per node? */
        is_imbalanced = (reduce_vals[1] == -reduce_vals[2]) ? 0 : 1;
        ranks_non_consecutive = reduce_vals[0];

        if ( ranks_non_consecutive && !is_imbalanced ) {
            /* kick off up_comm allgather to collect non-consecutive rank information at node leaders */
            ranks_map = malloc(sizeof(int)*size);
            up_comm->c_coll->coll_iallgather(my_low_rank_map, low_size, MPI_INT,
                                             ranks_map, low_size, MPI_INT, up_comm, &request,
                                             up_comm->c_coll->coll_iallgather_module);
        }
    }


    /* broadcast balanced and consecutive properties from node leaders to remaining ranks */
    int bcast_vals[] = {is_imbalanced, ranks_non_consecutive};
    low_comm->c_coll->coll_bcast(bcast_vals, 2, MPI_INT, 0,
                                 low_comm, low_comm->c_coll->coll_bcast_module);
    is_imbalanced = bcast_vals[0];
    ranks_non_consecutive = bcast_vals[1];

    /* error out if the rank distribution is not balanced */
    if (is_imbalanced) {
        assert(MPI_REQUEST_NULL == request);
        han_module->are_ppn_imbalanced = true;
        free(topo);
        if( NULL != my_low_rank_map ) free(my_low_rank_map);
        if( NULL != ranks_map ) free(ranks_map);
        return NULL;
    }

    han_module->are_ppn_imbalanced = false;

    if (!ranks_non_consecutive) {
        /* fast-path: all ranks are consecutive and balanced so fill topology locally */
        for (int i = 0; i < size; ++i) {
            topo[2*i]   = (i/low_size); // node leader is node ID
            topo[2*i+1] = i;
        }
        han_module->is_mapbycore = true;
    } else {
        han_module->is_mapbycore = false;
        /*
         * Slow path: gather global-to-node-local rank mappings at node leaders
         *
         * The topology will contain a mapping from global consecutive positions
         * to ranks in the communicator.
         *
         * ex: 4 ranks executing on 2 nodes, mapped by node
         *     ranks 0 and 2 on hid0
         *     ranks 1 and 3 on hid1
         * On entry the topo array looks like
         *     hid0 0 hid1 1 hid0 2 hid1 3
         * After the sort:
         *     hid0 0 hid0 2 hid1 1 hid1 3
         */
        if (0 == low_rank) {
            ompi_request_wait(&request, MPI_STATUS_IGNORE);
            /* fill topology */
            for (int i = 0; i < size; ++i) {
                topo[2*i]   = ranks_map[(i/low_size)*low_size]; // node leader is node ID
                topo[2*i+1] = ranks_map[i];
            }
            free(ranks_map);
        }
    }

    /* broadcast topology from node leaders to remaining ranks */
    low_comm->c_coll->coll_bcast(topo, num_topo_level*size, MPI_INT, 0,
                                low_comm, low_comm->c_coll->coll_bcast_module);
    free(my_low_rank_map);
    han_module->cached_topo = topo;
#if OPAL_ENABLE_DEBUG
    mca_coll_han_topo_print(topo, comm, num_topo_level);
#endif  /* OPAL_ENABLE_DEBUG */

    return topo;
}
