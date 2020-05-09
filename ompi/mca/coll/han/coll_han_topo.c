/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
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


/*
 * Local functions
 */

static int mca_coll_han_hostname_to_number(char* hostname, int size);
static void mca_coll_han_topo_get(int *topo,
                                     struct ompi_communicator_t* comm,
                                     int num_topo_level);
static void mca_coll_han_topo_sort(int *topo, int start, int end,
                                      int level, int num_topo_level);
static bool mca_coll_han_topo_is_mapbycore(int *topo,
                                              struct ompi_communicator_t *comm,
                                              int num_topo_level);
static void mca_coll_han_topo_print(int *topo,
                                       struct ompi_communicator_t *comm,
                                       int num_topo_level);


/*
 * takes the number part of a host: hhh2031 -->2031
 */
static int mca_coll_han_hostname_to_number(char* hostname, int size)
{
    int i, j;
    char *number_array = (char *)malloc(sizeof(char) * size);
    int number = 0;

    for (i = 0, j = 0; hostname[i] != '\0'; i++) {
        if ('0' <= hostname[i] && '9' >= hostname[i]) {
            number_array[j++] = hostname[i];
        }
    }
    number_array[j] = '\0';
    number = atoi(number_array);
    free(number_array);
    return number;
}

/*
 * Set the virtual topo id. It is made of num_topo_level ints (2 today):
 *    . the integer part of the host id
 *    . the rank in the main communicator
 * Gather the virtual topoid from each process so every process will know other
 * processes virtual topids
 */
static void mca_coll_han_topo_get(int *topo,
                                     struct ompi_communicator_t* comm,
                                     int num_topo_level)
{
    int *self_topo = (int *)malloc(sizeof(int) * num_topo_level);
    char hostname[1024];

    gethostname(hostname, 1024);
    self_topo[0] = mca_coll_han_hostname_to_number(hostname, 1024);
    self_topo[1] = ompi_comm_rank(comm);

    ompi_coll_base_allgather_intra_bruck(self_topo, num_topo_level, MPI_INT,
                                         topo, num_topo_level, MPI_INT, comm,
                                         comm->c_coll->coll_allgather_module);
    free(self_topo);

    return;
}

/*
 * Sort the topology array in order to have ranks sharing the same node
 * contiguous in the topology array.
 * Called from topo_init whenever the processes are not mapped by core.
 * ex: 4 ranks executing on 2 nodes, mapped by node
 *     ranks 0 and 2 on hid0
 *     ranks 1 and 3 on hid1
 * On entry the topo array looks like
 *     hid0 0 hid1 1 hid0 2 hid1 3
 * After the sort:
 *     hid0 0 hid0 2 hid1 1 hid1 3
 * This is to have the gather result in the right order
 *
 * @param topo (IN/OUT)         topology description array (sorted in out)
 * @param start (IN)            where to begin the processing
 *                              The index in topo will actually be:
 *                              start * num_topo_level + level
 *                              topo contains num_topo_level ids per rank.
 * @param end (IN)              where to stop the processing
 *                              The index in topo will actually be:
 *                              end * num_topo_level + level
 *                              topo contains num_topo_level ids per rank.
 * @param level (IN)            level number we are currently processing
 * @param num_topo_level (IN)   number of topological levels
 *
 */
static void mca_coll_han_topo_sort(int *topo, int start, int end,
                                      int level, int num_topo_level)
{
    int i, j;
    int min, min_loc;
    int last, new_start, new_end;

    if (level > num_topo_level-1 || start >= end) {
        return;
    }

    min = INT_MAX;
    min_loc = -1;
    for (i = start; i <= end; i++) {
        int temp;
        /* get the min value for current level and its location */
        for (j = i; j <= end; j++) {
            /* topo contains num_topo_level ids per rank. */
            if (topo[j * num_topo_level + level] < min) {
                min = topo[j*num_topo_level+level];
                min_loc = j;

            }
        }
        /*
         * swap i and min_loc
         * We have num_topo_level ids to swap
         */
        for (j = 0; j < num_topo_level; j++) {
            temp = topo[i * num_topo_level + j];
            topo[i * num_topo_level + j] = topo[min_loc * num_topo_level + j];
            topo[min_loc * num_topo_level + j] = temp;
        }
        min = INT_MAX;
        min_loc = -1;
    }

    /* Process next level */
    last = 0;
    new_start = 0;
    new_end = 0;
    for (i = start; i <= end; i++) {
        if (i == start) {
            last = topo[i * num_topo_level + level];
            new_start = start;
        } else if (i == end) {
            new_end = end;
            mca_coll_han_topo_sort(topo, new_start, new_end, level + 1,
                                      num_topo_level);
        } else if (last != topo[i * num_topo_level + level]) {
            new_end = i - 1;
            mca_coll_han_topo_sort(topo, new_start, new_end, level + 1,
                                      num_topo_level);
            new_start = i;
            last = topo[i * num_topo_level + level];
        }
    }
    return;
}

/*
 * Check whether the ranks in the communicator given as input are mapped by core
 * Mapped by core: each node is first filled with as many ranks as needed before
 * moving to the next one
 * This is checked as follows:
 *    . 2 contiguous ranks should be either on the same node or on node ids in
 *      ascending order
 * The topology is actually an array of ints:
 *   +----------+-------+----------+-------+------+----------+-------+-----+
 *   | host_id0 | rank0 | host_id1 | rank1 | .... | host_idX | rankX | ... |
 *   +----------+-------+----------+-------+------+----------+-------+-----+
 */
static bool mca_coll_han_topo_is_mapbycore(int *topo,
                                              struct ompi_communicator_t *comm,
                                              int num_topo_level)
{
    int i;
    int size = ompi_comm_size(comm);

    for (i = 1; i < size; i++) {
        /*
         * The host id for a given rank should be < host id for the next rank
         */
        if (topo[(i - 1) * num_topo_level] > topo[i * num_topo_level]) {
            return false;
        }
        /*
         * For the same host id, consecutive ranks should be sorted in
         * ascending order.
         */
        if (topo[(i - 1) * num_topo_level + 1] > topo[i * num_topo_level + 1]) {
            return false;
        }
    }
    return true;
}

/* The topo is supposed sorted by host */
static bool mca_coll_han_topo_are_ppn_imbalanced(int *topo,
                            struct ompi_communicator_t *comm,
                            int num_topo_level){
    int i;
    int size = ompi_comm_size(comm);
    if (size < 2){
        return false;
    }
    int ppn;
    int last_host = topo[0];

    /* Find the ppn for the first node */
    for (i = 1; i < size; i++) {
        if (topo[i * num_topo_level] != last_host){
            break;
        }
    }
    ppn = i;

    /* All on one node */
    if ( size == ppn){
        return false;
    }
    /* Trivial case */
    if (size % ppn != 0){
        return true;
    }

    last_host = topo[ppn * num_topo_level];
    /* Check that the 2nd and next hosts also this ppn. Since the topo is sorted
     * one just need to jump ppn ranks to check the supposed switch of host */
    for (i = 2 * ppn; i < size; i += ppn ){
        /* the list of ranks for the last known host have ended before */
        if (topo[(i-1) * num_topo_level] != last_host){
            return true;
        }
        /* the list of ranks for the last known host are bigger than excpected */
        if (topo[(i-1) * num_topo_level] == topo[i*num_topo_level]){
            return true;
        }
        last_host = topo[i * num_topo_level];
    }
    /* Check the last host */
    if (topo[(size-1) * num_topo_level] != last_host){
        return true;
    }

    return false;
}


/**
 * Topology initialization phase
 * Called each time a collective that needs buffer reordering is called
 *
 * @param num_topo_level (IN)   Number of the topological levels
 */
int *mca_coll_han_topo_init(struct ompi_communicator_t *comm,
                               mca_coll_han_module_t *han_module,
                               int num_topo_level)
{
    int size;
    int *topo;

    size = ompi_comm_size(comm);

    if (!((han_module->cached_topo) && (han_module->cached_comm == comm))) {
        if (han_module->cached_topo) {
            free(han_module->cached_topo);
            han_module->cached_topo = NULL;
        }

        topo = (int *)malloc(sizeof(int) * size * num_topo_level);

        /* get topo infomation */
        mca_coll_han_topo_get(topo, comm, num_topo_level);
        mca_coll_han_topo_print(topo, comm, num_topo_level);

        /*
         * All the ranks now have the topo information
         */

        /* check if the processes are mapped by core */
        han_module->is_mapbycore = mca_coll_han_topo_is_mapbycore(topo, comm, num_topo_level);

        /*
         * If not, sort the topo such that each group of ids is sorted by rank
         * i.e. ids for rank i are contiguous to ids for rank i+1.
         * This will be needed for the operations that are order sensitive
         * (like gather)
         */
        if (!han_module->is_mapbycore) {
            mca_coll_han_topo_sort(topo, 0, size-1, 0, num_topo_level);
        }
        han_module->are_ppn_imbalanced = mca_coll_han_topo_are_ppn_imbalanced(topo, comm , num_topo_level);
        han_module->cached_topo = topo;
        han_module->cached_comm = comm;
    } else {
        topo = han_module->cached_topo;
    }

    mca_coll_han_topo_print(topo, comm, num_topo_level);
    return topo;
}

static void mca_coll_han_topo_print(int *topo,
                                       struct ompi_communicator_t *comm,
                                       int num_topo_level)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    if (rank == 0) {
        int i;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d]: Han Scatter topo: ", rank));
        for (i=0; i<size*num_topo_level; i++) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "%d ", topo[i]));
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "\n"));
    }
}

