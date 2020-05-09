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

#include "mpi.h"
#include "coll_han.h"
#include "coll_han_dynamic.h"


/*
 * Local functions
 */
static void create_intranode_comm_new(ompi_communicator_t *,
                                      ompi_communicator_t **);
static void create_internode_comm_new(ompi_communicator_t *,
                                      int, int,
                                      ompi_communicator_t **);
static void create_intranode_comm(ompi_communicator_t *,
                                  const char *,
                                  int,
                                  ompi_communicator_t **);
static void create_internode_comm(ompi_communicator_t *,
                                  const char *,
                                  int, int,
                                  ompi_communicator_t **);

/**
 * Create a sub-communicator containing the ranks that share my node.
 *
 * @param comm (IN)          original communicator for the collective
 *                           target module priority
 * @param sub_comm (OUT)     created sub-communicator
 */
static void create_intranode_comm_new(ompi_communicator_t *comm,
                                  ompi_communicator_t **sub_comm)
{
    ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0,
                         (opal_info_t *)(&ompi_mpi_info_null), sub_comm);
    return;
}

/**
 * Create a sub-communicator containing one rank per node.
 *
 * @param comm (IN)          original communicator for the collective
 * @param my_rank (IN)       my rank in comm
 * @param intra_rank (IN)    local rank in the intra-node sub-communicator
 * @param sub_comm (OUT)     created sub-communicator
 */
static void create_internode_comm_new(ompi_communicator_t *comm,
                                  int my_rank,
                                  int intra_rank,
                                  ompi_communicator_t **sub_comm)
{
    ompi_comm_split(comm, intra_rank, my_rank, sub_comm, false);
    return;
}

/*
 * Routine that creates the local hierarchical sub-communicators
 * Called each time a collective is called.
 * comm: input communicator of the collective
 */
void mca_coll_han_comm_create_new(struct ompi_communicator_t *comm,
                                 mca_coll_han_module_t *han_module)
{
    int low_rank, low_size;
    int up_rank;
    int w_rank;
    int w_size;
    ompi_communicator_t **low_comm = &(han_module->sub_comm[INTRA_NODE]);
    ompi_communicator_t **up_comm = &(han_module->sub_comm[INTER_NODE]);
    const int *origin_priority;
    int han_var_id;
    int tmp_han_priority;
    int vrank, *vranks;

    mca_coll_base_module_allreduce_fn_t old_allreduce;
    mca_coll_base_module_t *old_allreduce_module;

    mca_coll_base_module_allgather_fn_t old_allgather;
    mca_coll_base_module_t *old_allgather_module;

    mca_coll_base_module_bcast_fn_t old_bcast;
    mca_coll_base_module_t *old_bcast_module;

    mca_coll_base_module_gather_fn_t old_gather;
    mca_coll_base_module_t *old_gather_module;

    mca_coll_base_module_reduce_fn_t old_reduce;
    mca_coll_base_module_t *old_reduce_module;

    /* The sub communicators have already been created */
    if (NULL != han_module->sub_comm[INTRA_NODE]
        && NULL != han_module->sub_comm[INTER_NODE]
        && NULL != han_module->cached_vranks) {
        return;
    }

    /*
     * We cannot use han allreduce and allgather without sub-communicators
     * Temporary set previous ones
     *
     * Allgather is used to compute vranks
     * Allreduce is used by ompi_comm_split_type in create_intranode_comm_new
     * Reduce + Bcast may be called by the allreduce implementation
     * Gather + Bcast may be called by the allgather implementation
     */
    old_allreduce = comm->c_coll->coll_allreduce;
    old_allreduce_module = comm->c_coll->coll_allreduce_module;

    old_allgather = comm->c_coll->coll_allgather;
    old_allgather_module = comm->c_coll->coll_allgather_module;

    old_reduce = comm->c_coll->coll_reduce;
    old_reduce_module = comm->c_coll->coll_reduce_module;

    old_bcast = comm->c_coll->coll_bcast;
    old_bcast_module = comm->c_coll->coll_bcast_module;

    old_gather = comm->c_coll->coll_gather;
    old_gather_module = comm->c_coll->coll_gather_module;

    comm->c_coll->coll_allreduce = han_module->previous_allreduce;
    comm->c_coll->coll_allreduce_module = han_module->previous_allreduce_module;

    comm->c_coll->coll_allgather = han_module->previous_allgather;
    comm->c_coll->coll_allgather_module = han_module->previous_allgather_module;

    comm->c_coll->coll_reduce = han_module->previous_reduce;
    comm->c_coll->coll_reduce_module = han_module->previous_reduce_module;

    comm->c_coll->coll_bcast = han_module->previous_bcast;
    comm->c_coll->coll_bcast_module = han_module->previous_bcast_module;

    comm->c_coll->coll_gather = han_module->previous_gather;
    comm->c_coll->coll_gather_module = han_module->previous_gather_module;

    /* Create topological sub-communicators */
    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    origin_priority = NULL;
    mca_base_var_find_by_name("coll_han_priority", &han_var_id);
    mca_base_var_get_value(han_var_id, &origin_priority, NULL, NULL);

    /*
     * Maximum priority for selector on sub-communicators
     */
    tmp_han_priority = 100;
    mca_base_var_set_flag(han_var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(han_var_id, &tmp_han_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    /*
     * This sub-communicator contains the ranks that share my node.
     */
    mca_coll_han_component.topo_level = INTRA_NODE;
    create_intranode_comm_new(comm, low_comm);

    /*
     * Get my local rank and the local size
     */
    low_size = ompi_comm_size(*low_comm);
    low_rank = ompi_comm_rank(*low_comm);

    /*
     * This sub-communicator contains one process per node: processes with the
     * same intra-node rank id share such a sub-communicator
     */
    mca_coll_han_component.topo_level = INTER_NODE;
    create_internode_comm_new(comm, w_rank, low_rank, up_comm);

    up_rank = ompi_comm_rank(*up_comm);

    /*
     * Set my virtual rank number.
     * my rank # = <intra-node comm size> * <inter-node rank number>
     *             + <intra-node rank number>
     * WARNING: this formula works only if the ranks are perfectly spread over
     *          the nodes
     * TODO: find a better way of doing
     */
    vrank = low_size * up_rank + low_rank;
    vranks = (int *)malloc(sizeof(int) * w_size);
    /*
     * gather vrank from each process so every process will know other processes
     * vrank
     */
    comm->c_coll->coll_allgather(&vrank,
                                1,
                                MPI_INT,
                                vranks,
                                1,
                                MPI_INT,
                                comm,
                                comm->c_coll->coll_allgather_module);

    /*
     * Set the cached info
     */
    han_module->cached_vranks = vranks;

    /*
     * Come back to the original han module priority
     */
    mca_base_var_set_value(han_var_id, origin_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    /* Put allreduce, allgather, reduce and bcast back */
    comm->c_coll->coll_allreduce = old_allreduce;
    comm->c_coll->coll_allreduce_module = old_allreduce_module;

    comm->c_coll->coll_allgather = old_allgather;
    comm->c_coll->coll_allgather_module = old_allgather_module;

    comm->c_coll->coll_reduce = old_reduce;
    comm->c_coll->coll_reduce_module = old_reduce_module;

    comm->c_coll->coll_bcast = old_bcast;
    comm->c_coll->coll_bcast_module = old_bcast_module;

    comm->c_coll->coll_gather = old_gather;
    comm->c_coll->coll_gather_module = old_gather_module;

    mca_coll_han_component.topo_level = GLOBAL_COMMUNICATOR;
}

/**
 * Create a sub-communicator containing the ranks that share my node.
 * Associate this sub-communicator a given collective module.
 * module can be one of:
 *    . sm
 *    . shared
 *
 * @param comm (IN)          original communicator for the collective
 * @param prio_string (IN)   string containing the mca variable associated to
 *                           target module priority
 * @param my_rank (IN)       my rank in comm
 * @param sub_comm (OUT)     created sub-communicator
 */
static void create_intranode_comm(ompi_communicator_t *comm,
                                  const char *prio_string,
                                  int my_rank,
                                  ompi_communicator_t **sub_comm)
{
    int var_id;
    const int *sav_priority;
    int tmp_priority = 100;

    /*
     * Upgrade the target module priority to make the resulting sub-communicator
     * use that collective module
     */
    mca_base_var_find_by_name(prio_string, &var_id);
    mca_base_var_get_value(var_id, &sav_priority, NULL, NULL);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d] %s origin %d\n",
                         my_rank, prio_string, *sav_priority));

    mca_base_var_set_flag(var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(var_id, &tmp_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);
    /*
     * Create the sub-communicator
     * Since the target priority has been set to the highest value, this
     * sub-communicator will inherit it as a collective module.
     */
    ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0,
                         (opal_info_t *)(&ompi_mpi_info_null), sub_comm);
    /*
     * Come back to the target module's original priority
     */
    mca_base_var_set_value(var_id, sav_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    return;
}

/**
 * Create a sub-communicator containing one rank per node.
 * Associate this sub-communicator a given collective module.
 * module can be one of:
 *    . libnbc
 *    . adapt
 *
 * @param comm (IN)          original communicator for the collective
 * @param prio_string (IN)   string containing the mca variable associated to
 *                           target module priority
 * @param my_rank (IN)       my rank in comm
 * @param intra_rank (IN)    local rank in the intra-node sub-communicator
 * @param sub_comm (OUT)     created sub-communicator
 */
static void create_internode_comm(ompi_communicator_t *comm,
                                  const char *prio_string,
                                  int my_rank,
                                  int intra_rank,
                                  ompi_communicator_t **sub_comm)
{
    int var_id;
    const int *sav_priority;
    int tmp_priority = 100;

    /*
     * Upgrade the target module priority to make the resulting sub-communicator
     * use that collective module
     */
    mca_base_var_find_by_name(prio_string, &var_id);
    mca_base_var_get_value(var_id, &sav_priority, NULL, NULL);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                        "[%d] %s origin %d\n", my_rank, prio_string,
                        *sav_priority));
    mca_base_var_set_flag(var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(var_id, &tmp_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    /*
     * Create the sub-communicator
     * Since the target priority has been set to the highest value, this
     * sub-communicator will inherit it as a collective module.
     */
    ompi_comm_split(comm, intra_rank, my_rank, sub_comm, false);
    mca_base_var_set_value(var_id, sav_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    return;
}


/*
 * Routine that creates the local hierarchical sub-communicators
 * Called each time a collective is called.
 * comm: input communicator of the collective
 */
void mca_coll_han_comm_create(struct ompi_communicator_t *comm,
                                 mca_coll_han_module_t *han_module)
{
    int low_rank, low_size;
    int up_rank;
    int w_rank;
    int w_size;
    ompi_communicator_t **low_comms;
    ompi_communicator_t **up_comms;
    const int *origin_priority;
    int han_var_id;
    int tmp_han_priority;
    int vrank, *vranks;

    mca_coll_base_module_allreduce_fn_t old_allreduce;
    mca_coll_base_module_t *old_allreduce_module;
    mca_coll_base_module_allgather_fn_t old_allgather;
    mca_coll_base_module_t *old_allgather_module;

    /* use cached communicators if possible */
    if (han_module->cached_comm == comm &&
                han_module->cached_low_comms != NULL &&
                han_module->cached_up_comms != NULL &&
                han_module->cached_vranks != NULL) {
        return;
    }

    /* We cannot use han allreduce and allgather without sub-communicators
     * Temporary set previous ones */
    old_allreduce = comm->c_coll->coll_allreduce;
    old_allreduce_module = comm->c_coll->coll_allreduce_module;

    old_allgather = comm->c_coll->coll_allgather;
    old_allgather_module = comm->c_coll->coll_allgather_module;

    comm->c_coll->coll_allreduce = han_module->previous_allreduce;
    comm->c_coll->coll_allreduce_module = han_module->previous_allreduce_module;

    comm->c_coll->coll_allgather = han_module->previous_allgather;
    comm->c_coll->coll_allgather_module = han_module->previous_allgather_module;

    /* create communicators if there is no cached communicator */

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);
    low_comms = (struct ompi_communicator_t **)malloc(COLL_HAN_LOW_MODULES *
                                                      sizeof(struct ompi_communicator_t *));
    up_comms = (struct ompi_communicator_t **)malloc(COLL_HAN_UP_MODULES *
                                                     sizeof(struct ompi_communicator_t *));
    origin_priority = NULL;
    mca_base_var_find_by_name("coll_han_priority", &han_var_id);
    mca_base_var_get_value(han_var_id, &origin_priority, NULL, NULL);

    /*
     * Lower down our current priority
     */
    tmp_han_priority = 0;
    mca_base_var_set_flag(han_var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(han_var_id, &tmp_han_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    /*
     * Upgrade sm module priority to set up low_comms[0] with sm module
     * This sub-communicator contains the ranks that share my node.
     */
    create_intranode_comm(comm, "coll_sm_priority", w_rank, &(low_comms[0]));

    /*
     * Get my local rank and the local size
     */
    low_size = ompi_comm_size(low_comms[0]);
    low_rank = ompi_comm_rank(low_comms[0]);

    /*
     * Upgrade shared module priority to set up low_comms[1] with shared module
     * This sub-communicator contains the ranks that share my node.
     */
    create_intranode_comm(comm, "coll_shared_priority", w_rank, &(low_comms[1]));

    /*
     * Upgrade libnbc module priority to set up up_comms[0] with libnbc module
     * This sub-communicator contains one process per node: processes with the
     * same intra-node rank id share such a sub-communicator
     */
    create_internode_comm(comm, "coll_libnbc_priority", w_rank, low_rank,
                          &(up_comms[0]));

    up_rank = ompi_comm_rank(up_comms[0]);

    /*
     * Upgrade adapt module priority to set up up_comms[0] with adapt module
     * This sub-communicator contains one process per node.
     */
    create_internode_comm(comm, "coll_adapt_priority", w_rank, low_rank,
                          &(up_comms[1]));

    /*
     * Set my virtual rank number.
     * my rank # = <intra-node comm size> * <inter-node rank number>
     *             + <intra-node rank number>
     * WARNING: this formula works only if the ranks are perfectly spread over
     *          the nodes
     * TODO: find a better way of doing
     */
    vrank = low_size * up_rank + low_rank;
    vranks = (int *)malloc(sizeof(int) * w_size);
    /*
     * gather vrank from each process so every process will know other processes
     * vrank
     */
    comm->c_coll->coll_allgather(&vrank, 1, MPI_INT, vranks, 1, MPI_INT, comm,
                                 comm->c_coll->coll_allgather_module);

    /*
     * Set the cached info
     */
    han_module->cached_comm = comm;
    han_module->cached_low_comms = low_comms;
    han_module->cached_up_comms = up_comms;
    han_module->cached_vranks = vranks;

    /*
     * Come back to the original han module priority
     */
    mca_base_var_set_value(han_var_id, origin_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);

    /* Put allreduce and allgather back */
    comm->c_coll->coll_allreduce = old_allreduce;
    comm->c_coll->coll_allreduce_module = old_allreduce_module;

    comm->c_coll->coll_allgather = old_allgather;
    comm->c_coll->coll_allgather_module = old_allgather_module;
}


