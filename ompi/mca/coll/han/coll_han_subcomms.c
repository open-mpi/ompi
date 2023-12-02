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

#define HAN_SUBCOM_SAVE_COLLECTIVE(FALLBACKS, COMM, HANM, COLL)                  \
    do {                                                                         \
        (FALLBACKS).COLL.module_fn.COLL = (COMM)->c_coll->coll_ ## COLL;         \
        (FALLBACKS).COLL.module = (COMM)->c_coll->coll_ ## COLL ## _module;      \
        (COMM)->c_coll->coll_ ## COLL = (HANM)->fallback.COLL.module_fn.COLL;    \
        (COMM)->c_coll->coll_ ## COLL ## _module = (HANM)->fallback.COLL.module; \
    } while(0)

#define HAN_SUBCOM_LOAD_COLLECTIVE(FALLBACKS, COMM, HANM, COLL)                  \
    do {                                                                         \
        (COMM)->c_coll->coll_ ## COLL = (FALLBACKS).COLL.module_fn.COLL;         \
        (COMM)->c_coll->coll_ ## COLL ## _module = (FALLBACKS).COLL.module;      \
    } while(0)

/*
 * Routine that creates the local hierarchical sub-communicators
 * Called each time a collective is called.
 * comm: input communicator of the collective
 */
int mca_coll_han_comm_create_new(struct ompi_communicator_t *comm,
                                 mca_coll_han_module_t *han_module)
{
    int low_rank, low_size, up_rank, w_rank, w_size;
    ompi_communicator_t **low_comm = &(han_module->sub_comm[INTRA_NODE]);
    ompi_communicator_t **up_comm = &(han_module->sub_comm[INTER_NODE]);
    mca_coll_han_collectives_fallback_t fallbacks;
    int rc = OMPI_SUCCESS, vrank, *vranks;
    opal_info_t comm_info;

    /* The sub communicators have already been created */
    if (han_module->enabled && NULL != han_module->sub_comm[INTRA_NODE]
        && NULL != han_module->sub_comm[INTER_NODE]
        && NULL != han_module->cached_vranks) {
        return OMPI_SUCCESS;
    }

    /*
     * We cannot use han allreduce and allgather without sub-communicators,
     * but we are in the creation of the data structures for the HAN, and
     * temporarily need to save back the old collective.
     *
     * Allgather is used to compute vranks
     * Allreduce is used by ompi_comm_split_type in create_intranode_comm_new
     * Reduce + Bcast may be called by the allreduce implementation
     * Gather + Bcast may be called by the allgather implementation
     */
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allgather);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allreduce);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, bcast);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, reduce);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, gather);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, scatter);

    /**
     * HAN is not yet optimized for a single process per node case, we should
     * avoid selecting it for collective communication support in such cases.
     * However, in order to decide if this is true, we need to know how many
     * local processes are on each node, a condition that cannot be verified
     * outside the MPI support (with PRRTE the info will be eventually available,
     * but we don't want to delay anything until then). We can achieve the same
     * goal by using a reduction over the maximum number of peers per node among
     * all participants.
     */
    int local_procs = ompi_group_count_local_peers(comm->c_local_group);
    rc = comm->c_coll->coll_allreduce(MPI_IN_PLACE, &local_procs, 1, MPI_INT,
                                      MPI_MAX, comm,
                                      comm->c_coll->coll_allreduce_module);
    if( OMPI_SUCCESS != rc ) {
        goto return_with_error;
    }
    if( local_procs == 1 ) {
        /* restore saved collectives */
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgather);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allreduce);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, bcast);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, reduce);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, gather);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, scatter);
        han_module->enabled = false;  /* entire module set to pass-through from now on */
        return OMPI_ERR_NOT_SUPPORTED;
    }

    OBJ_CONSTRUCT(&comm_info, opal_info_t);

    /* Create topological sub-communicators */
    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    /*
     * This sub-communicator contains the ranks that share my node.
     */
    opal_info_set(&comm_info, "ompi_comm_coll_preference", "han");
    opal_info_set(&comm_info, "ompi_comm_coll_han_topo_level", "INTRA_NODE");
    rc = ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0,
                              &comm_info, low_comm);
    if( OMPI_SUCCESS != rc ) {
        /* cannot create subcommunicators. Return the error upstream */
        goto return_with_error;
    }

    /*
     * Get my local rank and the local size
     */
    low_size = ompi_comm_size(*low_comm);
    low_rank = ompi_comm_rank(*low_comm);

    /*
     * This sub-communicator contains one process per node: processes with the
     * same intra-node rank id share such a sub-communicator
     */
    opal_info_set(&comm_info, "ompi_comm_coll_han_topo_level", "INTER_NODE");
    rc = ompi_comm_split_with_info(comm, low_rank, w_rank, &comm_info, up_comm, false);
    if( OMPI_SUCCESS != rc ) {
        /* cannot create subcommunicators. Return the error upstream */
        goto return_with_error;
    }

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
    rc = comm->c_coll->coll_allgather(&vrank, 1, MPI_INT,
                                 vranks, 1, MPI_INT,
                                 comm, comm->c_coll->coll_allgather_module);
    if( OMPI_SUCCESS != rc ) {
        /* cannot create subcommunicators. Return the error upstream */
        goto return_with_error;
    }

    /*
     * Set the cached info
     */
    han_module->cached_vranks = vranks;

    /* Reset the saved collectives to point back to HAN */
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgather);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allreduce);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, bcast);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, reduce);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, gather);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, scatter);

    OBJ_DESTRUCT(&comm_info);
    return OMPI_SUCCESS;

return_with_error:
    if( NULL != *low_comm ) {
        ompi_comm_free(low_comm);
        *low_comm = NULL;  /* don't leave the MPI_COMM_NULL set by ompi_comm_free */
    }
    if( NULL != *up_comm ) {
        ompi_comm_free(up_comm);
        *up_comm = NULL;  /* don't leave the MPI_COMM_NULL set by ompi_comm_free */
    }
    return rc;
}

/*
 * Routine that creates the local hierarchical sub-communicators
 * Called each time a collective is called.
 * comm: input communicator of the collective
 */
int mca_coll_han_comm_create(struct ompi_communicator_t *comm,
                             mca_coll_han_module_t *han_module)
{
    int low_rank, low_size, up_rank, w_rank, w_size;
    mca_coll_han_collectives_fallback_t fallbacks;
    ompi_communicator_t **low_comms;
    ompi_communicator_t **up_comms;
    int vrank, *vranks;
    opal_info_t comm_info;

    /* use cached communicators if possible */
    if (han_module->enabled && han_module->cached_low_comms != NULL &&
        han_module->cached_up_comms != NULL &&
        han_module->cached_vranks != NULL) {
        return OMPI_SUCCESS;
    }

    /*
     * We cannot use han allreduce and allgather without sub-communicators,
     * but we are in the creation of the data structures for the HAN, and
     * temporarily need to save back the old collective.
     *
     * Allgather is used to compute vranks
     * Allreduce is used by ompi_comm_split_type in create_intranode_comm_new
     * Reduce + Bcast may be called by the allreduce implementation
     * Gather + Bcast may be called by the allgather implementation
     */
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allgather);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, allreduce);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, bcast);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, reduce);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, gather);
    HAN_SUBCOM_SAVE_COLLECTIVE(fallbacks, comm, han_module, scatter);

    /**
     * HAN is not yet optimized for a single process per node case, we should
     * avoid selecting it for collective communication support in such cases.
     * However, in order to decide if this is tru, we need to know how many
     * local processes are on each node, a condition that cannot be verified
     * outside the MPI support (with PRRTE the info will be eventually available,
     * but we don't want to delay anything until then). We can achieve the same
     * goal by using a reduction over the maximum number of peers per node among
     * all participants.
     */
    int local_procs = ompi_group_count_local_peers(comm->c_local_group);
    comm->c_coll->coll_allreduce(MPI_IN_PLACE, &local_procs, 1, MPI_INT,
                                 MPI_MAX, comm,
                                 comm->c_coll->coll_allreduce_module);
    if( local_procs == 1 ) {
        /* restore saved collectives */
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgather);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allreduce);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, bcast);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, reduce);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, gather);
        HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, scatter);
        han_module->enabled = false;  /* entire module set to pass-through from now on */
        return OMPI_ERR_NOT_SUPPORTED;
    }

    /* create communicators if there is no cached communicator */
    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);
    low_comms = (struct ompi_communicator_t **)malloc(COLL_HAN_LOW_MODULES *
                                                      sizeof(struct ompi_communicator_t *));
    up_comms = (struct ompi_communicator_t **)malloc(COLL_HAN_UP_MODULES *
                                                     sizeof(struct ompi_communicator_t *));

    OBJ_CONSTRUCT(&comm_info, opal_info_t);

    /*
     * Upgrade sm module priority to set up low_comms[0] with sm module
     * This sub-communicator contains the ranks that share my node.
     */
    opal_info_set(&comm_info, "ompi_comm_coll_preference", "tuned,^han");
    ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0,
                         &comm_info, &(low_comms[0]));
    assert(OMPI_COMM_IS_DISJOINT_SET(low_comms[0]) && !OMPI_COMM_IS_DISJOINT(low_comms[0]));

    /*
     * Get my local rank and the local size
     */
    low_size = ompi_comm_size(low_comms[0]);
    low_rank = ompi_comm_rank(low_comms[0]);

    /*
     * Upgrade shared module priority to set up low_comms[1] with shared module
     * This sub-communicator contains the ranks that share my node.
     */
    opal_info_set(&comm_info, "ompi_comm_coll_preference", "sm,^han");
    ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0,
                         &comm_info, &(low_comms[1]));
    assert(OMPI_COMM_IS_DISJOINT_SET(low_comms[1]) && !OMPI_COMM_IS_DISJOINT(low_comms[1]));

    /*
     * Upgrade libnbc module priority to set up up_comms[0] with libnbc module
     * This sub-communicator contains one process per node: processes with the
     * same intra-node rank id share such a sub-communicator
     */
    opal_info_set(&comm_info, "ompi_comm_coll_preference", "libnbc,^han");
    ompi_comm_split_with_info(comm, low_rank, w_rank, &comm_info, &(up_comms[0]), false);
    up_rank = ompi_comm_rank(up_comms[0]);
    assert(OMPI_COMM_IS_DISJOINT_SET(up_comms[0]) && OMPI_COMM_IS_DISJOINT(up_comms[0]));

    /*
     * Upgrade adapt module priority to set up up_comms[0] with adapt module
     * This sub-communicator contains one process per node.
     */
    opal_info_set(&comm_info, "ompi_comm_coll_preference", "adapt,^han");
    ompi_comm_split_with_info(comm, low_rank, w_rank, &comm_info, &(up_comms[1]), false);
    assert(OMPI_COMM_IS_DISJOINT_SET(up_comms[1]) && OMPI_COMM_IS_DISJOINT(up_comms[1]));

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
    han_module->cached_low_comms = low_comms;
    han_module->cached_up_comms = up_comms;
    han_module->cached_vranks = vranks;

    /* Reset the saved collectives to point back to HAN */
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgatherv);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allgather);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, allreduce);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, bcast);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, reduce);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, gather);
    HAN_SUBCOM_LOAD_COLLECTIVE(fallbacks, comm, han_module, scatter);

    OBJ_DESTRUCT(&comm_info);
    return OMPI_SUCCESS;
}
