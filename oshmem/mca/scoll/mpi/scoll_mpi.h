/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_SCOLL_MPI_H
#define MCA_SCOLL_MPI_H

#include "oshmem_config.h"

#include "shmem.h"
#include "opal/mca/mca.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/request/request.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/proc/proc.h"
#include "ompi/communicator/communicator.h"

#include "orte/runtime/orte_globals.h"

#include "scoll_mpi_debug.h"
BEGIN_C_DECLS


/**
 * Globally exported structure
 */

struct mca_scoll_mpi_component_t {
    /** Base coll component */
    mca_scoll_base_component_1_0_0_t super;

    /** MCA parameter: Priority of this component */
    int mpi_priority;

    /** MCA parameter: Verbose level of this component */
    int mpi_verbose;

    /** MCA parameter: Enable MPI */
    int   mpi_enable;

    /** MCA parameter: Minimal number of processes in the communicator
        for the corresponding mpi context to be created */
    int mpi_np;
};
typedef struct mca_scoll_mpi_component_t mca_scoll_mpi_component_t;

OMPI_MODULE_DECLSPEC extern mca_scoll_mpi_component_t mca_scoll_mpi_component;


/**
 * MPI enabled communicator
 */
struct mca_scoll_mpi_module_t {
    mca_scoll_base_module_t super;

    ompi_communicator_t             *comm;
    int                             rank;
    /* Saved handlers - for fallback */
    mca_scoll_base_module_reduce_fn_t previous_reduce;
    mca_scoll_base_module_t *previous_reduce_module;
    mca_scoll_base_module_broadcast_fn_t previous_broadcast;
    mca_scoll_base_module_t *previous_broadcast_module;
    mca_scoll_base_module_barrier_fn_t previous_barrier;
    mca_scoll_base_module_t *previous_barrier_module;
    mca_scoll_base_module_collect_fn_t previous_collect;
    mca_scoll_base_module_t *previous_collect_module;
};
typedef struct mca_scoll_mpi_module_t mca_scoll_mpi_module_t;

OBJ_CLASS_DECLARATION(mca_scoll_mpi_module_t);


/* API functions */
int mca_scoll_mpi_init_query(bool enable_progress_threads, bool enable_mpi_threads);

mca_scoll_base_module_t* mca_scoll_mpi_comm_query(oshmem_group_t *osh_group, int *priority);

int mca_scoll_mpi_barrier(struct oshmem_group_t *group, long *pSync, int alg);

int mca_scoll_mpi_broadcast(struct oshmem_group_t *group,
                            int PE_root,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            int alg);

int mca_scoll_mpi_collect(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nlong,
                          long *pSync,
                          bool nlong_type,
                          int alg);

int mca_scoll_mpi_reduce(struct oshmem_group_t *group,
                         struct oshmem_op_t *op,
                         void *target,
                         const void *source,
                         size_t nlong,
                         long *pSync,
                         void *pWrk,
                         int alg);

END_C_DECLS

#endif
