/**
  Copyright (c) 2020      Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_MCCL_H
#define MCA_COLL_MCCL_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/memory/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/op/op.h"

#include "orte/runtime/orte_globals.h"

#include "api/mccl.h"

#include "coll_mccl_debug.h"
#ifndef MCCL_VERSION
#define MCCL_VERSION(major, minor) (((major)<<MCCL_MAJOR_BIT)|((minor)<<MCCL_MINOR_BIT))
#endif
BEGIN_C_DECLS

struct mca_coll_mccl_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int mccl_priority;

    /** MCA parameter: Verbose level of this component */
    int mccl_verbose;

    /** MCA parameter: Enable MCCL */
    int   mccl_enable;

    /** r/o MCA parameter: libmccl compiletime version */
    char* compiletime_version;

    /** r/o MCA parameter: libmccl runtime version */
    const char* runtime_version;

    /** MCA parameter: Minimal number of processes in the communicator
        for the corresponding mccl context to be created */
    int mccl_np;

    /** Whether or not mccl_init was ever called */
    bool libmccl_initialized;
    mccl_context_h mccl_context;
    opal_free_list_t requests;
};
typedef struct mca_coll_mccl_component_t mca_coll_mccl_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_mccl_component_t mca_coll_mccl_component;

/**
 * MCCL enabled communicator
 */
struct mca_coll_mccl_module_t {
    mca_coll_base_module_t              super;
    ompi_communicator_t*                comm;
    int                                 rank;
    mccl_comm_h                          mccl_comm;
    mca_coll_base_module_allreduce_fn_t previous_allreduce;
    mca_coll_base_module_t*             previous_allreduce_module;
    mca_coll_base_module_barrier_fn_t   previous_barrier;
    mca_coll_base_module_t*             previous_barrier_module;
    mca_coll_base_module_bcast_fn_t     previous_bcast;
    mca_coll_base_module_t*             previous_bcast_module;
};
typedef struct mca_coll_mccl_module_t mca_coll_mccl_module_t;
OBJ_CLASS_DECLARATION(mca_coll_mccl_module_t);



int mca_coll_mccl_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_mccl_comm_query(struct ompi_communicator_t *comm, int *priority);


int mca_coll_mccl_allreduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op, struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);
int mca_coll_mccl_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);
int mca_coll_mccl_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

END_C_DECLS
#endif
