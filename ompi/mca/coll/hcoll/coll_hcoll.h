/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_FCA_H
#define MCA_COLL_FCA_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"

#include "orte/runtime/orte_globals.h"

#include "hcoll_api.h"
#include "hcoll_constants.h"


#include "coll_hcoll_debug.h"
BEGIN_C_DECLS


/**
 * Globally exported structure
 */

typedef struct mca_coll_hcoll_ops_t {
    int (*hcoll_init) (void);
    int (*hcoll_finalize) (void);
    void * (*create_hcoll_context)(void *);
    int (*hcoll_barrier)(void *);
} mca_coll_hcoll_ops_t;

struct mca_coll_hcoll_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int hcoll_priority;

    /** MCA parameter: Verbose level of this component */
    int hcoll_verbose;

    /** MCA parameter: Path to libfca.so */
    char* hcoll_lib_path;

    /** MCA parameter: Enable FCA */
    int   hcoll_enable;

    /* FCA global stuff */
    void *hcoll_lib_handle;                               /* FCA dynamic library */
    mca_coll_hcoll_ops_t hcoll_ops;
    ompi_free_list_t requests;
};
typedef struct mca_coll_hcoll_component_t mca_coll_hcoll_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_hcoll_component_t mca_coll_hcoll_component;


/**
 * FCA enabled communicator
 */
struct mca_coll_hcoll_module_t {
    mca_coll_base_module_t super;

    MPI_Comm            comm;
    int                 rank;
    void *hcoll_context;
    /* Saved handlers - for fallback */
    mca_coll_base_module_reduce_fn_t previous_reduce;
    mca_coll_base_module_t *previous_reduce_module;
    mca_coll_base_module_allreduce_fn_t previous_allreduce;
    mca_coll_base_module_t *previous_allreduce_module;
    mca_coll_base_module_bcast_fn_t previous_bcast;
    mca_coll_base_module_t *previous_bcast_module;
    mca_coll_base_module_barrier_fn_t previous_barrier;
    mca_coll_base_module_t *previous_barrier_module;
    mca_coll_base_module_allgather_fn_t previous_allgather;
    mca_coll_base_module_t *previous_allgather_module;
    mca_coll_base_module_allgatherv_fn_t previous_allgatherv;
    mca_coll_base_module_t *previous_allgatherv_module;
    mca_coll_base_module_alltoall_fn_t previous_alltoall;
    mca_coll_base_module_t *previous_alltoall_module;
    mca_coll_base_module_alltoallv_fn_t previous_alltoallv;
    mca_coll_base_module_t *previous_alltoallv_module;
    mca_coll_base_module_alltoallw_fn_t previous_alltoallw;
    mca_coll_base_module_t *previous_alltoallw_module;
    mca_coll_base_module_gather_fn_t previous_gather;
    mca_coll_base_module_t *previous_gather_module;
    mca_coll_base_module_gatherv_fn_t previous_gatherv;
    mca_coll_base_module_t *previous_gatherv_module;
    mca_coll_base_module_reduce_scatter_fn_t previous_reduce_scatter;
    mca_coll_base_module_t *previous_reduce_scatter_module;
};
typedef struct mca_coll_hcoll_module_t mca_coll_hcoll_module_t;

OBJ_CLASS_DECLARATION(mca_coll_hcoll_module_t);


/* API functions */
int mca_coll_hcoll_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_hcoll_comm_query(struct ompi_communicator_t *comm, int *priority);
int mca_coll_hcoll_get_lib(void);
void hcoll_rte_fns_setup(void);


int mca_coll_hcoll_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_hcoll_bcast(void *buff, int count,
                        struct ompi_datatype_t *datatype, int root,
                        struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_hcoll_allgather(void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_allreduce(void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_alltoall(void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void* rbuf, int rcount,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

END_C_DECLS

#endif
