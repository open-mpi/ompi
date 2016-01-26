/**
  Copyright (c) 2011      Mellanox Technologies. All rights reserved.
  Copyright (c) 2015      Research Organization for Information Science
                          and Technology (RIST). All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_FCA_H
#define MCA_COLL_FCA_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/op/op.h"

#include "orte/runtime/orte_globals.h"

#include "hcoll/api/hcoll_api.h"
#include "hcoll/api/hcoll_constants.h"


#include "coll_hcoll_debug.h"
#ifndef HCOLL_VERSION
#define HCOLL_VERSION(major, minor) (((major)<<HCOLL_MAJOR_BIT)|((minor)<<HCOLL_MINOR_BIT))
#endif
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

    /** MCA parameter: Enable FCA */
    int   hcoll_enable;

    /** r/o MCA parameter: libhcoll compiletime version */
    char* compiletime_version;

    /** r/o MCA parameter: libhcoll runtime version */
    const char* runtime_version;

    /** MCA parameter: Minimal number of processes in the communicator
        for the corresponding hcoll context to be created */
    int hcoll_np;

    /** Whether or not hcoll_init was ever called */
    bool libhcoll_initialized;

    bool using_mem_hooks;

    /** MCA parameter: ON/OFF user defined datatype through HCOLL */
    int   hcoll_datatype_fallback;

#if HCOLL_API >= HCOLL_VERSION(3,2)
    /* hcoll init options */
    hcoll_init_opts_t *init_opts;
#endif

    /* FCA global stuff */
    mca_coll_hcoll_ops_t hcoll_ops;
    opal_free_list_t requests;
};
typedef struct mca_coll_hcoll_component_t mca_coll_hcoll_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_hcoll_component_t mca_coll_hcoll_component;




/**
 * FCA enabled communicator
 */
struct mca_coll_hcoll_module_t {
    mca_coll_base_module_t super;

    ompi_communicator_t            *comm;
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
    mca_coll_base_module_ibcast_fn_t previous_ibcast;
    mca_coll_base_module_t *previous_ibcast_module;
    mca_coll_base_module_ibarrier_fn_t previous_ibarrier;
    mca_coll_base_module_t *previous_ibarrier_module;
    mca_coll_base_module_iallgather_fn_t previous_iallgather;
    mca_coll_base_module_t *previous_iallgather_module;
    mca_coll_base_module_iallgatherv_fn_t previous_iallgatherv;
    mca_coll_base_module_t *previous_iallgatherv_module;
    mca_coll_base_module_iallreduce_fn_t previous_iallreduce;
    mca_coll_base_module_t *previous_iallreduce_module;
    mca_coll_base_module_ireduce_fn_t previous_ireduce;
    mca_coll_base_module_t *previous_ireduce_module;
    mca_coll_base_module_igatherv_fn_t previous_igatherv;
    mca_coll_base_module_t *previous_igatherv_module;
    mca_coll_base_module_ialltoall_fn_t previous_ialltoall;
    mca_coll_base_module_t *previous_ialltoall_module;
    mca_coll_base_module_ialltoallv_fn_t previous_ialltoallv;
    mca_coll_base_module_t *previous_ialltoallv_module;
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

int mca_coll_hcoll_allgather(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_allgatherv(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcount,
                            const int *displs,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_gather(const void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

int mca_coll_hcoll_allreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_reduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_alltoall(const void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void* rbuf, int rcount,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_hcoll_alltoallv(const void *sbuf, const int *scounts,
                            const int *sdisps,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcounts,
                            const int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_gatherv(const void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_ibarrier(struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_ibcast(void *buff, int count,
                            struct ompi_datatype_t *datatype, int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_iallgather(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_iallgatherv(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcount,
                            const int *displs,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_iallreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_ireduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_ialltoall(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t **req,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_ialltoallv(const void *sbuf, int *scounts,
                            int *sdisps,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int *rcounts,
                            int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t **req,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_igatherv(const void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module);

int mca_coll_hcoll_progress(void);
void mca_coll_hcoll_mem_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc);
END_C_DECLS

#endif
