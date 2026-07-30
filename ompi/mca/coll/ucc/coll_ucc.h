/**
  Copyright (c) 2021      Mellanox Technologies. All rights reserved.
  Copyright (c) 2022-2026 NVIDIA Corporation.  All rights reserved.
  Copyright (c) 2025      Fujitsu Limited. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_UCC_H
#define MCA_COLL_UCC_H

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/memory/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/op/op.h"
#include "coll_ucc_debug.h"
#include <ucc/api/ucc.h>

BEGIN_C_DECLS

#define COLL_UCC_CTS (UCC_COLL_TYPE_BARRIER        | UCC_COLL_TYPE_BCAST    | \
                      UCC_COLL_TYPE_ALLREDUCE      | UCC_COLL_TYPE_ALLTOALL | \
                      UCC_COLL_TYPE_ALLTOALLV      | UCC_COLL_TYPE_ALLGATHER | \
                      UCC_COLL_TYPE_REDUCE         | UCC_COLL_TYPE_ALLGATHERV | \
                      UCC_COLL_TYPE_GATHER         | UCC_COLL_TYPE_GATHERV | \
                      UCC_COLL_TYPE_REDUCE_SCATTER | UCC_COLL_TYPE_REDUCE_SCATTERV | \
                      UCC_COLL_TYPE_SCATTERV       | UCC_COLL_TYPE_SCATTER)

#define COLL_UCC_CTS_STR "barrier,bcast,allreduce,alltoall,alltoallv,allgather," \
                         "allgatherv,reduce,gather,gatherv,reduce_scatter_block,"\
                         "reduce_scatter,scatterv,scatter," \
                         "ibarrier,ibcast,iallreduce,ialltoall,ialltoallv,iallgather,"\
                         "iallgatherv,ireduce,igather,igatherv,ireduce_scatter_block,"\
                         "ireduce_scatter,iscatterv,iscatter"

typedef struct mca_coll_ucc_req {
    ompi_request_t super;
    ucc_coll_req_h ucc_req;
} mca_coll_ucc_req_t;
OBJ_CLASS_DECLARATION(mca_coll_ucc_req_t);

/**
 * A UCC "OOB domain": the intermediary that owns a single UCC context and
 * the out-of-band (OOB) bootstrap communicator used to create that context.
 *
 * The OOB is implemented over an MPI communicator (PML point-to-point).
 * UCC only uses it to bootstrap the context (an allgather at context create,
 * and context teardown); team create/destroy and all normal collectives are
 * collective over the team itself and do not touch the OOB.  A domain is
 * shared (refcounted) by the communicator that created it and every
 * communicator derived from it, so a whole family reuses one heavyweight UCC
 * context and addresses into it through an ep map.
 *
 * Because the context outlives the user communicator that bootstrapped it
 * (e.g. the user may free the bootstrap communicator while derived
 * communicators are still alive), the domain holds its own reference to
 * @comm (OBJ_RETAIN at creation, OBJ_RELEASE when the context is destroyed).
 * The bootstrap communicator therefore stays valid for the OOB for as long
 * as the context exists.  Communicators created without a usable parent
 * domain (MPI_Comm_create_from_group, MPI_Intercomm_merge) bootstrap their
 * own domain, since no existing context is guaranteed to span their ranks.
 */
typedef struct mca_coll_ucc_oob_domain_t {
    opal_list_item_t     super;
    /* Bootstrap communicator backing the OOB.  Owned reference: retained for
       the lifetime of the domain so the OOB stays usable even after the user
       frees its handle to this communicator. */
    ompi_communicator_t *comm;
    ucc_context_h        ucc_context;
    /* Number of UCC modules (communicators) currently referencing it. */
    int                  refcount;
} mca_coll_ucc_oob_domain_t;
OBJ_CLASS_DECLARATION(mca_coll_ucc_oob_domain_t);

struct mca_coll_ucc_component_t {
    mca_coll_base_component_3_0_0_t super;
    int                             ucc_priority;
    int                             ucc_verbose;
    int                             ucc_enable;
    int                             ucc_np;
    char                           *cls;
    char                           *cts;
    const char                     *compiletime_version;
    const char                     *runtime_version;
    ucc_lib_h                       ucc_lib;
    ucc_lib_attr_t                  ucc_lib_attr;
    ucc_coll_type_t                 cts_requested;
    ucc_coll_type_t                 nb_cts_requested;
    ucc_coll_type_t                 ps_cts_requested;
    /* List of live mca_coll_ucc_oob_domain_t.  Each holds its own UCC
       context; the single shared ucc_lib is created with the first domain
       and finalized with the last.  The progress callback iterates this
       list to progress every live context. */
    opal_list_t                     domains;
    int                             domain_count;
    /* The UCC library and the per-communicator attribute keyval are created
       lazily with the first domain and persist across domain churn. */
    bool                            keyval_created;
    bool                            requests_initialized;
    opal_free_list_t                requests;
};
typedef struct mca_coll_ucc_component_t mca_coll_ucc_component_t;

OMPI_DECLSPEC extern mca_coll_ucc_component_t mca_coll_ucc_component;

/**
 * UCC enabled communicator
 */
struct mca_coll_ucc_module_t {
    mca_coll_base_module_t                          super;
    ompi_communicator_t*                            comm;
    int                                             rank;
    /* OOB domain (and thus UCC context) used by this communicator. May be
       shared with the parent/ancestor communicator it was derived from. */
    mca_coll_ucc_oob_domain_t*                      domain;
    ucc_team_h                                      ucc_team;
    /* Backing array for the UCC team ep_map when this communicator is an
       arbitrary (non-strided) permutation of the domain's bootstrap
       communicator.  Maps team rank -> bootstrap-comm rank (i.e. context
       endpoint).  NULL for the FULL/STRIDED cases that need no array.  Kept
       alive for the team's lifetime and freed at module destruct. */
    int*                                            ep_map_ranks;
    mca_coll_base_module_allreduce_fn_t             previous_allreduce;
    mca_coll_base_module_t*                         previous_allreduce_module;
    mca_coll_base_module_iallreduce_fn_t            previous_iallreduce;
    mca_coll_base_module_t*                         previous_iallreduce_module;
    mca_coll_base_module_reduce_fn_t                previous_reduce;
    mca_coll_base_module_t*                         previous_reduce_module;
    mca_coll_base_module_ireduce_fn_t               previous_ireduce;
    mca_coll_base_module_t*                         previous_ireduce_module;
    mca_coll_base_module_barrier_fn_t               previous_barrier;
    mca_coll_base_module_t*                         previous_barrier_module;
    mca_coll_base_module_ibarrier_fn_t              previous_ibarrier;
    mca_coll_base_module_t*                         previous_ibarrier_module;
    mca_coll_base_module_bcast_fn_t                 previous_bcast;
    mca_coll_base_module_t*                         previous_bcast_module;
    mca_coll_base_module_ibcast_fn_t                previous_ibcast;
    mca_coll_base_module_t*                         previous_ibcast_module;
    mca_coll_base_module_alltoall_fn_t              previous_alltoall;
    mca_coll_base_module_t*                         previous_alltoall_module;
    mca_coll_base_module_ialltoall_fn_t             previous_ialltoall;
    mca_coll_base_module_t*                         previous_ialltoall_module;
    mca_coll_base_module_alltoallv_fn_t             previous_alltoallv;
    mca_coll_base_module_t*                         previous_alltoallv_module;
    mca_coll_base_module_ialltoallv_fn_t            previous_ialltoallv;
    mca_coll_base_module_t*                         previous_ialltoallv_module;
    mca_coll_base_module_allgather_fn_t             previous_allgather;
    mca_coll_base_module_t*                         previous_allgather_module;
    mca_coll_base_module_iallgather_fn_t            previous_iallgather;
    mca_coll_base_module_t*                         previous_iallgather_module;
    mca_coll_base_module_allgatherv_fn_t            previous_allgatherv;
    mca_coll_base_module_t*                         previous_allgatherv_module;
    mca_coll_base_module_iallgatherv_fn_t           previous_iallgatherv;
    mca_coll_base_module_t*                         previous_iallgatherv_module;
    mca_coll_base_module_gather_fn_t                previous_gather;
    mca_coll_base_module_t*                         previous_gather_module;
    mca_coll_base_module_igather_fn_t               previous_igather;
    mca_coll_base_module_t*                         previous_igather_module;
    mca_coll_base_module_gatherv_fn_t               previous_gatherv;
    mca_coll_base_module_t*                         previous_gatherv_module;
    mca_coll_base_module_igatherv_fn_t              previous_igatherv;
    mca_coll_base_module_t*                         previous_igatherv_module;
    mca_coll_base_module_reduce_scatter_block_fn_t  previous_reduce_scatter_block;
    mca_coll_base_module_t*                         previous_reduce_scatter_block_module;
    mca_coll_base_module_ireduce_scatter_block_fn_t previous_ireduce_scatter_block;
    mca_coll_base_module_t*                         previous_ireduce_scatter_block_module;
    mca_coll_base_module_reduce_scatter_fn_t        previous_reduce_scatter;
    mca_coll_base_module_t*                         previous_reduce_scatter_module;
    mca_coll_base_module_ireduce_scatter_fn_t       previous_ireduce_scatter;
    mca_coll_base_module_t*                         previous_ireduce_scatter_module;
    mca_coll_base_module_scatterv_fn_t              previous_scatterv;
    mca_coll_base_module_t*                         previous_scatterv_module;
    mca_coll_base_module_iscatterv_fn_t             previous_iscatterv;
    mca_coll_base_module_t*                         previous_iscatterv_module;
    mca_coll_base_module_scatter_fn_t               previous_scatter;
    mca_coll_base_module_t*                         previous_scatter_module;
    mca_coll_base_module_iscatter_fn_t              previous_iscatter;
    mca_coll_base_module_t*                         previous_iscatter_module;
    mca_coll_base_module_allreduce_init_fn_t        previous_allreduce_init;
    mca_coll_base_module_t*                         previous_allreduce_init_module;
    mca_coll_base_module_reduce_init_fn_t           previous_reduce_init;
    mca_coll_base_module_t*                         previous_reduce_init_module;
    mca_coll_base_module_barrier_init_fn_t          previous_barrier_init;
    mca_coll_base_module_t*                         previous_barrier_init_module;
    mca_coll_base_module_bcast_init_fn_t            previous_bcast_init;
    mca_coll_base_module_t*                         previous_bcast_init_module;
    mca_coll_base_module_alltoall_init_fn_t         previous_alltoall_init;
    mca_coll_base_module_t*                         previous_alltoall_init_module;
    mca_coll_base_module_alltoallv_init_fn_t        previous_alltoallv_init;
    mca_coll_base_module_t*                         previous_alltoallv_init_module;
    mca_coll_base_module_allgather_init_fn_t        previous_allgather_init;
    mca_coll_base_module_t*                         previous_allgather_init_module;
    mca_coll_base_module_allgatherv_init_fn_t       previous_allgatherv_init;
    mca_coll_base_module_t*                         previous_allgatherv_init_module;
    mca_coll_base_module_gather_init_fn_t           previous_gather_init;
    mca_coll_base_module_t*                         previous_gather_init_module;
    mca_coll_base_module_gatherv_init_fn_t          previous_gatherv_init;
    mca_coll_base_module_t*                         previous_gatherv_init_module;
    mca_coll_base_module_reduce_scatter_block_init_fn_t previous_reduce_scatter_block_init;
    mca_coll_base_module_t*                         previous_reduce_scatter_block_init_module;
    mca_coll_base_module_reduce_scatter_init_fn_t   previous_reduce_scatter_init;
    mca_coll_base_module_t*                         previous_reduce_scatter_init_module;
    mca_coll_base_module_scatterv_init_fn_t         previous_scatterv_init;
    mca_coll_base_module_t*                         previous_scatterv_init_module;
    mca_coll_base_module_scatter_init_fn_t          previous_scatter_init;
    mca_coll_base_module_t*                         previous_scatter_init_module;
};
typedef struct mca_coll_ucc_module_t mca_coll_ucc_module_t;
OBJ_CLASS_DECLARATION(mca_coll_ucc_module_t);

int mca_coll_ucc_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_ucc_comm_query(struct ompi_communicator_t *comm, int *priority);

int mca_coll_ucc_allreduce(const void *sbuf, void *rbuf, size_t count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_iallreduce(const void *sbuf, void *rbuf, size_t count,
                            struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_reduce(const void *sbuf, void* rbuf, size_t count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        struct mca_coll_base_module_3_0_0_t *module);

int mca_coll_ucc_ireduce(const void *sbuf, void* rbuf, size_t count,
                         struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         struct mca_coll_base_module_3_0_0_t *module);

int mca_coll_ucc_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_ibarrier(struct ompi_communicator_t *comm,
                          ompi_request_t** request,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_bcast(void *buf, size_t count, struct ompi_datatype_t *dtype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int mca_coll_ucc_ibcast(void *buf, size_t count, struct ompi_datatype_t *dtype,
                        int root, struct ompi_communicator_t *comm,
                        ompi_request_t** request,
                        mca_coll_base_module_t *module);

int mca_coll_ucc_alltoall(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                          void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_ialltoall(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                           void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           ompi_request_t** request,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_alltoallv(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdips,
                           struct ompi_datatype_t *sdtype,
                           void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_ialltoallv(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdips,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_allgather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                           void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_iallgather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                            void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_allgatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                            void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_iallgatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                             void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm,
                             ompi_request_t** request,
                             mca_coll_base_module_t *module);

int mca_coll_ucc_gather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                        void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_ucc_igather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_gatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_igatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                          void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                          struct ompi_datatype_t *rdtype, int root,
                          struct ompi_communicator_t *comm,
                          ompi_request_t** request,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter_block(const void *sbuf, void *rbuf, size_t rcount,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

int mca_coll_ucc_ireduce_scatter_block(const void *sbuf, void *rbuf, size_t rcount,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       ompi_request_t** request,
                                       mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module);

int mca_coll_ucc_ireduce_scatter(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                ompi_request_t** request,
                                mca_coll_base_module_t *module);

int mca_coll_ucc_scatterv(const void *sbuf, ompi_count_array_t scounts,
                          ompi_disp_array_t disps, struct ompi_datatype_t *sdtype,
                          void *rbuf, size_t rcount,
                          struct ompi_datatype_t *rdtype, int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_iscatterv(const void *sbuf, ompi_count_array_t scounts,
                           ompi_disp_array_t disps, struct ompi_datatype_t *sdtype,
                           void *rbuf, size_t rcount,
                           struct ompi_datatype_t *rdtype, int root,
                           struct ompi_communicator_t *comm,
                           ompi_request_t** request,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_scatter(const void *sbuf, size_t scount,
                         struct ompi_datatype_t *sdtype, void *rbuf, size_t rcount,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_iscatter(const void *sbuf, size_t scount,
                         struct ompi_datatype_t *sdtype, void *rbuf, size_t rcount,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_allreduce_init(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_init(const void *sbuf, void *rbuf, size_t count,
                             struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                             struct ompi_communicator_t *comm, struct ompi_info_t *info,
                             ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_barrier_init(struct ompi_communicator_t *comm, struct ompi_info_t *info,
                              ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_bcast_init(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                            struct ompi_communicator_t *comm, struct ompi_info_t *info,
                            ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_alltoall_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                               void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm, struct ompi_info_t *info,
                               ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_alltoallv_init(const void *sbuf, ompi_count_array_t scounts,
                                ompi_disp_array_t sdisps, struct ompi_datatype_t *sdtype,
                                void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                struct ompi_info_t *info, ompi_request_t **request,
                                mca_coll_base_module_t *module);

int mca_coll_ucc_allgather_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                                struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_allgatherv_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                 struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                 struct ompi_info_t *info, ompi_request_t **request,
                                 mca_coll_base_module_t *module);

int mca_coll_ucc_gather_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                             void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype, int root,
                             struct ompi_communicator_t *comm, struct ompi_info_t *info,
                             ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_gatherv_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                              void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                              struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm, struct ompi_info_t *info,
                              ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter_block_init(const void *sbuf, void *rbuf, size_t rcount,
                                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                           struct ompi_communicator_t *comm,
                                           struct ompi_info_t *info, ompi_request_t **request,
                                           mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter_init(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                     struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                     ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_scatterv_init(const void *sbuf, ompi_count_array_t scounts,
                               ompi_disp_array_t disps, struct ompi_datatype_t *sdtype, void *rbuf,
                               size_t rcount, struct ompi_datatype_t *rdtype, int root,
                               struct ompi_communicator_t *comm, struct ompi_info_t *info,
                               ompi_request_t **request, mca_coll_base_module_t *module);

int mca_coll_ucc_scatter_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                              void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm, struct ompi_info_t *info,
                              ompi_request_t **request, mca_coll_base_module_t *module);

END_C_DECLS
#endif
