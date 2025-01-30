/**
  Copyright (c) 2021      Mellanox Technologies. All rights reserved.
  Copyright (c) 2022 NVIDIA Corporation.  All rights reserved.
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

#define mca_coll_ucc_call_previous(__api, ucc_module, ...) \
    (ucc_module->previous_ ## __api == NULL ? \
        UCC_ERR_NOT_SUPPORTED : \
        ucc_module->previous_ ## __api (__VA_ARGS__, ucc_module->previous_ ## __api ## _module))

typedef struct mca_coll_ucc_req {
    ompi_request_t super;
    ucc_coll_req_h ucc_req;
} mca_coll_ucc_req_t;
OBJ_CLASS_DECLARATION(mca_coll_ucc_req_t);

struct mca_coll_ucc_component_t {
    mca_coll_base_component_2_4_0_t super;
    int                             ucc_priority;
    int                             ucc_verbose;
    int                             ucc_enable;
    int                             ucc_np;
    char                           *cls;
    char                           *cts;
    const char                     *compiletime_version;
    const char                     *runtime_version;
    bool                            libucc_initialized;
    ucc_lib_h                       ucc_lib;
    ucc_lib_attr_t                  ucc_lib_attr;
    ucc_coll_type_t                 cts_requested;
    ucc_coll_type_t                 nb_cts_requested;
    ucc_context_h                   ucc_context;
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
    ucc_team_h                                      ucc_team;
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
};
typedef struct mca_coll_ucc_module_t mca_coll_ucc_module_t;
OBJ_CLASS_DECLARATION(mca_coll_ucc_module_t);

int mca_coll_ucc_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_ucc_comm_query(struct ompi_communicator_t *comm, int *priority);

int mca_coll_ucc_allreduce(const void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_iallreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_reduce(const void *sbuf, void* rbuf, int count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        struct mca_coll_base_module_2_4_0_t *module);

int mca_coll_ucc_ireduce(const void *sbuf, void* rbuf, int count,
                         struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         struct mca_coll_base_module_2_4_0_t *module);

int mca_coll_ucc_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_ibarrier(struct ompi_communicator_t *comm,
                          ompi_request_t** request,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int mca_coll_ucc_ibcast(void *buf, int count, struct ompi_datatype_t *dtype,
                        int root, struct ompi_communicator_t *comm,
                        ompi_request_t** request,
                        mca_coll_base_module_t *module);

int mca_coll_ucc_alltoall(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                          void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_ialltoall(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           ompi_request_t** request,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_alltoallv(const void *sbuf, const int *scounts, const int *sdips,
                           struct ompi_datatype_t *sdtype,
                           void* rbuf, const int *rcounts, const int *rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_ialltoallv(const void *sbuf, const int *scounts, const int *sdips,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_allgather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_iallgather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                            void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_allgatherv(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_ucc_iallgatherv(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                             void* rbuf, const int *rcounts, const int *rdisps,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm,
                             ompi_request_t** request,
                             mca_coll_base_module_t *module);

int mca_coll_ucc_gather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                        void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_ucc_igather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_gatherv(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, const int *rcounts, const int *disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_igatherv(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                          void *rbuf, const int *rcounts, const int *disps,
                          struct ompi_datatype_t *rdtype, int root,
                          struct ompi_communicator_t *comm,
                          ompi_request_t** request,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

int mca_coll_ucc_ireduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       ompi_request_t** request,
                                       mca_coll_base_module_t *module);

int mca_coll_ucc_reduce_scatter(const void *sbuf, void *rbuf, const int *rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module);

int mca_coll_ucc_ireduce_scatter(const void *sbuf, void *rbuf, const int *rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                ompi_request_t** request,
                                mca_coll_base_module_t *module);

int mca_coll_ucc_scatterv(const void *sbuf, const int *scounts,
                          const int *disps, struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype, int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

int mca_coll_ucc_iscatterv(const void *sbuf, const int *scounts,
                           const int *disps, struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount,
                           struct ompi_datatype_t *rdtype, int root,
                           struct ompi_communicator_t *comm,
                           ompi_request_t** request,
                           mca_coll_base_module_t *module);

int mca_coll_ucc_scatter(const void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_ucc_iscatter(const void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype, void *rbuf, int rcount,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module);

END_C_DECLS
#endif
