/**
  Copyright (c) 2020      Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "coll_mccl.h"
#include "coll_mccl_dtypes.h"

#define COLL_MCCL_CHECK(_call) do {              \
        if (MCCL_SUCCESS != (_call)) {                \
            goto fallback;                      \
        }                                       \
    } while(0)

static inline int coll_mccl_req_wait(mccl_request_h req) {
    while (MCCL_IN_PROGRESS == mccl_test(req)) {
       opal_progress();
    }
    return mccl_request_free(req);
}

int mca_coll_mccl_allreduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op, struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mccl_request_h req;
    tccl_dt_t tccl_dt;
    tccl_op_t tccl_op;
    mca_coll_mccl_module_t *mccl_module = (mca_coll_mccl_module_t*)module;

    MCCL_VERBOSE(20,"RUNNING MCCL ALLREDUCE");
    tccl_dt = ompi_dtype_to_tccl_dtype(dtype);
        tccl_op = ompi_op_to_tccl_op(op);
    if (OPAL_UNLIKELY(TCCL_DT_UNSUPPORTED == tccl_dt || TCCL_OP_UNSUPPORTED == tccl_op)) {
        MCCL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback allreduce;",
                     dtype->super.name);
        goto fallback;
    }

    COLL_MCCL_CHECK(mccl_allreduce_init((void *)sbuf, rbuf, count, tccl_dt,
                                      tccl_op, mccl_module->mccl_comm, &req));
    COLL_MCCL_CHECK(mccl_start(req));
    COLL_MCCL_CHECK(coll_mccl_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    MCCL_VERBOSE(20,"RUNNING FALLBACK ALLREDUCE");
    return mccl_module->previous_allreduce(sbuf, rbuf, count, dtype, op,
                                          comm, mccl_module->previous_allreduce_module);
}

int mca_coll_mccl_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module)
{
    mccl_request_h req;
    mca_coll_mccl_module_t *mccl_module = (mca_coll_mccl_module_t*)module;

    MCCL_VERBOSE(20,"RUNNING MCCL BARRIER");
    COLL_MCCL_CHECK(mccl_barrier_init(mccl_module->mccl_comm, &req));
    COLL_MCCL_CHECK(mccl_start(req));
    COLL_MCCL_CHECK(coll_mccl_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    MCCL_VERBOSE(20,"RUNNING FALLBACK BARRIER");
    return mccl_module->previous_barrier(comm, mccl_module->previous_barrier_module);
}

int mca_coll_mccl_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module)
{
    mccl_request_h req;
    tccl_dt_t tccl_dt;
    mca_coll_mccl_module_t *mccl_module = (mca_coll_mccl_module_t*)module;

    MCCL_VERBOSE(20,"RUNNING MCCL BCAST");
    tccl_dt = ompi_dtype_to_tccl_dtype(dtype);
    COLL_MCCL_CHECK(mccl_bcast_init(buf, count, tccl_dt, root, mccl_module->mccl_comm, &req));
    COLL_MCCL_CHECK(mccl_start(req));
    COLL_MCCL_CHECK(coll_mccl_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    MCCL_VERBOSE(20,"RUNNING FALLBACK BCAST");
    return mccl_module->previous_bcast(buf, count, dtype, root,
                                       comm, mccl_module->previous_bcast_module);
}
