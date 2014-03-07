/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"
#include "oshmem/constants.h"
#include "scoll_fca.h"
#include <stdio.h>
#include "oshmem/proc/proc.h"
#include "oshmem/op/op.h"
int mca_scoll_fca_barrier(struct oshmem_group_t *group, long *pSync, int alg)
{
    mca_scoll_fca_module_t *fca_module =
            (mca_scoll_fca_module_t *) group->g_scoll.scoll_barrier_module;
    int rc;

    FCA_VERBOSE(5, "Using FCA Barrier");
    rc = fca_do_barrier(fca_module->fca_comm);
    if (rc < 0) {
        if (rc == -EUSESHMEM) {
            FCA_VERBOSE(5, "FCA Barrier failed, using original barrier");
            goto orig_barrier;
        }
        FCA_ERROR("Barrier failed: %s", fca_strerror(rc));
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
    orig_barrier:
    PREVIOUS_SCOLL_FN(fca_module, barrier, group,
            pSync,
            SCOLL_DEFAULT_ALG);
    return rc;
}

int mca_scoll_fca_broadcast(struct oshmem_group_t *group,
                            int PE_root,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            int alg)
{
    mca_scoll_fca_module_t *fca_module =
            (mca_scoll_fca_module_t *) group->g_scoll.scoll_broadcast_module;
    fca_bcast_spec_t spec;
    int rc;

    FCA_VERBOSE(5, "rank %i, DOING FCA BCAST\n", group->my_pe);
    spec.root = oshmem_proc_group_find_id(group, PE_root);
    if (group->my_pe == PE_root)
        spec.buf = (void *) source;
    else
        spec.buf = target;
    spec.size = nlong;
    if (spec.size > fca_module->fca_comm_caps.max_payload) {
        FCA_VERBOSE(5,
                    "Unsupported bcast operation size %d, using fallback",
                    spec.size);
        goto orig_bcast;
    }
    rc = fca_do_bcast(fca_module->fca_comm, &spec);
    if (rc < 0) {
        if (rc == -EUSESHMEM) {
            FCA_VERBOSE(5, "FCA Broadcast failed, using original Broadcast");
            goto orig_bcast;
        }
        FCA_ERROR("Bcast failed: %s", fca_strerror(rc));
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
    orig_bcast:
    PREVIOUS_SCOLL_FN(fca_module, broadcast, group,
            PE_root,
            target,
            source,
            nlong,
            pSync,
            SCOLL_DEFAULT_ALG);
    return rc;
}

int mca_scoll_fca_collect(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nlong,
                          long *pSync,
                          bool nlong_type,
                          int alg)
{
    int rc, i;
    mca_scoll_fca_module_t *fca_module =
            (mca_scoll_fca_module_t *) group->g_scoll.scoll_collect_module;

    FCA_VERBOSE(5,
                "rank %i, DOING FCA_COLLECT, nlong_type = %i\n",
                group->my_pe, (int)nlong_type);
#if OSHMEM_FCA_ALLGATHER
    if (nlong_type == true) {
        fca_gather_spec_t spec = {0,};
        spec.size = (int)nlong;
        spec.sbuf = (void *)source;
        spec.rbuf = target;
        rc = fca_do_allgather(fca_module->fca_comm, &spec);
        if (rc < 0) {
            if (rc == -EUSESHMEM) {
                FCA_VERBOSE(5,"FCA Fcollect(allgather) failed, using original Fcollect");
                goto orig_collect;
            }
            FCA_ERROR("Fcollect(allgather) failed: %s", fca_strerror(rc));
            return OSHMEM_ERROR;
        }
        return OSHMEM_SUCCESS;
    }
    else
    {
        size_t *sendcounts = (size_t *)malloc(group->proc_count*sizeof(size_t));
        mca_scoll_fca_collect(group,sendcounts,(void *)&nlong,sizeof(size_t),pSync,true,SCOLL_DEFAULT_ALG);
        fca_gatherv_spec_t spec;
        spec.sendsize = (int)nlong;
        spec.sbuf = (void *)source;
        spec.rbuf = target;
        spec.recvsizes = alloca(sizeof(*spec.recvsizes) * group->proc_count);
        spec.displs = alloca(sizeof(*spec.displs) * group->proc_count);
        for (i=0; i<group->proc_count; i++) {
            spec.recvsizes[i] = (int)sendcounts[i];
        }
        spec.displs[0] = 0;
        for (i=1; i<group->proc_count; i++) {
            spec.displs[i] = spec.displs[i-1]+spec.recvsizes[i-1];
        }
        rc = fca_do_allgatherv(fca_module->fca_comm, &spec);
        if (rc < 0) {
            if (rc == -EUSESHMEM) {
                FCA_VERBOSE(5,"FCA Collect(allgatherv) failed, using original Collect");
                goto orig_collect;
            }
            FCA_ERROR("Collect(allgatherv) failed: %s", fca_strerror(rc));
            return OSHMEM_ERROR;
        }
        free(sendcounts);
        return OSHMEM_SUCCESS;
    }
    orig_collect:
#endif
    PREVIOUS_SCOLL_FN(fca_module, collect, group,
                                        target,
                                        source,
                                        nlong,
                                        pSync,
                                        nlong_type,
                                        SCOLL_DEFAULT_ALG);
    return rc;
}

#define FCA_DTYPE_8_SIGNED  1
#define FCA_DTYPE_16_SIGNED 2
#define FCA_DTYPE_32_SIGNED 3
#define FCA_DTYPE_64_SIGNED 4
#define FCA_DTYPE_32_FLOAT  9
#define FCA_DTYPE_64_FLOAT 10
#define UNSUPPORTED_OP     -1

static bool if_floating_type(oshmem_op_t *op)
{
    if ((op->dt == OSHMEM_OP_TYPE_FLOAT) || (op->dt == OSHMEM_OP_TYPE_DOUBLE)
            || (op->dt == OSHMEM_OP_TYPE_LDOUBLE))
        return true;
    else
        return false;
}
static int shmem_dtype_to_fca_dtype(oshmem_op_t *op)
{
    if ((op->dt == OSHMEM_OP_TYPE_FCOMPLEX)
            || (op->dt == OSHMEM_OP_TYPE_DCOMPLEX)) {
        return UNSUPPORTED_OP;
    }
    switch (op->dt_size * 8) {
    case 64:
        if (if_floating_type(op))
            return FCA_DTYPE_64_FLOAT;
        else
            return FCA_DTYPE_64_SIGNED;
        break;
    case 32:
        if (if_floating_type(op))
            return FCA_DTYPE_32_FLOAT;
        else
            return FCA_DTYPE_32_SIGNED;
        break;
    case 16:
        if (OPAL_UNLIKELY(if_floating_type(op)))
            return UNSUPPORTED_OP;
        else
            return FCA_DTYPE_16_SIGNED;
        break;
    case 8:
        if (OPAL_UNLIKELY(if_floating_type(op)))
            return UNSUPPORTED_OP;
        else
            return FCA_DTYPE_8_SIGNED;
        break;
    default:
        return UNSUPPORTED_OP;
    }
}

static int shmem_op_to_fca_op(oshmem_op_t *op)
{
    switch (op->op) {
    case OSHMEM_OP_AND:
        return FCA_OP_BAND;
        break;
    case OSHMEM_OP_OR:
        return FCA_OP_BOR;
        break;
    case OSHMEM_OP_XOR:
        return FCA_OP_BXOR;
    case OSHMEM_OP_MAX:
        return FCA_OP_MAX;
        break;
    case OSHMEM_OP_MIN:
        return FCA_OP_MIN;
        break;
    case OSHMEM_OP_SUM:
        return FCA_OP_SUM;
        break;
    case OSHMEM_OP_PROD:
        return FCA_OP_PROD;
        break;
    default:
        return UNSUPPORTED_OP;
    }
}
int mca_scoll_fca_reduce(struct oshmem_group_t *group,
                         struct oshmem_op_t *op,
                         void *target,
                         const void *source,
                         size_t nlong,
                         long *pSync,
                         void *pWrk,
                         int alg)
{
    mca_scoll_fca_module_t *fca_module =
            (mca_scoll_fca_module_t *) group->g_scoll.scoll_reduce_module;
    int fca_dtype;
    int fca_op;
    int rc;
    fca_reduce_spec_t spec;

    FCA_VERBOSE(5, "rank %i, DOING FCA_REDUCE\n", group->my_pe);
    if ((fca_dtype = shmem_dtype_to_fca_dtype(op)) < 0) {
        FCA_VERBOSE(5,
                    "SHMEM_DATA_TYPE = %i is unsupported in the current version of FCA library; using original reduce",
                    op->dt);
        goto orig_reduce;
    }
    if ((fca_op = shmem_op_to_fca_op(op)) < 0) {
        FCA_VERBOSE(5,
                    "SHMEM_OPERATION_TYPE = %i is unsupported; using original reduce",
                    op->op);
        goto orig_reduce;
    }
    spec.sbuf = (void *) source;
    spec.rbuf = target;
    spec.dtype = (enum fca_reduce_dtype_t) fca_dtype;
    spec.op = (enum fca_reduce_op_t) fca_op;
    spec.length = (int) (nlong / op->dt_size);
    rc = fca_do_all_reduce(fca_module->fca_comm, &spec);
    if (rc < 0) {
        if (rc == -EUSESHMEM) {
            FCA_VERBOSE(5,
                        "FCA Reduce(allreduce) failed, using original Reduce");
            goto orig_reduce;
        }
        FCA_ERROR("Reduce (allreduce) failed: %s", fca_strerror(rc));
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
    orig_reduce:
    PREVIOUS_SCOLL_FN(fca_module, reduce, group,
            op,
            target,
            source,
            nlong,
            pSync,
            pWrk,
            SCOLL_DEFAULT_ALG);
    return rc;
}
