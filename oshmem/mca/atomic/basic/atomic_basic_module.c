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
#include <stdio.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/proc/proc.h"
#include "oshmem/op/op.h"
#include "atomic_basic.h"

static char *atomic_lock_sync;
static int *atomic_lock_turn;
static char *local_lock_sync;
static int *local_lock_turn;

enum {
    ATOMIC_LOCK_IDLE = 0,
    ATOMIC_LOCK_WAITING = 1,
    ATOMIC_LOCK_ACTIVE = 2
};

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_atomic_basic_startup(bool enable_progress_threads, bool enable_threads)
{
    int rc = OSHMEM_SUCCESS;
    void* ptr = NULL;
    int num_pe = oshmem_num_procs();

    rc = MCA_MEMHEAP_CALL(private_alloc((num_pe * sizeof(char)), &ptr));
    if (rc == OSHMEM_SUCCESS) {
        atomic_lock_sync = (char*) ptr;
        memset(atomic_lock_sync, ATOMIC_LOCK_IDLE, sizeof(char) * num_pe);

        rc = MCA_MEMHEAP_CALL(private_alloc(sizeof(int), &ptr));
        if (rc == OSHMEM_SUCCESS) {
            atomic_lock_turn = (int*) ptr;
            *atomic_lock_turn = 0;
            if (rc == OSHMEM_SUCCESS) {
                local_lock_sync = (char*) malloc(num_pe * sizeof(char));
                local_lock_turn = (int*) malloc(sizeof(int));
                if (!local_lock_sync || !local_lock_turn) {
                    rc = OSHMEM_ERR_OUT_OF_RESOURCE;
                } else {
                    memcpy((void*) local_lock_sync,
                           (void*) atomic_lock_sync,
                           sizeof(char) * num_pe);
                    *local_lock_turn = *atomic_lock_turn;
                }
            }
        }
    }

    return rc;
}

int mca_atomic_basic_finalize(void)
{
    void* ptr = NULL;

    ptr = (void*) atomic_lock_sync;
    MCA_MEMHEAP_CALL(private_free(ptr));
    atomic_lock_sync = NULL;

    ptr = (void*) atomic_lock_turn;
    MCA_MEMHEAP_CALL(private_free(ptr));
    atomic_lock_turn = NULL;

    if (local_lock_sync) {
        free((void*) local_lock_sync);
        local_lock_sync = NULL;
    }

    if (local_lock_turn) {
        free((void*) local_lock_turn);
        local_lock_turn = NULL;
    }

    return OSHMEM_SUCCESS;
}

static inline
int mca_atomic_basic_fop(shmem_ctx_t ctx,
                         void *target,
                         void *prev,
                         uint64_t value,
                         size_t size,
                         int pe,
                         struct oshmem_op_t *op)
{
    int rc = OSHMEM_SUCCESS;
    long long temp_value = 0;

    atomic_basic_lock(ctx, pe);

    rc = MCA_SPML_CALL(get(ctx, target, size, (void*)&temp_value, pe));

    memcpy(prev, (void*) &temp_value, size);

    op->o_func.c_fn((void*) value,
                    (void*) &temp_value,
                    size / op->dt_size);

    if (rc == OSHMEM_SUCCESS) {
        rc = MCA_SPML_CALL(put(ctx, target, size, (void*)&temp_value, pe));
        shmem_quiet();
    }

    atomic_basic_unlock(ctx, pe);

    return rc;
}

static inline
int mca_atomic_basic_op(shmem_ctx_t ctx,
                        void *target,
                        uint64_t value,
                        size_t size,
                        int pe,
                        struct oshmem_op_t *op)
{
    long long prev;

    return mca_atomic_basic_fop(ctx, target, &prev, value, size, pe, op);
}

static int mca_atomic_basic_add(shmem_ctx_t ctx, void *target, uint64_t value,
                                size_t size, int pe)
{
    return mca_atomic_basic_op(ctx, target, value, size, pe,
                               MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_sum_int64));
}

static int mca_atomic_basic_and(shmem_ctx_t ctx,
                                void *target, uint64_t value,
                                size_t size, int pe)
{
    return mca_atomic_basic_op(ctx, target, value, size, pe,
                               MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_and_int64));
}

static int mca_atomic_basic_or(shmem_ctx_t ctx, void *target, uint64_t value,
                               size_t size, int pe)
{
    return mca_atomic_basic_op(ctx, target, value, size, pe,
                               MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_or_int64));
}

static int mca_atomic_basic_xor(shmem_ctx_t ctx,
                                void *target, uint64_t value,
                                size_t size, int pe)
{
    return mca_atomic_basic_op(ctx, target, value, size, pe,
                               MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_xor_int64));
}

static int mca_atomic_basic_fadd(shmem_ctx_t ctx, void *target, void *prev, uint64_t value,
                                 size_t size, int pe)
{
    return mca_atomic_basic_fop(ctx, target, prev, value, size, pe,
                                MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_sum_int64));
}

static int mca_atomic_basic_fand(shmem_ctx_t ctx,
                                 void *target, void *prev, uint64_t value,
                                 size_t size, int pe)
{
    return mca_atomic_basic_fop(ctx, target, prev, value, size, pe,
                                MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_and_int64));
}

static int mca_atomic_basic_for(shmem_ctx_t ctx, void *target, void *prev, uint64_t value,
                                size_t size, int pe)
{
    return mca_atomic_basic_fop(ctx, target, prev, value, size, pe,
                                MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_or_int64));
}

static int mca_atomic_basic_fxor(shmem_ctx_t ctx, void *target, void *prev, uint64_t value,
                                 size_t size, int pe)
{
    return mca_atomic_basic_fop(ctx, target, prev, value, size, pe,
                                MCA_BASIC_OP(size, oshmem_op_sum_int32, oshmem_op_xor_int64));
}

static int mca_atomic_basic_swap(shmem_ctx_t ctx, void *target, void *prev, uint64_t value,
                                 size_t size, int pe)
{
    return mca_atomic_basic_fop(ctx, target, prev, value, size, pe,
                                MCA_BASIC_OP(size, oshmem_op_swap_int32, oshmem_op_swap_int64));
}

mca_atomic_base_module_t *
mca_atomic_basic_query(int *priority)
{
    mca_atomic_basic_module_t *module;

    *priority = mca_atomic_basic_component.priority;

    module = OBJ_NEW(mca_atomic_basic_module_t);
    if (module) {
        module->super.atomic_add   = mca_atomic_basic_add;
        module->super.atomic_and   = mca_atomic_basic_and;
        module->super.atomic_or    = mca_atomic_basic_or;
        module->super.atomic_xor   = mca_atomic_basic_xor;
        module->super.atomic_fadd  = mca_atomic_basic_fadd;
        module->super.atomic_fand  = mca_atomic_basic_fand;
        module->super.atomic_for   = mca_atomic_basic_for;
        module->super.atomic_fxor  = mca_atomic_basic_fxor;
        module->super.atomic_swap  = mca_atomic_basic_swap;
        module->super.atomic_cswap = mca_atomic_basic_cswap;
        return &(module->super);
    }

    return NULL ;
}

void atomic_basic_lock(shmem_ctx_t ctx, int pe)
{
    int index = -1;
    int me = oshmem_my_proc_id();
    int num_pe = oshmem_num_procs();
    char lock_required = ATOMIC_LOCK_WAITING;
    char lock_active = ATOMIC_LOCK_ACTIVE;
    int root_pe = pe;

    do {
        /* announce that we need the resource */
        do {
            MCA_SPML_CALL(put(ctx, (void*)(atomic_lock_sync + me), sizeof(lock_required), (void*)&lock_required, root_pe));
            MCA_SPML_CALL(get(ctx, (void*)atomic_lock_sync, num_pe * sizeof(*atomic_lock_sync), (void*)local_lock_sync, root_pe));
        } while (local_lock_sync[me] != lock_required);

        MCA_SPML_CALL(get(ctx, (void*)atomic_lock_turn, sizeof(index), (void*)&index, root_pe));
        while (index != me) {
            if (local_lock_sync[index] != ATOMIC_LOCK_IDLE) {
                MCA_SPML_CALL(get(ctx, (void*)atomic_lock_turn, sizeof(index), (void*)&index, root_pe));
                MCA_SPML_CALL(get(ctx, (void*)atomic_lock_sync, num_pe * sizeof(*atomic_lock_sync), (void*)local_lock_sync, root_pe));
            } else {
                index = (index + 1) % num_pe;
            }
        }

        /* now tentatively claim the resource */
        do {
            MCA_SPML_CALL(put(ctx, (void*)(atomic_lock_sync + me), sizeof(lock_active), (void*)&lock_active, root_pe));
            MCA_SPML_CALL(get(ctx, (void*)atomic_lock_sync, num_pe * sizeof(*atomic_lock_sync), (void*)local_lock_sync, root_pe));
        } while (local_lock_sync[me] != lock_active);

        index = 0;
        while ((index < num_pe)
                && ((index == me)
                        || (local_lock_sync[index] != ATOMIC_LOCK_ACTIVE))) {
            index = index + 1;
        }

        MCA_SPML_CALL(get(ctx, (void*)atomic_lock_turn, sizeof(*atomic_lock_turn), (void*)local_lock_turn, root_pe));
    } while (!((index >= num_pe)
            && ((*local_lock_turn == me)
                    || (local_lock_sync[*local_lock_turn] == ATOMIC_LOCK_IDLE))));

    MCA_SPML_CALL(put(ctx, (void*)atomic_lock_turn, sizeof(me), (void*)&me, root_pe));
}

void atomic_basic_unlock(shmem_ctx_t ctx, int pe)
{
    int index = -1;
    int me = oshmem_my_proc_id();
    int num_pe = oshmem_num_procs();
    char lock_idle = ATOMIC_LOCK_IDLE;
    int root_pe = pe;

    MCA_SPML_CALL(get(ctx, (void*)atomic_lock_sync, num_pe * sizeof(*atomic_lock_sync), (void*)local_lock_sync, root_pe));
    MCA_SPML_CALL(get(ctx, (void*)atomic_lock_turn, sizeof(index), (void*)&index, root_pe));

    do {
        index = (index + 1) % num_pe;
    } while (local_lock_sync[index] == ATOMIC_LOCK_IDLE);

    MCA_SPML_CALL(put(ctx, (void*)atomic_lock_turn, sizeof(index), (void*)&index, root_pe));

    do {
        MCA_SPML_CALL(put(ctx, (void*)(atomic_lock_sync + me), sizeof(lock_idle), (void*)&lock_idle, root_pe));
        MCA_SPML_CALL(get(ctx, (void*)atomic_lock_sync, num_pe * sizeof(*atomic_lock_sync), (void*)local_lock_sync, root_pe));
    } while (local_lock_sync[me] != lock_idle);
}
