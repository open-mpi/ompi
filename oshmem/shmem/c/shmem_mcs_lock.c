/*
 * Copyright (c) 2023      NVIDIA Corporation.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"
#include <stdlib.h>
#include <memory.h>

#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/shmem/shmem_lock.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/atomic/atomic.h"

#define OPAL_BITWISE_SIZEOF_LONG (SIZEOF_LONG * 8)


/** Use basic MCS distributed lock algorithm for lock */
struct shmem_mcs_lock {
    /** has meaning only on MCSQ_TAIL OWNER */
    int tail;
    /** It has meaning on all PEs */
    /** The next pointer is a combination of the PE ID and wait signal */
    int next;
};
typedef struct shmem_mcs_lock shmem_mcs_lock_t;

#define SHMEM_MCSL_TAIL_OWNER(lock_ptr)\
    (((uintptr_t)(lock_ptr) / sizeof(long)) % shmem_n_pes())

#define SHMEM_MCSL_NEXT_MASK            0x7FFFFFFFU
#define SHMEM_MCSL_SIGNAL_MASK          0x80000000U /** Wait signal mask */
#define SHMEM_MCSL_NEXT(lock_val)       ((lock_val) & SHMEM_MCSL_NEXT_MASK)
/** Improve readability */
#define SHMEM_MCSL_GET_PE(tail_val)     ((tail_val) & SHMEM_MCSL_NEXT_MASK)
#define SHMEM_MCSL_SIGNAL(lock_val)     ((lock_val) & SHMEM_MCSL_SIGNAL_MASK)
#define SHMEM_MCSL_SET_SIGNAL(lock_val) ((lock_val) | SHMEM_MCSL_SIGNAL_MASK)

void
_shmem_mcs_set_lock(long *lockp)
{
    shmem_mcs_lock_t *lock = (shmem_mcs_lock_t *) lockp;
    int mcs_tail_owner     = SHMEM_MCSL_TAIL_OWNER(lock);
    int new_tail_req       = 0;
    int *tail              = &(lock->tail);
    int *next              = &(lock->next);
    int my_pe              = shmem_my_pe();
    int curr               = 0;
    int out_value          = 0;
    int prev_tail          = 0;
    int prev_tailpe        = 0;
    int tval               = 0;
    int tmp_val            = 0;
    int retv               = 0;
    uint64_t value_tmp     = 0;

    RUNTIME_CHECK_INIT();
    /**
     *  Initializing next pointer to next mask
     *  Done atomically to avoid races as NEXT pointer
     *  can be modified by other PEs while acquiring or
     *  releasing it.
     */
    /**
     * Can make this to be shmem_atomic_set to be safe
     * in non-cc architectures
     * has an impact on performance
     */
    value_tmp = SHMEM_MCSL_NEXT_MASK;
    out_value = SHMEM_MCSL_NEXT_MASK;
    retv = MCA_ATOMIC_CALL(swap(oshmem_ctx_default, (void*)next,
                                (void*)&out_value, value_tmp,
                                sizeof(int), my_pe));
    RUNTIME_CHECK_RC(retv);
    MCA_SPML_CALL(quiet(oshmem_ctx_default));

    /** Signal for setting lock */
    new_tail_req = SHMEM_MCSL_SET_SIGNAL(my_pe);
    /**
     * Swap and make me the new tail and update in tail owner
     * Get the previous tail PE.
     */
    retv = MCA_ATOMIC_CALL(swap(oshmem_ctx_default, (void *)tail,
                                (void*)&prev_tail,
                                OSHMEM_ATOMIC_PTR_2_INT(&new_tail_req,
                                                        sizeof(new_tail_req)),
                                sizeof(int), mcs_tail_owner));
    RUNTIME_CHECK_RC(retv);

    prev_tailpe = SHMEM_MCSL_GET_PE(prev_tail);
    if (SHMEM_MCSL_SIGNAL(prev_tail)) {
        /**
         * Someone else has got the lock before this PE
         * Adding this PE to the previous tail PE's Next pointer
         * Substract the SIGNAL Bit to avoid changing it.
         */
        tmp_val = my_pe - SHMEM_MCSL_NEXT_MASK;
        retv = MCA_ATOMIC_CALL(add(oshmem_ctx_default, (void*)next, tmp_val,
                                   sizeof(int), prev_tailpe));
        RUNTIME_CHECK_RC(retv);
        /**
         * This value to be changed eventually by predecessor
         * when its lock is released.
         * Need to be done atomically to avoid any races where
         * next pointer is modified by another PE acquiring or
         * releasing this.
         */
        retv = MCA_ATOMIC_CALL(add(oshmem_ctx_default, (void *)next,
                                   SHMEM_MCSL_SIGNAL_MASK, sizeof(int),
                                   my_pe));
        RUNTIME_CHECK_RC(retv);
        MCA_SPML_CALL(quiet(oshmem_ctx_default));
        /**  Wait for predecessor release lock to this PE signal to false. */
        retv = MCA_ATOMIC_CALL(fadd(oshmem_ctx_default, (void*)next,
                                    (void*)&curr, tval, sizeof(int), my_pe));
        RUNTIME_CHECK_RC(retv);

        while (SHMEM_MCSL_SIGNAL(curr)) {
            retv = MCA_SPML_CALL(wait((void*)next, SHMEM_CMP_NE,
                                      (void*)&curr, SHMEM_INT));
            RUNTIME_CHECK_RC(retv);
            retv = MCA_ATOMIC_CALL(fadd(oshmem_ctx_default, (void*)next,
                                        (void*)&curr, tval, sizeof(int),
                                        my_pe));
            RUNTIME_CHECK_RC(retv);
        }
    }
/** else.. this pe has got the lock as no one else had it */
}

void
_shmem_mcs_clear_lock(long *lockp)
{
    shmem_mcs_lock_t *lock = (shmem_mcs_lock_t *) lockp;
    int mcs_tail_owner     = SHMEM_MCSL_TAIL_OWNER(lock);
    int *tail              = &(lock->tail);
    int *next              = &(lock->next);
    int my_pe              = shmem_my_pe();
    int next_value         = 0;
    int swap_cond          = 0;
    int prev_value         = 0;
    int tval               = 0;
    int val_tmp            = 0;
    int nmask              = 0;
    int a_val              = 0;
    int retv               = 0;

    /**
     * Can make atomic fetch to be safe in non-cc architectures
     * Has impact on performance
     */
    retv = MCA_ATOMIC_CALL(fadd(oshmem_ctx_default, (void*)next,
                                (void*)&next_value, tval, sizeof(int),
                                my_pe));
    RUNTIME_CHECK_RC(retv);
    MCA_SPML_CALL(quiet(oshmem_ctx_default));

    if (next_value == SHMEM_MCSL_NEXT_MASK) {
        swap_cond = SHMEM_MCSL_SET_SIGNAL(my_pe);
        retv = MCA_ATOMIC_CALL(cswap(oshmem_ctx_default,
                                     (void *)tail, (uint64_t *)&(prev_value),
                                     OSHMEM_ATOMIC_PTR_2_INT(&swap_cond,
                                     sizeof(swap_cond)),
                                     OSHMEM_ATOMIC_PTR_2_INT(&val_tmp,
                                     sizeof(val_tmp)), sizeof(int),
                                     mcs_tail_owner));
        RUNTIME_CHECK_RC(retv);

        /** I am the tail.. and lock is released */
        if (prev_value == swap_cond) {
            return;
        }
        /**
         * I am not the tail, another PE maybe racing to acquire lock,
         * let them complete setting themselves as our next
         */
        nmask = SHMEM_MCSL_NEXT_MASK;
        while(next_value == nmask) {
            retv = MCA_SPML_CALL(wait((void*)next, SHMEM_CMP_NE,
                                      (void*)&nmask, SHMEM_INT));
            RUNTIME_CHECK_RC(retv);
            retv = MCA_ATOMIC_CALL(fadd(oshmem_ctx_default, (void*)next,
                                        (void*)&next_value, tval,
                                        sizeof(int), my_pe));
            RUNTIME_CHECK_RC(retv);
        }
    }
    /** There is a successor release lock to the successor */
    a_val = SHMEM_MCSL_SIGNAL_MASK;
    retv = MCA_ATOMIC_CALL(add(oshmem_ctx_default,
                               (void *)next, a_val, sizeof(a_val),
                               SHMEM_MCSL_NEXT(next_value)));
    RUNTIME_CHECK_RC(retv);
    MCA_SPML_CALL(quiet(oshmem_ctx_default));
}

int
_shmem_mcs_test_lock(long *lockp)
{
    shmem_mcs_lock_t *lock = (shmem_mcs_lock_t *) lockp;
    int mcs_tail_owner     = SHMEM_MCSL_TAIL_OWNER(lock);
    int new_tail_req       = 0;
    int prev_tail          = 0;
    int tmp_cond           = 0;
    int *tail              = &(lock->tail);
    int *next              = &(lock->next);
    int my_pe              = shmem_my_pe();
    int retv               = 0;

    /** Initializing next pointer to next mask */
    *next = SHMEM_MCSL_NEXT_MASK;

    /** Signal for setting lock */
    new_tail_req = SHMEM_MCSL_SET_SIGNAL(my_pe);

    /** Check if previously cleared before swapping */
    retv = MCA_ATOMIC_CALL(cswap(oshmem_ctx_default,
                                 (void *)tail, (uint64_t *)&(prev_tail),
                                 OSHMEM_ATOMIC_PTR_2_INT(&tmp_cond,
                                                         sizeof(tmp_cond)),
                                 OSHMEM_ATOMIC_PTR_2_INT(&new_tail_req,
                                                         sizeof(new_tail_req)),
                                 sizeof(int), mcs_tail_owner));
    RUNTIME_CHECK_RC(retv);

    return (0 != prev_tail);
}
