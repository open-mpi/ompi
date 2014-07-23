/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

#include "osc_portals4.h"

#include "ompi/mca/mtl/portals4/mtl_portals4_endpoint.h"

enum locktype_t {
    lock_nocheck,
    lock_exclusive,
    lock_shared
};

struct ompi_osc_portals4_outstanding_lock_t {
    opal_list_item_t super;
    int target;
    enum locktype_t lock_type;
};
typedef struct ompi_osc_portals4_outstanding_lock_t ompi_osc_portals4_outstanding_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_portals4_outstanding_lock_t, opal_list_item_t,
                   NULL, NULL);

static inline int
lk_cas64(ompi_osc_portals4_module_t *module,
         int target,
         int64_t write_val,
         int64_t comp_val,
         int64_t *result_val)
{
    int ret;
    size_t offset = offsetof(ompi_osc_portals4_node_state_t, lock);
    ptl_handle_md_t result_md_h, write_md_h;
    void *result_base, *write_base;

    opal_atomic_add_64(&module->opcount, 1);

    ompi_osc_portals4_get_md(result_val, module->md_h, &result_md_h, &result_base);
    ompi_osc_portals4_get_md(&write_val, module->md_h, &write_md_h, &write_base);

    ret = PtlSwap(result_md_h,
                  (char*) result_val - (char*) result_base,
                  write_md_h,
                  (char*) &write_val - (char*) write_base,
                  sizeof(int64_t),
                  ompi_osc_portals4_get_peer(module, target),
                  module->pt_idx,
                  module->match_bits | OSC_PORTALS4_MB_CONTROL,
                  offset,
                  NULL,
                  0,
                  &comp_val,
                  PTL_CSWAP,
                  PTL_INT64_T);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = ompi_osc_portals4_complete_all(module);
    return ret;
}


static inline int
lk_write64(ompi_osc_portals4_module_t *module,
           int target,
           int64_t write_val)
{
    int ret;
    size_t offset = offsetof(ompi_osc_portals4_node_state_t, lock);
    ptl_handle_md_t md_h;
    void *base;

    opal_atomic_add_64(&module->opcount, 1);

    ompi_osc_portals4_get_md(&write_val, module->md_h, &md_h, &base);

    ret = PtlPut(md_h,
                 (char*) &write_val - (char*) base,
                 sizeof(int64_t),
                 PTL_ACK_REQ,
                 ompi_osc_portals4_get_peer(module, target),
                 module->pt_idx,
                 module->match_bits | OSC_PORTALS4_MB_CONTROL,
                 offset,
                 NULL,
                 0);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = ompi_osc_portals4_complete_all(module);
    return ret;
}


static inline int
lk_add64(ompi_osc_portals4_module_t *module,
         int target,
         int64_t write_val,
         int64_t *result_val)
{
    int ret;
    size_t offset = offsetof(ompi_osc_portals4_node_state_t, lock);
    ptl_handle_md_t result_md_h, write_md_h;
    void *result_base, *write_base;

    opal_atomic_add_64(&module->opcount, 1);

    ompi_osc_portals4_get_md(result_val, module->md_h, &result_md_h, &result_base);
    ompi_osc_portals4_get_md(&write_val, module->md_h, &write_md_h, &write_base);

    ret = PtlFetchAtomic(result_md_h,
                         (char*) result_val - (char*) result_base,
                         write_md_h,
                         (char*) &write_val - (char*) write_base,
                         sizeof(int64_t),
                         ompi_osc_portals4_get_peer(module, target),
                         module->pt_idx,
                         module->match_bits | OSC_PORTALS4_MB_CONTROL,
                         offset,
                         NULL,
                         0,
                         PTL_SUM,
                         PTL_INT64_T);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = ompi_osc_portals4_complete_all(module);
    return ret;
}


static inline int
start_exclusive(ompi_osc_portals4_module_t *module, 
                int target)
{
    int64_t result;
    int ret;

    while (true) {
        ret = lk_cas64(module, target, LOCK_EXCLUSIVE, 0, &result);
        if (OMPI_SUCCESS != ret) return ret;
        if (LOCK_ILLEGAL == (LOCK_ILLEGAL & result)) return OMPI_ERR_RMA_SYNC;
        if (0 == result) break;
    }

    return OMPI_SUCCESS;
}


static inline int
end_exclusive(ompi_osc_portals4_module_t *module, 
              int target)
{
    int ret;

    ret = lk_write64(module, target, LOCK_UNLOCKED);
    return ret;
}


static inline int
start_shared(ompi_osc_portals4_module_t *module, 
             int target)
{
    int64_t result;
    int ret;

    while (true) {
        ret = lk_add64(module, target, 1, &result);
        if (OMPI_SUCCESS != ret) return ret;

        if (result > (int64_t)LOCK_EXCLUSIVE) {
            if (LOCK_ILLEGAL == (LOCK_ILLEGAL & result)) return OMPI_ERR_RMA_SYNC;
            ret = lk_add64(module, target, -1, &result);
            if (OMPI_SUCCESS != ret) return ret;
        } else {
            break;
        }
    }

    return OMPI_SUCCESS;
}


static inline int
end_shared(ompi_osc_portals4_module_t *module, 
           int target)
{
    int64_t result;
    int ret;

    ret = lk_add64(module, target, -1, &result);
    return ret;
}


int
ompi_osc_portals4_lock(int lock_type,
                       int target,
                       int assert,
                       struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ompi_osc_portals4_outstanding_lock_t* lock;    
    int ret;

    module->passive_target_access_epoch = true;

    lock = OBJ_NEW(ompi_osc_portals4_outstanding_lock_t);
    lock->target = target;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        if (MPI_LOCK_EXCLUSIVE == lock_type) {
            lock->lock_type = lock_exclusive;
            ret = start_exclusive(module, target);
        } else {
            lock->lock_type = lock_shared;
            ret = start_shared(module, target);
        }
    } else {
        lock->lock_type = lock_nocheck;
        ret = OMPI_SUCCESS;
    }

    if (OMPI_SUCCESS == ret) {
        opal_list_append(&module->outstanding_locks, &lock->super);
    } else {
        OBJ_RELEASE(lock);
    }

    return ret;
}


int
ompi_osc_portals4_unlock(int target,
                         struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ompi_osc_portals4_outstanding_lock_t *lock = NULL, *item;
    int ret;

    OPAL_LIST_FOREACH(item, &module->outstanding_locks,
                      ompi_osc_portals4_outstanding_lock_t) {
        if (item->target == target) {
            lock = item;
            break;
        }
    }
    if (NULL != item) {
        opal_list_remove_item(&module->outstanding_locks, &lock->super);
    } else {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_osc_portals4_complete_all(module);
    if (ret != OMPI_SUCCESS) return ret;

    if (lock->lock_type == lock_exclusive) {
        ret = end_exclusive(module, target);
    } else if (lock->lock_type == lock_shared) {
        ret = end_shared(module, target);
    } else {
        ret = OMPI_SUCCESS;
    }

    module->passive_target_access_epoch = false;

    OBJ_RELEASE(lock);

    return ret;
}


int
ompi_osc_portals4_lock_all(int assert,
                           struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ompi_osc_portals4_outstanding_lock_t* lock;    
    int ret = OMPI_SUCCESS;

    module->passive_target_access_epoch = true;

    lock = OBJ_NEW(ompi_osc_portals4_outstanding_lock_t);
    lock->target = -1;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        int i, comm_size;

        lock->lock_type = lock_shared;
        comm_size = ompi_comm_size(module->comm);

        for (i = 0 ; i < comm_size ; ++i) {
            ret |= start_shared(module, i);
        }
    } else {
        lock->lock_type = lock_nocheck;
        ret = OMPI_SUCCESS;
    }

    if (OMPI_SUCCESS == ret) {
        opal_list_append(&module->outstanding_locks, &lock->super);
    } else {
        OBJ_RELEASE(lock);
    }

    return ret;
}


int
ompi_osc_portals4_unlock_all(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    ompi_osc_portals4_outstanding_lock_t *lock = NULL, *item;
    int ret;

    OPAL_LIST_FOREACH(item, &module->outstanding_locks,
                      ompi_osc_portals4_outstanding_lock_t) {
        if (item->target == -1) {
            lock = item;
            break;
        }
    }
    if (NULL != item) {
        opal_list_remove_item(&module->outstanding_locks, &lock->super);
    } else {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_osc_portals4_complete_all(module);
    if (ret != OMPI_SUCCESS) return ret;

    if (lock->lock_type == lock_shared) {
        int i, comm_size;

        comm_size = ompi_comm_size(module->comm);

        for (i = 0 ; i < comm_size ; ++i) {
            ret |= end_shared(module, i);
        }
    }

    module->passive_target_access_epoch = false;

    OBJ_RELEASE(lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_sync(struct ompi_win_t *win)
{
    /* Not sure this is strictly necessary, but why not? */
    opal_atomic_mb();
    PtlAtomicSync();

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_flush(int target,
                        struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_portals4_complete_all(module);
}


int
ompi_osc_portals4_flush_all(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_portals4_complete_all(module);
}


int
ompi_osc_portals4_flush_local(int target,
                              struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_portals4_complete_all(module);
}


int
ompi_osc_portals4_flush_local_all(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* flush is only allowed from within a passive target epoch */
    if (!module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_portals4_complete_all(module);
}
