/*
 * Copyright (C) 2001-2017 Mellanox Technologies Ltd. ALL RIGHTS RESERVED.
 * Copyright (c) 2019-2020 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation. All rights reserved.
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
#include "opal/mca/common/ucx/common_ucx.h"

#include "osc_ucx.h"
#include "osc_ucx_request.h"


#define CHECK_VALID_RKEY(_module, _target, _count)                               \
    if (!((_module)->win_info_array[_target]).rkey_init && ((_count) > 0)) {     \
        OSC_UCX_VERBOSE(1, "window with non-zero length does not have an rkey"); \
        return OMPI_ERROR;                                                       \
    }

typedef struct ucx_iovec {
    void *addr;
    size_t len;
} ucx_iovec_t;

static inline int check_sync_state(ompi_osc_ucx_module_t *module, int target,
                                   bool is_req_ops) {
    if (is_req_ops == false) {
        if (module->epoch_type.access == NONE_EPOCH) {
            return OMPI_ERR_RMA_SYNC;
        } else if (module->epoch_type.access == START_COMPLETE_EPOCH) {
            int i, size = ompi_group_size(module->start_group);
            for (i = 0; i < size; i++) {
                if (module->start_grp_ranks[i] == target) {
                    break;
                }
            }
            if (i == size) {
                return OMPI_ERR_RMA_SYNC;
            }
        } else if (module->epoch_type.access == PASSIVE_EPOCH) {
            ompi_osc_ucx_lock_t *item = NULL;
            opal_hash_table_get_value_uint32(&module->outstanding_locks, (uint32_t) target, (void **) &item);
            if (item == NULL) {
                return OMPI_ERR_RMA_SYNC;
            }
        }
    } else {
        if (module->epoch_type.access != PASSIVE_EPOCH &&
            module->epoch_type.access != PASSIVE_ALL_EPOCH) {
            return OMPI_ERR_RMA_SYNC;
        } else if (module->epoch_type.access == PASSIVE_EPOCH) {
            ompi_osc_ucx_lock_t *item = NULL;
            opal_hash_table_get_value_uint32(&module->outstanding_locks, (uint32_t) target, (void **) &item);
            if (item == NULL) {
                return OMPI_ERR_RMA_SYNC;
            }
        }
    }
    return OMPI_SUCCESS;
}

static inline int create_iov_list(const void *addr, int count, ompi_datatype_t *datatype,
                                  ucx_iovec_t **ucx_iov, uint32_t *ucx_iov_count) {
    int ret = OMPI_SUCCESS;
    size_t size;
    bool done = false;
    opal_convertor_t convertor;
    uint32_t iov_count, iov_idx;
    struct iovec iov[OSC_UCX_IOVEC_MAX];
    uint32_t ucx_iov_idx;

    OBJ_CONSTRUCT(&convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                   &datatype->super, count,
                                                   addr, 0, &convertor);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    (*ucx_iov_count) = 0;
    ucx_iov_idx = 0;

    do {
        iov_count = OSC_UCX_IOVEC_MAX;
        iov_idx = 0;

        done = opal_convertor_raw(&convertor, iov, &iov_count, &size);

        (*ucx_iov_count) += iov_count;
        (*ucx_iov) = (ucx_iovec_t *)realloc((*ucx_iov), (*ucx_iov_count) * sizeof(ucx_iovec_t));
        if (*ucx_iov == NULL) {
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        while (iov_idx != iov_count) {
            (*ucx_iov)[ucx_iov_idx].addr = iov[iov_idx].iov_base;
            (*ucx_iov)[ucx_iov_idx].len = iov[iov_idx].iov_len;
            ucx_iov_idx++;
            iov_idx++;
        }

        assert((*ucx_iov_count) == ucx_iov_idx);

    } while (!done);

    opal_convertor_cleanup(&convertor);
    OBJ_DESTRUCT(&convertor);

    return ret;
}

static inline int ddt_put_get(ompi_osc_ucx_module_t *module,
                              const void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_dt,
                              bool is_origin_contig, ptrdiff_t origin_lb,
                              int target, uint64_t remote_addr,
                              int target_count, struct ompi_datatype_t *target_dt,
                              bool is_target_contig, ptrdiff_t target_lb, bool is_get) {
    ucx_iovec_t *origin_ucx_iov = NULL, *target_ucx_iov = NULL;
    uint32_t origin_ucx_iov_count = 0, target_ucx_iov_count = 0;
    uint32_t origin_ucx_iov_idx = 0, target_ucx_iov_idx = 0;
    int status;
    int ret = OMPI_SUCCESS;

    if (!is_origin_contig) {
        ret = create_iov_list(origin_addr, origin_count, origin_dt,
                              &origin_ucx_iov, &origin_ucx_iov_count);
        if (ret != OMPI_SUCCESS) {
            goto cleanup;
        }
    }

    if (!is_target_contig) {
        ret = create_iov_list(NULL, target_count, target_dt,
                              &target_ucx_iov, &target_ucx_iov_count);
        if (ret != OMPI_SUCCESS) {
            goto cleanup;
        }
    }

    if (!is_origin_contig && !is_target_contig) {
        size_t curr_len = 0;
        opal_common_ucx_op_t op;
        while (origin_ucx_iov_idx < origin_ucx_iov_count) {
            curr_len = MIN(origin_ucx_iov[origin_ucx_iov_idx].len,
                           target_ucx_iov[target_ucx_iov_idx].len);
            if (is_get) {
                op = OPAL_COMMON_UCX_GET;
            } else {
                op = OPAL_COMMON_UCX_PUT;
            }
            status = opal_common_ucx_wpmem_putget(module->mem, op, target,
                                                origin_ucx_iov[origin_ucx_iov_idx].addr, curr_len,
                                                remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr));
            if (OPAL_SUCCESS != status) {
                OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", status);
                ret = OMPI_ERROR;
                goto cleanup;
            }

            origin_ucx_iov[origin_ucx_iov_idx].addr = (void *)((intptr_t)origin_ucx_iov[origin_ucx_iov_idx].addr + curr_len);
            target_ucx_iov[target_ucx_iov_idx].addr = (void *)((intptr_t)target_ucx_iov[target_ucx_iov_idx].addr + curr_len);

            origin_ucx_iov[origin_ucx_iov_idx].len -= curr_len;
            if (origin_ucx_iov[origin_ucx_iov_idx].len == 0) {
                origin_ucx_iov_idx++;
            }
            target_ucx_iov[target_ucx_iov_idx].len -= curr_len;
            if (target_ucx_iov[target_ucx_iov_idx].len == 0) {
                target_ucx_iov_idx++;
            }
        }

        assert(origin_ucx_iov_idx == origin_ucx_iov_count &&
               target_ucx_iov_idx == target_ucx_iov_count);

    } else if (!is_origin_contig) {
        size_t prev_len = 0;
        opal_common_ucx_op_t op;
        while (origin_ucx_iov_idx < origin_ucx_iov_count) {
            if (is_get) {
                op = OPAL_COMMON_UCX_GET;
            } else {
                op = OPAL_COMMON_UCX_PUT;
            }
            status = opal_common_ucx_wpmem_putget(module->mem, op, target,
                                                origin_ucx_iov[origin_ucx_iov_idx].addr,
                                                origin_ucx_iov[origin_ucx_iov_idx].len,
                                                remote_addr + target_lb + prev_len);
            if (OPAL_SUCCESS != status) {
                OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", status);
                ret = OMPI_ERROR;
                goto cleanup;
            }

            prev_len += origin_ucx_iov[origin_ucx_iov_idx].len;
            origin_ucx_iov_idx++;
        }
    } else {
        size_t prev_len = 0;
        opal_common_ucx_op_t op;
        while (target_ucx_iov_idx < target_ucx_iov_count) {
            if (is_get) {
                op = OPAL_COMMON_UCX_GET;
            } else {
                op = OPAL_COMMON_UCX_PUT;
            }

            status = opal_common_ucx_wpmem_putget(module->mem, op, target,
                                                (void *)((intptr_t)origin_addr + origin_lb + prev_len),
                                                target_ucx_iov[target_ucx_iov_idx].len,
                                                remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr));
            if (OPAL_SUCCESS != status) {
                OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", status);
                ret = OMPI_ERROR;
                goto cleanup;
            }

            prev_len += target_ucx_iov[target_ucx_iov_idx].len;
            target_ucx_iov_idx++;
        }
    }

cleanup:

    if (origin_ucx_iov != NULL) {
        free(origin_ucx_iov);
    }
    if (target_ucx_iov != NULL) {
        free(target_ucx_iov);
    }

    return ret;
}

static inline bool need_acc_lock(ompi_osc_ucx_module_t *module, int target)
{
    ompi_osc_ucx_lock_t *lock = NULL;
    opal_hash_table_get_value_uint32(&module->outstanding_locks,
                                     (uint32_t) target, (void **) &lock);

    /* if there is an exclusive lock there is no need to acqurie the accumulate lock */
    return !(NULL != lock && LOCK_EXCLUSIVE == lock->type);
}

static inline int start_atomicity(
    ompi_osc_ucx_module_t *module,
    int                    target,
    bool                  *lock_acquired) {
    uint64_t result_value = -1;
    uint64_t remote_addr = (module->state_addrs)[target] + OSC_UCX_STATE_ACC_LOCK_OFFSET;
    int ret = OMPI_SUCCESS;

    if (need_acc_lock(module, target)) {
        for (;;) {
            ret = opal_common_ucx_wpmem_cmpswp(module->state_mem,
                                            TARGET_LOCK_UNLOCKED, TARGET_LOCK_EXCLUSIVE,
                                            target, &result_value, sizeof(result_value),
                                            remote_addr);
            if (ret != OMPI_SUCCESS) {
                OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_cmpswp failed: %d", ret);
                return OMPI_ERROR;
            }
            if (result_value == TARGET_LOCK_UNLOCKED) {
                break;
            }

            opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);
        }

        *lock_acquired = true;
    } else {
        *lock_acquired = false;
    }

    return OMPI_SUCCESS;
}

static inline int end_atomicity(
    ompi_osc_ucx_module_t *module,
    int                    target,
    bool                   lock_acquired,
    void                  *free_ptr) {
    uint64_t remote_addr = (module->state_addrs)[target] + OSC_UCX_STATE_ACC_LOCK_OFFSET;
    int ret = OMPI_SUCCESS;

    if (lock_acquired) {
        uint64_t result_value = 0;
        /* fence any still active operations */
        ret = opal_common_ucx_wpmem_fence(module->mem);
        if (ret != OMPI_SUCCESS) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_fence failed: %d", ret);
            return OMPI_ERROR;
        }

        ret = opal_common_ucx_wpmem_fetch(module->state_mem,
                                        UCP_ATOMIC_FETCH_OP_SWAP, TARGET_LOCK_UNLOCKED,
                                        target, &result_value, sizeof(result_value),
                                        remote_addr);
        assert(result_value == TARGET_LOCK_EXCLUSIVE);
    } else if (NULL != free_ptr){
        /* flush before freeing the buffer */
        ret = opal_common_ucx_wpmem_flush(module->state_mem, OPAL_COMMON_UCX_SCOPE_EP, target);
    }
    /* TODO: encapsulate in a request and make the release non-blocking */
    if (NULL != free_ptr) {
        free(free_ptr);
    }
    if (ret != OMPI_SUCCESS) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_fetch failed: %d", ret);
        return OMPI_ERROR;
    }

    return ret;
}

static inline int get_dynamic_win_info(uint64_t remote_addr, ompi_osc_ucx_module_t *module,
                                       int target, int *win_idx) {
    uint64_t remote_state_addr = (module->state_addrs)[target] + OSC_UCX_STATE_DYNAMIC_WIN_CNT_OFFSET;
    size_t remote_state_len = sizeof(uint64_t) + sizeof(ompi_osc_dynamic_win_info_t) * OMPI_OSC_UCX_ATTACH_MAX;
    char *temp_buf = calloc(remote_state_len, 1);
    ompi_osc_dynamic_win_info_t *temp_dynamic_wins;
    uint64_t win_count;
    int insert = -1;
    int ret;

    ret = opal_common_ucx_wpmem_putget(module->state_mem, OPAL_COMMON_UCX_GET, target,
                                       (void *)((intptr_t)temp_buf),
                                       remote_state_len, remote_state_addr);
    if (OPAL_SUCCESS != ret) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", ret);
        ret = OMPI_ERROR;
        goto cleanup;
    }

    ret = opal_common_ucx_wpmem_flush(module->state_mem, OPAL_COMMON_UCX_SCOPE_EP, target);
    if (ret != OPAL_SUCCESS) {
        ret = OMPI_ERROR;
        goto cleanup;
    }

    memcpy(&win_count, temp_buf, sizeof(uint64_t));
    if (win_count > OMPI_OSC_UCX_ATTACH_MAX) {
        return MPI_ERR_RMA_RANGE;
    }

    temp_dynamic_wins = (ompi_osc_dynamic_win_info_t *)(temp_buf + sizeof(uint64_t));
    *win_idx = ompi_osc_find_attached_region_position(temp_dynamic_wins, 0, win_count,
                                                     remote_addr, 1, &insert);
    if (*win_idx < 0 || (uint64_t)*win_idx >= win_count) {
        return MPI_ERR_RMA_RANGE;
    }

    if (module->local_dynamic_win_info[*win_idx].mem->mem_addrs == NULL) {
        module->local_dynamic_win_info[*win_idx].mem->mem_addrs = calloc(ompi_comm_size(module->comm),
                                                                        OMPI_OSC_UCX_MEM_ADDR_MAX_LEN);
        module->local_dynamic_win_info[*win_idx].mem->mem_displs = calloc(ompi_comm_size(module->comm),
                                                                        sizeof(int));
    }

    memcpy(module->local_dynamic_win_info[*win_idx].mem->mem_addrs + target * OMPI_OSC_UCX_MEM_ADDR_MAX_LEN,
           temp_dynamic_wins[*win_idx].mem_addr, OMPI_OSC_UCX_MEM_ADDR_MAX_LEN);
    module->local_dynamic_win_info[*win_idx].mem->mem_displs[target] = target * OMPI_OSC_UCX_MEM_ADDR_MAX_LEN;

cleanup:
    free(temp_buf);

    return ret;
}

static inline
bool use_atomic_op(
    ompi_osc_ucx_module_t  *module,
    struct ompi_op_t       *op,
    uint64_t                remote_addr,
    struct ompi_datatype_t *origin_dt,
    struct ompi_datatype_t *target_dt,
    int                     origin_count,
    int                     target_count)
{

    if (module->acc_single_intrinsic &&
        ompi_datatype_is_predefined(origin_dt) &&
        origin_count == 1 &&
        (op == &ompi_mpi_op_replace.op ||
         op == &ompi_mpi_op_sum.op     ||
         op == &ompi_mpi_op_no_op.op)) {
        size_t origin_dt_bytes;
        size_t target_dt_bytes;
        ompi_datatype_type_size(origin_dt, &origin_dt_bytes);
        ompi_datatype_type_size(target_dt, &target_dt_bytes);
        /* UCX only supports 32 and 64-bit operands atm */
        if (ompi_osc_base_is_atomic_size_supported(remote_addr, origin_dt_bytes) &&
            origin_dt_bytes == target_dt_bytes && origin_count == target_count) {
            return true;
        }
    }

    return false;
}

static int do_atomic_op_intrinsic(
    ompi_osc_ucx_module_t  *module,
    struct ompi_op_t       *op,
    int                     target,
    const void             *origin_addr,
    int                     count,
    struct ompi_datatype_t *dt,
    ptrdiff_t               target_disp,
    void                   *result_addr,
    ompi_osc_ucx_request_t *ucx_req)
{
    int ret = OMPI_SUCCESS,
        win_idx = -1;
    size_t origin_dt_bytes;
    opal_common_ucx_wpmem_t *mem = module->mem;
    ompi_datatype_type_size(dt, &origin_dt_bytes);

    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    ucp_atomic_fetch_op_t opcode;
    bool is_no_op = false;
    if (op == &ompi_mpi_op_replace.op) {
        opcode = UCP_ATOMIC_FETCH_OP_SWAP;
    } else {
        opcode = UCP_ATOMIC_FETCH_OP_FADD;
        if (op == &ompi_mpi_op_no_op.op) {
            is_no_op = true;
        }
    }

    opal_common_ucx_user_req_handler_t user_req_cb = NULL;
    void *user_req_ptr = NULL;
    void *output_addr  = &(module->req_result);
    if( result_addr ) {
        output_addr = result_addr;
    }
    for (int i = 0; i < count; ++i) {
        uint64_t value = 0;
        if ((count - 1) == i && NULL != ucx_req) {
            // the last item is used to feed the request, if needed
            user_req_cb = &req_completion;
            user_req_ptr = ucx_req;
            // issue a fence if this is the last but not the only element
            if (0 < i) {
                ret = opal_common_ucx_wpmem_fence(mem);
                if (ret != OMPI_SUCCESS) {
                    OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_fence failed: %d", ret);
                    return OMPI_ERROR;
                }
            }
        }
        if (is_no_op) {
            value = 0;
        } else {
            value = opal_common_ucx_load_uint64(origin_addr, origin_dt_bytes);
        }
        ret = opal_common_ucx_wpmem_fetch_nb(mem, opcode, value, target,
                                             output_addr, origin_dt_bytes, remote_addr,
                                             user_req_cb, user_req_ptr);

        // advance origin and remote address
        origin_addr  = (void*)((intptr_t)origin_addr + origin_dt_bytes);
        remote_addr += origin_dt_bytes;
        if (result_addr) {
            output_addr = (void*)((intptr_t)output_addr + origin_dt_bytes);
        }
    }

    return ret;
}

int ompi_osc_ucx_put(const void *origin_addr, int origin_count, struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    bool is_origin_contig = false, is_target_contig = false;
    ptrdiff_t origin_lb, origin_extent, target_lb, target_extent;
    int ret = OMPI_SUCCESS, win_idx = -1;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    if (!target_count) {
        return OMPI_SUCCESS;
    }

    ompi_datatype_get_true_extent(origin_dt, &origin_lb, &origin_extent);
    ompi_datatype_get_true_extent(target_dt, &target_lb, &target_extent);

    is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);
    is_target_contig = ompi_datatype_is_contiguous_memory_layout(target_dt, target_count);

    if (is_origin_contig && is_target_contig) {
        /* fast path */
        size_t origin_len;

        ompi_datatype_type_size(origin_dt, &origin_len);
        origin_len *= origin_count;

        ret = opal_common_ucx_wpmem_putget(mem, OPAL_COMMON_UCX_PUT, target,
                                         (void *)((intptr_t)origin_addr + origin_lb),
                                         origin_len, remote_addr + target_lb);
        if (OPAL_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", ret);
            return OMPI_ERROR;
        }
        return ret;
    } else {
        return ddt_put_get(module, origin_addr, origin_count, origin_dt, is_origin_contig,
                           origin_lb, target, remote_addr, target_count, target_dt,
                           is_target_contig, target_lb, false);
    }
}

int ompi_osc_ucx_get(void *origin_addr, int origin_count,
                     struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    ptrdiff_t origin_lb, origin_extent, target_lb, target_extent;
    bool is_origin_contig = false, is_target_contig = false;
    int ret = OMPI_SUCCESS, win_idx = -1;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    if (!target_count) {
        return OMPI_SUCCESS;
    }


    ompi_datatype_get_true_extent(origin_dt, &origin_lb, &origin_extent);
    ompi_datatype_get_true_extent(target_dt, &target_lb, &target_extent);

    is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);
    is_target_contig = ompi_datatype_is_contiguous_memory_layout(target_dt, target_count);

    if (is_origin_contig && is_target_contig) {
        /* fast path */
        size_t origin_len;

        ompi_datatype_type_size(origin_dt, &origin_len);
        origin_len *= origin_count;

        ret = opal_common_ucx_wpmem_putget(mem, OPAL_COMMON_UCX_GET, target,
                                         (void *)((intptr_t)origin_addr + origin_lb),
                                         origin_len, remote_addr + target_lb);
        if (OPAL_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", ret);
            return OMPI_ERROR;
        }

        return ret;
    } else {
        return ddt_put_get(module, origin_addr, origin_count, origin_dt, is_origin_contig,
                           origin_lb, target, remote_addr, target_count, target_dt,
                           is_target_contig, target_lb, true);
    }
}

static
int accumulate_req(const void *origin_addr, int origin_count,
                   struct ompi_datatype_t *origin_dt,
                   int target, ptrdiff_t target_disp, int target_count,
                   struct ompi_datatype_t *target_dt,
                   struct ompi_op_t *op, struct ompi_win_t *win,
                   ompi_osc_ucx_request_t *ucx_req) {

    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;
    void *free_ptr = NULL;
    bool lock_acquired = false;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (op == &ompi_mpi_op_no_op.op) {
        return ret;
    }

    /* rely on UCX network atomics if the user told us that it safe */
    if (use_atomic_op(module, op, target_disp, origin_dt, target_dt, origin_count, target_count)) {
        return do_atomic_op_intrinsic(module, op, target,
                                      origin_addr, origin_count, origin_dt,
                                      target_disp, NULL, ucx_req);
    }

    ret = start_atomicity(module, target, &lock_acquired);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_osc_ucx_put(origin_addr, origin_count, origin_dt, target,
                               target_disp, target_count, target_dt, win);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    } else {
        void *temp_addr = NULL;
        uint32_t temp_count;
        ompi_datatype_t *temp_dt;
        ptrdiff_t temp_lb, temp_extent;
        bool is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);

        if (ompi_datatype_is_predefined(target_dt)) {
            temp_dt = target_dt;
            temp_count = target_count;
        } else {
            ret = ompi_osc_base_get_primitive_type_info(target_dt, &temp_dt, &temp_count);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
            temp_count *= target_count;
        }
        ompi_datatype_get_true_extent(temp_dt, &temp_lb, &temp_extent);
        temp_addr = free_ptr = malloc(temp_extent * temp_count);
        if (temp_addr == NULL) {
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        ret = ompi_osc_ucx_get(temp_addr, (int)temp_count, temp_dt,
                               target, target_disp, target_count, target_dt, win);
        if (ret != OMPI_SUCCESS) {
            free(temp_addr);
            return ret;
        }

        ret = opal_common_ucx_wpmem_flush(module->mem, OPAL_COMMON_UCX_SCOPE_EP, target);
        if (ret != OMPI_SUCCESS) {
            free(temp_addr);
            return ret;
        }

        if (ompi_datatype_is_predefined(origin_dt) || is_origin_contig) {
            ompi_op_reduce(op, (void *)origin_addr, temp_addr, (int)temp_count, temp_dt);
        } else {
            ucx_iovec_t *origin_ucx_iov = NULL;
            uint32_t origin_ucx_iov_count = 0;
            uint32_t origin_ucx_iov_idx = 0;

            ret = create_iov_list(origin_addr, origin_count, origin_dt,
                                  &origin_ucx_iov, &origin_ucx_iov_count);
            if (ret != OMPI_SUCCESS) {
                free(temp_addr);
                return ret;
            }

            if ((op != &ompi_mpi_op_maxloc.op && op != &ompi_mpi_op_minloc.op) ||
                ompi_datatype_is_contiguous_memory_layout(temp_dt, temp_count)) {
                size_t temp_size;
                char *curr_temp_addr = (char *)temp_addr;
                ompi_datatype_type_size(temp_dt, &temp_size);
                while (origin_ucx_iov_idx < origin_ucx_iov_count) {
                    int curr_count = origin_ucx_iov[origin_ucx_iov_idx].len / temp_size;
                    ompi_op_reduce(op, origin_ucx_iov[origin_ucx_iov_idx].addr,
                                   curr_temp_addr, curr_count, temp_dt);
                    curr_temp_addr += curr_count * temp_size;
                    origin_ucx_iov_idx++;
                }
            } else {
                int i;
                void *curr_origin_addr = origin_ucx_iov[origin_ucx_iov_idx].addr;
                for (i = 0; i < (int)temp_count; i++) {
                    ompi_op_reduce(op, curr_origin_addr,
                                   (void *)((char *)temp_addr + i * temp_extent),
                                   1, temp_dt);
                    curr_origin_addr = (void *)((char *)curr_origin_addr + temp_extent);
                    origin_ucx_iov_idx++;
                    if (curr_origin_addr >= (void *)((char *)origin_ucx_iov[origin_ucx_iov_idx].addr + origin_ucx_iov[origin_ucx_iov_idx].len)) {
                        origin_ucx_iov_idx++;
                        curr_origin_addr = origin_ucx_iov[origin_ucx_iov_idx].addr;
                    }
                }
            }

            free(origin_ucx_iov);
        }

        ret = ompi_osc_ucx_put(temp_addr, (int)temp_count, temp_dt, target, target_disp,
                               target_count, target_dt, win);
        if (ret != OMPI_SUCCESS) {
            free(temp_addr);
            return ret;
        }

    }

    if (NULL != ucx_req) {
        // nothing to wait for, mark request as completed
        ompi_request_complete(&ucx_req->super, true);
    }

    return end_atomicity(module, target, lock_acquired, free_ptr);
}

int ompi_osc_ucx_accumulate(const void *origin_addr, int origin_count,
                            struct ompi_datatype_t *origin_dt,
                            int target, ptrdiff_t target_disp, int target_count,
                            struct ompi_datatype_t *target_dt,
                            struct ompi_op_t *op, struct ompi_win_t *win) {
    return accumulate_req(origin_addr, origin_count, origin_dt, target,
                          target_disp, target_count, target_dt, op, win, NULL);
}

static int
do_atomic_compare_and_swap(const void *origin_addr, const void *compare_addr,
                           void *result_addr, struct ompi_datatype_t *dt,
                           int target, uint64_t remote_addr,
                           ompi_osc_ucx_module_t *module)
{
    int ret;
    bool lock_acquired = false;
    size_t dt_bytes;
    if (!module->acc_single_intrinsic) {
        ret = start_atomicity(module, target, &lock_acquired);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    }

    ompi_datatype_type_size(dt, &dt_bytes);
    uint64_t compare_val = opal_common_ucx_load_uint64(compare_addr, dt_bytes);
    uint64_t value       = opal_common_ucx_load_uint64(origin_addr,  dt_bytes);
    ret = opal_common_ucx_wpmem_cmpswp_nb(module->mem, compare_val, value, target,
                                          result_addr, dt_bytes, remote_addr,
                                          NULL, NULL);

    if (module->acc_single_intrinsic) {
        return ret;
    }

    return end_atomicity(module, target, lock_acquired, NULL);
}

int ompi_osc_ucx_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                  void *result_addr, struct ompi_datatype_t *dt,
                                  int target, ptrdiff_t target_disp,
                                  struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t *)win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    size_t dt_bytes;
    int ret = OMPI_SUCCESS, win_idx = -1;
    bool lock_acquired = false;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    ompi_datatype_type_size(dt, &dt_bytes);
    if (ompi_osc_base_is_atomic_size_supported(remote_addr, dt_bytes)) {
        // fast path using UCX atomic operations
        return do_atomic_compare_and_swap(origin_addr, compare_addr,
                                          result_addr, dt, target,
                                          remote_addr, module);
    }

    /* fall back to get-compare-put */

    ret = start_atomicity(module, target, &lock_acquired);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = opal_common_ucx_wpmem_putget(mem, OPAL_COMMON_UCX_GET, target,
                                       &result_addr, dt_bytes, remote_addr);
    if (OPAL_SUCCESS != ret) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", ret);
        return OMPI_ERROR;
    }

    ret = opal_common_ucx_wpmem_flush(mem, OPAL_COMMON_UCX_SCOPE_EP, target);
    if (ret != OPAL_SUCCESS) {
        return ret;
    }

    if (0 == memcmp(result_addr, compare_addr, dt_bytes)) {
        // write the new value
        ret = opal_common_ucx_wpmem_putget(mem, OPAL_COMMON_UCX_PUT, target,
                                           (void*)origin_addr, dt_bytes, remote_addr);
        if (OPAL_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_putget failed: %d", ret);
            return OMPI_ERROR;
        }
    }

    return end_atomicity(module, target, lock_acquired, NULL);
}

int ompi_osc_ucx_fetch_and_op(const void *origin_addr, void *result_addr,
                              struct ompi_datatype_t *dt, int target,
                              ptrdiff_t target_disp, struct ompi_op_t *op,
                              struct ompi_win_t *win) {
    size_t dt_bytes;
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    int ret = OMPI_SUCCESS, win_idx = -1;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    ompi_datatype_type_size(dt, &dt_bytes);

    /* UCX atomics are only supported on 32 and 64 bit values */
    if (ompi_osc_base_is_atomic_size_supported(remote_addr, dt_bytes) &&
        (op == &ompi_mpi_op_no_op.op || op == &ompi_mpi_op_replace.op ||
         op == &ompi_mpi_op_sum.op)) {
        uint64_t value;
        ucp_atomic_fetch_op_t opcode;
        bool lock_acquired = false;

        if (!module->acc_single_intrinsic) {
            ret = start_atomicity(module, target, &lock_acquired);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
        }

        if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
            ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
            mem = module->local_dynamic_win_info[win_idx].mem;
        }

        value = origin_addr ? opal_common_ucx_load_uint64(origin_addr, dt_bytes) : 0;

        if (op == &ompi_mpi_op_replace.op) {
            opcode = UCP_ATOMIC_FETCH_OP_SWAP;
        } else {
            opcode = UCP_ATOMIC_FETCH_OP_FADD;
            if (op == &ompi_mpi_op_no_op.op) {
                value = 0;
            }
        }

        ret = opal_common_ucx_wpmem_fetch_nb(mem, opcode, value, target,
                                             (void *)result_addr, dt_bytes,
                                             remote_addr, NULL, NULL);

        if (module->acc_single_intrinsic) {
            return ret;
        }

        return end_atomicity(module, target, lock_acquired, NULL);
    } else {
        return ompi_osc_ucx_get_accumulate(origin_addr, 1, dt, result_addr, 1, dt,
                                           target, target_disp, 1, dt, op, win);
    }
}

static
int get_accumulate_req(const void *origin_addr, int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       void *result_addr, int result_count,
                       struct ompi_datatype_t *result_dt,
                       int target, ptrdiff_t target_disp,
                       int target_count, struct ompi_datatype_t *target_dt,
                       struct ompi_op_t *op, struct ompi_win_t *win,
                       ompi_osc_ucx_request_t *ucx_req) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;
    void *free_addr = NULL;
    bool lock_acquired = false;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    /* rely on UCX network atomics if the user told us that it safe */
    if (use_atomic_op(module, op, target_disp, origin_dt, target_dt, origin_count, target_count)) {
        return do_atomic_op_intrinsic(module, op, target,
                                      origin_addr, origin_count, origin_dt,
                                      target_disp, result_addr, ucx_req);
    }

    ret = start_atomicity(module, target, &lock_acquired);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = ompi_osc_ucx_get(result_addr, result_count, result_dt, target,
                           target_disp, target_count, target_dt, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (op != &ompi_mpi_op_no_op.op) {
        if (op == &ompi_mpi_op_replace.op) {
            ret = ompi_osc_ucx_put(origin_addr, origin_count, origin_dt,
                                   target, target_disp, target_count,
                                   target_dt, win);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
        } else {
            void *temp_addr = NULL;
            uint32_t temp_count;
            ompi_datatype_t *temp_dt;
            ptrdiff_t temp_lb, temp_extent;
            bool is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);

            if (ompi_datatype_is_predefined(target_dt)) {
                temp_dt = target_dt;
                temp_count = target_count;
            } else {
                ret = ompi_osc_base_get_primitive_type_info(target_dt, &temp_dt, &temp_count);
                if (ret != OMPI_SUCCESS) {
                    return ret;
                }
            }
            ompi_datatype_get_true_extent(temp_dt, &temp_lb, &temp_extent);
            temp_addr = free_addr = malloc(temp_extent * temp_count);
            if (temp_addr == NULL) {
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            }

            ret = ompi_osc_ucx_get(temp_addr, (int)temp_count, temp_dt,
                                   target, target_disp, target_count, target_dt, win);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            ret = opal_common_ucx_wpmem_flush(module->mem, OPAL_COMMON_UCX_SCOPE_EP, target);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            if (ompi_datatype_is_predefined(origin_dt) || is_origin_contig) {
                ompi_op_reduce(op, (void *)origin_addr, temp_addr, (int)temp_count, temp_dt);
            } else {
                ucx_iovec_t *origin_ucx_iov = NULL;
                uint32_t origin_ucx_iov_count = 0;
                uint32_t origin_ucx_iov_idx = 0;

                ret = create_iov_list(origin_addr, origin_count, origin_dt,
                                      &origin_ucx_iov, &origin_ucx_iov_count);
                if (ret != OMPI_SUCCESS) {
                    return ret;
                }

                if ((op != &ompi_mpi_op_maxloc.op && op != &ompi_mpi_op_minloc.op) ||
                    ompi_datatype_is_contiguous_memory_layout(temp_dt, temp_count)) {
                    size_t temp_size;
                    char *curr_temp_addr = (char *)temp_addr;
                    ompi_datatype_type_size(temp_dt, &temp_size);
                    while (origin_ucx_iov_idx < origin_ucx_iov_count) {
                        int curr_count = origin_ucx_iov[origin_ucx_iov_idx].len / temp_size;
                        ompi_op_reduce(op, origin_ucx_iov[origin_ucx_iov_idx].addr,
                                       curr_temp_addr, curr_count, temp_dt);
                        curr_temp_addr += curr_count * temp_size;
                        origin_ucx_iov_idx++;
                    }
                } else {
                    int i;
                    void *curr_origin_addr = origin_ucx_iov[origin_ucx_iov_idx].addr;
                    for (i = 0; i < (int)temp_count; i++) {
                        ompi_op_reduce(op, curr_origin_addr,
                                       (void *)((char *)temp_addr + i * temp_extent),
                                       1, temp_dt);
                        curr_origin_addr = (void *)((char *)curr_origin_addr + temp_extent);
                        origin_ucx_iov_idx++;
                        if (curr_origin_addr >= (void *)((char *)origin_ucx_iov[origin_ucx_iov_idx].addr + origin_ucx_iov[origin_ucx_iov_idx].len)) {
                            origin_ucx_iov_idx++;
                            curr_origin_addr = origin_ucx_iov[origin_ucx_iov_idx].addr;
                        }
                    }
                }
                free(origin_ucx_iov);
            }

            ret = ompi_osc_ucx_put(temp_addr, (int)temp_count, temp_dt, target, target_disp,
                                   target_count, target_dt, win);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
        }
    }

    if (NULL != ucx_req) {
        // nothing to wait for, mark request as completed
        ompi_request_complete(&ucx_req->super, true);
    }


    return end_atomicity(module, target, lock_acquired, free_addr);
}

int ompi_osc_ucx_get_accumulate(const void *origin_addr, int origin_count,
                                struct ompi_datatype_t *origin_dt,
                                void *result_addr, int result_count,
                                struct ompi_datatype_t *result_dt,
                                int target, ptrdiff_t target_disp,
                                int target_count, struct ompi_datatype_t *target_dt,
                                struct ompi_op_t *op, struct ompi_win_t *win) {

    return get_accumulate_req(origin_addr, origin_count, origin_dt, result_addr,
                              result_count, result_dt, target, target_disp,
                              target_count, target_dt, op, win, NULL);
}

int ompi_osc_ucx_rput(const void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win, struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    ompi_osc_ucx_request_t *ucx_req = NULL;
    int ret = OMPI_SUCCESS, win_idx = -1;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = ompi_osc_ucx_put(origin_addr, origin_count, origin_dt, target, target_disp,
                           target_count, target_dt, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = opal_common_ucx_wpmem_fence(mem);
    if (ret != OMPI_SUCCESS) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_fence failed: %d", ret);
        return OMPI_ERROR;
    }

    mca_osc_ucx_component.num_incomplete_req_ops++;
    /* TODO: investigate whether ucp_worker_flush_nb is a better choice here */
    ret = opal_common_ucx_wpmem_fetch_nb(mem, UCP_ATOMIC_FETCH_OP_FADD,
                                         0, target, &(module->req_result),
                                         sizeof(uint64_t), remote_addr & (~0x7),
                                         req_completion, ucx_req);
    if (ret != OMPI_SUCCESS) {
        OMPI_OSC_UCX_REQUEST_RETURN(ucx_req);
        return ret;
    }

    *request = &ucx_req->super;

    return ret;
}

int ompi_osc_ucx_rget(void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt, struct ompi_win_t *win,
                      struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    opal_common_ucx_wpmem_t *mem = module->mem;
    uint64_t remote_addr = (module->addrs[target]) + target_disp * OSC_UCX_GET_DISP(module, target);
    ompi_osc_ucx_request_t *ucx_req = NULL;
    int ret = OMPI_SUCCESS, win_idx = -1;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        ret = get_dynamic_win_info(remote_addr, module, target, &win_idx);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
        mem = module->local_dynamic_win_info[win_idx].mem;
    }

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = ompi_osc_ucx_get(origin_addr, origin_count, origin_dt, target, target_disp,
                           target_count, target_dt, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = opal_common_ucx_wpmem_fence(mem);
    if (ret != OMPI_SUCCESS) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_fence failed: %d", ret);
        return OMPI_ERROR;
    }

    mca_osc_ucx_component.num_incomplete_req_ops++;
    /* TODO: investigate whether ucp_worker_flush_nb is a better choice here */
    ret = opal_common_ucx_wpmem_fetch_nb(mem, UCP_ATOMIC_FETCH_OP_FADD,
                                         0, target, &(module->req_result),
                                         sizeof(uint64_t), remote_addr & (~0x7),
                                         req_completion, ucx_req);
    if (ret != OMPI_SUCCESS) {
        OMPI_OSC_UCX_REQUEST_RETURN(ucx_req);
        return ret;
    }

    *request = &ucx_req->super;

    return ret;
}

int ompi_osc_ucx_raccumulate(const void *origin_addr, int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target, ptrdiff_t target_disp, int target_count,
                             struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                             struct ompi_win_t *win, struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ompi_osc_ucx_request_t *ucx_req = NULL;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = accumulate_req(origin_addr, origin_count, origin_dt, target, target_disp,
                         target_count, target_dt, op, win, ucx_req);
    if (ret != OMPI_SUCCESS) {
        OMPI_OSC_UCX_REQUEST_RETURN(ucx_req);
        return ret;
    }

    *request = &ucx_req->super;

    return ret;
}

int ompi_osc_ucx_rget_accumulate(const void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_datatype,
                                 void *result_addr, int result_count,
                                 struct ompi_datatype_t *result_datatype,
                                 int target, ptrdiff_t target_disp, int target_count,
                                 struct ompi_datatype_t *target_datatype,
                                 struct ompi_op_t *op, struct ompi_win_t *win,
                                 struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ompi_osc_ucx_request_t *ucx_req = NULL;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = get_accumulate_req(origin_addr, origin_count, origin_datatype,
                             result_addr, result_count, result_datatype,
                             target, target_disp, target_count,
                             target_datatype, op, win, ucx_req);
    if (ret != OMPI_SUCCESS) {
        OMPI_OSC_UCX_REQUEST_RETURN(ucx_req);
        return ret;
    }

    *request = &ucx_req->super;

    return ret;
}
