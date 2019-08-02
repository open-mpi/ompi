/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
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

static inline int incr_and_check_ops_num(ompi_osc_ucx_module_t *module, int target,
                                         ucp_ep_h ep) {
    int status;

    module->global_ops_num++;
    module->per_target_ops_nums[target]++;
    if (module->global_ops_num >= OSC_UCX_OPS_THRESHOLD) {
        status = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
        if (status != OMPI_SUCCESS) {
            return status;
        }
        module->global_ops_num -= module->per_target_ops_nums[target];
        module->per_target_ops_nums[target] = 0;
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
                              int target, ucp_ep_h ep, uint64_t remote_addr, ucp_rkey_h rkey,
                              int target_count, struct ompi_datatype_t *target_dt,
                              bool is_target_contig, ptrdiff_t target_lb, bool is_get) {
    ucx_iovec_t *origin_ucx_iov = NULL, *target_ucx_iov = NULL;
    uint32_t origin_ucx_iov_count = 0, target_ucx_iov_count = 0;
    uint32_t origin_ucx_iov_idx = 0, target_ucx_iov_idx = 0;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    if (!is_origin_contig) {
        ret = create_iov_list(origin_addr, origin_count, origin_dt,
                              &origin_ucx_iov, &origin_ucx_iov_count);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    }

    if (!is_target_contig) {
        ret = create_iov_list(NULL, target_count, target_dt,
                              &target_ucx_iov, &target_ucx_iov_count);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    }

    if (!is_origin_contig && !is_target_contig) {
        size_t curr_len = 0;
        while (origin_ucx_iov_idx < origin_ucx_iov_count) {
            curr_len = MIN(origin_ucx_iov[origin_ucx_iov_idx].len,
                           target_ucx_iov[target_ucx_iov_idx].len);

            if (!is_get) {
                status = ucp_put_nbi(ep, origin_ucx_iov[origin_ucx_iov_idx].addr, curr_len,
                                     remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr), rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_put_nbi failed: %d", status);
                    return OMPI_ERROR;
                }
            } else {
                status = ucp_get_nbi(ep, origin_ucx_iov[origin_ucx_iov_idx].addr, curr_len,
                                     remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr), rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_get_nbi failed: %d",status);
                    return OMPI_ERROR;
                }
            }

            ret = incr_and_check_ops_num(module, target, ep);
            if (ret != OMPI_SUCCESS) {
                return ret;
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
        while (origin_ucx_iov_idx < origin_ucx_iov_count) {
            if (!is_get) {
                status = ucp_put_nbi(ep, origin_ucx_iov[origin_ucx_iov_idx].addr,
                                     origin_ucx_iov[origin_ucx_iov_idx].len,
                                     remote_addr + target_lb + prev_len, rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_put_nbi failed: %d", status);
                    return OMPI_ERROR;
                }
            } else {
                status = ucp_get_nbi(ep, origin_ucx_iov[origin_ucx_iov_idx].addr,
                                     origin_ucx_iov[origin_ucx_iov_idx].len,
                                     remote_addr + target_lb + prev_len, rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_get_nbi failed: %d", status);
                    return OMPI_ERROR;
                }
            }

            ret = incr_and_check_ops_num(module, target, ep);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            prev_len += origin_ucx_iov[origin_ucx_iov_idx].len;
            origin_ucx_iov_idx++;
        }
    } else {
        size_t prev_len = 0;
        while (target_ucx_iov_idx < target_ucx_iov_count) {
            if (!is_get) {
                status = ucp_put_nbi(ep, (void *)((intptr_t)origin_addr + origin_lb + prev_len),
                                     target_ucx_iov[target_ucx_iov_idx].len,
                                     remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr), rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_put_nbi failed: %d", status);
                    return OMPI_ERROR;
                }
            } else {
                status = ucp_get_nbi(ep, (void *)((intptr_t)origin_addr + origin_lb + prev_len),
                                     target_ucx_iov[target_ucx_iov_idx].len,
                                     remote_addr + (uint64_t)(target_ucx_iov[target_ucx_iov_idx].addr), rkey);
                if (status != UCS_OK && status != UCS_INPROGRESS) {
                    OSC_UCX_VERBOSE(1, "ucp_get_nbi failed: %d", status);
                    return OMPI_ERROR;
                }
            }

            ret = incr_and_check_ops_num(module, target, ep);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            prev_len += target_ucx_iov[target_ucx_iov_idx].len;
            target_ucx_iov_idx++;
        }
    }

    if (origin_ucx_iov != NULL) {
        free(origin_ucx_iov);
    }
    if (target_ucx_iov != NULL) {
        free(target_ucx_iov);
    }

    return ret;
}

static inline int start_atomicity(ompi_osc_ucx_module_t *module, ucp_ep_h ep, int target) {
    uint64_t result_value = -1;
    ucp_rkey_h rkey = (module->state_info_array)[target].rkey;
    uint64_t remote_addr = (module->state_info_array)[target].addr + OSC_UCX_STATE_ACC_LOCK_OFFSET;
    ucs_status_t status;

    for (;;) {
        status = opal_common_ucx_atomic_cswap(ep, TARGET_LOCK_UNLOCKED, TARGET_LOCK_EXCLUSIVE,
                                              &result_value, sizeof(result_value),
                                              remote_addr, rkey,
                                              mca_osc_ucx_component.ucp_worker);
        if (status != UCS_OK) {
            OSC_UCX_VERBOSE(1, "ucp_atomic_cswap64 failed: %d", status);
            return OMPI_ERROR;
        }
        if (result_value == TARGET_LOCK_UNLOCKED) {
            return OMPI_SUCCESS;
        }

        ucp_worker_progress(mca_osc_ucx_component.ucp_worker);
    }

}

static inline int end_atomicity(ompi_osc_ucx_module_t *module, ucp_ep_h ep, int target) {
    uint64_t result_value = 0;
    ucp_rkey_h rkey = (module->state_info_array)[target].rkey;
    uint64_t remote_addr = (module->state_info_array)[target].addr + OSC_UCX_STATE_ACC_LOCK_OFFSET;
    int ret;

    ret = opal_common_ucx_atomic_fetch(ep, UCP_ATOMIC_FETCH_OP_SWAP, TARGET_LOCK_UNLOCKED,
                                       &result_value, sizeof(result_value),
                                       remote_addr, rkey, mca_osc_ucx_component.ucp_worker);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    assert(result_value == TARGET_LOCK_EXCLUSIVE);

    return OMPI_SUCCESS;
}

static inline int get_dynamic_win_info(uint64_t remote_addr, ompi_osc_ucx_module_t *module,
                                       ucp_ep_h ep, int target) {
    ucp_rkey_h state_rkey = (module->state_info_array)[target].rkey;
    uint64_t remote_state_addr = (module->state_info_array)[target].addr + OSC_UCX_STATE_DYNAMIC_WIN_CNT_OFFSET;
    size_t len = sizeof(uint64_t) + sizeof(ompi_osc_dynamic_win_info_t) * OMPI_OSC_UCX_ATTACH_MAX;
    char *temp_buf = malloc(len);
    ompi_osc_dynamic_win_info_t *temp_dynamic_wins;
    uint64_t win_count;
    int contain, insert = -1;
    ucs_status_t status;
    int ret;

    if ((module->win_info_array[target]).rkey_init == true) {
        ucp_rkey_destroy((module->win_info_array[target]).rkey);
        (module->win_info_array[target]).rkey_init = false;
    }

    status = ucp_get_nbi(ep, (void *)temp_buf, len, remote_state_addr, state_rkey);
    if (status != UCS_OK && status != UCS_INPROGRESS) {
        OSC_UCX_VERBOSE(1, "ucp_get_nbi failed: %d", status);
        return OMPI_ERROR;
    }

    ret = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    memcpy(&win_count, temp_buf, sizeof(uint64_t));
    assert(win_count > 0 && win_count <= OMPI_OSC_UCX_ATTACH_MAX);

    temp_dynamic_wins = (ompi_osc_dynamic_win_info_t *)(temp_buf + sizeof(uint64_t));
    contain = ompi_osc_find_attached_region_position(temp_dynamic_wins, 0, win_count,
                                                     remote_addr, 1, &insert);
    assert(contain >= 0 && (uint64_t)contain < win_count);

    status = ucp_ep_rkey_unpack(ep, temp_dynamic_wins[contain].rkey_buffer,
                                &((module->win_info_array[target]).rkey));
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_ep_rkey_unpack failed: %d", status);
        return OMPI_ERROR;
    }

    (module->win_info_array[target]).rkey_init = true;

    free(temp_buf);

    return status;
}

int ompi_osc_ucx_put(const void *origin_addr, int origin_count, struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    uint64_t remote_addr = (module->win_info_array[target]).addr + target_disp * OSC_UCX_GET_DISP(module, target);
    ucp_rkey_h rkey;
    bool is_origin_contig = false, is_target_contig = false;
    ptrdiff_t origin_lb, origin_extent, target_lb, target_extent;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        status = get_dynamic_win_info(remote_addr, module, ep, target);
        if (status != UCS_OK) {
            return OMPI_ERROR;
        }
    }

    CHECK_VALID_RKEY(module, target, target_count);

    if (!target_count) {
        return OMPI_SUCCESS;
    }

    rkey = (module->win_info_array[target]).rkey;

    ompi_datatype_get_true_extent(origin_dt, &origin_lb, &origin_extent);
    ompi_datatype_get_true_extent(target_dt, &target_lb, &target_extent);

    is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);
    is_target_contig = ompi_datatype_is_contiguous_memory_layout(target_dt, target_count);

    if (is_origin_contig && is_target_contig) {
        /* fast path */
        size_t origin_len;

        ompi_datatype_type_size(origin_dt, &origin_len);
        origin_len *= origin_count;

        status = ucp_put_nbi(ep, (void *)((intptr_t)origin_addr + origin_lb), origin_len,
                             remote_addr + target_lb, rkey);
        if (status != UCS_OK && status != UCS_INPROGRESS) {
            OSC_UCX_VERBOSE(1, "ucp_put_nbi failed: %d", status);
            return OMPI_ERROR;
        }
        return incr_and_check_ops_num(module, target, ep);
    } else {
        return ddt_put_get(module, origin_addr, origin_count, origin_dt, is_origin_contig,
                           origin_lb, target, ep, remote_addr, rkey, target_count, target_dt,
                           is_target_contig, target_lb, false);
    }
}

int ompi_osc_ucx_get(void *origin_addr, int origin_count,
                     struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    uint64_t remote_addr = (module->win_info_array[target]).addr + target_disp * OSC_UCX_GET_DISP(module, target);
    ucp_rkey_h rkey;
    ptrdiff_t origin_lb, origin_extent, target_lb, target_extent;
    bool is_origin_contig = false, is_target_contig = false;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        status = get_dynamic_win_info(remote_addr, module, ep, target);
        if (status != UCS_OK) {
            return OMPI_ERROR;
        }
    }

    CHECK_VALID_RKEY(module, target, target_count);

    if (!target_count) {
        return OMPI_SUCCESS;
    }

    rkey = (module->win_info_array[target]).rkey;

    ompi_datatype_get_true_extent(origin_dt, &origin_lb, &origin_extent);
    ompi_datatype_get_true_extent(target_dt, &target_lb, &target_extent);

    is_origin_contig = ompi_datatype_is_contiguous_memory_layout(origin_dt, origin_count);
    is_target_contig = ompi_datatype_is_contiguous_memory_layout(target_dt, target_count);

    if (is_origin_contig && is_target_contig) {
        /* fast path */
        size_t origin_len;

        ompi_datatype_type_size(origin_dt, &origin_len);
        origin_len *= origin_count;

        status = ucp_get_nbi(ep, (void *)((intptr_t)origin_addr + origin_lb), origin_len,
                             remote_addr + target_lb, rkey);
        if (status != UCS_OK && status != UCS_INPROGRESS) {
            OSC_UCX_VERBOSE(1, "ucp_get_nbi failed: %d", status);
            return OMPI_ERROR;
        }

        return incr_and_check_ops_num(module, target, ep);
    } else {
        return ddt_put_get(module, origin_addr, origin_count, origin_dt, is_origin_contig,
                           origin_lb, target, ep, remote_addr, rkey, target_count, target_dt,
                           is_target_contig, target_lb, true);
    }
}

int ompi_osc_ucx_accumulate(const void *origin_addr, int origin_count,
                            struct ompi_datatype_t *origin_dt,
                            int target, ptrdiff_t target_disp, int target_count,
                            struct ompi_datatype_t *target_dt,
                            struct ompi_op_t *op, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (op == &ompi_mpi_op_no_op.op) {
        return ret;
    }

    ret = start_atomicity(module, ep, target);
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
        void *temp_addr_holder = NULL;
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
        temp_addr = temp_addr_holder = malloc(temp_extent * temp_count);
        if (temp_addr == NULL) {
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        ret = ompi_osc_ucx_get(temp_addr, (int)temp_count, temp_dt,
                               target, target_disp, target_count, target_dt, win);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }

        ret = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
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

        ret = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }

        free(temp_addr_holder);
    }

    ret = end_atomicity(module, ep, target);

    return ret;
}

int ompi_osc_ucx_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                  void *result_addr, struct ompi_datatype_t *dt,
                                  int target, ptrdiff_t target_disp,
                                  struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t *)win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    uint64_t remote_addr = (module->win_info_array[target]).addr + target_disp * OSC_UCX_GET_DISP(module, target);
    ucp_rkey_h rkey;
    size_t dt_bytes;
    ompi_osc_ucx_internal_request_t *req = NULL;
    int ret = OMPI_SUCCESS;
    ucs_status_t status;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = start_atomicity(module, ep, target);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        status = get_dynamic_win_info(remote_addr, module, ep, target);
        if (status != UCS_OK) {
            return OMPI_ERROR;
        }
    }

    rkey = (module->win_info_array[target]).rkey;

    ompi_datatype_type_size(dt, &dt_bytes);
    memcpy(result_addr, origin_addr, dt_bytes);
    req = ucp_atomic_fetch_nb(ep, UCP_ATOMIC_FETCH_OP_CSWAP, *(uint64_t *)compare_addr,
                              result_addr, dt_bytes, remote_addr, rkey, req_completion);
    if (UCS_PTR_IS_PTR(req)) {
        ucp_request_release(req);
    }

    ret = incr_and_check_ops_num(module, target, ep);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    return end_atomicity(module, ep, target);
}

int ompi_osc_ucx_fetch_and_op(const void *origin_addr, void *result_addr,
                              struct ompi_datatype_t *dt, int target,
                              ptrdiff_t target_disp, struct ompi_op_t *op,
                              struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (op == &ompi_mpi_op_no_op.op || op == &ompi_mpi_op_replace.op ||
        op == &ompi_mpi_op_sum.op) {
        ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
        uint64_t remote_addr = (module->win_info_array[target]).addr + target_disp * OSC_UCX_GET_DISP(module, target);
        ucp_rkey_h rkey;
        uint64_t value = origin_addr ? *(uint64_t *)origin_addr : 0;
        ucp_atomic_fetch_op_t opcode;
        size_t dt_bytes;
        ompi_osc_ucx_internal_request_t *req = NULL;
        ucs_status_t status;

        ret = start_atomicity(module, ep, target);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }

        if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
            status = get_dynamic_win_info(remote_addr, module, ep, target);
            if (status != UCS_OK) {
                return OMPI_ERROR;
            }
        }

        rkey = (module->win_info_array[target]).rkey;

        ompi_datatype_type_size(dt, &dt_bytes);

        if (op == &ompi_mpi_op_replace.op) {
            opcode = UCP_ATOMIC_FETCH_OP_SWAP;
        } else {
            opcode = UCP_ATOMIC_FETCH_OP_FADD;
            if (op == &ompi_mpi_op_no_op.op) {
                value = 0;
            }
        }

        req = ucp_atomic_fetch_nb(ep, opcode, value, result_addr,
                                  dt_bytes, remote_addr, rkey, req_completion);
        if (UCS_PTR_IS_PTR(req)) {
            ucp_request_release(req);
        }

        ret = incr_and_check_ops_num(module, target, ep);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }

        return end_atomicity(module, ep, target);
    } else {
        return ompi_osc_ucx_get_accumulate(origin_addr, 1, dt, result_addr, 1, dt,
                                           target, target_disp, 1, dt, op, win);
    }
}

int ompi_osc_ucx_get_accumulate(const void *origin_addr, int origin_count,
                                struct ompi_datatype_t *origin_dt,
                                void *result_addr, int result_count,
                                struct ompi_datatype_t *result_dt,
                                int target, ptrdiff_t target_disp,
                                int target_count, struct ompi_datatype_t *target_dt,
                                struct ompi_op_t *op, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, false);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ret = start_atomicity(module, ep, target);
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
            void *temp_addr_holder = NULL;
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
            temp_addr = temp_addr_holder = malloc(temp_extent * temp_count);
            if (temp_addr == NULL) {
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            }

            ret = ompi_osc_ucx_get(temp_addr, (int)temp_count, temp_dt,
                                   target, target_disp, target_count, target_dt, win);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            ret = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
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

            ret = opal_common_ucx_ep_flush(ep, mca_osc_ucx_component.ucp_worker);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }

            free(temp_addr_holder);
        }
    }

    ret = end_atomicity(module, ep, target);

    return ret;
}

int ompi_osc_ucx_rput(const void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win, struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    uint64_t remote_addr = (module->state_info_array[target]).addr + OSC_UCX_STATE_REQ_FLAG_OFFSET;
    ucp_rkey_h rkey;
    ompi_osc_ucx_request_t *ucx_req = NULL;
    ompi_osc_ucx_internal_request_t *internal_req = NULL;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        status = get_dynamic_win_info(remote_addr, module, ep, target);
        if (status != UCS_OK) {
            return OMPI_ERROR;
        }
    }

    CHECK_VALID_RKEY(module, target, target_count);

    rkey = (module->state_info_array[target]).rkey;

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = ompi_osc_ucx_put(origin_addr, origin_count, origin_dt, target, target_disp,
                           target_count, target_dt, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    status = ucp_worker_fence(mca_osc_ucx_component.ucp_worker);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_worker_fence failed: %d", status);
        return OMPI_ERROR;
    }

    internal_req = ucp_atomic_fetch_nb(ep, UCP_ATOMIC_FETCH_OP_FADD, 0,
                                       &(module->req_result), sizeof(uint64_t),
                                       remote_addr, rkey, req_completion);

    if (UCS_PTR_IS_PTR(internal_req)) {
        internal_req->external_req = ucx_req;
        mca_osc_ucx_component.num_incomplete_req_ops++;
    } else {
        ompi_request_complete(&ucx_req->super, true);
    }

    *request = &ucx_req->super;

    return incr_and_check_ops_num(module, target, ep);
}

int ompi_osc_ucx_rget(void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt, struct ompi_win_t *win,
                      struct ompi_request_t **request) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, target);
    uint64_t remote_addr = (module->state_info_array[target]).addr + OSC_UCX_STATE_REQ_FLAG_OFFSET;
    ucp_rkey_h rkey;
    ompi_osc_ucx_request_t *ucx_req = NULL;
    ompi_osc_ucx_internal_request_t *internal_req = NULL;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    ret = check_sync_state(module, target, true);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
        status = get_dynamic_win_info(remote_addr, module, ep, target);
        if (status != UCS_OK) {
            return OMPI_ERROR;
        }
    }

    CHECK_VALID_RKEY(module, target, target_count);

    rkey = (module->state_info_array[target]).rkey;

    OMPI_OSC_UCX_REQUEST_ALLOC(win, ucx_req);
    assert(NULL != ucx_req);

    ret = ompi_osc_ucx_get(origin_addr, origin_count, origin_dt, target, target_disp,
                           target_count, target_dt, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    status = ucp_worker_fence(mca_osc_ucx_component.ucp_worker);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_worker_fence failed: %d", status);
        return OMPI_ERROR;
    }

    internal_req = ucp_atomic_fetch_nb(ep, UCP_ATOMIC_FETCH_OP_FADD, 0,
                                       &(module->req_result), sizeof(uint64_t),
                                       remote_addr, rkey, req_completion);

    if (UCS_PTR_IS_PTR(internal_req)) {
        internal_req->external_req = ucx_req;
        mca_osc_ucx_component.num_incomplete_req_ops++;
    } else {
        ompi_request_complete(&ucx_req->super, true);
    }

    *request = &ucx_req->super;

    return incr_and_check_ops_num(module, target, ep);
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

    ret = ompi_osc_ucx_accumulate(origin_addr, origin_count, origin_dt, target, target_disp,
                                  target_count, target_dt, op, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ompi_request_complete(&ucx_req->super, true);
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

    ret = ompi_osc_ucx_get_accumulate(origin_addr, origin_count, origin_datatype,
                                      result_addr, result_count, result_datatype,
                                      target, target_disp, target_count,
                                      target_datatype, op, win);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    ompi_request_complete(&ucx_req->super, true);

    *request = &ucx_req->super;

    return ret;
}
