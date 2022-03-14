/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

typedef struct ompi_osc_ucx_pending_post {
    opal_list_item_t super;
    int rank;
} ompi_osc_ucx_pending_post_t;

OBJ_CLASS_INSTANCE(ompi_osc_ucx_pending_post_t, opal_list_item_t, NULL, NULL);

static inline void ompi_osc_ucx_handle_incoming_post(ompi_osc_ucx_module_t *module, volatile uint64_t *post_ptr, int ranks_in_win_grp[], int grp_size) {
    int i, post_rank = (*post_ptr) - 1;
    ompi_osc_ucx_pending_post_t *pending_post = NULL;

    (*post_ptr) = 0;

    for (i = 0; i < grp_size; i++) {
        if (post_rank == ranks_in_win_grp[i]) {
            module->post_count++;
            return;
        }
    }

    /* post does not belong to this start epoch. save it for later */
    pending_post = OBJ_NEW(ompi_osc_ucx_pending_post_t);
    pending_post->rank = post_rank;
    opal_list_append(&module->pending_posts, &pending_post->super);
}

int ompi_osc_ucx_fence(int mpi_assert, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;

    if (module->epoch_type.access != NONE_EPOCH &&
        module->epoch_type.access != FENCE_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    if (mpi_assert & MPI_MODE_NOSUCCEED) {
        module->epoch_type.access = NONE_EPOCH;
    } else {
        module->epoch_type.access = FENCE_EPOCH;
    }

    if (!(mpi_assert & MPI_MODE_NOPRECEDE)) {
        if (module->flavor == MPI_WIN_FLAVOR_DYNAMIC) {
            int i; 
            for (i = 0; i < OMPI_OSC_UCX_ATTACH_MAX; i++) {
                ret = opal_common_ucx_wpmem_flush(module->local_dynamic_win_info[i].mem,
                            OPAL_COMMON_UCX_SCOPE_WORKER, 0/*ignore*/);
                if (ret != OMPI_SUCCESS) {
                    return ret;
                }
            }
        } else {
            ret = opal_common_ucx_wpmem_flush(module->mem, OPAL_COMMON_UCX_SCOPE_WORKER, 0/*ignore*/);
            if (ret != OMPI_SUCCESS) {
                return ret;
            }
        }
    }

    return module->comm->c_coll->coll_barrier(module->comm,
                                              module->comm->c_coll->coll_barrier_module);
}

int ompi_osc_ucx_start(struct ompi_group_t *group, int mpi_assert, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int i, size, *ranks_in_grp = NULL, *ranks_in_win_grp = NULL;
    ompi_group_t *win_group = NULL;
    int ret = OMPI_SUCCESS;

    if (module->epoch_type.access != NONE_EPOCH &&
        module->epoch_type.access != FENCE_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    module->epoch_type.access = START_COMPLETE_EPOCH;

    OBJ_RETAIN(group);
    module->start_group = group;
    size = ompi_group_size(module->start_group);

    ranks_in_grp = malloc(sizeof(int) * size);
    ranks_in_win_grp = malloc(sizeof(int) * ompi_comm_size(module->comm));

    for (i = 0; i < size; i++) {
        ranks_in_grp[i] = i;
    }

    ret = ompi_comm_group(module->comm, &win_group);
    if (ret != OMPI_SUCCESS) {
        free(ranks_in_grp);
        free(ranks_in_win_grp);
        return OMPI_ERROR;
    }

    ret = ompi_group_translate_ranks(module->start_group, size, ranks_in_grp,
                                     win_group, ranks_in_win_grp);
    if (ret != OMPI_SUCCESS) {
        free(ranks_in_grp);
        free(ranks_in_win_grp);
        return OMPI_ERROR;
    }

    if ((mpi_assert & MPI_MODE_NOCHECK) == 0) {
        ompi_osc_ucx_pending_post_t *pending_post, *next;

        /* first look through the pending list */
        OPAL_LIST_FOREACH_SAFE(pending_post, next, &module->pending_posts, ompi_osc_ucx_pending_post_t) {
            for (i = 0; i < size; i++) {
                if (pending_post->rank == ranks_in_win_grp[i]) {
                    opal_list_remove_item(&module->pending_posts, &pending_post->super);
                    OBJ_RELEASE(pending_post);
                    module->post_count++;
                    break;
                }
            }
        }

        /* waiting for the rest post requests to come */
        while (module->post_count != size) {
            for (i = 0; i < OMPI_OSC_UCX_POST_PEER_MAX; i++) {
                if (0 == module->state.post_state[i]) {
                    continue;
                }

                ompi_osc_ucx_handle_incoming_post(module, &(module->state.post_state[i]), ranks_in_win_grp, size);
            }
            opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);
        }

        module->post_count = 0;
    }

    free(ranks_in_grp);
    ompi_group_free(&win_group);

    module->start_grp_ranks = ranks_in_win_grp;

    return ret;
}

int ompi_osc_ucx_complete(struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int i, size;
    int ret = OMPI_SUCCESS;

    if (module->epoch_type.access != START_COMPLETE_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    module->epoch_type.access = NONE_EPOCH;

    ret = opal_common_ucx_wpmem_flush(module->mem, OPAL_COMMON_UCX_SCOPE_WORKER, 0/*ignore*/);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    size = ompi_group_size(module->start_group);
    for (i = 0; i < size; i++) {
        uint64_t remote_addr = module->state_addrs[module->start_grp_ranks[i]] + OSC_UCX_STATE_COMPLETE_COUNT_OFFSET; // write to state.complete_count on remote side

        ret = opal_common_ucx_wpmem_post(module->state_mem, UCP_ATOMIC_POST_OP_ADD,
                                       1, module->start_grp_ranks[i], sizeof(uint64_t),
                                       remote_addr);
        if (ret != OMPI_SUCCESS) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_mem_post failed: %d", ret);
        }

        ret = opal_common_ucx_wpmem_flush(module->state_mem, OPAL_COMMON_UCX_SCOPE_EP,
                                        module->start_grp_ranks[i]);
        if (ret != OMPI_SUCCESS) {
            return ret;
        }
    }

    OBJ_RELEASE(module->start_group);
    module->start_group = NULL;
    free(module->start_grp_ranks);

    return ret;
}

int ompi_osc_ucx_post(struct ompi_group_t *group, int mpi_assert, struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;

    if (module->epoch_type.exposure != NONE_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    OBJ_RETAIN(group);
    module->post_group = group;

    if ((mpi_assert & MPI_MODE_NOCHECK) == 0) {
        int i, j, size;
        ompi_group_t *win_group = NULL;
        int *ranks_in_grp = NULL, *ranks_in_win_grp = NULL;
        int myrank = ompi_comm_rank(module->comm);

        ret = ompi_comm_group(module->comm, &win_group);
        if (ret != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        size = ompi_group_size(module->post_group);
        ranks_in_grp = malloc(sizeof(int) * size);
        ranks_in_win_grp = malloc(sizeof(int) * ompi_comm_size(module->comm));

        for (i = 0; i < size; i++) {
            ranks_in_grp[i] = i;
        }

        ret = ompi_group_translate_ranks(module->post_group, size, ranks_in_grp,
                                         win_group, ranks_in_win_grp);
        if (ret != OMPI_SUCCESS) {
            ret = OMPI_ERROR;
            goto cleanup;
        }

        for (i = 0; i < size; i++) {
            uint64_t remote_addr = module->state_addrs[ranks_in_win_grp[i]] + OSC_UCX_STATE_POST_INDEX_OFFSET; // write to state.post_index on remote side
            uint64_t curr_idx = 0, result = 0;



            /* do fop first to get an post index */
            ret = opal_common_ucx_wpmem_fetch(module->state_mem, UCP_ATOMIC_FETCH_OP_FADD,
                                            1, ranks_in_win_grp[i], &result,
                                            sizeof(result), remote_addr);

            if (ret != OMPI_SUCCESS) {
                ret = OMPI_ERROR;
                goto cleanup;
            }

            curr_idx = result & (OMPI_OSC_UCX_POST_PEER_MAX - 1);

            remote_addr = module->state_addrs[ranks_in_win_grp[i]] + OSC_UCX_STATE_POST_STATE_OFFSET + sizeof(uint64_t) * curr_idx;

            /* do cas to send post message */
            do {

                result =  myrank + 1;
                ret = opal_common_ucx_wpmem_cmpswp(module->state_mem, 0, result,
                                                 ranks_in_win_grp[i], &result, sizeof(result),
                                                 remote_addr);

                if (ret != OMPI_SUCCESS) {
                    ret = OMPI_ERROR;
                    goto cleanup;
                }

                if (result == 0)
                    break;

                /* prevent circular wait by checking for post messages received */
                for (j = 0; j < OMPI_OSC_UCX_POST_PEER_MAX; j++) {
                    /* no post at this index (yet) */
                    if (0 == module->state.post_state[j]) {
                        continue;
                    }

                    ompi_osc_ucx_handle_incoming_post(module, &(module->state.post_state[j]), NULL, 0);
                }

                opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);
                usleep(100);
            } while (1);
        }

cleanup:
        free(ranks_in_grp);
        free(ranks_in_win_grp);
        ompi_group_free(&win_group);
        if (OMPI_SUCCESS != ret) return ret;
    }

    module->epoch_type.exposure = POST_WAIT_EPOCH;

    return ret;
}

int ompi_osc_ucx_wait(struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int size;

    if (module->epoch_type.exposure != POST_WAIT_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    size = ompi_group_size(module->post_group);

    while (module->state.complete_count != (uint64_t)size) {
        /* not sure if this is required */
        opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);
    }

    module->state.complete_count = 0;

    OBJ_RELEASE(module->post_group);
    module->post_group = NULL;

    module->epoch_type.exposure = NONE_EPOCH;

    return OMPI_SUCCESS;
}

int ompi_osc_ucx_test(struct ompi_win_t *win, int *flag) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int size;

    if (module->epoch_type.exposure != POST_WAIT_EPOCH) {
        return OMPI_ERR_RMA_SYNC;
    }

    size = ompi_group_size(module->post_group);

    opal_progress();

    if (module->state.complete_count == (uint64_t)size) {
        OBJ_RELEASE(module->post_group);
        module->post_group = NULL;

        module->state.complete_count = 0;

        module->epoch_type.exposure = NONE_EPOCH;
        *flag = 1;
    } else {
        *flag = 0;
    }

    return OMPI_SUCCESS;
}
