/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/sys/atomic.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

#include "osc_sm.h"


int
ompi_osc_sm_fence(int assert, struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    /* ensure all memory operations have completed */
    opal_atomic_mb();

    if (module->global_state->use_barrier_for_fence) {
        return module->comm->c_coll.coll_barrier(module->comm,
                                                 module->comm->c_coll.coll_barrier_module);
    } else {
        module->my_sense = !module->my_sense;
        pthread_mutex_lock(&module->global_state->mtx);
        module->global_state->count--;
        if (module->global_state->count == 0) {
            module->global_state->count = ompi_comm_size(module->comm);
            module->global_state->sense = module->my_sense;
            pthread_cond_broadcast(&module->global_state->cond);
        } else {
            while (module->global_state->sense != module->my_sense) {
                pthread_cond_wait(&module->global_state->cond, &module->global_state->mtx);
            }
        }
        pthread_mutex_unlock(&module->global_state->mtx);

        return OMPI_SUCCESS;
    }
}


int
ompi_osc_sm_start(struct ompi_group_t *group,
                  int assert,
                  struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        int size;

        OBJ_RETAIN(group);
        module->start_group = group;
        size = ompi_group_size(module->start_group);

        while (module->my_node_state->post_count != size) {
            opal_progress();
            opal_atomic_mb();
        }
    } else {
        module->start_group = NULL;
    }

    opal_atomic_mb();
    return OMPI_SUCCESS;
}


int
ompi_osc_sm_complete(struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    int gsize, csize;

    /* ensure all memory operations have completed */
    opal_atomic_mb();

    if (NULL != module->start_group) {
        module->my_node_state->post_count = 0;
        opal_atomic_mb();

        gsize = ompi_group_size(module->start_group);
        csize = ompi_comm_size(module->comm);
        for (int i = 0 ; i < gsize ; ++i) {
            for (int j = 0 ; j < csize ; ++j) {
                if (ompi_group_peer_lookup(module->start_group, i) ==
                    ompi_comm_peer_lookup(module->comm, j)) {
                    opal_atomic_add_32(&module->node_states[j].complete_count, 1);
                }
            }
        }

        OBJ_RELEASE(module->start_group);
        module->start_group = NULL;
    }

    opal_atomic_mb();
    return OMPI_SUCCESS;
}


int
ompi_osc_sm_post(struct ompi_group_t *group,
                       int assert,
                       struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    int gsize, csize;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        OBJ_RETAIN(group);
        module->post_group = group;

        module->my_node_state->complete_count = 0;
        opal_atomic_mb();

        gsize = ompi_group_size(module->post_group);
        csize = ompi_comm_size(module->comm);
        for (int i = 0 ; i < gsize ; ++i) {
            for (int j = 0 ; j < csize ; ++j) {
                if (ompi_group_peer_lookup(module->post_group, i) ==
                    ompi_comm_peer_lookup(module->comm, j)) {
                    opal_atomic_add_32(&module->node_states[j].post_count, 1);
                }
            }
        }
    } else {
        module->post_group = NULL;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_sm_wait(struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (NULL != module->post_group) {
        int size = ompi_group_size(module->post_group);

        while (module->my_node_state->complete_count != size) {
            opal_progress();
            opal_atomic_mb();
        }

        OBJ_RELEASE(module->post_group);
        module->post_group = NULL;
    }

    /* ensure all memory operations have completed */
    opal_atomic_mb();

    return OMPI_SUCCESS;
}


int
ompi_osc_sm_test(struct ompi_win_t *win,
                       int *flag)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (NULL != module->post_group) {
        int size = ompi_group_size(module->post_group);

        if (module->my_node_state->complete_count == size) {
            OBJ_RELEASE(module->post_group);
            module->post_group = NULL;
            *flag = 1;
        }
    } else {
        opal_atomic_mb();
        *flag = 0;
    }

    /* ensure all memory operations have completed */
    opal_atomic_mb();

    return OMPI_SUCCESS;
}
