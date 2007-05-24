/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"

#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/osc/base/base.h"
#include "mpi.h"


int
ompi_osc_rdma_module_free(ompi_win_t *win)
{
    int ret = OMPI_SUCCESS;
    int tmp;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    opal_output_verbose(1, ompi_osc_base_output,
                        "rdma component destroying window with id %d",
                        module->m_comm->c_contextid);

    OPAL_THREAD_LOCK(&module->m_lock);
    while (OMPI_WIN_EXPOSE_EPOCH & ompi_win_get_mode(win)) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }
    OPAL_THREAD_UNLOCK(&module->m_lock);

    /* finish with a barrier */
    if (ompi_group_size(win->w_group) > 1) {
        ret = module->m_comm->c_coll.coll_barrier(module->m_comm);
    }

    /* remove from component information */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    tmp = opal_hash_table_remove_value_uint32(&mca_osc_rdma_component.c_modules,
                                              module->m_comm->c_contextid);
    /* only take the output of hast_table_remove if there wasn't already an error */
    ret = (ret != OMPI_SUCCESS) ? ret : tmp;

    if (0 == opal_hash_table_get_size(&mca_osc_rdma_component.c_modules)) {
#if OMPI_ENABLE_PROGRESS_THREADS
        void *foo;

        mca_osc_rdma_component.c_thread_run = false;
        opal_condition_broadcast(&ompi_request_cond);
        opal_thread_join(&mca_osc_rdma_component.c_thread, &foo);
#else
        opal_progress_unregister(ompi_osc_rdma_component_progress);
#endif
    }
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);

    win->w_osc_module = NULL;

    OBJ_DESTRUCT(&module->m_unlocks_pending);
    OBJ_DESTRUCT(&module->m_locks_pending);
    OBJ_DESTRUCT(&module->m_copy_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_acc_lock);
    OBJ_DESTRUCT(&module->m_cond);
    OBJ_DESTRUCT(&module->m_lock);

    if (NULL != module->m_sc_remote_ranks) {
        free(module->m_sc_remote_ranks);
    }
    if (NULL != module->m_sc_remote_active_ranks) {
        free(module->m_sc_remote_active_ranks);
    }
    if (NULL != module->m_fence_coll_counts) {
        free(module->m_fence_coll_counts);
    }
    if (NULL != module->m_copy_num_pending_sendreqs) {
        free(module->m_copy_num_pending_sendreqs);
    }
    if (NULL != module->m_num_pending_sendreqs) {
        free(module->m_num_pending_sendreqs);
    }
    if (NULL != module->m_comm) ompi_comm_free(&module->m_comm);

    if (NULL != module) free(module);

    return ret;
}
