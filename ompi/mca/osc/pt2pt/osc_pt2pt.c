/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"

#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "mpi.h"


int
ompi_osc_pt2pt_module_free(ompi_win_t *win)
{
    int ret = OMPI_SUCCESS;
    int tmp;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    while (OMPI_WIN_EXPOSE_EPOCH & ompi_win_get_mode(win)) {
        opal_progress();
    }

    /* finish with a barrier */
    if (ompi_group_size(win->w_group) > 1) {
        ret = module->p2p_comm->c_coll.coll_barrier(module->p2p_comm);
    }

    /* remove window information */
    win->w_osc_module = NULL;

    /* remove from component information */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    tmp = opal_hash_table_remove_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                              module->p2p_comm->c_contextid);
    /* only take the output of hast_table_remove if there wasn't already an error */
    ret = (ret != OMPI_SUCCESS) ? ret : tmp;

    if (0 == opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules)) {
        /* stop progress thread */
        opal_progress_unregister(ompi_osc_pt2pt_progress);
    }

    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    OBJ_DESTRUCT(&(module->p2p_locks_pending));

    free(module->p2p_sc_remote_ranks);
    free(module->p2p_sc_remote_active_ranks);
    assert(module->p2p_sc_group == NULL);
    assert(module->p2p_pw_group == NULL);
    free(module->p2p_fence_coll_counts);

    free(module->p2p_copy_num_pending_sendreqs);
    OBJ_DESTRUCT(&(module->p2p_copy_pending_sendreqs));

    OBJ_DESTRUCT(&(module->p2p_long_msgs));

    free(module->p2p_num_pending_sendreqs);

    OBJ_DESTRUCT(&(module->p2p_pending_sendreqs));

    OBJ_DESTRUCT(&(module->p2p_pending_control_sends));

    ompi_comm_free(&(module->p2p_comm));
    module->p2p_comm = NULL;

    module->p2p_win = NULL;

    OBJ_DESTRUCT(&(module->p2p_acc_lock));
    OBJ_DESTRUCT(&(module->p2p_lock));

    free(module);

    return ret;
}
