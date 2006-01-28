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

#include "opal/threads/mutex.h"
#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/btl/btl.h"
#include "mpi.h"


int
ompi_osc_pt2pt_module_free(ompi_win_t *win)
{
    int ret = OMPI_SUCCESS;
    int i, tmp;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    /* are we in an epoch? */
    if ((OMPI_WIN_ACCESS_EPOCH & win->w_flags) || 
        (OMPI_WIN_EXPOSE_EPOCH & win->w_flags)) {
        /* finish off the epoch.  More for sanity checks than anything
           - could really just ignore this condition... */
        ret = ompi_osc_pt2pt_module_fence(MPI_MODE_NOPRECEDE|MPI_MODE_NOSUCCEED, win);
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
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    /* clean up p2p module part */
    for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
        OBJ_DESTRUCT(&(module->p2p_pending_out_sendreqs[i]));
    }
    free(module->p2p_pending_out_sendreqs);
    module->p2p_pending_out_sendreqs = NULL;

    ompi_comm_free(&(module->p2p_comm));
    module->p2p_comm = NULL;

    module->p2p_win = NULL;

    OBJ_DESTRUCT(&(module->p2p_long_msgs));
    OBJ_DESTRUCT(&(module->p2p_acc_lock));
    OBJ_DESTRUCT(&(module->p2p_lock));

    free(module);

    return ret;
}
