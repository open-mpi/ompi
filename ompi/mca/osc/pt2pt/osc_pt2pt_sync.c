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
#include "osc_pt2pt_longreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"


static inline void
ompi_osc_pt2pt_progress(ompi_osc_pt2pt_module_t *module)
{
    if (0 != module->p2p_num_long_msgs) {
        opal_list_item_t *item, *next;

        OPAL_THREAD_LOCK(&(module->p2p_lock));
        /* Have to go the convoluted while() route instead of a for()
           loop because the callback will likely remove the request
           from the list and free it, and that would lead to much
           badness. */
        next = opal_list_get_first(&(module->p2p_long_msgs));
        while (opal_list_get_end(&(module->p2p_long_msgs)) != (item = next)) {
            ompi_osc_pt2pt_longreq_t *longreq = 
                (ompi_osc_pt2pt_longreq_t*) item;
            int ret, completed;
            next = opal_list_get_next(item);

            ret = ompi_request_test(&(longreq->req_pml_req), &completed, NULL);
            /* BWB - FIX ME - error handling */
            if (completed > 0) {
                longreq->req_comp_cb(longreq);
            }
        }
        OPAL_THREAD_UNLOCK(&(module->p2p_lock));
    }
    opal_progress();
}


int
ompi_osc_pt2pt_module_fence(int assert, ompi_win_t *win)
{
    short *outgoing_reqs = NULL;
    short incoming_reqs;
    int *counts = NULL;
    int ret = OMPI_SUCCESS;
    int i;

    OPAL_THREAD_LOCK(&(OSC_PT2PT_DATA(win)->p2p_lock));

    if (0 == (assert & MPI_MODE_NOPRECEDE)) {
        /* user has not promised nothing has happened - need to make
           sure we've done all our requests */
        P2P_MODULE(win)->p2p_num_pending_out = 0;

        outgoing_reqs = malloc(sizeof(short) * 
                               ompi_comm_size(P2P_MODULE(win)->p2p_comm));
        if (NULL == outgoing_reqs) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }

        for (i = 0 ; i < ompi_comm_size(P2P_MODULE(win)->p2p_comm) ; ++i) {
            outgoing_reqs[i] = 
                opal_list_get_size(&(P2P_MODULE(win)->p2p_pending_out_sendreqs[i]));
            P2P_MODULE(win)->p2p_num_pending_out += outgoing_reqs[i];
        }

        counts = malloc(sizeof(int) * 
                        ompi_comm_size(P2P_MODULE(win)->p2p_comm));
        if (NULL == counts) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }
        for (i = 0 ; i < ompi_comm_size(P2P_MODULE(win)->p2p_comm) ; ++i) {
            counts[i] = 1;
        }

        /* find out how much data everyone is going to send us... */
        ret = P2P_MODULE(win)->p2p_comm->c_coll.coll_reduce_scatter(outgoing_reqs,
                                                                  &incoming_reqs,
                                                                  counts,
                                                                  MPI_SHORT,
                                                                  MPI_SUM,
                                                                  P2P_MODULE(win)->p2p_comm);
        if (OMPI_SUCCESS != ret) goto cleanup;

        P2P_MODULE(win)->p2p_num_pending_in += incoming_reqs;

        for (i = 0 ; i < ompi_comm_size(P2P_MODULE(win)->p2p_comm) ; ++i) {
            opal_list_item_t *item;
            opal_list_t *req_list =
                &(P2P_MODULE(win)->p2p_pending_out_sendreqs[i]);

            while (NULL != (item = opal_list_remove_first(req_list))) {
                ompi_osc_pt2pt_sendreq_t *req = 
                    (ompi_osc_pt2pt_sendreq_t*) item;

                ret = ompi_osc_pt2pt_sendreq_send(P2P_MODULE(win), req);

                if (OMPI_SUCCESS != ret) {
                    opal_output(0, "fence: failure in starting sendreq");
                    opal_list_prepend(req_list, item);
                    goto cleanup;
                }
            }
        }

    } else {
        /* Don't trust the user that nothing has happened in this
           epoch and count through all the pending sendreqs to
           verify */
        int tmp = 0;

        for (i = 0 ; i < ompi_comm_size(P2P_MODULE(win)->p2p_comm) ; ++i) {
            tmp +=
                opal_list_get_size(&(P2P_MODULE(win)->p2p_pending_out_sendreqs[i]));
        }

        if (0 != tmp) {
            ret = MPI_ERR_ASSERT;
            goto cleanup;
        }

        incoming_reqs = 0;
    }

    /* now we know how many things we're waiting for - wait for them... */
    while (0 != P2P_MODULE(win)->p2p_num_pending_in ||
           0 != P2P_MODULE(win)->p2p_num_pending_out) {
        ompi_osc_pt2pt_progress(P2P_MODULE(win));
    }

    /* all transfers are done - back to the real world we go */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        win->w_flags = OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_EXPOSE_EPOCH;
    } else {
        win->w_flags = 0;
    }

 cleanup:
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    if (NULL != outgoing_reqs) free(outgoing_reqs);
    if (NULL != counts) free(counts);

    return ret;
}


int
ompi_osc_pt2pt_module_start(ompi_group_t *group,
                                int assert,
                                ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_osc_pt2pt_module_complete(ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int
ompi_osc_pt2pt_module_post(ompi_group_t *group,
                               int assert,
                               ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_osc_pt2pt_module_wait(ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int 
ompi_osc_pt2pt_module_test(ompi_win_t *win,
                               int flag)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_osc_pt2pt_module_lock(int lock_type,
                               int target,
                               int assert,
                               ompi_win_t *win)
{
    if (!P2P_MODULE(win)->p2p_want_locks) {
        return MPI_ERR_OTHER;
    }

    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_osc_pt2pt_module_unlock(int target,
                                 ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
