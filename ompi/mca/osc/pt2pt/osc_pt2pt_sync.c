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


/* should have p2p_lock before calling */
static inline void
ompi_osc_pt2pt_progress(ompi_osc_pt2pt_module_t *module)
{
    if (0 != opal_list_get_size(&(module->p2p_long_msgs))) {
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

static inline void
ompi_osc_pt2pt_flip_sendreqs(ompi_osc_pt2pt_module_t *module)
{
    short *tmp;

    OPAL_THREAD_LOCK(&(module->p2p_lock));
    /* user has not promised nothing has happened - need to make
       sure we've done all our requests */
    module->p2p_num_pending_out = 0;

    tmp = module->p2p_copy_num_pending_sendreqs;
    module->p2p_copy_num_pending_sendreqs = 
        module->p2p_num_pending_sendreqs;
    module->p2p_num_pending_sendreqs = tmp;
    memset(module->p2p_num_pending_sendreqs, 0,
           sizeof(short) * ompi_comm_size(module->p2p_comm));

    /* Copy in all the pending requests */
    opal_list_join(&module->p2p_copy_pending_sendreqs,
                   opal_list_get_end(&module->p2p_copy_pending_sendreqs),
                   &module->p2p_pending_sendreqs);

    OPAL_THREAD_UNLOCK(&(module->p2p_lock));
}


int
ompi_osc_pt2pt_module_fence(int assert, ompi_win_t *win)
{
    short incoming_reqs;
    int ret, i;

    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        int num_pending;

        /* check that the user didn't lie to us - since NOPRECEDED
           must be specified by all processes if it is specified by
           any process, if we see this it is safe to assume that there
           are no pending operations anywhere needed to close out this
           epoch. */
        OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
        num_pending = opal_list_get_size(&(P2P_MODULE(win)->p2p_pending_sendreqs));
        OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));
        if (0 != num_pending) {
            return MPI_ERR_RMA_SYNC;
        }

    } else {
        opal_list_item_t *item;

        ompi_osc_pt2pt_flip_sendreqs(P2P_MODULE(win));

        /* find out how much data everyone is going to send us.  Need
           to have the lock during this period so that we have a sane
           view of the number of sendreqs */
        ret = P2P_MODULE(win)->p2p_comm->
            c_coll.coll_reduce_scatter(P2P_MODULE(win)->p2p_copy_num_pending_sendreqs,
                                       &incoming_reqs,
                                       P2P_MODULE(win)->p2p_fence_coll_counts,
                                       MPI_SHORT,
                                       MPI_SUM,
                                       P2P_MODULE(win)->p2p_comm);
        if (OMPI_SUCCESS != ret) {
            /* put the stupid data back for the user.  This is not
               cheap, but the user lost his data if we don't. */
            OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
            opal_list_join(&P2P_MODULE(win)->p2p_pending_sendreqs,
                           opal_list_get_end(&P2P_MODULE(win)->p2p_pending_sendreqs),
                           &P2P_MODULE(win)->p2p_copy_pending_sendreqs);
            
            for (i = 0 ; i < ompi_comm_size(P2P_MODULE(win)->p2p_comm) ; ++i) {
                P2P_MODULE(win)->p2p_num_pending_sendreqs[i] +=
                    P2P_MODULE(win)->p2p_copy_num_pending_sendreqs[i];
            }

            OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));
            return ret;
        }

        /* possible we've already received a couple in messages, so
           atomicall add however many we're going to wait for */
        OPAL_THREAD_ADD32(&(P2P_MODULE(win)->p2p_num_pending_in), incoming_reqs);
        OPAL_THREAD_ADD32(&(P2P_MODULE(win)->p2p_num_pending_out), 
                          opal_list_get_size(&(P2P_MODULE(win)->p2p_copy_pending_sendreqs)));

        /* try to start all the requests.  We've copied everything we
           need out of pending_sendreqs, so don't need the lock
           here */
        while (NULL != 
               (item = opal_list_remove_first(&(P2P_MODULE(win)->p2p_copy_pending_sendreqs)))) {
            ompi_osc_pt2pt_sendreq_t *req = 
                (ompi_osc_pt2pt_sendreq_t*) item;

            ret = ompi_osc_pt2pt_sendreq_send(P2P_MODULE(win), req);

            if (OMPI_SUCCESS != ret) {
                opal_output(0, "fence: failure in starting sendreq (%d).  Will try later.",
                            ret);
                opal_list_append(&(P2P_MODULE(win)->p2p_copy_pending_sendreqs), item);
            }
        }

        /* now we know how many things we're waiting for - wait for them... */
        while (0 != P2P_MODULE(win)->p2p_num_pending_in ||
               0 != P2P_MODULE(win)->p2p_num_pending_out) {
            ompi_osc_pt2pt_progress(P2P_MODULE(win));
        }
    }

    /* all transfers are done - back to the real world we go */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        ompi_win_set_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_EXPOSE_EPOCH);
    } else {
        ompi_win_set_mode(win, 0);
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_start(ompi_group_t *group,
                            int assert,
                            ompi_win_t *win)
{
    OBJ_RETAIN(group);
    /* BWB - do I need this? */
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
    if (NULL != P2P_MODULE(win)->p2p_sc_group || NULL != P2P_MODULE(win)->p2p_pw_group) {
        OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));
        return MPI_ERR_RMA_CONFLICT;
    }
    P2P_MODULE(win)->p2p_sc_group = group;    
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    /* Set our mode to access w/ start */
    ompi_win_set_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_STARTED);

    /* possible we've already received a couple in messages, so
       atomicall add however many we're going to wait for */
    OPAL_THREAD_ADD32(&(P2P_MODULE(win)->p2p_num_pending_in),
                      ompi_group_size(P2P_MODULE(win)->p2p_sc_group));

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_complete(ompi_win_t *win)
{
    int i;
    int ret = OMPI_SUCCESS;
    ompi_group_t *group;
    opal_list_item_t *item;

    /* wait for all the post messages */
    while (0 != P2P_MODULE(win)->p2p_num_pending_in) {
        ompi_osc_pt2pt_progress(P2P_MODULE(win));        
    }

    ompi_osc_pt2pt_flip_sendreqs(P2P_MODULE(win));

    /* for each process in group, send a control message with number
       of updates coming, then start all the requests */
    for (i = 0 ; i < ompi_group_size(P2P_MODULE(win)->p2p_sc_group) ; ++i) {
        int comm_rank = -1, j;
        /* no need to increment ref count - the communicator isn't
           going anywhere while we're here */
        ompi_group_t *comm_group = P2P_MODULE(win)->p2p_comm->c_local_group;

        /* find the rank in the communicator associated with this windows */
        for (j = 0 ; 
             j < ompi_group_size(comm_group) ;
             ++j) {
            if (P2P_MODULE(win)->p2p_sc_group->grp_proc_pointers[i] ==
                comm_group->grp_proc_pointers[j]) {
                comm_rank = j;
                break;
            }
        }
        if (comm_rank == -1) {
            ret = MPI_ERR_RMA_SYNC;
            goto cleanup;
        }

        OPAL_THREAD_ADD32(&(P2P_MODULE(win)->p2p_num_pending_out), 
                          P2P_MODULE(win)->p2p_copy_num_pending_sendreqs[comm_rank]);
        ompi_osc_pt2pt_control_send(P2P_MODULE(win), 
                                    P2P_MODULE(win)->p2p_sc_group->grp_proc_pointers[i],
                                    OMPI_OSC_PT2PT_HDR_COMPLETE,
                                    P2P_MODULE(win)->p2p_copy_num_pending_sendreqs[comm_rank]);
    }

    /* try to start all the requests.  We've copied everything we
       need out of pending_sendreqs, so don't need the lock
       here */
    while (NULL != 
           (item = opal_list_remove_first(&(P2P_MODULE(win)->p2p_copy_pending_sendreqs)))) {
        ompi_osc_pt2pt_sendreq_t *req = 
            (ompi_osc_pt2pt_sendreq_t*) item;

        ret = ompi_osc_pt2pt_sendreq_send(P2P_MODULE(win), req);

        if (OMPI_SUCCESS != ret) {
            opal_output(0, "complete: failure in starting sendreq (%d).  Will try later.",
                        ret);
            opal_list_prepend(&(P2P_MODULE(win)->p2p_copy_pending_sendreqs), item);
        }
    }

    /* wait for all the requests */
    while (0 != P2P_MODULE(win)->p2p_num_pending_out) {
        ompi_osc_pt2pt_progress(P2P_MODULE(win));        
    }

 cleanup:
    /* set our mode back to nothing */
    ompi_win_set_mode(win, 0);

    OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
    group = P2P_MODULE(win)->p2p_pw_group;
    P2P_MODULE(win)->p2p_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    /* BWB - do I need this? */
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return ret;
}

int
ompi_osc_pt2pt_module_post(ompi_group_t *group,
                           int assert,
                           ompi_win_t *win)
{
    int i;

    OBJ_RETAIN(group);
    /* BWB - do I need this? */
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
    if (NULL != P2P_MODULE(win)->p2p_sc_group || NULL != P2P_MODULE(win)->p2p_pw_group) {
        OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));
        return MPI_ERR_RMA_CONFLICT;
    }
    P2P_MODULE(win)->p2p_pw_group = group;    
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    /* Set our mode to expose w/ post */
    ompi_win_set_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    /* list how many complete counters we're still waiting on */
    OPAL_THREAD_ADD32(&(P2P_MODULE(win)->p2p_num_pending_out),
                      ompi_group_size(P2P_MODULE(win)->p2p_pw_group));

    /* send a hello counter to everyone in group */
    for (i = 0 ; i < ompi_group_size(P2P_MODULE(win)->p2p_pw_group) ; ++i) {
        ompi_osc_pt2pt_control_send(P2P_MODULE(win), 
                                    group->grp_proc_pointers[i],
                                    OMPI_OSC_PT2PT_HDR_POST, 1);
    }    

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_wait(ompi_win_t *win)
{
    ompi_group_t *group;

    while (0 != (P2P_MODULE(win)->p2p_num_pending_in) ||
           0 != (P2P_MODULE(win)->p2p_num_pending_out)) {
        ompi_osc_pt2pt_progress(P2P_MODULE(win));        
    }

    ompi_win_set_mode(win, 0);

    OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
    group = P2P_MODULE(win)->p2p_pw_group;
    P2P_MODULE(win)->p2p_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    /* BWB - do I need this? */
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


int 
ompi_osc_pt2pt_module_test(ompi_win_t *win,
                           int *flag)
{
    ompi_group_t *group;

    if (0 != (P2P_MODULE(win)->p2p_num_pending_in) ||
        0 != (P2P_MODULE(win)->p2p_num_pending_out)) {
        ompi_osc_pt2pt_progress(P2P_MODULE(win));        
        if (0 != (P2P_MODULE(win)->p2p_num_pending_in) ||
            0 != (P2P_MODULE(win)->p2p_num_pending_out)) {
            *flag = 0;
            return OMPI_SUCCESS;
        }
    }

    *flag = 1;

    ompi_win_set_mode(win, 0);

    OPAL_THREAD_LOCK(&(P2P_MODULE(win)->p2p_lock));
    group = P2P_MODULE(win)->p2p_pw_group;
    P2P_MODULE(win)->p2p_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(P2P_MODULE(win)->p2p_lock));

    /* BWB - do I need this? */
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_lock(int lock_type,
                           int target,
                           int assert,
                           ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_osc_pt2pt_module_unlock(int target,
                             ompi_win_t *win)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
