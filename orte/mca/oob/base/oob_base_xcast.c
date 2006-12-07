/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/bit_ops.h"

#include "orte/util/proc_info.h"
#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/smr/smr.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  process_first   Whether or not to process the message locally before sending it on
 *  @param  buffer  The data to broadcast - only significant at root.
 *  @param  cbfunc  Callback function on receipt of data - not significant at root.
 */

static opal_mutex_t xcastmutex;
static int xcast_bitmap, bitmap_save;
static bool bitmap_init = false;

static int mca_oob_xcast_binomial_tree(orte_jobid_t job,
                                       bool process_first,
                                       orte_buffer_t* buffer,
                                       orte_gpr_trigger_cb_fn_t cbfunc);

static int mca_oob_xcast_linear(orte_jobid_t job,
                                bool process_first,
                                orte_buffer_t* buffer,
                                orte_gpr_trigger_cb_fn_t cbfunc);

int mca_oob_xcast(orte_jobid_t job,
                  bool process_first,
                  orte_buffer_t* buffer,
                  orte_gpr_trigger_cb_fn_t cbfunc)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    
    if (orte_oob_xcast_timing) {
        if (NULL != buffer) {
            opal_output(0, "xcast [%ld,%ld,%ld]: buffer size %lu", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME),
                        (unsigned long)buffer->bytes_used);
        }
        gettimeofday(&start, NULL);
    }
    switch(orte_oob_xcast_mode) {
        case 0:  /* binomial tree */
            rc = mca_oob_xcast_binomial_tree(job, process_first, buffer, cbfunc);
            break;
            
        case 1: /* linear */
            rc = mca_oob_xcast_linear(job, process_first, buffer, cbfunc);
            break;
    }
    if (orte_oob_xcast_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "xcast [%ld,%ld,%ld]: mode %s time %ld usec", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME),
                    orte_oob_xcast_mode ? "linear" : "binomial",
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    
    return rc;
}

static int mca_oob_xcast_binomial_tree(orte_jobid_t job,
                                       bool process_first,
                                       orte_buffer_t* buffer,
                                       orte_gpr_trigger_cb_fn_t cbfunc)
{
    orte_std_cntr_t i;
    int rc;
    int tag = ORTE_RML_TAG_XCAST;
    int peer, size, rank, hibit, mask;
    orte_buffer_t rbuf, sbuf;
    orte_gpr_notify_message_t *msg;
    orte_process_name_t target;

    /* check to see if there is something to send - this is only true on the HNP end.
     * However, we cannot just test to see if we are the HNP since, if we are a singleton,
     * we are the HNP *and* we still need to handle both ends of the xcast
     */
    if (NULL != buffer) {        
        /* this is the HNP end, so it starts the procedure. Accordingly, it sends its
         * message to the first process in the job in the peer list, which takes it from there
         */
        OBJ_CONSTRUCT(&xcastmutex, opal_mutex_t);
        OPAL_THREAD_LOCK(&xcastmutex);

        target.cellid = ORTE_PROC_MY_NAME->cellid;
        target.jobid = job;
        target.vpid = 0;
        if (0 > (rc = mca_oob_send_packed(&target, buffer, tag, 0))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&xcastmutex);
            OBJ_DESTRUCT(&xcastmutex);
            return rc;
        }

        OPAL_THREAD_UNLOCK(&xcastmutex);
        OBJ_DESTRUCT(&xcastmutex);

        return ORTE_SUCCESS;
        
    } 
    
    
    /* this process is one of the application procs - accordingly, it will
     * receive the message from its "parent" in the broadcast tree, and
     * then send it along to some set of children
     */
    
    /* compute the bitmap, if we haven't already done so */
    if (!bitmap_init) {
        bitmap_save = opal_cube_dim((int)orte_process_info.num_procs);
        bitmap_init = true;
    }
    
    xcast_bitmap = bitmap_save;
    rank = (int)(ORTE_PROC_MY_NAME->vpid);
    size = (int)orte_process_info.num_procs;
    
    hibit = opal_hibit(rank, xcast_bitmap);
    --xcast_bitmap;
    
    /* regardless of who we are, we first have to receive the message */
    OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
    if (0 > (rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, tag))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rbuf);
        return rc;
    }
    
    msg = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == msg) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    i=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &msg, &i, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    OBJ_DESTRUCT(&rbuf);
    
    /* repack the message so we can send it on */
    OBJ_CONSTRUCT(&sbuf, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&sbuf, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sbuf);
        return rc;
    }
    
    /* since the OOB contact info for our peers is in the STG1 message, we have to
     * process it BEFORE we can relay the message to any "children"
     */
    if (cbfunc != NULL && process_first) {
        /* process the message */
        cbfunc(msg);
    }
            
    /* send data to any children */
    target.cellid = ORTE_PROC_MY_NAME->cellid;
    target.jobid = ORTE_PROC_MY_NAME->jobid;
    
    for (i = hibit + 1, mask = 1 << i; i <= xcast_bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < size) {
            target.vpid = (orte_vpid_t)peer;
            if (0 > (rc = mca_oob_send_packed(&target, &sbuf, tag, 0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(msg);
                return rc;
            }
        }
    }
    OBJ_DESTRUCT(&sbuf);
    
    /* if it wasn't the STG1 message, then process it here */
    if (cbfunc != NULL && !process_first) {
        cbfunc(msg);
    }
    OBJ_RELEASE(msg);
    
    return ORTE_SUCCESS;
}

static int mca_oob_xcast_linear(orte_jobid_t job,
                                bool process_first,
                                orte_buffer_t* buffer,
                                orte_gpr_trigger_cb_fn_t cbfunc)
{
    orte_std_cntr_t i;
    int rc;
    int tag = ORTE_RML_TAG_XCAST;
    int status;
    orte_process_name_t *peers=NULL;
    orte_std_cntr_t n=0;
    orte_proc_state_t state;
    opal_list_t attrs;
    opal_list_item_t *item;
    
    /* check to see if there is something to send - this is only true on the HNP end.
     * However, we cannot just test to see if we are the HNP since, if we are a singleton,
     * we are the HNP *and* we still need to handle both ends of the xcast
     */
    if (NULL != buffer) {        
        OBJ_CONSTRUCT(&xcastmutex, opal_mutex_t);
        OPAL_THREAD_LOCK(&xcastmutex);

        /* this is the HNP end, so it does all the sends in this algorithm. First, we need
         * to get the job peers so we know who to send the message to
        */
        OBJ_CONSTRUCT(&attrs, opal_list_t);
        orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOBID, ORTE_JOBID, &job, ORTE_RMGR_ATTR_OVERRIDE);
        if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &n, &attrs))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&xcastmutex);
            OBJ_DESTRUCT(&xcastmutex);
            return rc;
        }
        item = opal_list_remove_first(&attrs);
        OBJ_RELEASE(item);
        OBJ_DESTRUCT(&attrs);
        
        for(i=0; i<n; i++) {
            /* check status of peer to ensure they are alive */
            if (ORTE_SUCCESS != (rc = orte_smr.get_proc_state(&state, &status, peers+i))) {
                ORTE_ERROR_LOG(rc);
                free(peers);
                OPAL_THREAD_UNLOCK(&xcastmutex);
                OBJ_DESTRUCT(&xcastmutex);
                return rc;
            }
            if (state != ORTE_PROC_STATE_TERMINATED && state != ORTE_PROC_STATE_ABORTED) {
                rc = mca_oob_send_packed(peers+i, buffer, tag, 0);
                if (rc < 0) {
                    ORTE_ERROR_LOG(rc);
                    free(peers);
                    OPAL_THREAD_UNLOCK(&xcastmutex);
                    OBJ_DESTRUCT(&xcastmutex);
                    return rc;
                }
            }
        }
        free(peers);
        OPAL_THREAD_UNLOCK(&xcastmutex);
        OBJ_DESTRUCT(&xcastmutex);
        return ORTE_SUCCESS;
        
    /* if we are not the HNP, then we need to just receive the message and process it */        
    } else {
        orte_buffer_t rbuf;
        orte_gpr_notify_message_t *msg;
        
        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, tag);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        if (cbfunc != NULL) {
            msg = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == msg) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            i=1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &msg, &i, ORTE_GPR_NOTIFY_MSG))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(msg);
                return rc;
            }
            cbfunc(msg);
            OBJ_RELEASE(msg);
        }
        OBJ_DESTRUCT(&rbuf);
    }
    return ORTE_SUCCESS;
    
}
