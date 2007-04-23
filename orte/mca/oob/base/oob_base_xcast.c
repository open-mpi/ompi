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
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"

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

static int mca_oob_xcast_binomial_tree(orte_jobid_t job,
                                       orte_gpr_notify_message_t *msg,
                                       orte_gpr_trigger_cb_fn_t cbfunc);

static int mca_oob_xcast_linear(orte_jobid_t job,
                                orte_gpr_notify_message_t *msg,
                                orte_gpr_trigger_cb_fn_t cbfunc);

static int mca_oob_xcast_direct(orte_jobid_t job,
                                orte_gpr_notify_message_t *msg,
                                orte_gpr_trigger_cb_fn_t cbfunc);

int mca_oob_xcast(orte_jobid_t job,
                  orte_gpr_notify_message_t *msg,
                  orte_gpr_trigger_cb_fn_t cbfunc)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    
    if (orte_oob_xcast_timing) {
        gettimeofday(&start, NULL);
    }
    
    switch(orte_oob_xcast_mode) {
        case 0:  /* binomial tree */
            rc = mca_oob_xcast_binomial_tree(job, msg, cbfunc);
            break;
            
        case 1: /* linear */
            rc = mca_oob_xcast_linear(job, msg, cbfunc);
            break;
                
        case 2: /* direct */
            rc = mca_oob_xcast_direct(job, msg, cbfunc);
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
                                       orte_gpr_notify_message_t *msg,
                                       orte_gpr_trigger_cb_fn_t cbfunc)
{
    orte_std_cntr_t i;
    int rc, ret;
    int peer, size, rank, hibit, mask;
    orte_process_name_t target;
    orte_buffer_t buffer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
    orte_daemon_cmd_flag_t mode=ORTE_DAEMON_ROUTE_BINOMIAL;
    orte_vpid_t daemon_start=0, num_daemons;
    int bitmap;

    /* check to see if there is something to send - this is only true on the HNP end.
     * However, we cannot just test to see if we are the HNP since, if we are a singleton,
     * we are the HNP *and* we still need to handle both ends of the xcast
     */
    if (NULL != msg) {        
        /* this is the HNP end, so it starts the procedure. Since the HNP is always the
         * vpid=0 at this time, we take advantage of that fact to figure out who we
         * should send this to on the first step
         */
        OBJ_CONSTRUCT(&xcastmutex, opal_mutex_t);
        OPAL_THREAD_LOCK(&xcastmutex);

        /* need to pack the msg for sending - be sure to include routing info so it
         * can properly be sent through the daemons
         */
        OBJ_CONSTRUCT(&buffer, orte_buffer_t);
        /* tell the daemon this is a message for its local procs */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* tell the daemon the routing algorithm is binomial so it can figure
         * out who to forward the message to
         */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &mode, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* get the number of daemons currently in the system and tell the daemon so
         * it can properly route
         */
        if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &num_daemons))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &daemon_start, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &num_daemons, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* tell the daemon the jobid of the procs that are to receive the message */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* pack the message itself */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        if (orte_oob_xcast_timing) {
            opal_output(0, "xcast [%ld,%ld,%ld]: buffer size %ld",
                        ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buffer.bytes_used);
        }
        
        /* start setting up the target recipients */
        target.cellid = ORTE_PROC_MY_NAME->cellid;
        target.jobid = 0;
        
        /* if there is only one daemon, then we just send it - don't try to
         * compute the binomial algorithm as it won't yield a meaningful
         * result for just one
         */
        if (num_daemons < 2) {
            target.vpid = 1;
            if (0 > (ret = mca_oob_send_packed(&target, &buffer, ORTE_RML_TAG_PLS_ORTED, 0))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(msg);
                rc = ret;
                goto CLEANUP;
            }
        } else {
            /* compute the bitmap */
            bitmap = opal_cube_dim((int)num_daemons);
            rank = 0;
            size = (int)num_daemons;
            
            hibit = opal_hibit(rank, bitmap);
            --bitmap;
                    
            target.cellid = ORTE_PROC_MY_NAME->cellid;
            target.jobid = 0;
            for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
                peer = rank | mask;
                if (peer < size) {
                    target.vpid = (orte_vpid_t)(daemon_start+peer);
                    if (0 > (ret = mca_oob_send_packed(&target, &buffer, ORTE_RML_TAG_PLS_ORTED, 0))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(msg);
                        rc = ret;
                        goto CLEANUP;
                    }
                }
            }
        }

CLEANUP:
        OBJ_DESTRUCT(&buffer);
        
        OPAL_THREAD_UNLOCK(&xcastmutex);
        OBJ_DESTRUCT(&xcastmutex);

        return rc;
    }  else {
        /* if we are not the HNP, then we need to just receive the message and process it */        
        orte_std_cntr_t i;
        orte_buffer_t rbuf;
        orte_gpr_notify_message_t *mesg;
                
        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, ORTE_RML_TAG_XCAST);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        
        if (cbfunc != NULL) {
            mesg = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == mesg) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            i=1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &mesg, &i, ORTE_GPR_NOTIFY_MSG))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(mesg);
                return rc;
            }
            cbfunc(mesg);
            OBJ_RELEASE(mesg);
        }
        OBJ_DESTRUCT(&rbuf);
        return ORTE_SUCCESS;
    }
    
}

static int mca_oob_xcast_linear(orte_jobid_t job,
                                orte_gpr_notify_message_t *msg,
                                orte_gpr_trigger_cb_fn_t cbfunc)
{
    int rc;
    orte_buffer_t buffer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
    orte_daemon_cmd_flag_t mode=ORTE_DAEMON_ROUTE_NONE;
    orte_vpid_t i, range;
    orte_process_name_t dummy;
    
    if (NULL != msg) {
        /* if we are the HNP, then we need to send the message out */
        OBJ_CONSTRUCT(&xcastmutex, opal_mutex_t);
        OPAL_THREAD_LOCK(&xcastmutex);
        
        /* pack the msg for sending - indicate that no further routing is required */
        OBJ_CONSTRUCT(&buffer, orte_buffer_t);
        /* tell the daemon this is a message for its local procs */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* tell the daemon that no further routing required */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &mode, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* tell the daemon the jobid of the procs that are to receive the message */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* pack the message itself */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        if (orte_oob_xcast_timing) {
            opal_output(0, "xcast [%ld,%ld,%ld]: buffer size %ld",
                        ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buffer.bytes_used);
        }
        
        /* get the number of daemons out there */
        orte_ns.get_vpid_range(0, &range);
        
        /* send the message to each daemon as fast as we can */
        dummy.cellid = ORTE_PROC_MY_NAME->cellid;
        dummy.jobid = 0;
        for (i=0; i < range; i++) {            
            if (ORTE_PROC_MY_NAME->vpid != i) { /* don't send to myself */
                dummy.vpid = i;
                if (0 > (rc = orte_rml.send_buffer(&dummy, &buffer, ORTE_RML_TAG_PLS_ORTED, 0))) {
                    if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                        rc = ORTE_ERR_COMM_FAILURE;
                        goto CLEANUP;
                    }
                }
            }
        }
        rc = ORTE_SUCCESS;
        
        /* cleanup */
CLEANUP:
        OBJ_DESTRUCT(&buffer);
        
        OPAL_THREAD_UNLOCK(&xcastmutex);
        OBJ_DESTRUCT(&xcastmutex);
        
        return rc;    
    } else {
        /* if we are not the HNP, then we need to just receive the message and process it */        
        orte_std_cntr_t i;
        orte_buffer_t rbuf;
        orte_gpr_notify_message_t *mesg;
        
        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, ORTE_RML_TAG_XCAST);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        
        if (cbfunc != NULL) {
            mesg = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == mesg) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            i=1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &mesg, &i, ORTE_GPR_NOTIFY_MSG))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(mesg);
                return rc;
            }
            cbfunc(mesg);
            OBJ_RELEASE(mesg);
        }
        OBJ_DESTRUCT(&rbuf);
        return ORTE_SUCCESS;
   }
}

static int mca_oob_xcast_direct(orte_jobid_t job,
                                orte_gpr_notify_message_t *msg,
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
    orte_buffer_t buffer;
    
    /* check to see if there is something to send - this is only true on the HNP end.
     * However, we cannot just test to see if we are the HNP since, if we are a singleton,
     * we are the HNP *and* we still need to handle both ends of the xcast
     */
    if (NULL != msg) {        
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
        
        /* need to pack the msg for sending - no routing info here as this message
         * goes DIRECTLY to the processes
         */
        OBJ_CONSTRUCT(&buffer, orte_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buffer);
            free(peers);
            return rc;
        }
        
        if (orte_oob_xcast_timing) {
            opal_output(0, "xcast [%ld,%ld,%ld]: buffer size %ld",
                        ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buffer.bytes_used);
        }
        
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
                rc = mca_oob_send_packed(peers+i, &buffer, tag, 0);
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
        OBJ_DESTRUCT(&buffer);

        OPAL_THREAD_UNLOCK(&xcastmutex);
        OBJ_DESTRUCT(&xcastmutex);
        return ORTE_SUCCESS;
        
    /* if we are not the HNP, then we need to just receive the message and process it */        
    } else {
        orte_buffer_t rbuf;
        orte_gpr_notify_message_t *mesg;
        
        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, tag);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        if (cbfunc != NULL) {
            mesg = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == mesg) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            i=1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &mesg, &i, ORTE_GPR_NOTIFY_MSG))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(mesg);
                return rc;
            }
            cbfunc(mesg);
            OBJ_RELEASE(mesg);
        }
        OBJ_DESTRUCT(&rbuf);
    }
    return ORTE_SUCCESS;
    
}
