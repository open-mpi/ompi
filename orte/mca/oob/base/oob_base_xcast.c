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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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


/* Local variables */
static orte_std_cntr_t xcast_num_active;
static bool xcast_in_progress=false;
static char *strmode[] = {"binomial", "linear", "direct", "unknown"};

/* Local functions */
static int mca_oob_xcast_binomial_tree(orte_jobid_t job,
                                       orte_buffer_t *buffer,
                                       orte_rml_tag_t tag);

static int mca_oob_xcast_linear(orte_jobid_t job,
                                orte_buffer_t *buffer,
                                orte_rml_tag_t tag);

static int mca_oob_xcast_direct(orte_jobid_t job,
                                orte_buffer_t *buffer,
                                orte_rml_tag_t tag);


/* define a callback function for use by the blocking version
 * of xcast so we can "hold" the caller here until all non-blocking
 * sends have completed
 */
static void mca_oob_xcast_send_cb(int status,
                                  orte_process_name_t* peer,
                                  orte_buffer_t* buffer,
                                  int tag,
                                  void* cbdata)
{
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    
    xcast_num_active--;
    if (xcast_num_active == 0) {
        xcast_in_progress = false;
        opal_condition_signal(&orte_oob_xcast_cond);
    }
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    return;
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Non-blocking version */
int mca_oob_xcast_nb(orte_jobid_t job,
                     orte_buffer_t *buffer,
                     orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    if (xcast_in_progress) {
        /* you can't have two xcasts simultaneously ongoing */
        ORTE_ERROR_LOG(ORTE_ERR_RESOURCE_BUSY);
        return ORTE_ERR_RESOURCE_BUSY;
    }
    
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    xcast_in_progress = true;
    xcast_num_active = 0;
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    
    if (orte_oob_xcast_timing) {
        gettimeofday(&start, NULL);
    }
    
    switch(orte_oob_xcast_mode) {
        case 0:  /* binomial tree */
            rc = mca_oob_xcast_binomial_tree(job, buffer, tag);
            break;
            
        case 1: /* linear */
            rc = mca_oob_xcast_linear(job, buffer, tag);
            break;
                
        case 2: /* direct */
            rc = mca_oob_xcast_direct(job, buffer, tag);
            break;
    }
    
    if (orte_oob_xcast_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "xcast_nb [%ld,%ld,%ld]: mode %s time %ld usec", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME),
                    (orte_oob_xcast_mode < 0 || orte_oob_xcast_mode > 2) ?
                         strmode[3] : strmode[orte_oob_xcast_mode],
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    
    return rc;
}

/* Blocking version */
int mca_oob_xcast(orte_jobid_t job,
                  orte_buffer_t *buffer,
                  orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }

    if (xcast_in_progress) {
        /* you can't have two xcasts simultaneously ongoing */
        ORTE_ERROR_LOG(ORTE_ERR_RESOURCE_BUSY);
        return ORTE_ERR_RESOURCE_BUSY;
    }
    
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    xcast_in_progress = true;
    xcast_num_active = 0;
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    
    if (orte_oob_xcast_timing) {
        gettimeofday(&start, NULL);
    }
    
    switch(orte_oob_xcast_mode) {
        case 0:  /* binomial tree */
            rc = mca_oob_xcast_binomial_tree(job, buffer, tag);
            break;
            
        case 1: /* linear */
            rc = mca_oob_xcast_linear(job, buffer, tag);
            break;
                
        case 2: /* direct */
            rc = mca_oob_xcast_direct(job, buffer, tag);
            break;
    }
    
    /* now go to sleep until woken up */
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    if (xcast_num_active > 0) {
        opal_condition_wait(&orte_oob_xcast_cond, &orte_oob_xcast_mutex);
    }
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    
    if (orte_oob_xcast_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "xcast_nb [%ld,%ld,%ld]: mode %s time %ld usec", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME),
                    (orte_oob_xcast_mode < 0 || orte_oob_xcast_mode > 2) ?
                         strmode[3] : strmode[orte_oob_xcast_mode],
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    
    return rc;
}

static int mca_oob_xcast_binomial_tree(orte_jobid_t job,
                                       orte_buffer_t *buffer,
                                       orte_rml_tag_t tag)
{
    orte_std_cntr_t i;
    int rc;
    int peer, size, rank, hibit, mask;
    orte_process_name_t target;
    orte_buffer_t *buf;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
    orte_daemon_cmd_flag_t mode=ORTE_DAEMON_ROUTE_BINOMIAL;
    orte_vpid_t num_daemons;
    int bitmap;

    /* this is the HNP end, so it starts the procedure. Since the HNP is always the
     * vpid=0 at this time, we take advantage of that fact to figure out who we
     * should send this to on the first step
     */
    /* need to pack the msg for sending - be sure to include routing info so it
     * can properly be sent through the daemons
     */
    buf = OBJ_NEW(orte_buffer_t);
    
    /* ========  LOAD THE VALUES THAT ARE COMMON TO ALL NON-DIRECT MESSAGE PATHS ======== */
    /* tell the daemon this is a message for its local procs */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* tell the daemon the routing algorithm is binomial so it can figure
     * out who to forward the message down the tree
     */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &mode, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* tell the daemon the jobid of the procs that are to receive the message */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* tell the daemon the tag where the message is to be sent */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* =======  DONE WITH COMMON VALUES ====== */
    
    /* get the number of daemons currently in the system and tell the daemon so
     * it can properly route
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &num_daemons))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &num_daemons, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
        
    if (orte_oob_xcast_timing) {
        opal_output(0, "xcast [%ld,%ld,%ld]: mode binomial buffer size %ld",
                    ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buf->bytes_used);
    }
    
    /* start setting up the target recipients */
    target.cellid = ORTE_PROC_MY_NAME->cellid;
    target.jobid = 0;
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    xcast_num_active = num_daemons;
    if (orte_process_info.daemon ||
        orte_process_info.seed ||
        orte_process_info.singleton) {
        /* we never send to ourselves,
         * so we need to adjust the number of sends
         * we are expecting to complete
         */
        xcast_num_active--;
        if (xcast_num_active <= 0) {
            /* if we aren't going to send anything at all, we
            * need to reset the xcast_in_progress flag so
            * we don't block the entire system and return
            */
            xcast_in_progress = false;
            OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
            rc = ORTE_SUCCESS;
            goto CLEANUP;
        }
    }
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    
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
            target.vpid = (orte_vpid_t)peer;
            opal_output(0, "[%ld,%ld,%ld] xcast to [%ld,%ld,%ld]", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), ORTE_NAME_ARGS(&target));
            if (0 > (rc = mca_oob_send_packed_nb(&target, buf, ORTE_RML_TAG_PLS_ORTED,
                                                 0, mca_oob_xcast_send_cb, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    rc = ORTE_ERR_COMM_FAILURE;
                    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
                    xcast_num_active -= (num_daemons-i);
                    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
                    goto CLEANUP;
                }
                /* decrement the number we are waiting to see */
                OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
                xcast_num_active--;
                OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
            }
        }
    }

CLEANUP:
    OBJ_RELEASE(buf);  /* done with this object */
    
    return rc;
}

static int mca_oob_xcast_linear(orte_jobid_t job,
                                orte_buffer_t *buffer,
                                orte_rml_tag_t tag)
{
    int rc;
    orte_buffer_t *buf;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
    orte_daemon_cmd_flag_t mode=ORTE_DAEMON_ROUTE_NONE;
    orte_vpid_t i, range;
    orte_process_name_t dummy;
    
    /* since we have to pack some additional info into the buffer to be
     * sent to the daemons, we create a new buffer into which we will
     * put the intermediate payload - i.e., the info that goes to the
     * daemon. This buffer will contain all the info needed by the
     * daemon, plus the payload intended for the processes themselves
     */
    buf = OBJ_NEW(orte_buffer_t);
    
    /* tell the daemon this is a message for its local procs */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* tell the daemon that no further routing required */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &mode, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* tell the daemon the jobid of the procs that are to receive the message */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* tell the daemon the tag where the message is to be sent */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    if (orte_oob_xcast_timing) {
        opal_output(0, "xcast [%ld,%ld,%ld]: mode linear buffer size %ld",
                    ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buf->bytes_used);
    }
    
    /* get the number of daemons out there */
    orte_ns.get_vpid_range(0, &range);
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    xcast_num_active = range;
    if (orte_process_info.daemon ||
        orte_process_info.seed ||
        orte_process_info.singleton) {
        /* we never send to ourselves,
         * so we need to adjust the number of sends
         * we are expecting to complete
         */
        xcast_num_active--;
        if (xcast_num_active <= 0) {
            /* if we aren't going to send anything at all, we
             * need to reset the xcast_in_progress flag so
             * we don't block the entire system and return
             */
            xcast_in_progress = false;
            OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
            rc = ORTE_SUCCESS;
            goto CLEANUP;
        }
    }
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
    
    /* send the message to each daemon as fast as we can */
    dummy.cellid = ORTE_PROC_MY_NAME->cellid;
    dummy.jobid = 0;
    for (i=0; i < range; i++) {            
        if (ORTE_PROC_MY_NAME->vpid != i) { /* don't send to myself */
            dummy.vpid = i;
            if (0 > (rc = mca_oob_send_packed_nb(&dummy, buf, ORTE_RML_TAG_PLS_ORTED,
                                                 0, mca_oob_xcast_send_cb, NULL))) {
                if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    rc = ORTE_ERR_COMM_FAILURE;
                    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
                    xcast_num_active -= (range-i);
                    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
                    goto CLEANUP;
                }
                /* decrement the number we are waiting to see */
                OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
                xcast_num_active--;
                OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
            }
        }
    }
    rc = ORTE_SUCCESS;
    
    /* cleanup */
CLEANUP:
    OBJ_RELEASE(buf);  /* done with this object */

    return rc;    
}

static int mca_oob_xcast_direct(orte_jobid_t job,
                                orte_buffer_t *buffer,
                                orte_rml_tag_t tag)
{
    orte_std_cntr_t i;
    int rc;
    orte_process_name_t *peers=NULL;
    orte_std_cntr_t n=0;
    opal_list_t attrs;
    opal_list_item_t *item;
    
    /* need to get the job peers so we know who to send the message to */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOBID, ORTE_JOBID, &job, ORTE_RMGR_ATTR_OVERRIDE);
    if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &n, &attrs))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&attrs);
        return rc;
    }
    item = opal_list_remove_first(&attrs);
    OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
    
    /* no need to re-pack the msg for sending - no routing info here as this message
     * goes DIRECTLY to the processes
     */
    
    if (orte_oob_xcast_timing) {
        opal_output(0, "xcast [%ld,%ld,%ld]: mode direct buffer size %ld",
                    ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), (long)buffer->bytes_used);
    }
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
    xcast_num_active = n;
    OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);

    for(i=0; i<n; i++) {
        if (0 > (rc = mca_oob_send_packed_nb(peers+i, buffer, tag, 0, mca_oob_xcast_send_cb, NULL))) {
            if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                rc = ORTE_ERR_COMM_FAILURE;
                OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
                xcast_num_active -= (n-i);
                OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
                goto CLEANUP;
            }
            /* decrement the number we are waiting to see */
            OPAL_THREAD_LOCK(&orte_oob_xcast_mutex);
            xcast_num_active--;
            OPAL_THREAD_UNLOCK(&orte_oob_xcast_mutex);
        }          
    }
    rc = ORTE_SUCCESS;

CLEANUP:
    free(peers);

    return rc;
}

int mca_oob_xcast_gate(orte_gpr_trigger_cb_fn_t cbfunc)
{
    int rc;
    orte_std_cntr_t i;
    orte_buffer_t rbuf;
    orte_gpr_notify_message_t *mesg;
    
    OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
    rc = mca_oob_recv_packed(ORTE_NAME_WILDCARD, &rbuf, ORTE_RML_TAG_XCAST_BARRIER);
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
