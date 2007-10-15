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
#include "orte/runtime/params.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_basic.h"

/* Local functions */
static int xcast_binomial_tree(orte_jobid_t job,
                               orte_buffer_t *buffer,
                               orte_rml_tag_t tag);

static int xcast_linear(orte_jobid_t job,
                        orte_buffer_t *buffer,
                        orte_rml_tag_t tag);

static int xcast_direct(orte_jobid_t job,
                        orte_buffer_t *buffer,
                        orte_rml_tag_t tag);

/* define a callback function for use by the blocking version
 * of xcast so we can "hold" the caller here until all non-blocking
 * sends have completed
 */
static void xcast_send_cb(int status,
                          orte_process_name_t* peer,
                          orte_buffer_t* buffer,
                          orte_rml_tag_t tag,
                          void* cbdata)
{
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    
    orte_grpcomm_basic.num_active--;
    if (orte_grpcomm_basic.num_active <= 0) {
        orte_grpcomm_basic.num_active = 0; /* just to be safe */
        opal_condition_signal(&orte_grpcomm_basic.cond);
    }
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    return;
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Non-blocking version */
static int xcast_nb(orte_jobid_t job,
                    orte_buffer_t *buffer,
                    orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    orte_vpid_t num_daemons;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_nb sent to job %ld tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)job, (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    if (orte_timing) {
        gettimeofday(&start, NULL);
    }
    
    /* get the number of daemons currently in the system so we can
     * select the "optimal" algorithm
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &num_daemons))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_nb: num_daemons %ld linear xover: %ld binomial xover: %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)num_daemons, (long)orte_grpcomm_basic.xcast_linear_xover,
                         (long)orte_grpcomm_basic.xcast_binomial_xover));
    
    if (num_daemons < 2 || orte_daemon_died) {
        /* if there is only one daemon in the system, then we must
         * use the direct mode - there is no other option. Note that
         * since the HNP is the one that typically does xcast sends,
         * only one daemon means that the HNP itself is sending to
         * itself. This is required in singletons - where the
         * singleton acts as the HNP - and as an HNP starts
         * itself up
         *
         * NOTE: although we allow users to alter crossover points
         * for selecting specific xcast modes, this required
         * use-case behavior MUST always be retained or else
         * singletons and HNP startup will fail!
         *
         * We also insist that the direct xcast mode be used when
         * an orted has failed as we cannot rely on alternative
         * methods to reach all orteds and/or procs
         */
        rc = xcast_direct(job, buffer, tag);
        goto DONE;
    }
    
    /* now use the crossover points to select the proper transmission
     * mode. We have built-in default crossover points for this
     * decision tree, but the user is free to alter them as
     * they wish via MCA params
     */
    
    if (num_daemons < orte_grpcomm_basic.xcast_linear_xover) {
        rc = xcast_direct(job, buffer, tag);
    } else if (num_daemons < orte_grpcomm_basic.xcast_binomial_xover) {
        rc = xcast_linear(job, buffer, tag);
    } else {
        rc = xcast_binomial_tree(job, buffer, tag);
    }
    
DONE:
    if (orte_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "xcast_nb %s: time %ld usec", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    
    return rc;
}

/* Blocking version */
static int xcast(orte_jobid_t job,
                 orte_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    orte_vpid_t num_daemons;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast sent to job %ld tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)job, (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }

    if (orte_timing) {
        gettimeofday(&start, NULL);
    }
    
    /* get the number of daemons currently in the system so we can
     * select the "optimal" algorithm
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &num_daemons))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast: num_daemons %ld linear xover: %ld binomial xover: %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)num_daemons, (long)orte_grpcomm_basic.xcast_linear_xover,
                         (long)orte_grpcomm_basic.xcast_binomial_xover));

    if (num_daemons < 2 || orte_daemon_died) {
        /* if there is only one daemon in the system, then we must
         * use the direct mode - there is no other option. Note that
         * since the HNP is the one that typically does xcast sends,
         * only one daemon means that the HNP itself is sending to
         * itself. This is required in singletons - where the
         * singleton acts as the HNP - and as an HNP starts
         * itself up
         *
         * NOTE: although we allow users to alter crossover points
         * for selecting specific xcast modes, this required
         * use-case behavior MUST always be retained or else
         * singletons and HNP startup will fail!
         *
         * We also insist that the direct xcast mode be used when
         * an orted has failed as we cannot rely on alternative
         * methods to reach all orteds and/or procs
         */
        rc = xcast_direct(job, buffer, tag);
        goto DONE;
    }
    
    /* now use the crossover points to select the proper transmission
     * mode. We have built-in default crossover points for this
     * decision tree, but the user is free to alter them as
     * they wish via MCA params
     */
    
    if (num_daemons < orte_grpcomm_basic.xcast_linear_xover) {
        rc = xcast_direct(job, buffer, tag);
    } else if (num_daemons < orte_grpcomm_basic.xcast_binomial_xover) {
        rc = xcast_linear(job, buffer, tag);
    } else {
        rc = xcast_binomial_tree(job, buffer, tag);
    }
    
DONE:
    /* if a daemon has failed AND this message was going to
     * the daemons, then we don't want to wait - just return
     */
    if (0 == job && orte_daemon_died) {
        return rc;
    }
    
    /* now go to sleep until woken up */
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    if (orte_grpcomm_basic.num_active > 0) {
        opal_condition_wait(&orte_grpcomm_basic.cond, &orte_grpcomm_basic.mutex);
    }
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    
    if (orte_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "xcast %s: time %ld usec", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    return rc;
}

static int xcast_binomial_tree(orte_jobid_t job,
                               orte_buffer_t *buffer,
                               orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command, mode;
    int rc;
    orte_process_name_t target;
    orte_buffer_t *buf;
    orte_vpid_t nd;
    orte_std_cntr_t num_daemons;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_binomial",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* this is the HNP end, so it starts the procedure. Since the HNP is always the
     * vpid=0 at this time, we take advantage of that fact to figure out who we
     * should send this to on the first step
     */
    /* need to pack the msg for sending - be sure to include routing info so it
     * can properly be sent through the daemons
     */
    buf = OBJ_NEW(orte_buffer_t);
    
    /* tell the daemon the routing algorithm so it can figure
     * out how to forward the message down the tree, if at all
     */
    mode = ORTE_DAEMON_ROUTE_BINOMIAL;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &mode, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the number of daemons currently in the system and tell the daemon so
     * it can properly route
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(0, &nd))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    num_daemons = (orte_std_cntr_t)nd;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &num_daemons, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
        
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_binomial: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buf->bytes_used));
    
    /* all we need to do is send this to ourselves - our relay logic
     * will ensure everyone else gets it!
     */
    target.jobid = 0;
    target.vpid = 0;
    ++orte_grpcomm_basic.num_active;
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "xcast_binomial: num_active now %ld sending %s => %s",
                         (long)orte_grpcomm_basic.num_active,
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&target)));
    
    if (0 > (rc = orte_rml.send_buffer_nb(&target, buf, ORTE_RML_TAG_ORTED_ROUTED,
                                          0, xcast_send_cb, NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        rc = ORTE_ERR_COMM_FAILURE;
        OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
        --orte_grpcomm_basic.num_active;
        OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
        goto CLEANUP;
    }

CLEANUP:  
    OBJ_RELEASE(buf);
    
    return rc;
}

static int xcast_linear(orte_jobid_t job,
                        orte_buffer_t *buffer,
                        orte_rml_tag_t tag)
{
    int rc;
    orte_buffer_t *buf;
    orte_daemon_cmd_flag_t command, mode=ORTE_DAEMON_ROUTE_NONE;
    orte_vpid_t i, range;
    orte_process_name_t dummy;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_linear",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* since we have to pack some additional info into the buffer to be
     * sent to the daemons, we create a new buffer into which we will
     * put the intermediate payload - i.e., the info that goes to the
     * daemon. This buffer will contain all the info needed by the
     * daemon, plus the payload intended for the processes themselves
     */
    buf = OBJ_NEW(orte_buffer_t);
    
    /* tell the daemon that no further routing required */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &mode, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_linear: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buf->bytes_used));
    
    /* get the number of daemons out there */
    orte_ns.get_vpid_range(0, &range);
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    orte_grpcomm_basic.num_active += range;
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_linear: num_active now %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)orte_grpcomm_basic.num_active));
    
    /* send the message to each daemon as fast as we can */
    dummy.jobid = 0;
    for (i=0; i < range; i++) {            
        dummy.vpid = i;
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "xcast_linear: %s => %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&dummy)));
        if (0 > (rc = orte_rml.send_buffer_nb(&dummy, buf, ORTE_RML_TAG_ORTED_ROUTED,
                                              0, xcast_send_cb, NULL))) {
            if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                rc = ORTE_ERR_COMM_FAILURE;
                OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
                orte_grpcomm_basic.num_active -= (range-i);
                OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
                goto CLEANUP;
            }
            /* decrement the number we are waiting to see */
            OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
            orte_grpcomm_basic.num_active--;
            OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
        }
    }
    rc = ORTE_SUCCESS;
    
    /* cleanup */
CLEANUP:
    OBJ_RELEASE(buf);
    return rc;    
}

static int xcast_direct(orte_jobid_t job,
                        orte_buffer_t *buffer,
                        orte_rml_tag_t tag)
{
    orte_std_cntr_t i;
    int rc;
    orte_process_name_t *peers=NULL;
    orte_std_cntr_t n;
    opal_list_t attrs;
    opal_list_item_t *item;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_direct",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_direct: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buffer->bytes_used));
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    orte_grpcomm_basic.num_active += n;
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_direct: num_active now %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)orte_grpcomm_basic.num_active));
    
    for(i=0; i<n; i++) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "xcast_direct: %s => %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(peers+i)));
        if (0 > (rc = orte_rml.send_buffer_nb(peers+i, buffer, tag, 0, xcast_send_cb, NULL))) {
            if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                rc = ORTE_ERR_COMM_FAILURE;
                OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
                orte_grpcomm_basic.num_active -= (n-i);
                OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
                goto CLEANUP;
            }
            /* decrement the number we are waiting to see */
            OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
            orte_grpcomm_basic.num_active--;
            OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
        }          
    }
    rc = ORTE_SUCCESS;

CLEANUP:
    free(peers);

    return rc;
}

static int allgather(orte_buffer_t *sbuf, orte_buffer_t *rbuf)
{
    orte_process_name_t name;
    int rc;
    orte_std_cntr_t i;
    orte_buffer_t tmpbuf;
    
    /* everything happens within my jobid */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        /* everyone but rank=0 sends data */
        name.vpid = 0;
        if (0 > orte_rml.send_buffer(&name, sbuf, ORTE_RML_TAG_ALLGATHER, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s allgather buffer sent",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* now receive the final result from rank=0 */
        if (0 > orte_rml.recv_buffer(ORTE_NAME_WILDCARD, rbuf, ORTE_RML_TAG_ALLGATHER, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s allgather buffer received",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    
    /* seed the outgoing buffer with the num_procs so it can be unpacked */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(rbuf, &orte_process_info.num_procs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* put my own information into the outgoing buffer */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(rbuf, sbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather collecting buffers",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* rank=0 receives everyone else's data */
    for (i=1; i < orte_process_info.num_procs; i++) {
        name.vpid = (orte_vpid_t)i;
        OBJ_CONSTRUCT(&tmpbuf, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(&name, &tmpbuf, ORTE_RML_TAG_ALLGATHER, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s allgather buffer %ld received",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)i));
        /* append this data to the rbuf */
        if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(rbuf, &tmpbuf))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* clear out the tmpbuf */
        OBJ_DESTRUCT(&tmpbuf);
    }
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather xcasting collected data",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* xcast the results */
    orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, rbuf, ORTE_RML_TAG_ALLGATHER);
    
    /* xcast automatically ensures that the sender -always- gets a copy
     * of the message. This is required to ensure proper operation of the
     * launch system as the HNP -must- get a copy itself. So we have to
     * post our own receive here so that we don't leave a message rattling
     * around in our RML
     */
    OBJ_CONSTRUCT(&tmpbuf, orte_buffer_t);
    if (0 > (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &tmpbuf, ORTE_RML_TAG_ALLGATHER, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather buffer received",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* don't need the received buffer - we already have what we need */
    OBJ_DESTRUCT(&tmpbuf);
    
    return ORTE_SUCCESS;
}

static int allgather_list(opal_list_t *names, orte_buffer_t *sbuf, orte_buffer_t *rbuf)
{
    opal_list_item_t *item;
    orte_namelist_t *peer, *root;
    orte_std_cntr_t i, num_peers;
    orte_buffer_t tmpbuf;
    int rc;
    
    /* the first entry on the list is the "root" that collects
     * all the data - everyone else just sends and gets back
     * the results
     */
    root = (orte_namelist_t*)opal_list_get_first(names);
    
    if (ORTE_EQUAL != orte_dss.compare(root->name, ORTE_PROC_MY_NAME, ORTE_NAME)) {
        /* everyone but root sends data */
        if (0 > orte_rml.send_buffer(root->name, sbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        /* now receive the final result */
        if (0 > orte_rml.recv_buffer(root->name, rbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        return ORTE_SUCCESS;
    }
    
    /* count how many peers are participating, including myself */
    num_peers = (orte_std_cntr_t)opal_list_get_size(names);

    /* seed the outgoing buffer with the num_procs so it can be unpacked */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(rbuf, &num_peers, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* put my own information into the outgoing buffer */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(rbuf, sbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* root receives everyone else's data */
    for (i=1; i < num_peers; i++) {
        /* receive the buffer from this process */
        OBJ_CONSTRUCT(&tmpbuf, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &tmpbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        /* append this data to the rbuf */
        if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(rbuf, &tmpbuf))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* clear out the tmpbuf */
        OBJ_DESTRUCT(&tmpbuf);
    }
    
    /* broadcast the results */
    for (item = opal_list_get_first(names);
         item != opal_list_get_end(names);
         item = opal_list_get_next(item)) {
        peer = (orte_namelist_t*)item;
        
        /* skip myself */
        if (ORTE_EQUAL == orte_dss.compare(root->name, peer->name, ORTE_NAME)) {
            continue;
        }
        
        /* transmit the buffer to this process */
        if (0 > orte_rml.send_buffer(peer->name, rbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
    }
    
    return ORTE_SUCCESS;
}


static int barrier(void)
{
    orte_process_name_t name;
    orte_std_cntr_t i;
    orte_buffer_t buf;
    int rc;
    
    /* everything happens within the same jobid */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* All non-root send & receive zero-length message. */
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        name.vpid = 0;
        OBJ_CONSTRUCT(&buf, orte_buffer_t);
        i=0;
        orte_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
        
        rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_BARRIER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* get the release from rank=0 */
        OBJ_CONSTRUCT(&buf, orte_buffer_t);
        rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD,&buf,ORTE_RML_TAG_BARRIER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        return ORTE_SUCCESS;
    }
    
    for (i = 1; i < orte_process_info.num_procs; i++) {
        name.vpid = (orte_vpid_t)i;
        OBJ_CONSTRUCT(&buf, orte_buffer_t);
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s barrier %ld received",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)i));
        rc = orte_rml.recv_buffer(&name,&buf,ORTE_RML_TAG_BARRIER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s barrier xcasting release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* xcast the release */
    OBJ_CONSTRUCT(&buf, orte_buffer_t);
    orte_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
    orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, &buf, ORTE_RML_TAG_BARRIER);
    OBJ_DESTRUCT(&buf);

    /* xcast automatically ensures that the sender -always- gets a copy
     * of the message. This is required to ensure proper operation of the
     * launch system as the HNP -must- get a copy itself. So we have to
     * post our own receive here so that we don't leave a message rattling
     * around in our RML
     */
    OBJ_CONSTRUCT(&buf, orte_buffer_t);
    if (0 > (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_BARRIER, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s barrier release received",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}

orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    xcast,
    xcast_nb,
    allgather,
    allgather_list,
    barrier
};

