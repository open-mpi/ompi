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
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/bit_ops.h"

#include "orte/class/orte_proc_table.h"
#include "orte/util/proc_info.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_basic.h"


/* Local functions */
static int xcast_binomial_tree(orte_jobid_t job,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tag);

static int xcast_linear(orte_jobid_t job,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag);

static int xcast_direct(orte_jobid_t job,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag);

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Blocking version */
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    struct timeval start, stop;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }

    if (orte_timing) {
        gettimeofday(&start, NULL);
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast: num_procs %ld linear xover: %ld binomial xover: %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)orte_process_info.num_procs,
                         (long)orte_grpcomm_basic.xcast_linear_xover,
                         (long)orte_grpcomm_basic.xcast_binomial_xover));

    if (orte_process_info.num_procs < 2 || orte_daemon_died) {
        /* if there is only one proc in the system, then we must
         * use the direct mode - there is no other option. Note that
         * since the HNP is the one that typically does xcast sends,
         * only one daemon means that the HNP is sending to
         * itself. This is required as an HNP starts
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
    
    if (orte_process_info.num_procs < orte_grpcomm_basic.xcast_linear_xover) {
        rc = xcast_direct(job, buffer, tag);
    } else if (orte_process_info.num_procs < orte_grpcomm_basic.xcast_binomial_xover) {
        rc = xcast_linear(job, buffer, tag);
    } else {
        rc = xcast_binomial_tree(job, buffer, tag);
    }
    
DONE:
    
    if (orte_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "%s xcast: time %ld usec", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
    }
    return rc;
}

static int xcast_binomial_tree(orte_jobid_t job,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    orte_grpcomm_mode_t mode;
    int rc;
    opal_buffer_t *buf;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_binomial",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* binomial xcast can only go through the daemons as app procs are
     * not allowed to relay messages.
     * first, need to pack the msg and be sure to include routing info so it
     * can properly be sent through the daemons
     */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* tell the daemon to process and relay */
    command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* tell the daemon the routing algorithm this xmission is using */
    mode = ORTE_GRPCOMM_BINOMIAL;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &mode, 1, ORTE_GRPCOMM_MODE))) {
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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
        
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_binomial: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buf->bytes_used));
    
    /* all we need to do is send this to the HNP - the relay logic
     * will ensure everyone else gets it!
     */
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_binomial: sending %s => %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
    
    /* if I am the HNP, just set things up so the cmd processor gets called.
     * We don't want to message ourselves as this can create circular logic
     * in the RML. Instead, this macro will set a zero-time event which will
     * cause the buffer to be processed by the cmd processor - probably will
     * fire right away, but that's okay
     * The macro makes a copy of the buffer, so it's okay to release it here
     */
    if (orte_process_info.hnp) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
    } else {
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
   }

CLEANUP:  
    OBJ_RELEASE(buf);

    return rc;
}

static int xcast_linear(orte_jobid_t job,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag)
{
    int rc;
    opal_buffer_t *buf;
    orte_daemon_cmd_flag_t command;
    orte_vpid_t i, range;
    orte_process_name_t dummy;
    orte_grpcomm_mode_t mode;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_linear",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* since we have to pack some additional info into the buffer to be
     * sent to the daemons, we create a new buffer into which we will
     * put the intermediate payload - i.e., the info that goes to the
     * daemon. This buffer will contain all the info needed by the
     * daemon, plus the payload intended for the processes themselves
     */
    buf = OBJ_NEW(opal_buffer_t);

    /* if we are an application proc, then send this to our HNP so
     * we don't try to talk to every daemon directly ourselves. This
     * is necessary since we don't know how many daemons there are!
     */
    if (!orte_process_info.hnp && !orte_process_info.daemon) {
        /* we are an application proc */
        /* tell the HNP to relay */
        command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* tell the daemon the routing algorithm this xmission is using */
        mode = ORTE_GRPCOMM_LINEAR;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &mode, 1, ORTE_GRPCOMM_MODE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_linear: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buf->bytes_used));
    
    /* if we are not a daemon or the HNP, then just send this to the HNP */
    if (!orte_process_info.hnp && !orte_process_info.daemon) {
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }
    
    /* if we are a daemon or the HNP, get the number of daemons out there */
    range = orte_process_info.num_procs;
    
    /* we have to account for all of the messages we are about to send
     * because the non-blocking send can come back almost immediately - before
     * we would get the chance to increment the num_active. This causes us
     * to not correctly wakeup and reset the xcast_in_progress flag
     */
    
    /* send the message to each daemon as fast as we can */
    dummy.jobid = ORTE_PROC_MY_HNP->jobid;
    for (i=0; i < range; i++) {            
        dummy.vpid = i;
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s xcast_linear: %s => %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&dummy)));

        /* if the target is the HNP and I am the HNP, then just setup to call the cmd processor */
        if (0 == i && orte_process_info.hnp) {
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
        } else {
            if (0 > (rc = orte_rml.send_buffer(&dummy, buf, ORTE_RML_TAG_DAEMON, 0))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
    }
    rc = ORTE_SUCCESS;
        
CLEANUP:
    /* release the buffer */
    OBJ_RELEASE(buf);
    return rc;    
}

static int xcast_direct(orte_jobid_t job,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag)
{
    int rc;
    orte_process_name_t peer;
    orte_vpid_t i;
    opal_buffer_t *buf=NULL, *bfr=buffer;
    orte_daemon_cmd_flag_t command;
    orte_grpcomm_mode_t mode;
    orte_rml_tag_t target=tag;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s xcast_direct",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am applicaton proc and this is going to some job other
     * than my own, then we have to send it via the daemons as the proc would have
     * no way of knowing how many procs are in the other job.
     */
    if (ORTE_PROC_MY_NAME->jobid != job &&
        !orte_process_info.hnp && !orte_process_info.daemon) {
        /* since we have to pack some additional info into the buffer
         * for this case, we create a new buffer into to contain all the
         * info needed plus the payload
         */
        buf = OBJ_NEW(opal_buffer_t);
        /* I have to send this to the HNP for handling as I have no idea
         * how many recipients there are! start by telling the HNP to relay
         */
        command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* default to the LINEAR mode since this is equivalent to
         * DIRECT for daemons
         */
        mode = ORTE_GRPCOMM_LINEAR;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &mode, 1, ORTE_GRPCOMM_MODE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* if the target isn't the daemon tag, then we have to add the proper
         * command so the daemon's know what to do
         */
        if (ORTE_RML_TAG_DAEMON != tag) {
            command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
        /* copy the payload into the new buffer - this is non-destructive, so our
         * caller is still responsible for releasing any memory in the buffer they
         * gave to us
         */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buf, buffer))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }
    
    /* if I am a daemon or the HNP and this is going to the daemon job to
     * someplace other than the daemon cmd processor, then I need to add
     * a command to the buffer so the recipient daemons know what to do
     */
    if ((orte_process_info.hnp || orte_process_info.daemon) &&
        ORTE_PROC_MY_NAME->jobid == job &&
        ORTE_RML_TAG_DAEMON != tag) {
        /* since we have to pack some additional info into the buffer
         * for this case, we create a new buffer into to contain all the
         * info needed plus the payload
         */
        buf = OBJ_NEW(opal_buffer_t);
        /* if the target isn't the daemon tag, then we have to add the proper
         * command so the daemon's know what to do
         */
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* copy the payload into the new buffer - this is non-destructive, so our
         * caller is still responsible for releasing any memory in the buffer they
         * gave to us
         */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buf, buffer))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* point to correct buffer to be sent */
        bfr = buf;
        /* send this to the daemon tag so it gets processed correctly */
        target = ORTE_RML_TAG_DAEMON;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_direct: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buffer->bytes_used));
    
   peer.jobid = job;
    for(i=0; i<orte_process_info.num_procs; i++) {
        peer.vpid = i;
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s xcast_direct: %s => %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));

        /* if I am the HNP, just set things up so the cmd processor gets called.
         * We don't want to message ourselves as this can create circular logic
         * in the RML. Instead, this macro will set a zero-time event which will
         * cause the buffer to be processed by the cmd processor - probably will
         * fire right away, but that's okay
         * The macro makes a copy of the buffer, so it's okay to release it later
         */
        if (peer.jobid == ORTE_PROC_MY_NAME->jobid &&
            peer.vpid == ORTE_PROC_MY_NAME->vpid &&
            orte_process_info.hnp) {
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, bfr, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
        } else {
            if (0 > (rc = orte_rml.send_buffer(&peer, bfr, target, 0))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            rc = ORTE_SUCCESS;
        }
    }
    rc = ORTE_SUCCESS;
    
CLEANUP:
    /* release buf if used */
    if (NULL != buf) {
        OBJ_RELEASE(buf);
    }
    return rc;
}

static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    orte_process_name_t name;
    int rc;
    orte_vpid_t i;
    opal_buffer_t tmpbuf;
    
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
    if (ORTE_SUCCESS != (rc = opal_dss.pack(rbuf, &orte_process_info.num_procs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* put my own information into the outgoing buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, sbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather collecting buffers",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* rank=0 receives everyone else's data */
    for (i=1; i < orte_process_info.num_procs; i++) {
        name.vpid = i;
        OBJ_CONSTRUCT(&tmpbuf, opal_buffer_t);
        if (0 > orte_rml.recv_buffer(&name, &tmpbuf, ORTE_RML_TAG_ALLGATHER, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s allgather buffer %ld received",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)i));
        /* append this data to the rbuf */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, &tmpbuf))) {
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
    xcast(ORTE_PROC_MY_NAME->jobid, rbuf, ORTE_RML_TAG_ALLGATHER);
    
    /* xcast automatically ensures that the sender -always- gets a copy
     * of the message. This is required to ensure proper operation of the
     * launch system as the HNP -must- get a copy itself. So we have to
     * post our own receive here so that we don't leave a message rattling
     * around in our RML
     */
    OBJ_CONSTRUCT(&tmpbuf, opal_buffer_t);
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

static int allgather_list(opal_list_t *names, opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    opal_list_item_t *item;
    orte_namelist_t *peer, *root;
    orte_std_cntr_t i, num_peers;
    opal_buffer_t tmpbuf;
    int rc;
    
    /* the first entry on the list is the "root" that collects
     * all the data - everyone else just sends and gets back
     * the results
     */
    root = (orte_namelist_t*)opal_list_get_first(names);
    
    if (OPAL_EQUAL != opal_dss.compare(&root->name, ORTE_PROC_MY_NAME, ORTE_NAME)) {
        /* everyone but root sends data */
        if (0 > (rc = orte_rml.send_buffer(&root->name, sbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* now receive the final result */
        if (0 > (rc = orte_rml.recv_buffer(&root->name, rbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
    }
    
    /* count how many peers are participating, including myself */
    num_peers = (orte_std_cntr_t)opal_list_get_size(names);

    /* seed the outgoing buffer with the num_procs so it can be unpacked */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(rbuf, &num_peers, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* put my own information into the outgoing buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, sbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* root receives everyone else's data */
    for (i=1; i < num_peers; i++) {
        /* receive the buffer from this process */
        OBJ_CONSTRUCT(&tmpbuf, opal_buffer_t);
        if (0 > orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &tmpbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
        /* append this data to the rbuf */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, &tmpbuf))) {
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
        if (OPAL_EQUAL == opal_dss.compare(&root->name, &peer->name, ORTE_NAME)) {
            continue;
        }
        
        /* transmit the buffer to this process */
        if (0 > orte_rml.send_buffer(&peer->name, rbuf, ORTE_RML_TAG_ALLGATHER_LIST, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
    }
    
    return ORTE_SUCCESS;
}


static int barrier(void)
{
    orte_process_name_t name;
    orte_vpid_t i;
    opal_buffer_t buf;
    int rc;
    
    /* everything happens within the same jobid */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* All non-root send & receive zero-length message. */
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        name.vpid = 0;
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        i=0;
        opal_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s sending barrier",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_BARRIER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* get the release from rank=0 */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD,&buf,ORTE_RML_TAG_BARRIER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s received barrier release",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    
    for (i = 1; i < orte_process_info.num_procs; i++) {
        name.vpid = (orte_vpid_t)i;
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
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
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
    xcast(ORTE_PROC_MY_NAME->jobid, &buf, ORTE_RML_TAG_BARRIER);
    OBJ_DESTRUCT(&buf);

    /* xcast automatically ensures that the sender -always- gets a copy
     * of the message. This is required to ensure proper operation of the
     * launch system as the HNP -must- get a copy itself. So we have to
     * post our own receive here so that we don't leave a message rattling
     * around in our RML
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
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

static int chain_recips(opal_list_t *names)
{
    orte_namelist_t *target;
    
    /* chain just sends to the next vpid up the line */
    if (ORTE_PROC_MY_NAME->vpid < orte_process_info.num_procs-1) {
        /* I am not at the end of the chain */
        if (NULL == (target = OBJ_NEW(orte_namelist_t))) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        target->name.jobid = ORTE_PROC_MY_NAME->jobid;
        target->name.vpid = ORTE_PROC_MY_NAME->vpid + 1;
        opal_list_append(names, &target->item);
    }
    return ORTE_SUCCESS;
}

static int binomial_recips(opal_list_t *names)
{
    int i, bitmap, peer, size, rank, hibit, mask;
    orte_namelist_t *target;
    
    /* compute the bitmap */
    bitmap = opal_cube_dim((int)orte_process_info.num_procs);
    rank = (int)ORTE_PROC_MY_NAME->vpid;
    size = (int)orte_process_info.num_procs;
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < size) {
            if (NULL == (target = OBJ_NEW(orte_namelist_t))) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            target->name.jobid = ORTE_PROC_MY_NAME->jobid;
            target->name.vpid = (orte_vpid_t)peer;
            opal_list_append(names, &target->item);
        }
    }
    return ORTE_SUCCESS;
}

static int linear_recips(opal_list_t *names)
{
    orte_namelist_t *target;
    orte_vpid_t i;
    
    /* if we are not the HNP, we just return - only
     * the HNP sends in this mode
     */
    if (!orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    /* if we are the HNP, then just add the names of
     * all daemons to the list
     */
    for (i=1; i < orte_process_info.num_procs; i++) {
        if (NULL == (target = OBJ_NEW(orte_namelist_t))) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        target->name.jobid = ORTE_PROC_MY_NAME->jobid;
        target->name.vpid = i;
        opal_list_append(names, &target->item);
    }
    return ORTE_SUCCESS;
}

static int next_recips(opal_list_t *names, orte_grpcomm_mode_t mode)
{
    int rc;
    
    /* check the mode to select the proper algo */
    switch (mode) {
        case ORTE_GRPCOMM_CHAIN:
            rc = chain_recips(names);
            break;
        case ORTE_GRPCOMM_BINOMIAL:
            rc = binomial_recips(names);
            break;
        case ORTE_GRPCOMM_LINEAR:
            rc = linear_recips(names);
            break;
        default:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            break;
    }
    return rc;
}


/***************  MODEX SECTION **************/

/**
 * MODEX DESIGN
 *
 * Modex data is always associated with a given orte process name, in
 * an opal hash table. The hash table is necessary because modex data is
 * received for entire jobids and when working with
 * dynamic processes, it is possible we will receive data for a
 * process not yet in the ompi_proc_all() list of processes. This
 * information must be kept for later use, because if accept/connect
 * causes the proc to be added to the ompi_proc_all() list, it could
 * cause a connection storm.  Therefore, we use an
 * orte_proc_table backing store to contain all modex information.
 *
 * While we could add the now discovered proc into the ompi_proc_all()
 * list, this has some problems, in that we don't have the
 * architecture and hostname information needed to properly fill in
 * the ompi_proc_t structure and we don't want to cause RML
 * communication to get it when we don't really need to know anything
 * about the remote proc.
 *
 * All data put into the modex (or received from the modex) is
 * associated with a given proc,attr_name pair.  The data structures
 * to maintain this data look something like:
 *
 * opal_hash_table_t modex_data -> list of attr_proc_t objects
 * 
 * +-----------------------------+
 * | modex_proc_data_t           |
 * |  - opal_list_item_t         |
 * +-----------------------------+
 * | opal_mutex_t modex_lock     |
 * | bool modex_received_data    |     1
 * | opal_list_t modules         |     ---------+
 * +-----------------------------+              |
 *                                      *       |
 * +--------------------------------+  <--------+
 * | modex_module_data_t            |
 * |  - opal_list_item_t            |
 * +--------------------------------+
 * | mca_base_component_t component |
 * | void *module_data              |
 * | size_t module_data_size        |
 * +--------------------------------+
 *
 */


/**
 * Modex data for a particular orte process
 *
 * Locking infrastructure and list of module data for a given orte
 * process name.  The name association is maintained in the
 * modex_data hash table.
 */
struct modex_proc_data_t {
    /** Structure can be put on lists (including in hash tables) */
    opal_list_item_t super;
    /* Lock held whenever the modex data for this proc is being
       modified */
    opal_mutex_t modex_lock;
    /* True if modex data has ever been received from this process,
       false otherwise. */
    bool modex_received_data;
    /* List of modex_module_data_t structures containing all data
       received from this process, sorted by component name. */
    opal_list_t modex_module_data;
};
typedef struct modex_proc_data_t modex_proc_data_t;

static void
modex_construct(modex_proc_data_t * modex)
{
    OBJ_CONSTRUCT(&modex->modex_lock, opal_mutex_t);
    modex->modex_received_data = false;
    OBJ_CONSTRUCT(&modex->modex_module_data, opal_list_t);
}

static void
modex_destruct(modex_proc_data_t * modex)
{
    OBJ_DESTRUCT(&modex->modex_module_data);
    OBJ_DESTRUCT(&modex->modex_lock);
}

OBJ_CLASS_INSTANCE(modex_proc_data_t, opal_object_t,
                   modex_construct, modex_destruct);



/**
 * Data for a particular attribute
 *
 * Container for data for a particular module,attribute pair.  This
 * structure should be contained in the modex_module_data list in an
 * modex_proc_data_t structure to maintain an association with a
 * given proc.  The list is then searched for a matching attribute
 * name.
 *
 * While searching the list or reading from (or writing to) this
 * structure, the lock in the proc_data_t should be held.
 */
struct modex_attr_data_t {
    /** Structure can be put on lists */
    opal_list_item_t super;
    /** Attribute name */
    char * attr_name;
    /** Binary blob of data associated with this proc,component pair */
    void *attr_data;
    /** Size (in bytes) of module_data */
    size_t attr_data_size;
};
typedef struct modex_attr_data_t modex_attr_data_t;

static void
modex_attr_construct(modex_attr_data_t * module)
{
    module->attr_name = NULL;
    module->attr_data = NULL;
    module->attr_data_size = 0;
}

static void
modex_attr_destruct(modex_attr_data_t * module)
{
    if (NULL != module->attr_name) {
        free(module->attr_name);
    }
    if (NULL != module->attr_data) {
        free(module->attr_data);
    }
}

OBJ_CLASS_INSTANCE(modex_attr_data_t,
                   opal_list_item_t,
                   modex_attr_construct,
                   modex_attr_destruct);


/**
 * Find data for a given attribute in a given modex_proc_data_t
 * container.
 *
 * The proc_data's modex_lock must be held during this
 * search.
 */
static modex_attr_data_t *
modex_lookup_attr_data(modex_proc_data_t *proc_data,
                       const char *attr_name,
                       bool create_if_not_found)
{
    modex_attr_data_t *attr_data = NULL;
    for (attr_data = (modex_attr_data_t *) opal_list_get_first(&proc_data->modex_module_data);
         attr_data != (modex_attr_data_t *) opal_list_get_end(&proc_data->modex_module_data);
         attr_data = (modex_attr_data_t *) opal_list_get_next(attr_data)) {
        if (0 == strcmp(attr_name, attr_data->attr_name)) {
            return attr_data;
        }
    }
    
    if (create_if_not_found) {
        attr_data = OBJ_NEW(modex_attr_data_t);
        if (NULL == attr_data) return NULL;
        
        attr_data->attr_name = strdup(attr_name);
        opal_list_append(&proc_data->modex_module_data, &attr_data->super);
        
        return attr_data;
    }
    
    return NULL;
}


/**
* Find modex_proc_data_t container associated with given
 * orte_process_name_t.
 *
 * The global lock should *NOT* be held when
 * calling this function.
 */
static modex_proc_data_t*
modex_lookup_orte_proc(const orte_process_name_t *orte_proc)
{
    modex_proc_data_t *proc_data;
    
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    proc_data = (modex_proc_data_t*)
        orte_hash_table_get_proc(&orte_grpcomm_basic.modex_data, orte_proc);
    if (NULL == proc_data) {
        /* The proc clearly exists, so create a modex structure
        for it */
        proc_data = OBJ_NEW(modex_proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "grpcomm_basic_modex_lookup_orte_proc: unable to allocate modex_proc_data_t\n");
            OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
            return NULL;
        }
        orte_hash_table_set_proc(&orte_grpcomm_basic.modex_data, orte_proc, proc_data);
    }
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    
    return proc_data;
}


static int set_proc_attr(const char *attr_name,
                         const void *data,
                         size_t size)
{
    int rc;
    
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    
    /* Pack the attribute name information into the local buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&orte_grpcomm_basic.modex_buffer, &attr_name, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* pack the size */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&orte_grpcomm_basic.modex_buffer, &size, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* Pack the actual data into the buffer */
    if (0 != size) {
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&orte_grpcomm_basic.modex_buffer, (void *) data, size, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    /* track the number of entries */
    ++orte_grpcomm_basic.modex_num_entries;
    
cleanup:
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    
    return rc;
}

static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size)
{
    modex_proc_data_t *proc_data;
    modex_attr_data_t *attr_data;
    
    proc_data = modex_lookup_orte_proc(&proc);
    if (NULL == proc_data) return ORTE_ERR_NOT_FOUND;
    
    OPAL_THREAD_LOCK(&proc_data->modex_lock);
    
    /* look up attribute */
    attr_data = modex_lookup_attr_data(proc_data, attribute_name, false);
    
    /* copy the data out to the user */
    if ((NULL == attr_data) ||
        (attr_data->attr_data_size == 0)) {
        opal_output(0, "grpcomm_basic_get_proc_attr: no attr avail or zero byte size");
        *val = NULL;
        *size = 0;
    } else {
        void *copy = malloc(attr_data->attr_data_size);
        
        if (copy == NULL) {
            OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        memcpy(copy, attr_data->attr_data, attr_data->attr_data_size);
        *val = copy;
        *size = attr_data->attr_data_size;
    }
    OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
    
    return ORTE_SUCCESS;
} 


static int modex(opal_list_t *procs)
{
    opal_buffer_t buf, rbuf;
    orte_std_cntr_t i, j, num_procs, num_entries;
    void *bytes = NULL;
    orte_std_cntr_t cnt;
    orte_process_name_t proc_name;
    modex_proc_data_t *proc_data;
    modex_attr_data_t *attr_data;
    int rc;
    
    /* setup the buffer that will actually be sent */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    OBJ_CONSTRUCT(&rbuf, opal_buffer_t);
    
    /* put our process name in the buffer so it can be unpacked later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* put the number of entries into the buffer */
    OPAL_THREAD_LOCK(&orte_grpcomm_basic.mutex);
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &orte_grpcomm_basic.modex_num_entries, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
        goto cleanup;
    }
    
    /* if there are entries, non-destructively copy the data across */
    if (0 < orte_grpcomm_basic.modex_num_entries) {
        if (ORTE_SUCCESS != (opal_dss.copy_payload(&buf, &orte_grpcomm_basic.modex_buffer))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
            goto cleanup;
        }
    }
    OPAL_THREAD_UNLOCK(&orte_grpcomm_basic.mutex);
    
    /* exchange the buffer with the list of peers (if provided) or all my peers */
    if (NULL == procs) {
        if (ORTE_SUCCESS != (rc = allgather(&buf, &rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        if (ORTE_SUCCESS != (rc = allgather_list(procs, &buf, &rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    /* process the results */
    /* extract the number of procs that put data in the buffer */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_procs, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* process the buffer */
    for (i=0; i < num_procs; i++) {
        /* unpack the process name */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &proc_name, &cnt, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* look up the modex data structure */
        proc_data = modex_lookup_orte_proc(&proc_name);
        if (proc_data == NULL) {
            /* report the error */
            opal_output(0, "grpcomm_basic_modex: received modex info for unknown proc %s\n",
                        ORTE_NAME_PRINT(&proc_name));
            rc = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
        
        /* unpack the number of entries for this proc */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_entries, &cnt, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_THREAD_LOCK(&proc_data->modex_lock);
        
        /*
         * Extract the attribute names and values
         */
        for (j = 0; j < num_entries; j++) {
            size_t num_bytes;
            char *attr_name;
            
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &attr_name, &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                goto cleanup;
            }

            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_bytes, &cnt, OPAL_SIZE))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                goto cleanup;
            }
            if (num_bytes != 0) {
                if (NULL == (bytes = malloc(num_bytes))) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                    OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                    goto cleanup;
                }
                cnt = (orte_std_cntr_t) num_bytes;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, bytes, &cnt, OPAL_BYTE))) {
                    ORTE_ERROR_LOG(rc);
                    OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                    goto cleanup;
                }
                num_bytes = cnt;
            } else {
                bytes = NULL;
            }
            
            /*
             * Lookup the corresponding modex structure
             */
            if (NULL == (attr_data = modex_lookup_attr_data(proc_data, 
                                                            attr_name, true))) {
                opal_output(0, "grpcomm_basic_modex: modex_lookup_attr_data failed\n");
                OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                rc = ORTE_ERR_NOT_FOUND;
                goto cleanup;
            }
            if (NULL != attr_data->attr_data) {
                /* some pre-existing value must be here - release it */
                free(attr_data->attr_data);
            }
            attr_data->attr_data = bytes;
            attr_data->attr_data_size = num_bytes;
            proc_data->modex_received_data = true;            
        }
        OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    OBJ_DESTRUCT(&rbuf);
    
    return rc;
}

static int purge_proc_attrs(void)
{
    /*
     * Purge the attributes
     */
    opal_hash_table_remove_all(&orte_grpcomm_basic.modex_data);
    OBJ_DESTRUCT(&orte_grpcomm_basic.modex_data);
    OBJ_CONSTRUCT(&orte_grpcomm_basic.modex_data, opal_hash_table_t);    
    opal_hash_table_init(&orte_grpcomm_basic.modex_data, 256);

    /*
     * Clear the modex buffer
     */
    OBJ_DESTRUCT(&orte_grpcomm_basic.modex_buffer);
    OBJ_CONSTRUCT(&orte_grpcomm_basic.modex_buffer, opal_buffer_t);
    orte_grpcomm_basic.modex_num_entries = 0;

    return ORTE_SUCCESS;
}

orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    xcast,
    allgather,
    allgather_list,
    barrier,
    next_recips,
    set_proc_attr,
    get_proc_attr,
    modex,
    purge_proc_attrs
};

