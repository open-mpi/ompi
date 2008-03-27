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

#include "opal/class/opal_hash_table.h"
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

/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int next_recips(opal_list_t *names, orte_grpcomm_mode_t mode);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    orte_grpcomm_base_allgather,
    orte_grpcomm_base_allgather_list,
    orte_grpcomm_base_barrier,
    next_recips,
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    orte_grpcomm_base_modex,
    orte_grpcomm_base_purge_proc_attrs
};


/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    orte_grpcomm_base_modex_finalize();
}


/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast: num_procs %ld linear xover: %ld binomial xover: %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)orte_process_info.num_procs,
                         (long)orte_grpcomm_basic.xcast_linear_xover,
                         (long)orte_grpcomm_basic.xcast_binomial_xover));

    if (orte_process_info.num_procs < 2 || orte_abnormal_term_ordered) {
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
                         "%s grpcomm:entering xcast_binomial",
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
                         "%s grpcomm:xcast_binomial: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buf->bytes_used));
    
    /* all we need to do is send this to the HNP - the relay logic
     * will ensure everyone else gets it!
     */
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:xcast_binomial: sending %s => %s",
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

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast_binomial: completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
                         "%s grpcomm:entering xcast_linear",
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
                         "%s grpcomm:xcast_linear: buffer size %ld",
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
    
    /* send the message to each daemon as fast as we can */
    dummy.jobid = ORTE_PROC_MY_HNP->jobid;
    for (i=0; i < range; i++) {            
        dummy.vpid = i;
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:xcast_linear: %s => %s",
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
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast_linear: completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;    
}

static int relay_via_hnp(orte_jobid_t job,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag) {
    opal_buffer_t *buf;
    orte_daemon_cmd_flag_t command;
    orte_grpcomm_mode_t mode;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: relaying buffer to HNP",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* since we have to pack some additional info into the buffer
    * for this case, we create a new buffer into to contain all the
    * info needed plus the payload
    */
    buf = OBJ_NEW(opal_buffer_t);
    /* start by telling the HNP to relay */
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

CLEANUP:
    OBJ_RELEASE(buf);

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: buffer relayed to HNP",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static int xcast_direct(orte_jobid_t job,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag)
{
    int rc;
    orte_process_name_t peer;
    orte_vpid_t i, num_targets=0;
    opal_buffer_t *buf=NULL, *bfr=buffer;
    orte_daemon_cmd_flag_t command;
    orte_rml_tag_t target=tag;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: entering xcast_direct",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am applicaton proc */
    if (!orte_process_info.hnp &&
        !orte_process_info.daemon &&
        !orte_process_info.tool) {
        /* if this is going to some job other
         * than my own, then we have to send it via the HNP as I have
         * no way of knowing how many procs are in the other job.
         */
        if (ORTE_PROC_MY_NAME->jobid != job) {
            if (ORTE_SUCCESS != (rc = relay_via_hnp(job, buffer, tag))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
        /* if it is my jobid, then we can just send this ourselves -
         * set the target tag
         */
        target = tag;
        /* set number of procs to the #procs in our job */
        num_targets = orte_process_info.num_procs;
        /* point to the right buffer */
        bfr = buffer;
        /* go to send it */
        goto SEND;
    }
    
    /* if I am a daemon */
    if (orte_process_info.daemon) {
        /* if this is going to another job, then I have to relay
         * it through the HNP as I have no idea how many procs
         * are in that job
         */
        if (ORTE_PROC_MY_NAME->jobid != job) {
            if (ORTE_SUCCESS != (rc = relay_via_hnp(job, buffer, tag))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
        /* if this is going to the daemon job to
         * someplace other than the daemon cmd processor, then I need to add
         * a command to the buffer so the recipient daemons know what to do
         */
        if (ORTE_RML_TAG_DAEMON != tag) {
            /* setup a buffer to handle the additional info */
            buf = OBJ_NEW(opal_buffer_t);
            /* add the proper command so the daemon's know what to do */
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
            /* set the number of targets to be the number of daemons */
            num_targets = orte_process_info.num_procs;
            /* send it */
            goto SEND;
        }
    }

    /* if I am the HNP */
    if (orte_process_info.hnp) {
        orte_job_t *jdata;
        
        /* if this is going to the daemon job */
        if (ORTE_PROC_MY_NAME->jobid == job) {
            /* if this is going someplace other than the daemon cmd
             * processor, then I need to add a command to the buffer
             * so the recipient daemons know what to do
             */
            if (ORTE_RML_TAG_DAEMON != tag) {
                /* since we have to pack some additional info into the buffer
                 * for this case, we create a new buffer to contain all the
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
                /* set the number of targets to be the number of daemons */
                num_targets = orte_process_info.num_procs;
                /* send it */
                goto SEND;
            } else {
                /* if already going to the daemon tag, then just point to
                 * the right places and send it
                 */
                bfr = buffer;
                target = tag;
                num_targets = orte_process_info.num_procs;
                goto SEND;
            }
        }
        /* if this is going to any other job,
         * then I need to know the number of procs in that job so I can
         * send it
         */
        if (NULL == (jdata = orte_get_job_data_object(job))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        /* set the number of targets */
        num_targets = jdata->num_procs;
        /* set the tag */
        target = tag;
        /* point to correct buffer to be sent */
        bfr = buffer;
        /* send it */
        goto SEND;
    }
    

SEND:
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s xcast_direct: buffer size %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buffer->bytes_used));

   peer.jobid = job;
    for(i=0; i<num_targets; i++) {
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

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: xcast_direct completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
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


