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
static int find_parent(int rank, int parent, int me, int num_procs,
                       int *num_children, opal_list_t *children);


/* Local global variables */
static orte_process_name_t my_parent;
static opal_list_t *my_children;
static int my_num_children;

/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int barrier(void);
static opal_list_t* next_recips(orte_grpcomm_mode_t mode);
static int daemon_collective(orte_jobid_t jobid,
                             orte_std_cntr_t num_local_contributors,
                             orte_grpcomm_coll_t type,
                             opal_buffer_t *data,
                             orte_rmaps_dp_t flag,
                             opal_value_array_t *participants);
static int update_trees(void);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    daemon_collective,
    update_trees,
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
    
    /* setup the local global variables */
    if (orte_process_info.hnp || orte_process_info.daemon) {
        update_trees();
    }
    
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
    opal_list_item_t *item;
    
    if (orte_process_info.hnp || orte_process_info.daemon) {
        /* deconstruct the child list */
        while (NULL != (item = opal_list_remove_first(my_children))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(my_children);
        my_num_children = 0;
    }
    
    orte_grpcomm_base_modex_finalize();
}

static int update_trees(void)
{
    opal_list_item_t *item;
    
    if (NULL != my_children) {
        while (NULL != (item = opal_list_remove_first(my_children))) {
            OBJ_RELEASE(item);
        }
    } else {
        my_children = OBJ_NEW(opal_list_t);
    }
    my_num_children = 0;
    my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
    my_parent.vpid = find_parent(0, 0, ORTE_PROC_MY_NAME->vpid,
                                 orte_process_info.num_procs,
                                 &my_num_children, my_children);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic update trees found %d children num_procs %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         my_num_children, orte_process_info.num_procs));
    
    return ORTE_SUCCESS;
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
    
    if (orte_abnormal_term_ordered) {
        /* We insist that the direct xcast mode be used when
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
        /* otherwise, send it to the HNP for relay */
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
     *
     * Likewise, a daemon who is not the HNP will also let the HNP
     * act as the relay to avoid opening unnecessary connections
     * and rattling messages around the system if daemons are not
     * fully connected
     */
    if (!orte_process_info.hnp) {
        /* tell the HNP to relay */
        command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* tell the HNP the routing algorithm this xmission is using */
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
        /* otherwise, send it to the HNP for relay */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
    }
        
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

static opal_list_t* next_recips(orte_grpcomm_mode_t mode)
{
    /* check the mode to select the proper algo */
    switch (mode) {
        case ORTE_GRPCOMM_CHAIN:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            return NULL;
            break;
        case ORTE_GRPCOMM_BINOMIAL:
            return my_children;
            break;
        case ORTE_GRPCOMM_LINEAR:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            return NULL;
            break;
        default:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
            break;
    }
}


static bool barrier_recvd;
static bool barrier_timer;

static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    /* flag as recvd */
    barrier_recvd = true;
}

static void barrier_timer_recv(int status, orte_process_name_t* sender,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tag, void *cbdata)
{
    barrier_timer = true;
}

static int find_parent(int rank, int parent, int me, int num_procs,
                       int *num_children, opal_list_t *children)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_namelist_t *child;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                if (NULL != children) {
                    child = OBJ_NEW(orte_namelist_t);
                    child->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    child->name.vpid = peer;
                    OPAL_OUTPUT_VERBOSE((3, orte_grpcomm_base_output,
                                         "%s grpcomm:basic find-parent found child %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&child->name)));
                    
                    opal_list_append(children, &child->item);
                }
                (*num_children)++;
            }
        }
        OPAL_OUTPUT_VERBOSE((3, orte_grpcomm_base_output,
                             "%s grpcomm:basic find-parent found parent %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             parent));
        return parent;
    }
    
    /* find the children of this rank */
    bitmap = opal_cube_dim(num_procs);
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < num_procs) {
            /* execute compute on this child */
            if (0 <= (found = find_parent(peer, rank, me, num_procs, num_children, children))) {
                return found;
            }
        }
    }
    return -1;
}


static int barrier(void)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_COLL_CMD;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_BARRIER;
    int rc;
    struct timeval ompistart, ompistop;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing && ORTE_PROC_MY_NAME->vpid == 0) {
        gettimeofday(&ompistart, NULL);
    }
    
    /* everyone sends barrier to local daemon */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* tell the daemon to collect the data */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* tell the daemon we are doing a barrier */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }    
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic barrier sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* now receive the release. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    barrier_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(barrier_recvd, 0, 1);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing) {
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup a receive to hear when the rank=N proc has received the data
             * release - in most xcast schemes, this will always be the final recvr
             */
            barrier_timer = false;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLLECTIVE_TIMER,
                                    ORTE_RML_NON_PERSISTENT, barrier_timer_recv, NULL);
            if (rc != ORTE_SUCCESS) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ORTE_PROGRESSED_WAIT(barrier_timer, 0, 1);
            gettimeofday(&ompistop, NULL);
            opal_output(0, "%s time to complete barrier %ld usec",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                                   (ompistop.tv_usec - ompistart.tv_usec)));
        } else if (ORTE_PROC_MY_NAME->vpid == orte_process_info.num_procs-1) {
            /* if we are rank=N, send a message back to indicate
             * the xcast completed for timing purposes
             */
            orte_process_name_t name;
            
            name.jobid = ORTE_PROC_MY_NAME->jobid;
            name.vpid = 0;
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (0 > (rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_COLLECTIVE_TIMER,0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            rc = ORTE_SUCCESS;
            OBJ_DESTRUCT(&buf);
        }
    }
    
    return ORTE_SUCCESS;
}

static opal_buffer_t *allgather_buf;
static orte_std_cntr_t allgather_complete;

static void allgather_recv(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(allgather_buf, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    allgather_complete = true;
}

static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_COLL_CMD;
    struct timeval ompistart, ompistop;
    opal_buffer_t coll;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_ALLGATHER;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing && ORTE_PROC_MY_NAME->vpid == 0) {
        gettimeofday(&ompistart, NULL);
    }
    
    /* everyone sends data to their local daemon */
    OBJ_CONSTRUCT(&coll, opal_buffer_t);
    /* tell the daemon to collect the data */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&coll, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    /* tell the daemon we are doing an allgather */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&coll, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }    
    /* add our data to it */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&coll, sbuf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &coll, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    OBJ_DESTRUCT(&coll);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather buffer sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the buffer that will recv the results */
    allgather_buf = OBJ_NEW(opal_buffer_t);
    
    /* now receive the final result. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    allgather_complete = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                 ORTE_RML_NON_PERSISTENT, allgather_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(allgather_complete, 0, 1);
    
    /* copy payload to the caller's buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, allgather_buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(allgather_buf);
        return rc;
    }
    OBJ_RELEASE(allgather_buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather buffer received",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing) {
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup a receive to hear when the rank=N proc has received the data
             * release - in most xcast schemes, this will always be the final recvr
             */
            barrier_timer = false;
            rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLLECTIVE_TIMER,
                                    ORTE_RML_NON_PERSISTENT, barrier_timer_recv, NULL);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ORTE_PROGRESSED_WAIT(barrier_timer, 0, 1);
            gettimeofday(&ompistop, NULL);
            opal_output(0, "%s allgather: time to complete %ld usec",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                                   (ompistop.tv_usec - ompistart.tv_usec)));
        } else if (ORTE_PROC_MY_NAME->vpid == orte_process_info.num_procs-1) {
            /* if we are rank=N, send a message back to indicate
             * the xcast completed for timing purposes
             */
            orte_process_name_t name;
            opal_buffer_t buf;
            
            name.jobid = ORTE_PROC_MY_NAME->jobid;
            name.vpid = 0;
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (0 > (rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_COLLECTIVE_TIMER,0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            rc = ORTE_SUCCESS;
            OBJ_DESTRUCT(&buf);
        }
    }
    
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

static orte_std_cntr_t collective_num_recvd;
static bool collective_failed;
static opal_buffer_t *collection;
static orte_std_cntr_t num_contributors;

static void collective_recv(int status, orte_process_name_t* sender,
                                opal_buffer_t *buffer,
                                orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    orte_std_cntr_t contributors, cnt;
    
    /* extract the #contributors */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &contributors, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
    }
    num_contributors += contributors;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(collection, buffer))) {
        ORTE_ERROR_LOG(rc);
        collective_failed = true;
    }
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic collective recv - got %d bytes from %s with %d contributors",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), 
                         (int)(buffer->bytes_used-sizeof(orte_std_cntr_t)),
                         ORTE_NAME_PRINT(sender), (int)contributors));
    
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                 ORTE_RML_NON_PERSISTENT, collective_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        collective_failed = true;
    }
    /* bump counter */
    ++collective_num_recvd;
}


static int daemon_leader(orte_jobid_t jobid,
                         orte_std_cntr_t num_local_contributors,
                         orte_grpcomm_coll_t type,
                         opal_buffer_t *data,
                         orte_rmaps_dp_t flag,
                         opal_value_array_t *participants)
{
    int rc;
    opal_buffer_t buf;
    int num_children;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic daemon_collective - I am the leader!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_RMAPS_ALL_DAEMONS == flag) {
        /* if everyone is participating, then I must be the HNP,
         * so the #children is just the #children determined for
         * my outgoing xcast
         */
        num_children = my_num_children;
    } else if (ORTE_RMAPS_ALL_EXCEPT_HNP == flag) {
        /* if the HNP has no local procs, then it won't
         * know that a collective is underway, so that means
         * I must be rank=1. The number of messages I must get
         * therefore consists of both my children + all other
         * children of rank=0 as they will redirect their messages
         * to me
         */
        num_children = 0;
        /* find #children for rank=0 */
        find_parent(0, 0, 0, orte_process_info.num_procs, &num_children, NULL);
        /* I am one of those children, so we should get num_children-1 of
         * my peers sending to me, plus my own children
         */
        num_children = num_children - 1 + my_num_children;
    } else if (ORTE_RMAPS_DAEMON_SUBSET == flag) {
        /* for this first cut, all members will send to me direct,
         * so the #children I should hear from is just the
         * size of the value array - 1
         */
        num_children = opal_value_array_get_size(participants) - 1;
    } else {
        /* no idea */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* setup to recv the messages from my children */
    collective_num_recvd = 0;
    collective_failed = false;
    collection = OBJ_NEW(opal_buffer_t);
    num_contributors = num_local_contributors;  /* seed with the number I added */
    
    /* ensure my data gets included in the outcome */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(collection, data))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(collection);
        return rc;
    }
    
    /* if we have children, get their messages */
    if (0 < num_children) {
        /* post the non-blocking recv */
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                     ORTE_RML_NON_PERSISTENT, collective_recv, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(collection);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(collective_failed, collective_num_recvd, num_children);
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic daemon_collective - leader has received collective from %d children",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_children));
        
        /* cancel the lingering recv */
        if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE)) &&
            ORTE_ERR_NOT_FOUND != rc) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(collection);
            return rc;
        }
    }
    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    if (ORTE_GRPCOMM_BARRIER == type) {
        if (ORTE_SUCCESS != (rc = xcast(jobid, &buf, ORTE_RML_TAG_BARRIER))) {
            ORTE_ERROR_LOG(rc);
        }
    } else if (ORTE_GRPCOMM_ALLGATHER == type) {
        /* send the data */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_contributors, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, collection))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = xcast(jobid, &buf, ORTE_RML_TAG_ALLGATHER))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        /* no other collectives currently supported! */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        rc = ORTE_ERR_NOT_IMPLEMENTED;
    }
    
cleanup:
    OBJ_RELEASE(collection);
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic daemon_collective - leader has completed collective",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}


static int daemon_collective(orte_jobid_t jobid,
                             orte_std_cntr_t num_local_contributors,
                             orte_grpcomm_coll_t type,
                             opal_buffer_t *data,
                             orte_rmaps_dp_t flag,
                             opal_value_array_t *participants)
{
    orte_process_name_t lead, parent;
    orte_vpid_t *vptr;
    int num_children;
    opal_buffer_t buf;
    int rc;
        
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic daemon_collective entered with dp flag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)flag));
    
    parent.jobid = ORTE_PROC_MY_NAME->jobid;
    lead.jobid = ORTE_PROC_MY_NAME->jobid;

    /* if the participation is full, then the HNP is the lead */
    if (ORTE_RMAPS_ALL_DAEMONS == flag) {
        lead.vpid = ORTE_PROC_MY_HNP->vpid;
    } else if (ORTE_RMAPS_ALL_EXCEPT_HNP == flag) {
        /* if the HNP has no local procs, then it won't
         * know that a collective is underway, so let
         * rank=1 be the lead
         */
        lead.vpid = 1;
    } else if (ORTE_RMAPS_DAEMON_SUBSET == flag) {
        /* let the first proc in the array be the lead */
        vptr = (orte_vpid_t*)opal_value_array_get_item(participants, 0);
        lead.vpid = *vptr;
    } else {
        /* no idea */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if I am the lead, do my own thing */
    if (ORTE_PROC_MY_NAME->vpid == lead.vpid) {
        return daemon_leader(jobid, num_local_contributors, type, data, flag, participants);
    }
    
    
    /* I am NOT the lead, so I first must figure out how many children
     * I need to collect messages from and who my parent will be
     */
    
    if (ORTE_RMAPS_ALL_DAEMONS == flag) {
        /* everyone is participating, so my parent and
         * num_children can be as initially computed
         */
        parent.vpid = my_parent.vpid;
        num_children = my_num_children;
    } else if (ORTE_RMAPS_ALL_EXCEPT_HNP == flag) {
        /* if the HNP has no local procs, then it won't
         * know that a collective is underway, so we need
         * to send to rank=1 if our parent would have been
         * rank=0. Our num_children, though,
         * remains unchanged
         */
        if (0 == my_parent.vpid) {
            parent.vpid = 1;
        } else {
            /* just send as normal */
            parent.vpid = my_parent.vpid;
        }
        num_children = my_num_children;
    } else if (ORTE_RMAPS_DAEMON_SUBSET == flag) {
        /* regardless of mode, we always send direct */
        num_children = 0;
        parent.vpid = lead.vpid;
    } else {
        /* no idea */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic daemon_collective preparing to receive from %d children",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_children));
    
    
    /* setup for collecting data */
    collection = OBJ_NEW(opal_buffer_t);
    num_contributors = num_local_contributors;  /* seed with the number I added */
    
    /* ensure my data gets included in the outcome */
    opal_dss.copy_payload(collection, data);

    /* if num_children > 0, setup recv's to wait until we hear from
     * them all - the recv will look just like that for the leader,
     * collecting data and #contributors
     */
    
    if (0 < num_children) {
        /* setup to recv the messages from my children */
        collective_num_recvd = 0;
        collective_failed = false;
        
        /* post the non-blocking recv */
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                     ORTE_RML_NON_PERSISTENT, collective_recv, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(collective_failed, collective_num_recvd, num_children);
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic daemon_collective - I have received collective from children",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* cancel the lingering recv */
        if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE)) &&
            ORTE_ERR_NOT_FOUND != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* construct and send message to our parent */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* insert #contributors */
    opal_dss.pack(&buf, &num_contributors, 1, ORTE_STD_CNTR);
    
    if (ORTE_GRPCOMM_BARRIER == type) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic daemon_collective sending barrier to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&parent)));
        
        if (0 > (rc = orte_rml.send_buffer(&parent,&buf,ORTE_RML_TAG_DAEMON_COLLECTIVE,0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc= ORTE_SUCCESS;
    } else if (ORTE_GRPCOMM_ALLGATHER == type) {
        /* xfer the data */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, collection))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* send the data */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic daemon_collective sending allgather data to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&parent)));
        
        if (0 > (rc = orte_rml.send_buffer(&parent,&buf,ORTE_RML_TAG_DAEMON_COLLECTIVE,0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = ORTE_SUCCESS;
    } else {
        /* we don't currently support any other collectives */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        rc = ORTE_ERR_NOT_IMPLEMENTED;
    }
    OBJ_DESTRUCT(&buf);
    OBJ_RELEASE(collection);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic daemon_collective completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

