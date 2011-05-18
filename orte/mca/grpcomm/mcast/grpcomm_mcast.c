/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
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
#include <fcntl.h>

#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rmcast/rmcast.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rmcast/rmcast.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_mcast.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int mcast_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int mcast_barrier(void);
static int modex(opal_list_t *procs);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_mcast_module = {
    init,
    finalize,
    xcast,
    mcast_allgather,
    orte_grpcomm_base_allgather_list,
    mcast_barrier,
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};

/* Local functions */
static void daemon_recv(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_seq_t seq_num,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata);

/* Local variables */
static orte_grpcomm_collective_t barrier, allgather;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* setup global variables */
    OBJ_CONSTRUCT(&barrier, orte_grpcomm_collective_t);
    OBJ_CONSTRUCT(&allgather, orte_grpcomm_collective_t);
    
    /* point to our collective function */
    orte_grpcomm_base.daemon_coll = orte_grpcomm_mcast_daemon_coll;
    
    /* if we are a daemon or the hnp, we need to post a
     * recv to catch any collective operations or cmds
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_SYS_CHANNEL,
                                                             ORTE_RMCAST_TAG_WILDCARD,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             daemon_recv,
                                                             NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_daemon_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    orte_grpcomm_base_modex_finalize();
    
    /* cancel the recv we posted */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        orte_rmcast.cancel_recv(ORTE_RMCAST_SYS_CHANNEL, ORTE_RMCAST_TAG_WILDCARD);
    }
    
    /* destruct the globals */
    OBJ_DESTRUCT(&barrier);
    OBJ_DESTRUCT(&allgather);
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
    int32_t n;
    opal_buffer_t buf;
    orte_rml_tag_t rmltag;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* setup a buffer to handle the xcast command to an app */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* insert the target tag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* load the std data */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_pack_xcast(ORTE_DAEMON_PROCESS_CMD,
                                                               job, &buf, buffer, tag))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send it */
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer(ORTE_RMCAST_SYS_CHANNEL,
                                                      ORTE_RMCAST_TAG_MSG, &buf))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* unpack the rml tag so the buffer is in the right place
     * for processing
     */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* multicast will not deliver it to myself, so do it manually */
    ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
    goto CLEANUP;
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    return rc;
}


static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    orte_grpcomm_collective_t *coll = (orte_grpcomm_collective_t*)cbdata;
    
    OPAL_THREAD_LOCK(&coll->lock);
    /* flag as recvd */
    coll->recvd = 1;
    opal_condition_broadcast(&coll->cond);
    OPAL_THREAD_UNLOCK(&coll->lock);
}

static int mcast_barrier(void)
{
    int rc;
    opal_buffer_t buf;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just return */
    if (1 == orte_process_info.num_procs) {
        return ORTE_SUCCESS;
    }

    /* if I am a daemon, then multicast the barrier to
     * all other daemons and wait to hear them all
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        OPAL_THREAD_LOCK(&barrier.lock);
        barrier.recvd += 1; /* account for me */
        OPAL_THREAD_UNLOCK(&barrier.lock);
        
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        /* send to everyone in my job */
        if (ORTE_SUCCESS != (rc = xcast(ORTE_PROC_MY_NAME->jobid, &buf, ORTE_RML_TAG_XCAST_BARRIER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        /* wait to complete */
        OPAL_THREAD_LOCK(&barrier.lock);
        while (barrier.recvd < orte_process_info.num_procs) {
            opal_condition_wait(&barrier.cond, &barrier.lock);
        }
        barrier.recvd = 0; /* reset for next time */
        OPAL_THREAD_UNLOCK(&barrier.lock);
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:mcast received barrier release",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        return ORTE_SUCCESS;
    }
    
    /* if I am an application process, then I must start by sending an RML
     * message to my local daemon. I cannot just multicast to all other procs
     * in my job as this barrier might be occurring during startup - and the
     * other procs might not have started yet, and so will miss my message
     */

    /* setup the recv to get the response */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, &barrier);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* send it and wait for the response */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_barrier(ORTE_PROC_MY_DAEMON, &barrier))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf);

    /* don't need to cancel the recv as it only fires once */

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:mcast received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static void allgather_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t *buffer,
                           orte_rml_tag_t tag, void *cbdata)
{
    orte_grpcomm_collective_t *coll = (orte_grpcomm_collective_t*)cbdata;
    int rc;
    
    OPAL_THREAD_LOCK(&coll->lock);
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&coll->results, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    /* the daemon returns ALL of our recipients in a single message */
    coll->recvd = orte_process_info.num_procs;
    opal_condition_broadcast(&coll->cond);
    OPAL_THREAD_UNLOCK(&coll->lock);
}

static int mcast_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup to receive results */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                 ORTE_RML_NON_PERSISTENT, allgather_recv, &allgather);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* everyone sends data to their local daemon and waits for response */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_allgather(ORTE_PROC_MY_DAEMON,
                                                              &allgather, sbuf, rbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* don't need to cancel the recv as it only fires once */

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return rc;    
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc=ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we were given a list of procs to modex with, then this is happening
     * as part of a connect/accept operation.
     */
    if (NULL != procs) {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_full_modex(procs))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    } else {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex())) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static void process_msg(opal_buffer_t *data)
{
    orte_jobid_t jobid;
    orte_odls_job_t *jobdat;
    orte_odls_child_t *child;
    orte_std_cntr_t n;
    opal_list_item_t *item;
    int32_t num_contributors;
    opal_buffer_t buf;
    int rc;
    int32_t numc;
    orte_rml_tag_t rmltag;
    opal_list_t daemons;
    orte_process_name_t proc;
    orte_vpid_t i, daemonvpid;
    orte_namelist_t *nm;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:mcast:daemon_coll: daemon collective called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* unpack the jobid using this collective */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobid, &n, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    /* lookup the job record for it */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* race condition - someone sent us a collective before we could
         * parse the add_local_procs cmd. Just add the jobdat object
         * and continue
         */
        jobdat = OBJ_NEW(orte_odls_job_t);
        jobdat->jobid = jobid;
        opal_list_append(&orte_local_jobdata, &jobdat->super);
    }
    
    /* it may be possible to get here prior to having actually finished processing our
     * local launch msg due to the race condition between different nodes and when
     * they start their individual procs. Hence, we have to first ensure that we
     * -have- finished processing the launch msg, or else we won't know whether
     * or not to wait before sending this on
     */
    OPAL_THREAD_LOCK(&jobdat->lock);
    while (!jobdat->launch_msg_processed) {
        opal_condition_wait(&jobdat->cond, &jobdat->lock);
    }
    OPAL_THREAD_UNLOCK(&jobdat->lock);
    
    /* unpack the tag for this collective */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    /* unpack the number of contributors in this data bucket */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &num_contributors, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    jobdat->num_contributors += num_contributors;
    
    /* xfer the data */
    opal_dss.copy_payload(&jobdat->collection_bucket, data);
    
    /* count the number of participants collected */
    jobdat->num_collected++;
    
    /* if we haven't already done so, figure out how many participants we
     * should be expecting
     */
    if (jobdat->num_participating < 0) {
        /* initialize the counter */
        jobdat->num_participating = 0;
        /* create a list of all daemons in job */
        OBJ_CONSTRUCT(&daemons, opal_list_t);
        for (i=0; i < orte_process_info.num_procs; i++) {
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.vpid = i;
            opal_list_append(&daemons, &nm->item);
        }
        /* count the number of daemons that have procs for this job */
        proc.jobid = jobid;
        proc.vpid = 0;
        while (proc.vpid < jobdat->num_procs && 0 < opal_list_get_size(&daemons)) {
            /* get the daemon that hosts this proc */
            daemonvpid = orte_ess.proc_get_daemon(&proc);
            /* is it on list? */
            for (item = opal_list_get_first(&daemons);
                 item != opal_list_get_end(&daemons);
                 item = opal_list_get_next(item)) {
                nm = (orte_namelist_t*)item;
                if (nm->name.vpid == daemonvpid) {
                    jobdat->num_participating++;
                    opal_list_remove_item(&daemons, item);
                    break;
                }
            }
            /* next proc */
            proc.vpid++;
        }
        OBJ_DESTRUCT(&daemons);
    }

    if (jobdat->num_collected != jobdat->num_participating) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:mcast:daemon_coll: collection complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup a buffer to send the results back to the job members */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* add any collected data */
    numc = jobdat->num_contributors;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &numc, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, &jobdat->collection_bucket))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    /* reset everything for next collective */
    jobdat->num_contributors = 0;
    jobdat->num_collected = 0;
    OBJ_DESTRUCT(&jobdat->collection_bucket);
    OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
    /* send the buffer to each of my children from this job */
    if (0 < jobdat->num_local_procs) {
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;

            if (child->name->jobid == jobdat->jobid) {
                if (0 > (rc = orte_rml.send_buffer(child->name, &buf, rmltag, 0))) {
                    ORTE_ERROR_LOG(rc);
                }
            }
        }
    }
}

static void daemon_recv(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_seq_t seq_num,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata)
{
    int32_t n;
    orte_rml_tag_t rmltag;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast: recvd mcast message",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the rml tag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (rmltag) {
    case ORTE_RML_TAG_DAEMON:
        /* this is a cmd, so deliver it */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:mcast: recvd command",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ORTE_MESSAGE_EVENT(sender, buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
        /* cycle the progress engine since we are not in the event lib */
        opal_progress();
        break;
            
    case ORTE_RML_TAG_BARRIER:
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:mcast: recvd barrier",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* send barrier release to my children */
        process_msg(buf);
        break;
            
    case ORTE_RML_TAG_ALLGATHER:
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:mcast: recvd allgather",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        process_msg(buf);
        break;

    default:
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:mcast: recvd unrecognized tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tag));
        break;
    }
}

/* this function gets called when the daemon has received input from all
 * of its local procs
 */
void orte_grpcomm_mcast_daemon_coll(orte_process_name_t* sender, opal_buffer_t* buffer)
{
    opal_buffer_t buf;
    int32_t n;
    orte_jobid_t jobid;
    orte_rml_tag_t rmltag;
    int rc;
    
    /* we have to partially unpack the provided buffer so it can be
     * reconstructed properly for use here
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* unpack the jobid */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jobid, &n, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* unpack the target tag */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack things in the proper order */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &rmltag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &rmltag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* copy the rest of the data */
    opal_dss.copy_payload(&buf, buffer);

    /* now share it across all daemons */
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast: sending collective results",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer(ORTE_RMCAST_SYS_CHANNEL,
                                                      ORTE_RMCAST_TAG_MSG, &buf))) {
        ORTE_ERROR_LOG(rc);
    }

    /* since I cannot recv my own mcast, process this myself */
    daemon_recv(ORTE_SUCCESS, ORTE_RMCAST_SYS_CHANNEL, 0,
                ORTE_RMCAST_TAG_MSG, ORTE_PROC_MY_NAME,
                &buf, NULL);

 CLEANUP:
    OBJ_DESTRUCT(&buf);
    return;
}
