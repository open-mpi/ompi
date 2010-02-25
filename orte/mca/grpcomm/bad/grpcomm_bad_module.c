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


#include "orte/util/proc_info.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_bad.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int bad_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int bad_barrier(void);
static int bad_onesided_barrier(void);
static int modex(opal_list_t *procs);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_bad_module = {
    init,
    finalize,
    xcast,
    bad_allgather,
    orte_grpcomm_base_allgather_list,
    bad_barrier,
    bad_onesided_barrier,
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};

/* Local variables */
static orte_grpcomm_collective_t barrier, allgather, onesided_barrier;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup global variables */
    OBJ_CONSTRUCT(&barrier, orte_grpcomm_collective_t);
    OBJ_CONSTRUCT(&allgather, orte_grpcomm_collective_t);
    OBJ_CONSTRUCT(&onesided_barrier, orte_grpcomm_collective_t);

    /* if we are a daemon or the hnp, we need to post a
     * recv to catch any collective operations
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_daemon_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    orte_grpcomm_base_modex_finalize();

    /* destruct the globals */
    OBJ_DESTRUCT(&barrier);
    OBJ_DESTRUCT(&allgather);
    OBJ_DESTRUCT(&onesided_barrier);

    /* if we are a daemon or the hnp, we need to cancel the
     * recv we posted
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE);
    }
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
    opal_buffer_t buf;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* prep the output buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_pack_xcast(ORTE_DAEMON_PROCESS_AND_RELAY_CMD,
                                                               job, &buf, buffer, tag))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if I am the HNP, just set things up so the cmd processor gets called.
     * We don't want to message ourselves as this can create circular logic
     * in the RML. Instead, this macro will set a zero-time event which will
     * cause the buffer to be processed by the cmd processor - probably will
     * fire right away, but that's okay
     * The macro makes a copy of the buffer, so it's okay to release it here
     */
    if (ORTE_PROC_IS_HNP) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
    } else {
        /* otherwise, send it to the HNP for relay */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
    }
    
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

static int bad_barrier(void)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just return */
    if (1 == orte_process_info.num_procs) {
        return ORTE_SUCCESS;
    }
    
    /* setup the recv to get the response */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, &barrier);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send it and wait for the response */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_barrier(ORTE_PROC_MY_DAEMON, &barrier))) {
        ORTE_ERROR_LOG(rc);
    }

    /* don't need to cancel the recv as it only fires once */
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:bad received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return rc;
}

static void onesided_barrier_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    orte_grpcomm_collective_t *coll = (orte_grpcomm_collective_t*)cbdata;
    
    OPAL_THREAD_LOCK(&coll->lock);
    /* flag as recvd */
    coll->recvd += 1;
    if (orte_process_info.num_procs == coll->recvd) {
        opal_condition_broadcast(&coll->cond);
    }
    OPAL_THREAD_UNLOCK(&coll->lock);
}

/* quick timeout loop */
static bool timer_fired;

static void quicktime_cb(int fd, short event, void *cbdata)
{
    /* declare it fired */
    timer_fired = true;
}

static int bad_onesided_barrier(void)
{
    opal_list_t daemon_tree;
    opal_list_item_t *item;
    opal_buffer_t buf;
    orte_process_name_t my_parent;
    opal_event_t *quicktime=NULL;
    struct timeval quicktimeval;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:bad: onesided barrier called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are not to use the barrier, then just return */
    if (!orte_orted_exit_with_barrier) {
        if (ORTE_PROC_IS_HNP) {
            /* if we are the HNP, we need to do a little delay to give
             * the orteds a chance to exit before we leave
             */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:bad: onesided barrier adding delay timer",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            quicktimeval.tv_sec = 0;
            quicktimeval.tv_usec = 100;
            timer_fired = false;
            ORTE_DETECT_TIMEOUT(&quicktime, orte_process_info.num_procs, 1000, 10000, quicktime_cb);
            ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
        }
        return ORTE_SUCCESS;
    }
    
    /* figure out how many participants we should be expecting */
    OBJ_CONSTRUCT(&daemon_tree, opal_list_t);
    my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
    my_parent.vpid = orte_routed.get_routing_tree(&daemon_tree);
    OPAL_THREAD_LOCK(&onesided_barrier.lock);
    onesided_barrier.recvd += orte_process_info.num_procs - opal_list_get_size(&daemon_tree);
    OPAL_THREAD_UNLOCK(&onesided_barrier.lock);
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:bad: onesided barrier num_participating %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)(orte_process_info.num_procs - opal_list_get_size(&daemon_tree))));

    /* disassemble the daemon tree */
    while (NULL != (item = opal_list_remove_first(&daemon_tree))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemon_tree);
    
    /* set the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_ONESIDED_BARRIER,
                                                      ORTE_RML_PERSISTENT,
                                                      onesided_barrier_recv,
                                                      &onesided_barrier))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* wait to get all my inputs */
    OPAL_THREAD_LOCK(&onesided_barrier.lock);
    while (onesided_barrier.recvd < orte_process_info.num_procs) {
        opal_condition_wait(&onesided_barrier.cond, &onesided_barrier.lock);
    }
    /* reset the collective */
    onesided_barrier.recvd = 0;
    OPAL_THREAD_UNLOCK(&onesided_barrier.lock);
    
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ONESIDED_BARRIER);

    /* if I am the HNP, then we are done */
    if (ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    /* send a zero-byte msg to my parent */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* send it */
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:bad:onsided:barrier not the HNP - sending to parent %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&my_parent)));
    if (0 > (rc = orte_rml.send_buffer(&my_parent, &buf, ORTE_RML_TAG_ONESIDED_BARRIER, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
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

static int bad_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup to receive results */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                 ORTE_RML_NON_PERSISTENT, allgather_recv, &allgather);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* everyone sends data to their local daemon */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_app_allgather(ORTE_PROC_MY_DAEMON,
                                                              &allgather, sbuf, rbuf))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* don't need to cancel the recv as it only fires once */
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc;
    opal_buffer_t buf, rbuf;
    bool modex_reqd = true;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (NULL == procs) {
        /* The modex will be realized in the background by the daemons. The processes will
         * only be informed when all data has been collected from all processes. The get_attr
         * will realize the blocking, it will not return until the data has been received.
         */
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:bad:peer:modex: performing modex",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* setup the buffers */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        OBJ_CONSTRUCT(&rbuf, opal_buffer_t);
        
        /* put our process name in the buffer so it can be unpacked later */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* pack the entries we have received */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_modex_entries(&buf, &modex_reqd))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* perform the allgather */
        if (ORTE_SUCCESS != (rc = bad_allgather(&buf, &rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* store the results */
        if( ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_unpack(&rbuf, true)) ) {
            ORTE_ERROR_LOG(rc);
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:bad: modex posted",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
 cleanup:
        OBJ_DESTRUCT(&buf);
        OBJ_DESTRUCT(&rbuf);

        return rc;
    } else {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_full_modex(procs, true))) {
            ORTE_ERROR_LOG(rc);
        }        
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:bad: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}
