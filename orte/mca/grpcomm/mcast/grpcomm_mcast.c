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
static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_mcast_module = {
    init,
    finalize,
    xcast,
    mcast_allgather,
    orte_grpcomm_base_allgather_list,
    mcast_barrier,
    orte_grpcomm_base_set_proc_attr,
    get_proc_attr,
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
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG_T))) {
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
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &rmltag, &n, ORTE_RML_TAG_T))) {
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
     * as part of a connect/accept operation. In this case, we -must- do the
     * modex for two reasons:
     *
     * (a) the modex could involve procs from different mpiruns. In this case,
     *     there is no way for the two sets of procs to know which node the
     *     other procs are on, so we cannot use the profile_file to determine
     *     their contact info
     *
     * (b) in a comm_spawn, the parent job does not have a pidmap for the
     *     child job. Thus, it cannot know where the child procs are located,
     *     and cannot use the profile_file to determine their contact info
     *
     */
    if (NULL != procs) {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_full_modex(procs, false))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    if (OPAL_ENABLE_HETEROGENEOUS_SUPPORT) {
        /* decide if we need to add the architecture to the modex. Check
         * first to see if hetero is enabled - if not, then we clearly
         * don't need to exchange arch's as they are all identical
         */
        /* Case 1: If different apps in this job were built differently - e.g., some
         * are built 32-bit while others are built 64-bit - then we need to modex
         * regardless of any other consideration. The user is reqd to tell us via a
         * cmd line option if this situation exists, which will result in an mca param
         * being set for us, so all we need to do is check for the global boolean
         * that corresponds to that param
         *
         * Case 2: the nodes are hetero, but the app binaries were built
         * the same - i.e., either they are both 32-bit, or they are both 64-bit, but
         * no mixing of the two. In this case, we include the info in the modex
         */
        if (orte_hetero_apps || !orte_homogeneous_nodes) {
            OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                                 "%s grpcomm:mcast: modex is required",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex(false))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    
    /* no modex is required - see if the data was included in the launch message */
    if (orte_send_profile) {
        /* the info was provided in the nidmap - there is nothing more we have to do */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:mcast:modex using nidmap",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:mcast: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size)
{
    orte_nid_t *nid;
    opal_list_item_t *item;
    orte_attr_t *attr;
    
    /* find this proc's node in the nidmap */
    if (NULL == (nid = orte_util_lookup_nid((orte_process_name_t*)&proc))) {
        /* proc wasn't found - return error */
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:mcast:get_proc_attr: no modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc)));
        return ORTE_ERR_NOT_FOUND;
        
    }
    
    /* look for this attribute */
    for (item = opal_list_get_first(&nid->attrs);
         item != opal_list_get_end(&nid->attrs);
         item = opal_list_get_next(item)) {
        attr = (orte_attr_t*)item;
        if (0 == strcmp(attr->name, attribute_name)) {
            /* copy the data to the caller */
            void *copy = malloc(attr->size);
            
            if (copy == NULL) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            memcpy(copy, attr->bytes, attr->size);
            *val = copy;
            *size = attr->size;
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:mcast:get_proc_attr: found %d bytes for attr %s on proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)attr->size,
                                 attribute_name, ORTE_NAME_PRINT(&proc)));
            return ORTE_SUCCESS;
        }
    }
    
    /* get here if attribute isn't found */
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:mcast:get_proc_attr: no attr avail or zero byte size for proc %s attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc), attribute_name));
    *val = NULL;
    *size = 0;
    
    return ORTE_SUCCESS;
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
    
    /* unpack the rml tag */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &rmltag, &n, ORTE_RML_TAG_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (tag) {
        case ORTE_RML_TAG_DAEMON:
            /* this is a cmd, so deliver it */
            ORTE_MESSAGE_EVENT(sender, buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
            break;
            
        case ORTE_RML_TAG_BARRIER:
            OPAL_THREAD_LOCK(&barrier.lock);
            /* the recv is the trigger */
            barrier.recvd = 1;
            opal_condition_broadcast(&barrier.cond);
            OPAL_THREAD_UNLOCK(&barrier.lock);            
            
            break;
            
        case ORTE_RML_TAG_ALLGATHER:
            OPAL_THREAD_LOCK(&allgather.lock);
            allgather.recvd += 1;
            /* xfer the data */
            opal_dss.copy_payload(&allgather.results, buf);
            /* check for completion */
            if (orte_process_info.num_procs <= allgather.recvd) {
                opal_condition_broadcast(&allgather.cond);
            }
            OPAL_THREAD_UNLOCK(&allgather.lock);
            break;

        default:
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
    
CLEANUP:
    return;
}
