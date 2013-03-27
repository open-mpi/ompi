/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_hier.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int hier_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int hier_barrier(void);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_hier_module = {
    init,
    finalize,
    xcast,
    hier_allgather,
    orte_grpcomm_base_allgather_list,
    hier_barrier,
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    orte_grpcomm_base_full_modex,
    orte_grpcomm_base_purge_proc_attrs
};


/* Local data */
static orte_local_rank_t my_local_rank;
static opal_list_t my_local_peers;
static orte_process_name_t my_local_rank_zero_proc;
static size_t num_local_peers=0;
static bool coll_initialized = false;
static orte_vpid_t *my_coll_peers=NULL;
static int cpeers=0;
static orte_grpcomm_collective_t barrier, allgather;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    OBJ_CONSTRUCT(&my_local_peers, opal_list_t);
    OBJ_CONSTRUCT(&barrier, orte_grpcomm_collective_t);
    OBJ_CONSTRUCT(&allgather, orte_grpcomm_collective_t);

    my_local_rank_zero_proc.jobid = ORTE_PROC_MY_NAME->jobid;
    my_local_rank_zero_proc.vpid = ORTE_VPID_INVALID;

    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    opal_list_item_t *item;
    
    orte_grpcomm_base_modex_finalize();
    
    while (NULL != (item = opal_list_remove_first(&my_local_peers))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&my_local_peers);
    
    OBJ_DESTRUCT(&barrier);
    OBJ_DESTRUCT(&allgather);

    if (NULL != my_coll_peers) {
        free(my_coll_peers);
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
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:xcast sent to job %s tag %ld",
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


/* the barrier is executed as an allgather with data length of zero */
static int hier_barrier(void)
{
    opal_buffer_t buf1, buf2;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:hier entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    OBJ_CONSTRUCT(&buf1, opal_buffer_t);
    OBJ_CONSTRUCT(&buf2, opal_buffer_t);
    
    if (ORTE_SUCCESS != (rc = hier_allgather(&buf1, &buf2))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf1);
    OBJ_DESTRUCT(&buf2);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:hier barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static void allgather_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t *buffer,
                           orte_rml_tag_t tag, void *cbdata)
{
    orte_grpcomm_collective_t *coll = (orte_grpcomm_collective_t*)cbdata;
    int rc;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&coll->results, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    coll->recvd += 1;
    if (0 == my_local_rank) {
        /* I have to wait until I get a message from
         * every local proc other than myself
         */
        if (num_local_peers == coll->recvd) {
            opal_condition_broadcast(&coll->cond);
        }
    } else {
        /* the rest of the local procs only recv one message back,
         * coming from the local_rank=0 proc
         */
        opal_condition_broadcast(&coll->cond);
    }
}

static int hier_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc=ORTE_SUCCESS;
    opal_list_item_t *item;
    orte_namelist_t *nm;
    opal_buffer_t tmp_buf;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:hier entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* have I initialized my local info? */
    if (!coll_initialized) {
        orte_process_name_t proc;
        orte_vpid_t v;
        
        /* get my local rank so I can locally cache it */
        my_local_rank = orte_ess.get_local_rank(ORTE_PROC_MY_NAME);
        
        /* if I am local_rank=0 for this node and job, then setup
         * my array of local_rank=0 peers
         */
        if (0 == my_local_rank) {
            /* we need one entry/node in this job */
            my_coll_peers = (orte_vpid_t*)malloc(orte_process_info.num_nodes * sizeof(orte_vpid_t));
            cpeers = 0;
        }
        
        /* cycle through the procs to create a list of those that are local to me */
        proc.jobid = ORTE_PROC_MY_NAME->jobid;
        for (v=0; v < orte_process_info.num_procs; v++) {
            proc.vpid = v;

            /* is this proc local_rank=0 on its node? */
            if (0 == my_local_rank && 0 == orte_ess.get_local_rank(&proc)) {
                my_coll_peers[cpeers++] = v;
            }
            /* if this is me, or this proc isn't on our node, ignore it */
            if (v == ORTE_PROC_MY_NAME->vpid ||
                !OPAL_PROC_ON_LOCAL_NODE(orte_ess.proc_get_locality(&proc))) {
                continue;
            }
            /* add this proc to our list of local peers */
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = proc.jobid;
            nm->name.vpid = proc.vpid;

            opal_list_append(&my_local_peers, &nm->item);
            /* if I am not local_rank=0, is this one? */
            if (0 != my_local_rank &&
                0 == orte_ess.get_local_rank(&proc)) {
                my_local_rank_zero_proc.jobid = proc.jobid;
                my_local_rank_zero_proc.vpid = proc.vpid;
            }
        }

        /* compute the number of local peers - note that this number
         * does not include me!!
         */
        num_local_peers = opal_list_get_size(&my_local_peers);
        
        /* flag that I have initialized things */
        coll_initialized = true;
    }

    /* if I am not local rank = 0 */
    if (0 != my_local_rank) {
        if (ORTE_VPID_INVALID == my_local_rank_zero_proc.vpid) {
            /* something is broken */
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            return ORTE_ERR_FATAL;
        }

        /* setup the collective */
        allgather.recvd = 0;
        /* reset the collector */
        OBJ_DESTRUCT(&allgather.results);
        OBJ_CONSTRUCT(&allgather.results, opal_buffer_t);
        
        /* send our data to the local_rank=0 proc on this node */
        if (0 > (rc = orte_rml.send_buffer(&my_local_rank_zero_proc, sbuf, ORTE_RML_TAG_ALLGATHER, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* now receive the final result. Be sure to do this in
         * a manner that allows us to return without being in a recv!
         */
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                     ORTE_RML_NON_PERSISTENT, allgather_recv, &allgather);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* wait to complete - we will receive a single message
         * sent from our local_rank=0 peer
         */

        /* copy payload to the caller's buffer */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, &allgather.results))) {
            ORTE_ERROR_LOG(rc);
        }
        
        
    } else {
        /* I am local_rank = 0 on this node! */

        /* setup the collective */
        allgather.recvd = 0;
        /* reset the collector */
        OBJ_DESTRUCT(&allgather.results);
        OBJ_CONSTRUCT(&allgather.results, opal_buffer_t);
        /* seed with my data */
        opal_dss.copy_payload(&allgather.results, sbuf);

        /* wait to receive their data. Be sure to do this in
         * a manner that allows us to return without being in a recv!
         */
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                     ORTE_RML_PERSISTENT, allgather_recv, &allgather);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* wait to complete - we need to receive input from every
         * local peer (excluding myself)
         */

        /* xfer to the tmp buf in case another allgather comes along */
        OBJ_CONSTRUCT(&tmp_buf, opal_buffer_t);
        opal_dss.copy_payload(&tmp_buf, &allgather.results);
        
        /* cancel the lingering recv */
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER);
        
        /* take the recv'd data and use one of the base collectives
         * to exchange it with all other local_rank=0 procs in a scalable
         * manner - the exact collective will depend upon the number of
         * nodes in the job
         */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_allgather(&tmp_buf, rbuf, num_local_peers + 1,
                                                              ORTE_PROC_MY_NAME->jobid,
                                                              cpeers, my_coll_peers))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&tmp_buf);
            return rc;
        }
        OBJ_DESTRUCT(&tmp_buf);  /* done with this */

        /* distribute the results to our local peers */
        for (item = opal_list_get_first(&my_local_peers);
             item != opal_list_get_end(&my_local_peers);
             item = opal_list_get_next(item)) {
            nm = (orte_namelist_t*)item;
            if (0 > (rc = orte_rml.send_buffer(&nm->name, rbuf, ORTE_RML_TAG_ALLGATHER, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:hier allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}
