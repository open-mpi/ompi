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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/routed/base/base.h"

void orte_routed_base_xcast_routing(orte_grpcomm_collective_t *coll,
                                    opal_list_t *my_children)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    orte_namelist_t *nm;
    int i;
    orte_proc_t *proc;
    orte_job_t *daemons;

    /* if we are the HNP and an abnormal termination is underway,
     * then send it directly to everyone
     */
    if (ORTE_PROC_IS_HNP) {
        if (orte_abnormal_term_ordered || !orte_routing_is_enabled) {
            daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            for (i=1; i < daemons->procs->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, i))) {
                    continue;
                }
                /* exclude anyone known not alive */
                if (proc->alive) {
                    nm = OBJ_NEW(orte_namelist_t);
                    nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    nm->name.vpid = proc->name.vpid;
                    opal_list_append(&coll->targets, &nm->super);
                }
            }
            /* if nobody is known alive, then we need to die */
            if (0 == opal_list_get_size(&coll->targets)) {
                ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
            }
        } else {
            /* the xcast always goes to our children */
            for (item = opal_list_get_first(my_children);
                 item != opal_list_get_end(my_children);
                 item = opal_list_get_next(item)) {
                child = (orte_routed_tree_t*)item;
                nm = OBJ_NEW(orte_namelist_t);
                nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
                nm->name.vpid = child->vpid;
                opal_list_append(&coll->targets, &nm->super);
            }
        }
    } else {
        /* I am a daemon - route to my children */
        for (item = opal_list_get_first(my_children);
             item != opal_list_get_end(my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
            nm->name.vpid = child->vpid;
            opal_list_append(&coll->targets, &nm->super);
        }
    }
}

void orte_routed_base_coll_relay_routing(orte_grpcomm_collective_t *coll)
{
    opal_list_item_t *item, *itm;
    orte_namelist_t *nm, *n2, *n3;
    bool dup;
    orte_job_t *jdata;
    orte_proc_t *proc;

    if (ORTE_PROC_IS_HNP) {
        /* nobody to send to */
        return;
    }
    /* if we are a daemon, then we look at the list of
     * participants. If there is a wildcard, then we
     * know that all procs are participating, so we
     * can send it to our parent. If not, then we have
     * to send the collective to the daemon hosting
     * the participating proc
     */
    for (item = opal_list_get_first(&coll->participants);
         item != opal_list_get_end(&coll->participants);
         item = opal_list_get_next(item)) {
        n2 = (orte_namelist_t*)item;
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        dup = false;
        if (ORTE_VPID_WILDCARD == n2->name.vpid) {
            nm->name.vpid = ORTE_PROC_MY_PARENT->vpid;
        } else {
            jdata = orte_get_job_data_object(n2->name.jobid);
            proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, n2->name.vpid);
            if (NULL == proc || NULL == proc->node || NULL == proc->node->daemon) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                continue;
            }
            nm->name.vpid = proc->node->daemon->name.vpid;
        }
        /* if it is me, then ignore */
        if (nm->name.vpid == ORTE_PROC_MY_NAME->vpid) {
            dup = true;
        } else {
            /* if it is already on the list, we ignore */
            for (itm = opal_list_get_first(&coll->targets);
                 itm != opal_list_get_end(&coll->targets);
                 itm = opal_list_get_next(itm)) {
                n3 = (orte_namelist_t*)itm;
                if (n3->name.vpid == nm->name.vpid) {
                    /* duplicate */
                    dup = true;
                    break;
                }
            }
        }
        if (dup) {
            OBJ_RELEASE(nm);
        } else {
            opal_list_append(&coll->targets, &nm->super);
        }
    }
}

void orte_routed_base_coll_complete_routing(orte_grpcomm_collective_t *coll)
{
    opal_list_item_t *item;
    orte_namelist_t *nm, *n2;
    int i;
    orte_proc_t *proc;

    if (ORTE_PROC_IS_HNP) {
        /* send it to everyone that participated */
        for (item = opal_list_get_first(&coll->participants);
             item != opal_list_get_end(&coll->participants);
             item = opal_list_get_next(item)) {
            n2 = (orte_namelist_t*)item;
            /* if the vpid is wildcard, then the result will go
             * to everyone in the job via xcast, so just carry it
             * across
             */
            if (ORTE_VPID_WILDCARD == n2->name.vpid) {
                nm = OBJ_NEW(orte_namelist_t);
                nm->name.jobid = n2->name.jobid;
                nm->name.vpid = n2->name.vpid;
                opal_list_append(&coll->targets, &nm->super);
            } else {
                /* only include it if the proc is local to us */
                for (i=0; i < orte_local_children->size; i++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                        continue;
                    }
                    if (proc->name.jobid == n2->name.jobid &&
                        proc->name.vpid == n2->name.vpid) {
                        nm = OBJ_NEW(orte_namelist_t);
                        nm->name.jobid = n2->name.jobid;
                        nm->name.vpid = n2->name.vpid;
                        opal_list_append(&coll->targets, &nm->super);
                        break;
                    }
                }
            }
        }
    } else {
        /* if the participants are wildcard, then the HNP will
         * be sending the result to the procs via xcast. For all
         * other cases, handle our own local children
         */
        for (item = opal_list_get_first(&coll->participants);
             item != opal_list_get_end(&coll->participants);
             item = opal_list_get_next(item)) {
            n2 = (orte_namelist_t*)item;
            if (ORTE_VPID_WILDCARD == n2->name.vpid) {
                continue;
            }
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                    continue;
                }
                if (proc->name.jobid == n2->name.jobid &&
                    proc->name.vpid == n2->name.vpid) {
                    nm = OBJ_NEW(orte_namelist_t);
                    nm->name.jobid = n2->name.jobid;
                    nm->name.vpid = n2->name.vpid;
                    opal_list_append(&coll->targets, &nm->super);
                    break;
                }
            }
        }
    }
}

void orte_routed_base_coll_peers(orte_grpcomm_collective_t *coll,
                                 opal_list_t *my_children)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    orte_namelist_t *nm;

    /* tree-based systems require input from their children */
    for (item = opal_list_get_first(my_children);
         item != opal_list_get_end(my_children);
         item = opal_list_get_next(item)) {
        child = (orte_routed_tree_t*)item;
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = child->vpid;
        opal_list_append(&coll->targets, &nm->super);
    }
 }


static void report_sync(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag, void *cbdata)
{
    bool *sync_waiting = (bool*)cbdata;

    /* just copy the payload to the sync_buf */
    opal_dss.copy_payload(orte_process_info.sync_buf, buffer);
    /* flag as complete */
    *sync_waiting = false;
}

int orte_routed_base_register_sync(bool setup)
{
    opal_buffer_t *buffer;
    int rc;
    orte_daemon_cmd_flag_t command;
    char *rml_uri;
    uint8_t flag;
    bool sync_waiting;

    if (orte_abnormal_term_ordered) {
        /* if we are abnormally terminating, don't
         * even try to deregister from the daemon - there
         * is no guarantee we won't just hang in
         * the communication
         */
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                         "%s %s with daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON),
                         setup ? "registering" : "deregistering"));
    
    /* we need to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    buffer = OBJ_NEW(opal_buffer_t);
    
    if (setup) {
        /* tell the daemon to send back a nidmap */
        command = ORTE_DAEMON_SYNC_WANT_NIDMAP;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
    
        /* add our contact info to the buffer so the daemon can explicitly
         * store it
         */
        rml_uri = orte_rml.get_contact_info();
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &rml_uri, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            free(rml_uri);
            return rc;
        }
        if (NULL != rml_uri) free(rml_uri);
    
        /* tell the daemon if we are an MPI proc */
        if (ORTE_PROC_IS_MPI) {
            flag = 1;
        } else {
            flag = 0;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &flag, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
    } else {
        /* deregister with the daemon */
        command = ORTE_DAEMON_SYNC_BY_PROC;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
    }

    /* setup to receive the response */
    sync_waiting = true;
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SYNC,
                            ORTE_RML_NON_PERSISTENT, report_sync, &sync_waiting);

    /* send the sync command to our daemon */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_DAEMON, buffer,
                                          ORTE_RML_TAG_DAEMON,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                         "%s registering sync waiting for ack",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    
    /* get the ack - includes the nidmap */
    ORTE_WAIT_FOR_COMPLETION(sync_waiting);
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                         "%s registering sync ack recvd",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return ORTE_SUCCESS;
}

int orte_routed_base_process_callback(orte_jobid_t job, opal_buffer_t *buffer)
{
    orte_proc_t *proc;
    orte_job_t *jdata;
    orte_std_cntr_t cnt;
    char *rml_uri;
    orte_vpid_t vpid;
    int rc;

    /* lookup the job object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* came from a different job family - this is an error */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* unpack the data for each entry */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {

        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                             "%s routed_binomial:callback got uri %s for job %s rank %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == rml_uri) ? "NULL" : rml_uri,
                             ORTE_JOBID_PRINT(job), ORTE_VPID_PRINT(vpid)));
        
        if (NULL == rml_uri) {
            /* should not happen */
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            return ORTE_ERR_FATAL;
        }
        
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            continue;
        }
        
        /* update the record */
        proc->rml_uri = strdup(rml_uri);
        free(rml_uri);
        
        cnt = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    

    return ORTE_SUCCESS;    
}
