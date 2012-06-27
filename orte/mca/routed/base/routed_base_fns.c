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
        if (orte_abnormal_term_ordered) {
            daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            for (i=1; i < daemons->procs->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, i))) {
                    continue;
                }
                nm = OBJ_NEW(orte_namelist_t);
                nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
                nm->name.vpid = proc->name.vpid;
                opal_list_append(&coll->targets, &nm->super);
            }
        } else {
            /* the binomial xcast always goes to our children */
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


static bool sync_waiting = false;

static void report_sync(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag, void *cbdata)
{
    /* just copy the payload to the sync_buf */
    opal_dss.copy_payload(orte_process_info.sync_buf, buffer);
    /* flag as complete */
    sync_waiting = false;
}

int orte_routed_base_register_sync(bool setup)
{
    opal_buffer_t *buffer;
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SYNC_BY_PROC;
    char *rml_uri;
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync to daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON)));
    
    /* we need to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    buffer = OBJ_NEW(opal_buffer_t);
    
    /* if we are setting up, tell the daemon to send back a nidmap */
    if (setup) {
        command = ORTE_DAEMON_SYNC_WANT_NIDMAP;
    }


    /* tell the daemon to sync */
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
    
    /* send the sync command to our daemon */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_DAEMON, buffer,
                                          ORTE_RML_TAG_DAEMON, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the ack - need this to ensure that the sync communication
     * gets serviced by the event library on the orted prior to the
     * process exiting
     */
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync waiting for ack",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    sync_waiting = true;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SYNC,
                                 ORTE_RML_NON_PERSISTENT, report_sync, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* it is okay to block here as we are -not- in an event */
    ORTE_WAIT_FOR_COMPLETION(sync_waiting);
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
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

    if (ORTE_JOB_FAMILY(job) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* came from singleton - don't process it */
        return ORTE_SUCCESS;
    }

    /* lookup the job object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* came from my job family - this is an error */
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
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
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
