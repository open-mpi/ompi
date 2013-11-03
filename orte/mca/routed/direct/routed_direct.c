/*
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/dss/dss_types.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_direct.h"

static int init(void);
static int finalize(void);
static int delete_route(orte_process_name_t *proc);
static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route);
static orte_process_name_t get_route(orte_process_name_t *target);
static int init_routes(orte_jobid_t job, opal_buffer_t *ndat);
static int route_lost(const orte_process_name_t *route);
static bool route_is_defined(const orte_process_name_t *target);
static void update_routing_plan(void);
static void get_routing_list(orte_grpcomm_coll_t type,
                             orte_grpcomm_collective_t *coll);
static int get_wireup_info(opal_buffer_t *buf);
static int set_lifeline(orte_process_name_t *proc);
static size_t num_routes(void);

#if OPAL_ENABLE_FT_CR == 1
static int direct_ft_event(int state);
#endif

orte_routed_module_t orte_routed_direct_module = {
    init,
    finalize,
    delete_route,
    update_route,
    get_route,
    init_routes,
    route_lost,
    route_is_defined,
    set_lifeline,
    update_routing_plan,
    get_routing_list,
    get_wireup_info,
    num_routes,
#if OPAL_ENABLE_FT_CR == 1
    direct_ft_event
#else
    NULL
#endif
};

static int init(void)
{
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_direct_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_direct_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));

    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;

    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* all routes go direct */
    ret = target;

 found:
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s routed_direct_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    int rc;
    
    /* if I am a tool, then I stand alone - there is nothing to do */
    if (ORTE_PROC_IS_TOOL) {
        return ORTE_SUCCESS;
    }
    
    /* if I am a daemon or HNP, then I have to extract the routing info for this job
     * from the data sent to me for launch and update the routing tables to
     * point at the daemon for each proc
     */
    if (ORTE_PROC_IS_DAEMON) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s direct: init routes for daemon job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        
        if (NULL == ndat) {
            /* indicates this is being called during orte_init.
             * Get the HNP's name for possible later use
             */
            if (NULL == orte_process_info.my_hnp_uri) {
                /* fatal error */
                ORTE_ERROR_LOG(ORTE_ERR_FATAL);
                return ORTE_ERR_FATAL;
            }
            /* set the contact info into the hash table */
            orte_rml.set_contact_info(orte_process_info.my_hnp_uri);
            
            /* extract the hnp name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            /* daemons will send their contact info back to the HNP as
             * part of the message confirming they are read to go. HNP's
             * load their contact info during orte_init
             */
        } else {
            /* ndat != NULL means we are getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                             "%s routed_direct: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    
    
    if (ORTE_PROC_IS_HNP) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_direct: init routes for HNP job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job)));
        
        if (NULL != ndat) {
            /* if this is for my own jobid, then I am getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_PROC_MY_NAME->jobid == job) {
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
        
        return ORTE_SUCCESS;
    }

    /***   MUST BE A PROC   ***/
    
    /* if ndat=NULL, then we are being called during orte_init */
    if (NULL == ndat) {
        if (NULL != orte_process_info.my_daemon_uri) {
            /* we are being launched by a daemon, so we need to
             * register a sync with it to get our nidmap back
             */
            /* Set the contact info in the RML - this won't actually establish
             * the connection, but just tells the RML how to reach the daemon
             * if/when we attempt to send to it
             */
            orte_rml.set_contact_info(orte_process_info.my_daemon_uri);
            /* extract the daemon's name so we can update the routing table */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                               ORTE_PROC_MY_DAEMON, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* register ourselves -this sends a message to the daemon (warming up that connection)
             * and sends our contact info to the HNP when all local procs have reported
             */
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* no answer is expected or coming */
        }
        return ORTE_SUCCESS;
    }
    
    /* if ndat != NULL, then this is being invoked by the proc to
     * init a route to a specified process that is outside of our
     * job family. It really doesn't matter as everything must
     * go direct
     */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_direct: init routes w/non-NULL data",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

static int route_lost(const orte_process_name_t *route)
{
    /* there is no lifeline, so we don't care */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    /* all routes are defined */
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    /* there is no lifeline */
    return ORTE_SUCCESS;
}

static void update_routing_plan(void)
{
    /* nothing to do here */
    return;
}

static void get_routing_list(orte_grpcomm_coll_t type,
                             orte_grpcomm_collective_t *coll)
{
    orte_namelist_t *nm;
    int32_t i;
    orte_job_t *jdata;
    orte_proc_t *proc;
    
    /* if I am anything other than daemons and the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON || !ORTE_PROC_IS_HNP) {
        return;
    }
    
    if (ORTE_GRPCOMM_XCAST == type) {
        /* daemons don't route */
        if (ORTE_PROC_IS_DAEMON) {
            return;
        }
        /* HNP sends direct to each daemon */
        if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return;
        }
        for (i=1; i < jdata->procs->size; i++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if( proc->state <= ORTE_PROC_STATE_UNTERMINATED &&
                NULL != proc->rml_uri ) {
                OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                                     "%s get_routing_tree: Adding process %s state %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&(proc->name)),
                                     orte_proc_state_to_str(proc->state)));

                nm = OBJ_NEW(orte_namelist_t);
                nm->name.jobid = proc->name.jobid;
                nm->name.vpid = proc->name.vpid;
                opal_list_append(&coll->targets, &nm->super);
            } else {
                OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                                     "%s get_routing_tree: Skipped process %15s state %s (non functional daemon)",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&(proc->name)),
                                     orte_proc_state_to_str(proc->state)));
            }
        }
    } else if (ORTE_GRPCOMM_COLL_RELAY == type) {
        orte_routed_base_coll_relay_routing(coll);
    } else if (ORTE_GRPCOMM_COLL_COMPLETE == type) {
        orte_routed_base_coll_complete_routing(coll);
    } else if (ORTE_GRPCOMM_COLL_PEERS == type) {
        if (ORTE_PROC_IS_DAEMON) {
            return;
        }
        /* HNP receives from all */
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = ORTE_VPID_WILDCARD;
        opal_list_append(&coll->targets, &nm->super);
    }
}

static int get_wireup_info(opal_buffer_t *buf)
{
    opal_byte_object_t bo, *boptr;

    /* this is a meaningless command for a direct as I am not allowed to route */
    bo.bytes = NULL;
    bo.size = 0;
    boptr = &bo;

    opal_dss.pack(buf, &boptr, 1, OPAL_BYTE_OBJECT);
    return ORTE_SUCCESS;
}

static size_t num_routes(void)
{
    orte_job_t *jdata;

    if (!ORTE_PROC_IS_HNP) {
        return 0;
    }

    /* if I am the HNP, then the number of routes is
     * the number of daemons still alive (other than me)
     */
    if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return 0;
    }

    return (jdata->num_procs - jdata->num_terminated - 1);
}

#if OPAL_ENABLE_FT_CR == 1
static int direct_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        /*
         * Re-exchange the routes
         */
        if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Nothing */
    }
    else {
        /* Error state = Nothing */
    }

 cleanup:
    return exit_status;
}
#endif

