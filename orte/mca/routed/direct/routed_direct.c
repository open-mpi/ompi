/*
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014      Intel Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"
#include "orte/runtime/orte_wait.h"

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
static void get_routing_list(opal_list_t *coll);
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

static orte_process_name_t mylifeline;
static orte_process_name_t *lifeline = NULL;
static opal_list_t my_children;

static int init(void)
{
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    OPAL_LIST_DESTRUCT(&my_children);
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
    orte_process_name_t *ret, daemon;
    orte_routed_jobfam_t *jfam;
    int i;
    uint16_t jfamily;

    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* initialize */
    daemon.jobid = ORTE_PROC_MY_DAEMON->jobid;
    daemon.vpid = ORTE_PROC_MY_DAEMON->vpid;

    if (ORTE_PROC_IS_APP) {
        /* if I am an application, AND I have knowledge of
         * my daemon (i.e., a daemon launched me), then I
         * always route thru the daemon */
        if (NULL != orte_process_info.my_daemon_uri) {
            ret = ORTE_PROC_MY_DAEMON;
        } else {
            /* I was direct launched and do not have
             * a daemon, so I have to route direct */
            ret = target;
        }
        goto found;
    }

    /* if I am a tool, the route is direct if target is in
     * my own job family, and to the target's HNP if not
     */
    if (ORTE_PROC_IS_TOOL) {
        if (ORTE_JOB_FAMILY(target->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            ret = target;
            goto found;
        } else {
            ORTE_HNP_NAME_FROM_JOB(&daemon, target->jobid);
            ret = &daemon;
            goto found;
        }
    }

    /******     HNP AND DAEMONS ONLY     ******/
    /* IF THIS IS FOR A DIFFERENT JOB FAMILY... */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* if I am a daemon, route this via the HNP */
        if (ORTE_PROC_IS_DAEMON) {
            ret = ORTE_PROC_MY_HNP;
            goto found;
        }

        /* if I am the HNP, then I stored a route to
         * this job family, so look it up
         */
        jfamily = ORTE_JOB_FAMILY(target->jobid);
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                continue;
            }
            if (jfam->job_family == jfamily) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routed_direct: route to %s found",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOB_FAMILY_PRINT(target->jobid)));
                ret = &jfam->route;
                goto found;
            }
        }
        /* not found - so we have no route */
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* THIS CAME FROM OUR OWN JOB FAMILY... */
    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, target)) {
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                    "%s routing direct to the HNP",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ret = ORTE_PROC_MY_HNP;
        goto found;
    }

    daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    /* find out what daemon hosts this proc */
    if (ORTE_VPID_INVALID == (daemon.vpid = orte_get_proc_daemon_vpid(target))) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* if the daemon is me, then send direct to the target! */
    if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
        ret = target;
        goto found;
    }

    /* else route to this daemon directly */
    ret = &daemon;

 found:
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s routed_direct_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target),
                         ORTE_NAME_PRINT(ret)));

    return *ret;
}


static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t *buffer,
                     orte_rml_tag_t tag, void *cbdata)
{
    bool *ack_waiting = (bool*)cbdata;

    /* flag as complete */
    *ack_waiting = false;
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
            
            /* extract the hnp name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* set the contact info into the hash table */
            orte_rml.set_contact_info(orte_process_info.my_hnp_uri);    
            /* the HNP is my lifeline */
            lifeline = ORTE_PROC_MY_HNP;

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
    if (NULL == ndat) {
        /* if we were direct launched, there is nothing we need to do. If we
         * were launched by mpirun, then we need to set the HNP and daemon info */
        if (NULL != orte_process_info.my_hnp_uri) {
            /* extract the hnp name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* we don't set the HNP's contact info as we don't need it - we
             * only contact our local daemon, which might be the HNP (in which
             * case it will have also been passed as our daemon uri) */
        }

        if (NULL != orte_process_info.my_daemon_uri) {
            /* extract the daemon's name so we can update the routing table */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                               ORTE_PROC_MY_DAEMON, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            orte_rml.set_contact_info(orte_process_info.my_daemon_uri);
            /* my daemon is my lifeline */
            lifeline = ORTE_PROC_MY_DAEMON;
        }
        return ORTE_SUCCESS;
    }

    /* if ndat != NULL, then this is being invoked by the proc to
     * init a route to a specified process that is outside of our
     * job family. We want that route to go through our HNP, routed via
     * out local daemon - however, we cannot know for
     * certain that the HNP already knows how to talk to the specified
     * procs. For example, in OMPI's publish/subscribe procedures, the
     * DPM framework looks for an mca param containing the global ompi-server's
     * uri. This info will come here so the proc can setup a route to
     * the server - we need to pass the routing info to our HNP.
     *
     * Obviously, if we were direct launched, we won't have an HNP, in
     * which case we just update our own contact info and go direct
     */
    if (NULL == orte_process_info.my_hnp_uri) {
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                    "%s routed_direct: init routes w/non-NULL data and direct launched",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else {
        opal_buffer_t *xfer;
        orte_rml_cmd_flag_t cmd=ORTE_RML_UPDATE_CMD;
        bool ack_waiting;

        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_direct: init routes w/non-NULL data",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
        if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) != ORTE_JOB_FAMILY(job)) {
            /* if this is for a different job family, then we route via our HNP
             * to minimize connection counts to entities such as ompi-server, so
             * start by sending the contact info to the HNP for update
             */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                 "%s routed_direct_init_routes: diff job family - sending update to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
                
            /* prep the buffer for transmission to the HNP */
            xfer = OBJ_NEW(opal_buffer_t);
            opal_dss.pack(xfer, &cmd, 1, ORTE_RML_CMD);
            opal_dss.copy_payload(xfer, ndat);

            /* save any new connections for use in subsequent connect_accept calls */
            orte_routed_base_update_hnps(ndat);

            if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, xfer,
                                                  ORTE_RML_TAG_RML_INFO_UPDATE,
                                                  orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(xfer);
                return rc;
            }

            /* wait right here until the HNP acks the update to ensure that
             * any subsequent messaging can succeed
             */
            ack_waiting = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                    ORTE_RML_TAG_UPDATE_ROUTE_ACK,
                                    ORTE_RML_NON_PERSISTENT,
                                    recv_ack, &ack_waiting);
            ORTE_WAIT_FOR_COMPLETION(ack_waiting);                

            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                 "%s routed_direct_init_routes: ack recvd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
            /* our get_route function automatically routes all messages for
             * other job families via the HNP, so nothing more to do here
             */
        }
    }

    return ORTE_SUCCESS;
}

static int route_lost(const orte_process_name_t *route)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    orte_routed_jobfam_t *jfam;
    uint16_t jfamily;
    int i;

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s route to %s lost",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(route)));

    /* if the route is to a different job family and we are the HNP, look it up */
    if ((ORTE_JOB_FAMILY(route->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) &&
        ORTE_PROC_IS_HNP) {
        jfamily = ORTE_JOB_FAMILY(route->jobid);
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                continue;
            }
            if (jfam->job_family == jfamily) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routed_direct: route to %s lost",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOB_FAMILY_PRINT(route->jobid)));
                opal_pointer_array_set_item(&orte_routed_jobfams, i, NULL);
                OBJ_RELEASE(jfam);
                break;
            }
        }
    }

    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, tell the OOB to abort.
     * NOTE: we cannot call abort from here as the OOB needs to first
     * release a thread-lock - otherwise, we will hang!!
     */
    if (!orte_finalizing &&
        NULL != lifeline &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route, lifeline)) {
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                             "%s routed:direct: Connection to lifeline %s lost",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(lifeline)));
        return ORTE_ERR_FATAL;
    }

    /* if we are the HNP, and the route is a daemon,
     * see if it is one of our children - if so, remove it
     */
    if (ORTE_PROC_IS_HNP &&
        route->jobid == ORTE_PROC_MY_NAME->jobid) {
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            if (child->vpid == route->vpid) {
                opal_list_remove_item(&my_children, item);
                OBJ_RELEASE(item);
                return ORTE_SUCCESS;
            }
        }
    }

    /* we don't care about this one, so return success */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    /* all routes are defined */
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s routed:direct: set lifeline to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    mylifeline = *proc;
    lifeline = &mylifeline;
    return ORTE_SUCCESS;
}

static void update_routing_plan(void)
{
    orte_routed_tree_t *child;
    int32_t i;
    orte_job_t *jdata;
    orte_proc_t *proc;

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s routed:direct: update routing plan",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (!ORTE_PROC_IS_HNP) {
        /* nothing to do */
        return;
    }

    /* clear the current list */
    OPAL_LIST_DESTRUCT(&my_children);
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    
    /* HNP is directly connected to each daemon */
    if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
    for (i=1; i < jdata->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        child = OBJ_NEW(orte_routed_tree_t);
        child->vpid = proc->name.vpid;
        opal_list_append(&my_children, &child->super);
    }

    return;
}

static void get_routing_list(opal_list_t *coll)
{
    
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s routed:direct: get routing list",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if I am anything other than daemons and the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON && !ORTE_PROC_IS_HNP) {
        return;
    }
    
    orte_routed_base_xcast_routing(coll, &my_children);
}

static int get_wireup_info(opal_buffer_t *buf)
{
    int rc;

    if (ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    return ORTE_SUCCESS;
}

static size_t num_routes(void)
{
    if (!ORTE_PROC_IS_HNP) {
        return 0;
    }
    return opal_list_get_size(&my_children);
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

