/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stddef.h>

#include "opal/dss/dss.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_debruijn.h"


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
static int debruijn_ft_event(int state);
#endif

orte_routed_module_t orte_routed_debruijn_module = {
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
    debruijn_ft_event
#else
    NULL
#endif
};

/* local globals */
static orte_process_name_t      *lifeline=NULL;
static orte_process_name_t      local_lifeline;
static opal_list_t              my_children;
static bool                     hnp_direct=true;
static int                      log_nranks;
static int                      log_npeers;
static unsigned int             rank_mask;

static int init(void)
{
    lifeline = NULL;
    
    /* setup the list of children */
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    ORTE_PROC_MY_PARENT->jobid = ORTE_PROC_MY_NAME->jobid;
    
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int rc;
    opal_list_item_t *item;

    /* if I am an application process, indicate that I am
        * truly finalizing prior to departure
        */
    if (ORTE_PROC_IS_APP) {
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(false))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
   
    lifeline = NULL;

    /* deconstruct the list of children */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&my_children);

    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    int i;
    orte_routed_jobfam_t *jfam;
    uint16_t jfamily;

    if (proc->jobid == ORTE_JOBID_INVALID ||
        proc->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* if I am an application process, I don't have any routes
     * so there is nothing for me to do
     */
    if (!ORTE_PROC_IS_HNP && !ORTE_PROC_IS_DAEMON &&
        !ORTE_PROC_IS_TOOL) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_debruijn_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    
    /* if this is from a different job family, then I need to
     * look it up appropriately
     */
    if (ORTE_JOB_FAMILY(proc->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so I have nothing
         * in my routing table and thus have nothing to do
         * here, just return
         */
        if (ORTE_PROC_IS_DAEMON) {
            return ORTE_SUCCESS;
        }
        
        /* see if this job family is present */
        jfamily = ORTE_JOB_FAMILY(proc->jobid);
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                continue;
            }
            if (jfam->job_family == jfamily) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routed_debruijn: deleting route to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOB_FAMILY_PRINT(proc->jobid)));
                opal_pointer_array_set_item(&orte_routed_jobfams, i, NULL);
                OBJ_RELEASE(jfam);
                return ORTE_SUCCESS;
            }
        }        
        /* not present - nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY...there is nothing
     * to do here. The routes will be redefined when we update
     * the routing tree
     */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    int i;
    orte_routed_jobfam_t *jfam;
    uint16_t jfamily;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* if I am an application process, we don't update the route since
     * we automatically route everything through the local daemon
     */
    if (ORTE_PROC_IS_APP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_debruijn_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* if I am a daemon and the target is my HNP, then check
     * the route - if it isn't direct, then we just flag that
     * we have a route to the HNP
     */
    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, target) &&
        OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, route)) {
        hnp_direct = false;
        return ORTE_SUCCESS;
    }

    /* if this is from a different job family, then I need to
     * track how to send messages to it
     */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so nothing to do
         * here, just return
         */
        if (ORTE_PROC_IS_DAEMON) {
            return ORTE_SUCCESS;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_debruijn_update: diff job family routing job %s --> %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(target->jobid), 
                             ORTE_NAME_PRINT(route)));
        
        /* see if this target is already present */
        jfamily = ORTE_JOB_FAMILY(target->jobid);
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                continue;
            }
            if (jfam->job_family == jfamily) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routed_debruijn: updating route to %s via %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOB_FAMILY_PRINT(target->jobid),
                                     ORTE_NAME_PRINT(route)));
                jfam->route.jobid = route->jobid;
                jfam->route.vpid = route->vpid;
                return ORTE_SUCCESS;
            }
        }

        /* not there, so add the route FOR THE JOB FAMILY*/
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                             "%s routed_debruijn: adding route to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOB_FAMILY_PRINT(target->jobid)));
        jfam = OBJ_NEW(orte_routed_jobfam_t);
        jfam->job_family = jfamily;
        jfam->route.jobid = route->jobid;
        jfam->route.vpid = route->vpid;
        opal_pointer_array_add(&orte_routed_jobfams, jfam);
        return ORTE_SUCCESS;
    }
    
    return ORTE_SUCCESS;
}

static inline unsigned int debruijn_next_hop (int target)
{
    const int my_id = ORTE_PROC_MY_NAME->vpid;
    uint64_t route, mask = rank_mask;
    unsigned int i, next_hop;

    if (target == my_id) {
        return my_id;
    }

    i = -log_npeers;
    do {
        i += log_npeers;
        mask = (mask >> i) << i;
        route = (my_id << i) | target;
    } while ((route & mask) != (((my_id << i) & target) & mask));

    next_hop = (int)((route >> (i - log_npeers)) & rank_mask);

    /* if the next hop does not exist route to the lowest proc with the same lower routing bits */
    return (next_hop < orte_process_info.num_procs) ? next_hop : (next_hop & (rank_mask >> log_npeers));
}

static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_routed_jobfam_t *jfam;
    orte_process_name_t ret;
    uint16_t jfamily;
    int i;

    /* initialize */

    do {
        ret = *ORTE_NAME_INVALID;

        if (ORTE_JOBID_INVALID == target->jobid ||
            ORTE_VPID_INVALID == target->vpid) {
            break;
        }

        /* if it is me, then the route is just direct */
        if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
            ret = *target;
            break;
        }

        /* if I am an application process, always route via my local daemon */
        if (ORTE_PROC_IS_APP) {
            ret = *ORTE_PROC_MY_DAEMON;
            break;
        }

        /* if I am a tool, the route is direct if target is in
         * my own job family, and to the target's HNP if not
         */
        if (ORTE_PROC_IS_TOOL) {
            if (ORTE_JOB_FAMILY(target->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
                ret = *target;
            } else {
                ORTE_HNP_NAME_FROM_JOB(&ret, target->jobid);
            }

            break;
        }
    
        /******     HNP AND DAEMONS ONLY     ******/

        /* IF THIS IS FOR A DIFFERENT JOB FAMILY... */
        if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            /* if I am a daemon, route this via the HNP */
            if (ORTE_PROC_IS_DAEMON) {
                ret = *ORTE_PROC_MY_HNP;
                break;
            }
        
            /* if I am the HNP or a tool, then I stored a route to
             * this job family, so look it up
             */
            jfamily = ORTE_JOB_FAMILY(target->jobid);
            for (i = 0 ; i < orte_routed_jobfams.size ; ++i) {
                if (NULL ==
                    (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                    continue;
                }
                if (jfam->job_family == jfamily) {
                    ret = jfam->route;
                    break;
                }
            }

            /* not found - so we have no route */
            break;
        }
     
        /* THIS CAME FROM OUR OWN JOB FAMILY... */

        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, target)) {
            if (!hnp_direct || orte_static_ports) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routing to the HNP through my parent %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_PARENT)));
                ret = *ORTE_PROC_MY_PARENT;
            } else {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routing direct to the HNP",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                ret = *ORTE_PROC_MY_HNP;
            }

            break;
        }
    
        ret.jobid = ORTE_PROC_MY_NAME->jobid;
        /* find out what daemon hosts this proc */
        if (ORTE_VPID_INVALID == (ret.vpid = orte_get_proc_daemon_vpid(target))) {
            /* we don't yet know about this daemon. just route this to the "parent" */
            ret = *ORTE_PROC_MY_PARENT;
            break;
        }

        /* if the daemon is me, then send direct to the target! */
        if (ORTE_PROC_MY_NAME->vpid == ret.vpid) {
            ret = *target;
            break;
        }
    
        /* find next hop */
        ret.vpid = debruijn_next_hop (ret.vpid);
    } while (0);
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_debruijn_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(&ret)));
    
    return ret;
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
    /* the debruijn module routes all proc communications through
     * the local daemon. Daemons must identify which of their
     * daemon-peers is "hosting" the specified recipient and
     * route the message to that daemon. Daemon contact info
     * is handled elsewhere, so all we need to do here is
     * ensure that the procs are told to route through their
     * local daemon, and that daemons are told how to route
     * for each proc
     */
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
                             "%s routed_debruijn: init routes for daemon job %s\n\thnp_uri %s",
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

            /* if we are using static ports, set my lifeline to point at my parent */
            if (orte_static_ports) {
                lifeline = ORTE_PROC_MY_PARENT;
            } else {
                /* set our lifeline to the HNP - we will abort if that connection is lost */
                lifeline = ORTE_PROC_MY_HNP;
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
                             "%s routed_debruijn: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    

    if (ORTE_PROC_IS_HNP) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_debruijn: init routes for HNP job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job)));
        
        if (NULL == ndat) {
            /* the HNP has no lifeline */
            lifeline = NULL;
        } else {
            /* if this is for my own jobid, then I am getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_PROC_MY_NAME->jobid == job) {
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
                /* if not, then I need to process the callback */
                if (ORTE_SUCCESS != (rc = orte_routed_base_process_callback(job, ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }

        return ORTE_SUCCESS;
    }

    {  /* MUST BE A PROC */
        /* if ndat != NULL, then this is being invoked by the proc to
         * init a route to a specified process that is outside of our
         * job family. We want that route to go through our HNP, routed via
         * out local daemon - however, we cannot know for
         * certain that the HNP already knows how to talk to the specified
         * procs. For example, in OMPI's publish/subscribe procedures, the
         * DPM framework looks for an mca param containing the global ompi-server's
         * uri. This info will come here so the proc can setup a route to
         * the server - we need to pass the routing info to our HNP
         */
        if (NULL != ndat) {
            int rc;
            opal_buffer_t *xfer;
            orte_rml_cmd_flag_t cmd=ORTE_RML_UPDATE_CMD;
            bool ack_waiting;

            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                 "%s routed_debruijn: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) != ORTE_JOB_FAMILY(job)) {
                /* if this is for a different job family, then we route via our HNP
                 * to minimize connection counts to entities such as ompi-server, so
                 * start by sending the contact info to the HNP for update
                 */
                OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                     "%s routed_debruijn_init_routes: diff job family - sending update to %s",
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
                                     "%s routed_debruijn_init_routes: ack recvd",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
                /* our get_route function automatically routes all messages for
                 * other job families via the HNP, so nothing more to do here
                 */
            }
            return ORTE_SUCCESS;
        }
        
        /* if ndat=NULL, then we are being called during orte_init. In this
         * case, we need to setup a few critical pieces of info
         */
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_debruijn: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
                
        if (NULL == orte_process_info.my_daemon_uri) {
            /* in this module, we absolutely MUST have this information - if
             * we didn't get it, then error out
             */
            opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: This is a fatal condition when the debruijn router",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: has been selected - either select the unity router",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: or ensure that the local daemon info is provided",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return ORTE_ERR_FATAL;
        }
            
        /* we have to set the HNP's name, even though we won't route messages directly
         * to it. This is required to ensure that we -do- send messages to the correct
         * HNP name
         */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                           ORTE_PROC_MY_HNP, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
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
        
        /* set our lifeline to the local daemon - we will abort if this connection is lost */
        lifeline = ORTE_PROC_MY_DAEMON;
        
        /* register ourselves -this sends a message to the daemon (warming up that connection)
         * and sends our contact info to the HNP when all local procs have reported
         *
         * NOTE: it may seem odd that we send our contact info to the HNP - after all,
         * the HNP doesn't really need to know how to talk to us directly if we are
         * using this routing method. However, this is good for two reasons:
         *
         * (1) some debuggers and/or tools may need RML contact
         *     info to set themselves up
         *
         * (2) doing so allows the HNP to "block" in a dynamic launch
         *     until all procs are reported running, thus ensuring that no communication
         *     is attempted until the overall ORTE system knows how to talk to everyone -
         *     otherwise, the system can just hang.
         */
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* no answer is expected or coming */
        
        return ORTE_SUCCESS;
    }
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
                                     "%s routed_debruijn: route to %s lost",
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
                             "%s routed:debruijn: Connection to lifeline %s lost",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(lifeline)));
        return ORTE_ERR_FATAL;
    }

    /* if we are the HNP or daemon, and the route is a daemon,
     * see if it is one of our children - if so, remove it
     */
    if ((ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) &&
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
    int i;
    orte_routed_jobfam_t *jfam;
    uint16_t jfamily;

    /* if the route is to a different job family and we are the HNP, look it up */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        if (ORTE_PROC_IS_HNP) {
            jfamily = ORTE_JOB_FAMILY(target->jobid);
            for (i=0; i < orte_routed_jobfams.size; i++) {
                if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                    continue;
                }
                if (jfam->job_family == jfamily) {
                    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                         "%s routed_debruijn: route to %s is defined",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_JOB_FAMILY_PRINT(target->jobid)));
                    return true;
                }
            }
            return false;
        }
        /* if we are not the HNP, then the answer is always true as
         * we send it via the HNP
         */
        return true;
    }

    /* find out what daemon hosts this proc */
    if (ORTE_VPID_INVALID == orte_get_proc_daemon_vpid((orte_process_name_t*)target)) {
        return false;
    }
    
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    /* we have to copy the proc data because there is no
     * guarantee that it will be preserved
     */
    local_lifeline.jobid = proc->jobid;
    local_lifeline.vpid = proc->vpid;
    lifeline = &local_lifeline;
    
    return ORTE_SUCCESS;
}

static unsigned int ilog2 (unsigned int v)
{
    const unsigned int b[] = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
    const unsigned int S[] = {1, 2, 4, 8, 16};
    int i;

    register unsigned int r = 0;
    for (i = 4; i >= 0; i--) {
        if (v & b[i]) {
            v >>= S[i];
            r |= S[i];
        }
    }

    return r;
}

static void update_routing_plan(void)
{
    orte_routed_tree_t *child;
    opal_list_item_t *item;
    int my_vpid = ORTE_PROC_MY_NAME->vpid;
    int i;

    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON && !ORTE_PROC_IS_HNP) {
        return;
    }
    
    /* clear the list of children if any are already present */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    
    log_nranks = (int) ilog2 ((unsigned int)orte_process_info.num_procs) ;
    assert(log_nranks < 31);

    if (log_nranks < 3) {
      log_npeers = 1;
    } else if (log_nranks < 7) {
      log_npeers = 2;
    } else { 
      log_npeers = 4;
    }

    /* round log_nranks to a multiple of log_npeers */
    log_nranks = ((log_nranks + log_npeers) & ~(log_npeers - 1)) - 1;

    rank_mask = (1 << (log_nranks + 1)) - 1;

    /* compute my parent */
    ORTE_PROC_MY_PARENT->vpid = my_vpid ? my_vpid >> log_npeers : -1;

    /* only add peers to the routing tree if this rank is the smallest rank that will send to
       the any peer */
    if ((my_vpid >> (log_nranks + 1 - log_npeers)) == 0) {
        for (i = (1 << log_npeers) - 1 ; i >= 0 ; --i) {
            int next = ((my_vpid << log_npeers) | i) & rank_mask;

            /* add a peer to the routing tree only if its vpid is smaller than this rank */
            if (next > my_vpid && next < (int)orte_process_info.num_procs) {
                child = OBJ_NEW(orte_routed_tree_t);
                child->vpid = next;
                opal_list_append (&my_children, &child->super);
            }
        }
    }
}

static void get_routing_list(orte_grpcomm_coll_t type,
                             orte_grpcomm_collective_t *coll)
{
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON && !ORTE_PROC_IS_HNP) {
        return;
    }
    
    if (ORTE_GRPCOMM_XCAST == type) {
        orte_routed_base_xcast_routing(coll, &my_children);
    } else if (ORTE_GRPCOMM_COLL_RELAY == type) {
        orte_routed_base_coll_relay_routing(coll);
    } else if (ORTE_GRPCOMM_COLL_COMPLETE == type) {
        orte_routed_base_coll_complete_routing(coll);
    } else if (ORTE_GRPCOMM_COLL_PEERS == type) {
        orte_routed_base_coll_peers(coll, &my_children);
    }
}

static int get_wireup_info(opal_buffer_t *buf)
{
    int rc;
    int i;
    orte_routed_jobfam_t *jfam;

    if (ORTE_PROC_IS_HNP) {
        /* if we are not using static ports, then we need to share the
         * comm info - otherwise, just return
         */
        if (orte_static_ports) {
            return ORTE_SUCCESS;
        }
    
        if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* if I am an application, this is occurring during connect_accept.
     * We need to return the stored information of other HNPs we
     * know about, if any
     */
    if (ORTE_PROC_IS_APP) {
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL != (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                opal_dss.pack(buf, &(jfam->hnp_uri), 1, OPAL_STRING);
            }
        }
        return ORTE_SUCCESS;
    }

    return ORTE_SUCCESS;
}

static size_t num_routes(void)
{
    return opal_list_get_size(&my_children);
}

#if OPAL_ENABLE_FT_CR == 1
static int debruijn_ft_event(int state)
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

