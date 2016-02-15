/*
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
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
#include "opal/runtime/opal_progress.h"
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
#include "routed_radix.h"


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
static int radix_ft_event(int state);
#endif

orte_routed_module_t orte_routed_radix_module = {
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
    radix_ft_event
#else
    NULL
#endif
};

/* local globals */
static orte_process_name_t      *lifeline=NULL;
static orte_process_name_t      local_lifeline;
static int                      num_children;
static opal_list_t              my_children;
static bool                     hnp_direct=true;

static int init(void)
{
    lifeline = NULL;

    /* setup the list of children */
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    num_children = 0;
    ORTE_PROC_MY_PARENT->jobid = ORTE_PROC_MY_NAME->jobid;

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    opal_list_item_t *item;

    lifeline = NULL;

    /* deconstruct the list of children */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&my_children);
    num_children = 0;

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
                         "%s routed_radix_delete_route for %s",
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
                                     "%s routed_binomial: deleting route to %s",
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
                         "%s routed_radix_update: %s --> %s",
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
                             "%s routed_radix_update: diff job family routing job %s --> %s",
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
                                     "%s routed_radix: updating route to %s via %s",
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
                             "%s routed_radix: adding route to %s",
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


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret, daemon;
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    int i;
    orte_routed_jobfam_t *jfam;
    uint16_t jfamily;

    if (!orte_routing_is_enabled) {
        ret = target;
        goto found;
    }

    /* initialize */
    daemon.jobid = ORTE_PROC_MY_DAEMON->jobid;
    daemon.vpid = ORTE_PROC_MY_DAEMON->vpid;

    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = target;
        goto found;
    }

    /* if I am an application process, always route via my local daemon */
    if (ORTE_PROC_IS_APP) {
        ret = ORTE_PROC_MY_DAEMON;
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

        /* if I am the HNP or a tool, then I stored a route to
         * this job family, so look it up
         */
        jfamily = ORTE_JOB_FAMILY(target->jobid);
        for (i=0; i < orte_routed_jobfams.size; i++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
                continue;
            }
            if (jfam->job_family == jfamily) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                     "%s routed_binomial: route to %s found",
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

    /* if this is going to the HNP, then send it direct if we don't know
     * how to get there - otherwise, send it via the tree
     */
    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, target)) {
        if (!hnp_direct || orte_static_ports) {
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                 "%s routing to the HNP through my parent %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_PARENT)));
            ret = ORTE_PROC_MY_PARENT;
            goto found;
        } else {
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                                 "%s routing direct to the HNP",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            ret = ORTE_PROC_MY_HNP;
            goto found;
        }
    }

    daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    /* find out what daemon hosts this proc */
    if (ORTE_VPID_INVALID == (daemon.vpid = orte_get_proc_daemon_vpid(target))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* if the daemon is me, then send direct to the target! */
    if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
        ret = target;
        goto found;
    } else if (orte_process_info.num_procs < mca_routed_radix_component.max_connections) {
        /* if the job is small enough, send direct to the target's daemon */
        ret = &daemon;
        goto found;
    } else {
        /* search routing tree for next step to that daemon */
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            if (child->vpid == daemon.vpid) {
                /* the child is hosting the proc - just send it there */
                ret = &daemon;
                goto found;
            }
            /* otherwise, see if the daemon we need is below the child */
            if (opal_bitmap_is_set_bit(&child->relatives, daemon.vpid)) {
                /* yep - we need to step through this child */
                daemon.vpid = child->vpid;
                ret = &daemon;
                goto found;
            }
        }
    }

    /* if we get here, then the target daemon is not beneath
     * any of our children, so we have to step up through our parent
     */
    daemon.vpid = ORTE_PROC_MY_PARENT->vpid;

    ret = &daemon;

found:
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_radix_get(%s) --> %s",
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
    /* the radix module routes all proc communications through
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
                             "%s routed_radix: init routes for daemon job %s\n\thnp_uri %s",
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
                             "%s routed_radix: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        return ORTE_SUCCESS;
    }


    if (ORTE_PROC_IS_HNP) {

        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_radix: init routes for HNP job %s",
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
        /* if we are a singleton and have not yet exec'd our HNP, then
         * just return success */
        if (ORTE_PROC_IS_SINGLETON && !orte_routing_is_enabled) {
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
         * the server - we need to pass the routing info to our HNP
         */
        if (NULL != ndat) {
            int rc;
            opal_buffer_t *xfer;
            orte_rml_cmd_flag_t cmd=ORTE_RML_UPDATE_CMD;
            bool ack_waiting;

            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                 "%s routed_radix: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

            if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) != ORTE_JOB_FAMILY(job)) {
                /* if this is for a different job family, then we route via our HNP
                 * to minimize connection counts to entities such as ompi-server, so
                 * start by sending the contact info to the HNP for update
                 */
                OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                                     "%s routed_radix_init_routes: diff job family - sending update to %s",
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
                                     "%s routed_radix_init_routes: ack recvd",
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
                             "%s routed_radix: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));

        if (NULL == orte_process_info.my_daemon_uri) {
            /* in this module, we absolutely MUST have this information - if
             * we didn't get it, then error out
             */
            opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: This is a fatal condition when the radix router",
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

        /* extract the daemon's name so we can update the routing table */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                           ORTE_PROC_MY_DAEMON, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* Set the contact info in the RML - this won't actually establish
         * the connection, but just tells the RML how to reach the daemon
         * if/when we attempt to send to it
         */
        orte_rml.set_contact_info(orte_process_info.my_daemon_uri);

        /* set our lifeline to the local daemon - we will abort if this connection is lost */
        lifeline = ORTE_PROC_MY_DAEMON;

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
                                     "%s routed_radix: route to %s lost",
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
                             "%s routed:radix: Connection to lifeline %s lost",
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
                                         "%s routed_radix: route to %s is defined",
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

static void radix_tree(int rank, int *num_children,
                       opal_list_t *children, opal_bitmap_t *relatives)
{
    int i, peer, Sum, NInLevel;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;

    /* compute how many procs are at my level */
    Sum=1;
    NInLevel=1;

    while ( Sum < (rank+1) ) {
        NInLevel *= mca_routed_radix_component.radix;
        Sum += NInLevel;
    }

    /* our children start at our rank + num_in_level */
    peer = rank + NInLevel;
    for (i = 0; i < mca_routed_radix_component.radix; i++) {
        if (peer < (int)orte_process_info.num_procs) {
            child = OBJ_NEW(orte_routed_tree_t);
            child->vpid = peer;
            if (NULL != children) {
                /* this is a direct child - add it to my list */
                opal_list_append(children, &child->super);
                (*num_children)++;
                /* setup the relatives bitmap */
                opal_bitmap_init(&child->relatives, orte_process_info.num_procs);
                /* point to the relatives */
                relations = &child->relatives;
            } else {
                /* we are recording someone's relatives - set the bit */
                if (OPAL_SUCCESS != opal_bitmap_set_bit(relatives, peer)) {
                    opal_output(0, "%s Error: could not set relations bit!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                }
                /* point to this relations */
                relations = relatives;
                OBJ_RELEASE(child);
            }
            /* search for this child's relatives */
            radix_tree(peer, NULL, NULL, relations);
        }
        peer += NInLevel;
    }
}

static void update_routing_plan(void)
{
    orte_routed_tree_t *child;
    int j;
    opal_list_item_t *item;
    int Level,Sum,NInLevel,Ii;
    int NInPrevLevel;

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
    num_children = 0;

    /* compute my parent */
    Ii =  ORTE_PROC_MY_NAME->vpid;
    Level=0;
    Sum=1;
    NInLevel=1;

    while ( Sum < (Ii+1) ) {
        Level++;
        NInLevel *= mca_routed_radix_component.radix;
        Sum += NInLevel;
    }
    Sum -= NInLevel;

    NInPrevLevel = NInLevel/mca_routed_radix_component.radix;

    if( 0 == Ii ) {
        ORTE_PROC_MY_PARENT->vpid = -1;
    }  else {
        ORTE_PROC_MY_PARENT->vpid = (Ii-Sum) % NInPrevLevel;
        ORTE_PROC_MY_PARENT->vpid += (Sum - NInPrevLevel);
    }

    /* compute my direct children and the bitmap that shows which vpids
     * lie underneath their branch
     */
    radix_tree(Ii, &num_children, &my_children, NULL);

    if (0 < opal_output_get_verbosity(orte_routed_base_framework.framework_output)) {
        opal_output(0, "%s: parent %d num_children %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_PROC_MY_PARENT->vpid, num_children);
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            opal_output(0, "%s: \tchild %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), child->vpid);
            for (j=0; j < (int)orte_process_info.num_procs; j++) {
                if (opal_bitmap_is_set_bit(&child->relatives, j)) {
                    opal_output(0, "%s: \t\trelation %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
                }
            }
        }
    }
}

static void get_routing_list(opal_list_t *coll)
{
    /* if I am anything other than a daemon or the HNP, this
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
static int radix_ft_event(int state)
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

