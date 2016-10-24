/*
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
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
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_binomial.h"

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
static int set_lifeline(orte_process_name_t *proc);
static size_t num_routes(void);

#if OPAL_ENABLE_FT_CR == 1
static int binomial_ft_event(int state);
#endif

orte_routed_module_t orte_routed_binomial_module = {
    .initialize = init,
    .finalize = finalize,
    .delete_route = delete_route,
    .update_route = update_route,
    .get_route = get_route,
    .init_routes = init_routes,
    .route_lost = route_lost,
    .route_is_defined = route_is_defined,
    .set_lifeline = set_lifeline,
    .update_routing_plan = update_routing_plan,
    .get_routing_list = get_routing_list,
    .num_routes = num_routes,
#if OPAL_ENABLE_FT_CR == 1
    .ft_event = binomial_ft_event
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
                         "%s routed_binomial_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));


    /* THIS CAME FROM OUR OWN JOB FAMILY...there is nothing
     * to do here. The routes will be redefined when we update
     * the routing tree
     */

    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{
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
                         "%s routed_binomial_update: %s --> %s",
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

    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret, daemon;
    opal_list_item_t *item;
    orte_routed_tree_t *child;

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
        /*ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);*/
        ret = ORTE_NAME_INVALID;
        goto found;
    }

    /* if the daemon is me, then send direct to the target! */
    if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
        ret = target;
        goto found;
    }

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

    /* if we get here, then the target daemon is not beneath
     * any of our children, so we have to step up through our parent
     */
    daemon.vpid = ORTE_PROC_MY_PARENT->vpid;

    ret = &daemon;

 found:
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                         "%s routed_binomial_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target),
                         ORTE_NAME_PRINT(ret)));

    return *ret;
}

static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    /* the binomial module routes all proc communications through
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
                             "%s routed_binomial: init routes for daemon job %s\n\thnp_uri %s",
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
                             "%s routed_binomial: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        return ORTE_SUCCESS;
    }


    if (ORTE_PROC_IS_HNP) {

        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_binomial: init routes for HNP job %s",
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
       /* if ndat=NULL, then we are being called during orte_init. In this
         * case, we need to setup a few critical pieces of info
         */

        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_framework.framework_output,
                             "%s routed_binomial: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));

        /* if we are a singleton and we have not spawned our
         * supporting HNP, then we don't route and don't need
         * the corresponding URIs
         */
        if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
            !orte_routing_is_enabled) {
            return ORTE_SUCCESS;
        }

        if (NULL == orte_process_info.my_daemon_uri) {
            /* in this module, we absolutely MUST have this information - if
             * we didn't get it, then error out
             */
            opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: This is a fatal condition when the binomial router",
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
        orte_routing_is_enabled = true;
        return ORTE_SUCCESS;
    }
}

static int route_lost(const orte_process_name_t *route)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child;

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s route to %s lost",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(route)));

    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, tell the OOB to abort.
     * NOTE: we cannot call abort from here as the OOB needs to first
     * release a thread-lock - otherwise, we will hang!!
     */
    if (!orte_finalizing &&
        NULL != lifeline &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route, lifeline)) {
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                             "%s routed:binomial: Connection to lifeline %s lost",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(lifeline)));
        return ORTE_ERR_FATAL;
    }

    /* if we are the HNP or a daemon, is it a daemon, and one of my children? if so, then
     * remove it from the child list
     */
    if ((ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) &&
        route->jobid == ORTE_PROC_MY_NAME->jobid) {
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            if (child->vpid == route->vpid) {
                OPAL_OUTPUT_VERBOSE((4, orte_routed_base_framework.framework_output,
                                     "%s routed_binomial: removing route to child daemon %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(route)));
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

static int binomial_tree(int rank, int parent, int me, int num_procs,
                         int *nchildren, opal_list_t *childrn,
                         opal_bitmap_t *relatives, bool mine)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;

    OPAL_OUTPUT_VERBOSE((3, orte_routed_base_framework.framework_output,
                         "%s routed:binomial rank %d parent %d me %d num_procs %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         rank, parent, me, num_procs));

    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);

        hibit = opal_hibit(rank, bitmap);
        --bitmap;

        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                child = OBJ_NEW(orte_routed_tree_t);
                child->vpid = peer;
                OPAL_OUTPUT_VERBOSE((3, orte_routed_base_framework.framework_output,
                                     "%s routed:binomial %d found child %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     rank,
                                     ORTE_VPID_PRINT(child->vpid)));

                if (mine) {
                    /* this is a direct child - add it to my list */
                    opal_list_append(childrn, &child->super);
                    (*nchildren)++;
                    /* setup the relatives bitmap */
                    opal_bitmap_init(&child->relatives, num_procs);

                    /* point to the relatives */
                    relations = &child->relatives;
                } else {
                    /* we are recording someone's relatives - set the bit */
                    opal_bitmap_set_bit(relatives, peer);
                    /* point to this relations */
                    relations = relatives;
                }
                /* search for this child's relatives */
                binomial_tree(0, 0, peer, num_procs, nchildren, childrn, relations, false);
            }
        }
        return parent;
    }

    /* find the children of this rank */
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                         "%s routed:binomial find children of rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rank));
    bitmap = opal_cube_dim(num_procs);

    hibit = opal_hibit(rank, bitmap);
    --bitmap;

    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                             "%s routed:binomial find children checking peer %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), peer));
        if (peer < num_procs) {
            OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                                 "%s routed:binomial find children computing tree",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* execute compute on this child */
            if (0 <= (found = binomial_tree(peer, rank, me, num_procs, nchildren, childrn, relatives, mine))) {
                OPAL_OUTPUT_VERBOSE((5, orte_routed_base_framework.framework_output,
                                     "%s routed:binomial find children returning found value %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), found));
                return found;
            }
        }
    }
    return -1;
}

static void update_routing_plan(void)
{
    orte_routed_tree_t *child;
    int j;
    opal_list_item_t *item;

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

    /* compute my direct children and the bitmap that shows which vpids
     * lie underneath their branch
     */
    ORTE_PROC_MY_PARENT->vpid = binomial_tree(0, 0, ORTE_PROC_MY_NAME->vpid,
                                   orte_process_info.max_procs,
                                   &num_children, &my_children, NULL, true);

    if (0 < opal_output_get_verbosity(orte_routed_base_framework.framework_output)) {
        opal_output(0, "%s: parent %d num_children %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_PROC_MY_PARENT->vpid, num_children);
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            opal_output(0, "%s: \tchild %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), child->vpid);
            for (j=0; j < (int)orte_process_info.max_procs; j++) {
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

static size_t num_routes(void)
{
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_framework.framework_output,
                         "%s num routes %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)opal_list_get_size(&my_children)));
    return opal_list_get_size(&my_children);
}

#if OPAL_ENABLE_FT_CR == 1
static int binomial_ft_event(int state)
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

