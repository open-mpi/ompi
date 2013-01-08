/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_hdmon.h"

/* the hdmon module supports a network of daemons that host
 * no local apps - they are simply monitoring the system
 * resource usage. Thus, they use static ports to wire
 * themselves up to a set of identified masters via a
 * radix topology. If a peer dies, the daemon will rewire
 * the network by reconnecting to the downstream peer(s).
 */

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
static int hdmon_ft_event(int state);
#endif

orte_routed_module_t orte_routed_hdmon_module = {
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
    hdmon_ft_event
#else
    NULL
#endif
};

/* local globals */
static orte_process_name_t *lifeline=NULL;
static orte_process_name_t local_lifeline;
static int num_children;
static opal_list_t my_children;
static orte_job_t *daemons=NULL;

static int init(void)
{
    lifeline = NULL;
    
    /* setup the list of children */
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    num_children = 0;
    ORTE_PROC_MY_PARENT->jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* get the daemon object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

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
    /* There is nothing to do here. The routes will be
     * redefined when we update the routing tree
     */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    /* There is nothing to do here. The routes will be
     * redefined when we update the routing tree
     */
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret, step;
    opal_list_item_t *item;
    orte_routed_tree_t *child;

    if (!orte_routing_is_enabled) {
        ret = target;
        goto found;
    }
    step.jobid = ORTE_PROC_MY_NAME->jobid;

    /* if the target is me, then send direct to the target! */
    if (ORTE_PROC_MY_NAME->vpid == target->vpid) {
        ret = target;
        goto found;
    } else {
        /* search routing tree for next step to that target */
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            if (child->vpid == target->vpid) {
                /* this is the target - just send it there */
                ret = target;
                goto found;
            }
            /* otherwise, see if the target we need is below the child */
            if (opal_bitmap_is_set_bit(&child->relatives, target->vpid)) {
                /* yep - we need to step through this child */
                step.vpid = child->vpid;
                ret = &step;
                goto found;
            }
        }
    }
    
    /* if we get here, then the target is not beneath
     * any of our children, so we have to step up through our parent
     */
    step.vpid = ORTE_PROC_MY_PARENT->vpid;
    
    ret = &step;
    
found:
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_hdmon_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}

static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_hdmon: init routes for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
        
    /* we use static ports, so set my lifeline to point at my parent */
    lifeline = ORTE_PROC_MY_PARENT;
            
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_hdmon: completed init routes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    return ORTE_SUCCESS;
}

static int route_lost(const orte_process_name_t *route)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    orte_proc_t *proc;
    orte_vpid_t n;

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s route to %s lost",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(route)));

    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, rewire to route to the parent of my lifeline
     */
    if (!orte_finalizing &&
        NULL != lifeline &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route, lifeline)) {
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed:hdmon: Connection to lifeline %s lost",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(lifeline)));
        /* if we are an app, just die */
        if (ORTE_PROC_IS_APP) {
            return ORTE_ERR_FATAL;
        }
        proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, ORTE_PROC_MY_PARENT->vpid);
        proc->alive = false;
        n=0;
        while (!proc->alive && n < orte_process_info.num_procs) {
            proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, proc->parent);
            n++;
        }
        if (!proc->alive) {
            /* couldn't find a new parent - we must be last man alive */
            ORTE_PROC_MY_PARENT->vpid = -1;
        } else {
            ORTE_PROC_MY_PARENT->vpid = proc->name.vpid;
        }
        return ORTE_SUCCESS;
    }

    /* if we are the HNP or daemon, and the route is a daemon,
     * see if it is one of our children. If so, remove it and
     * let the children beneath it reconnect to us
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
    /* by definition, we always have a route */
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
        NInLevel *= mca_routed_hdmon_component.radix;
        Sum += NInLevel;
    }
    
    /* our children start at our rank + num_in_level */
    peer = rank + NInLevel;
    for (i = 0; i < mca_routed_hdmon_component.radix; i++) {
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
    int level, nlevel;
    orte_proc_t *proc;
    orte_vpid_t sum, v, nprev;

    /* clear the list of children if any are already present */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    num_children = 0;
    
    /* compute all parents */
    for (v = 1 ; v < orte_process_info.num_procs ; v++) {
        level = 0;
        sum = 1;
        nlevel = 1;

        while (sum < (v+1)) {
            level++;
            nlevel *= mca_routed_hdmon_component.radix;
            sum += nlevel;
        }
        sum -= nlevel;
        nprev = nlevel/mca_routed_hdmon_component.radix;
        proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v);
        proc->parent = ((v-sum) % nprev) + (sum - nprev);
        opal_output_verbose(2, orte_routed_base_output,
                            "%s proc %s has parent %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_VPID_PRINT(v), ORTE_VPID_PRINT(proc->parent));
    }
    /* set the parent for v=0 */
    proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, 0);
    proc->parent = -1;

    /* now set my parent */
    proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, ORTE_PROC_MY_NAME->vpid);
    ORTE_PROC_MY_PARENT->vpid = proc->parent;
    
    /* compute my direct children and the bitmap that shows which vpids
     * lie underneath their branch
     */
    radix_tree(ORTE_PROC_MY_NAME->vpid, &num_children, &my_children, NULL);
    
    if (0 < opal_output_get_verbosity(orte_routed_base_output)) {
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

static void get_routing_list(orte_grpcomm_coll_t type,
                             orte_grpcomm_collective_t *coll)
{
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
    /* we use static ports, so just return */
    return ORTE_SUCCESS;
}

static size_t num_routes(void)
{
    return opal_list_get_size(&my_children);
}

#if OPAL_ENABLE_FT_CR == 1
static int hdmon_ft_event(int state)
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

