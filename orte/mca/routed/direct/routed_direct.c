/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/threads/condition.h"
#include "opal/runtime/opal_progress.h"
#include "opal/dss/dss_types.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

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
static int update_routing_tree(void);
static orte_vpid_t get_routing_tree(opal_list_t *children);
static int get_wireup_info(opal_buffer_t *buf);
static int set_lifeline(orte_process_name_t *proc);

#if OPAL_ENABLE_FT == 1
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
    update_routing_tree,
    get_routing_tree,
    get_wireup_info,
#if OPAL_ENABLE_FT == 1
    direct_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_condition_t         cond;
static opal_mutex_t             lock;


static int init(void)
{
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_direct_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
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
    } else {
        /* all routes are direct */
        ret = target;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_direct_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    int rc;
    
    /* if ndat=NULL, then we are being called during orte_init. In this
     * case, there is nothing to do
     */
    if (NULL == ndat) {
        return ORTE_SUCCESS;
    }
    
    /* if ndat != NULL, then this is being invoked by the proc to
     * init a route to a specified process that is outside of our
     * job family. It really doesn't matter as everything must
     * go direct
     */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
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

static int update_routing_tree(void)
{
    /* this is a meaningless command for a direct as I am not allowed to route */
    return ORTE_ERR_NOT_SUPPORTED;
}

static orte_vpid_t get_routing_tree(opal_list_t *children)
{
    /* this is a meaningless command for a direct as I am not allowed to route */
    return ORTE_VPID_INVALID;
}

static int get_wireup_info(opal_buffer_t *buf)
{
    /* this is a meaningless command for a direct as I am not allowed to route */
    return ORTE_ERR_NOT_SUPPORTED;
}


#if OPAL_ENABLE_FT == 1
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

