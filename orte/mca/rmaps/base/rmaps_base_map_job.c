/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <string.h>

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"


/*
 * Local functions
 */
static orte_rmaps_base_module_t *select_preferred(char *name);
static orte_rmaps_base_module_t *select_any(void);


/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmaps_base_map_job(orte_jobid_t job, opal_list_t *incoming_attributes)
{
    orte_rmaps_base_module_t *module=NULL;
    orte_attribute_t *attr;
    char *desired_mapper;
    opal_list_t working_attrs;
    opal_list_t *attributes;
    opal_list_item_t *item;
    orte_jobid_t *jptr, parent_job=ORTE_JOBID_INVALID;
    orte_job_map_t *map;
    orte_std_cntr_t scntr;
    int rc;
    bool using_local_attr=false;
    
    /* check for NULL attributes - we need this list locally */
    if (NULL == incoming_attributes) {
        attributes = OBJ_NEW(opal_list_t);
        using_local_attr = true;
    } else {
        attributes = incoming_attributes;
    }
    
    /* if we are not on the head node, use the proxy component */
    if (!orte_process_info.seed) {
        return orte_rmaps_base_proxy_map_job(job, attributes);
    }
    
    /* check the attributes to see if anything in the environment
     * has been overridden. If not, then install the environment
     * values to correctly control the behavior of the RMAPS component.
     */
    
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_USE_PARENT_PLAN))) {
        /* was provided - lookup the specified jobid's mapping plan and use it. This
         * includes the FULL list of mapping attributes that were used. We will
         * subsequently override those settings with anything that was specifically
         * provided by the caller
         */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        parent_job = *jptr;
        /* lookup that job's mapping policy */
        OBJ_CONSTRUCT(&working_attrs, opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_mapping_plan(parent_job, &working_attrs))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&working_attrs);
            goto CLEANUP;
        }
        /* go through the parent policy and "fill" anything that was missing in the
         * list of attributes provided. We specifically don't overwrite anything provided
         * by the caller - the caller is allowed to "override" any specific attribute
         * of the parent's plan
         */
        if (ORTE_SUCCESS != (rc = orte_rmgr.merge_attributes(attributes, &working_attrs,
                                                             ORTE_RMGR_ATTR_NO_OVERRIDE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&working_attrs);
            goto CLEANUP;
        }
        /* clean up */
        while (NULL != (item = opal_list_remove_first(&working_attrs))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&working_attrs);
    }


    /* check the mapping policy - set it IFF the user specified it in the environment */
    if (orte_rmaps_base.user_specified_policy) {
        if (orte_rmaps_base.bynode) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_MAP_POLICY,
                                                              ORTE_STRING, "bynode",
                                                              ORTE_RMGR_ATTR_NO_OVERRIDE))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_MAP_POLICY,
                                                              ORTE_STRING, "byslot",
                                                              ORTE_RMGR_ATTR_NO_OVERRIDE))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }            
        }
    }
       
    /* check pernode - add it if it was set by the environment. Note that this
     * attribute only cares if it exists - its value is irrelevant and hence
     * not provided
     */
    if (orte_rmaps_base.per_node) {
        if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_PERNODE,
                                                          ORTE_UNDEF, NULL,
                                                          ORTE_RMGR_ATTR_NO_OVERRIDE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* check n_pernode - add it if it was set by the environment. Note that this
     * attribute does convey a value as well
     */
    if (0 < orte_rmaps_base.n_per_node) {
        scntr = (orte_std_cntr_t)orte_rmaps_base.n_per_node;
        if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_N_PERNODE,
                                                          ORTE_STD_CNTR, &scntr,
                                                          ORTE_RMGR_ATTR_NO_OVERRIDE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* check no_local - add it if it was set by the environment. Note that this
    * attribute only cares if it exists - its value is irrelevant and hence
    * not provided
    */
    if (orte_rmaps_base.no_use_local) {
        if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_NO_USE_LOCAL,
                                                          ORTE_UNDEF, NULL,
                                                          ORTE_RMGR_ATTR_NO_OVERRIDE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* check no-oversubscribe - add it if it was set by the environment. Note that this
    * attribute only cares if it exists - its value is irrelevant and hence
    * not provided
    */
    if (!orte_rmaps_base.oversubscribe) {
        if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_NO_OVERSUB,
                                                          ORTE_UNDEF, NULL,
                                                          ORTE_RMGR_ATTR_NO_OVERRIDE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* see if they provided a desired mapper */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_DESIRED_MAPPER))) {
        /* they did - extract its name */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&desired_mapper, attr->value, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        module = select_preferred(desired_mapper);
    } else {
        module = select_any();
    }
    
    /* check for error */
    if (NULL == module) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        opal_output(orte_rmaps_base.rmaps_output,
                    "orte:rmaps:base:map: could not find desired mapper component %s", desired_mapper);
        rc = ORTE_ERR_NOT_FOUND;
        goto CLEANUP;
    }
    
    /* go ahead and map the job */
    if (ORTE_SUCCESS != (rc = module->map_job(job, attributes))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* store the mapping plan in case we need it later. We need to do this AFTER
     * the mapping component finishes in case the component added/modified the
     * attributes. The component should, at the least, have updated the
     * attribute indicating where it stopped so that any subsequent mappings by
     * child jobs can know where to start
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_store_mapping_plan(job, attributes))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if we were using a parent policy, then we need to update that job's info
     * on where we finished mapping. The mapping components provide that info
     * via the attributes
     */
    if (ORTE_JOBID_INVALID != parent_job) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_update_mapping_state(parent_job, attributes))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* if we wanted to display the map, now is the time to do it */
    if (orte_rmaps_base.display_map ||
        NULL != orte_rmgr.find_attribute(attributes, ORTE_RMAPS_DISPLAY_AFTER_MAP)) {
        orte_rmaps.get_job_map(&map, job);
        orte_dss.dump(0, map, ORTE_JOB_MAP);
    }
    
    
CLEANUP:
    /* if we setup our own local attribute list, then get rid of it */
    if (using_local_attr) {
        OBJ_RELEASE(attributes);
    }
    
    return rc;
}


static orte_rmaps_base_module_t *select_preferred(char *name)
{
    opal_list_item_t *item;
    orte_rmaps_base_cmp_t *cmp;

    /* Look for a matching selected name */

    opal_output(orte_rmaps_base.rmaps_output,
                "orte:rmaps:base:map: looking for component %s", name);
    for (item = opal_list_get_first(&orte_rmaps_base.rmaps_available);
         item != opal_list_get_end(&orte_rmaps_base.rmaps_available);
         item = opal_list_get_next(item)) {
        cmp = (orte_rmaps_base_cmp_t *) item;

        if (0 == strcmp(name, 
                        cmp->component->rmaps_version.mca_component_name)) {
            opal_output(orte_rmaps_base.rmaps_output,
                        "orte:rmaps:base:map: found module for component %s", name);
            return cmp->module;
        }
    }

    /* Didn't find a matching name */

    opal_output(orte_rmaps_base.rmaps_output,
                "orte:rmaps:base:map: did not find module for compoent %s", name);
    return NULL;
}


static orte_rmaps_base_module_t *select_any(void)
{
    opal_list_item_t *item;
    orte_rmaps_base_cmp_t *cmp;

    /* If the list is empty, return NULL */

    if (opal_list_is_empty(&orte_rmaps_base.rmaps_available)) {
        opal_output(orte_rmaps_base.rmaps_output,
                    "orte:rmaps:base:map: no components available!");
        return NULL;
    }

    /* Otherwise, return the first item (it's already sorted in
       priority order) */

    item = opal_list_get_first(&orte_rmaps_base.rmaps_available);
    cmp = (orte_rmaps_base_cmp_t *) item;
    opal_output(orte_rmaps_base.rmaps_output,
                "orte:rmaps:base:map: highest priority component: %s",
                cmp->component->rmaps_version.mca_component_name);
    return cmp->module;
}
