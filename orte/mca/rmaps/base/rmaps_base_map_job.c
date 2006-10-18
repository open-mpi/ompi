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
int orte_rmaps_base_map_job(orte_jobid_t job, opal_list_t *attributes)
{
    orte_rmaps_base_module_t *module=NULL;
    orte_attribute_t *attr;
    char *desired_mapper;
    int rc;
    
    /* check the attributes to see if anything in the environment
     * has been overridden. If not, then install the environment
     * values to correctly control the behavior of the RMAPS component.
     */
    if (NULL == (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_MAP_POLICY))) {
        /* was NOT provided - use what was set by the environment */
        if (orte_rmaps_base.bynode) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_MAP_POLICY,
                                                              ORTE_STRING, "bynode"))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_MAP_POLICY,
                                                              ORTE_STRING, "byslot"))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }            
        }
    }
    
    if (NULL == (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_PERNODE))) {
        /* was NOT provided - add it if it was set by the environment. Note that this
         * attribute only cares if it exists - its value is irrelevant and hence
         * not provided
         */
        if (orte_rmaps_base.per_node) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_PERNODE,
                                                              ORTE_UNDEF, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    if (NULL == (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_NO_USE_LOCAL))) {
        /* was NOT provided - add it if it was set by the environment. Note that this
        * attribute only cares if it exists - its value is irrelevant and hence
        * not provided
        */
        if (orte_rmaps_base.no_use_local) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_NO_USE_LOCAL,
                                                              ORTE_UNDEF, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    if (NULL == (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_NO_OVERSUB))) {
        /* was NOT provided - add it if it was set by the environment. Note that this
        * attribute only cares if it exists - its value is irrelevant and hence
        * not provided
        */
        if (!orte_rmaps_base.oversubscribe) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_NO_OVERSUB,
                                                              ORTE_UNDEF, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    /* see if they provided a desired mapper */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_DESIRED_MAPPER))) {
        /* they did - extract its name */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&desired_mapper, attr->value, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
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
        return ORTE_ERR_NOT_FOUND;
    }
    
    if (ORTE_SUCCESS != (rc = module->map_job(job, attributes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
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
