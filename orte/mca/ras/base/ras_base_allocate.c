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
#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/class/opal_list.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/ras/base/proxy/ras_base_proxy.h"
#include "orte/mca/ras/base/ras_private.h"

/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_ras_base_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    int ret;
    opal_list_item_t *item;
    orte_ras_base_cmp_t *cmp;
    opal_list_t nodes;
    orte_attribute_t * attr;
    orte_jobid_t * jptr;

    /* so there are a lot of possibilities here */
    /* Case 1: we are not on the head node, so use the proxy component */
    if (!orte_process_info.seed) {
        return orte_ras_base_proxy_allocate(jobid, attributes);
    }

    /* Case 2: We want to use our parent's allocation. This can occur if we 
     * are doing a dynamic process spawn and don't want to do go through 
     * the allocators again. */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RAS_USE_PARENT_ALLOCATION))) {
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:base:allocate: reallocating parent's allocation as our own");
        /* attribute was given - just reallocate to the new jobid */
        if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        if (ORTE_SUCCESS != (ret = orte_ras_base_reallocate(*jptr, jobid))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        return ORTE_SUCCESS;
    }
    
    /* Case 3: We want to get a new allocation. This can happen if we
     * are spawning a new process that does not want to use its parent's
     * allocation.  */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RAS_USE_NEW_ALLOCATION))) {
        /* If no components are available, then return an error */
        if (opal_list_is_empty(&orte_ras_base.ras_available)) {
            opal_output(orte_ras_base.ras_output,
                        "orte:ras:base:allocate: no components available!");
            ret = ORTE_ERR_NOT_FOUND;
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        
        /* Otherwise, go through the [already sorted in priority order]
        * list and call them until one of them puts something on
        * the node segment */
        for (item = opal_list_get_first(&orte_ras_base.ras_available);
             item != opal_list_get_end(&orte_ras_base.ras_available);
             item = opal_list_get_next(item)) {
            cmp = (orte_ras_base_cmp_t *) item;
            opal_output(orte_ras_base.ras_output,
                        "orte:ras:base:allocate: attemping to allocate using module: %s",
                        cmp->component->ras_version.mca_component_name);
            
            if (NULL != cmp->module->allocate_job) {
                ret = cmp->module->allocate_job(jobid, attributes);
                if (ORTE_SUCCESS == ret) {
                    bool empty;
                    
                    if (ORTE_SUCCESS != 
                        (ret = orte_ras_base_node_segment_empty(&empty))) {
                        ORTE_ERROR_LOG(ret);
                        return ret;
                    }
                    
                    /* If this module put something on the node segment,
                        we're done */
                    
                    if (!empty) {
                        opal_output(orte_ras_base.ras_output,
                                    "orte:ras:base:allocate: found good module: %s",
                                    cmp->component->ras_version.mca_component_name);
                        return ORTE_SUCCESS;
                    }
                }
            }
        }
        
        /* We didn't find anyone who put anything on the node segment */
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:base:allocate: no module put anything in the node segment");
        ret = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* Case 4: no RAS-specific directive was passed. This means that if the node segment is empty, we
     * want to allocate new nodes. Otherwise allocate all the existing nodes to
     * our job */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    /* See if there are any nodes already on the registry. Most of the time
     * these would have been put there by the RDS reading the hostfile. */
    if (ORTE_SUCCESS != (ret = orte_ras_base_node_query(&nodes))) {
        OBJ_DESTRUCT(&nodes);
        return ret;
    }
    /* If there are any nodes at all, allocate them all to this job */
    if (!opal_list_is_empty(&nodes)) {
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:base:allocate: reallocating nodes that are already on registry");
        ret = orte_ras_base_allocate_nodes(jobid, &nodes);
        OBJ_DESTRUCT(&nodes);
        return ret;
    }

    /* there were no nodes already on the registry, so get them from the
     * RAS components */
 
    /* If no components are available, then return an error */
    if (opal_list_is_empty(&orte_ras_base.ras_available)) {
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:base:allocate: no components available!");
        ret = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* Otherwise, go through the [already sorted in priority order]
     * list and initialize them until one of them puts something on
     * the node segment */
    for (item = opal_list_get_first(&orte_ras_base.ras_available);
         item != opal_list_get_end(&orte_ras_base.ras_available);
         item = opal_list_get_next(item)) {
        cmp = (orte_ras_base_cmp_t *) item;
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:base:allocate: attemping to allocate using module: %s",
                    cmp->component->ras_version.mca_component_name);

        if (NULL != cmp->module->allocate_job) {
            ret = cmp->module->allocate_job(jobid, attributes);
            if (ORTE_SUCCESS == ret) {
                bool empty;

                if (ORTE_SUCCESS != 
                    (ret = orte_ras_base_node_segment_empty(&empty))) {
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }

                /* If this module put something on the node segment,
                   we're done */

                if (!empty) {
                    opal_output(orte_ras_base.ras_output,
                                "orte:ras:base:allocate: found good module: %s",
                                cmp->component->ras_version.mca_component_name);
                    return ORTE_SUCCESS;
                }
            }
        }
    }

    /* We didn't find anyone who put anything on the node segment */
    opal_output(orte_ras_base.ras_output,
                "orte:ras:base:allocate: no module put anything in the node segment");
    ret = ORTE_ERR_NOT_FOUND;
    ORTE_ERROR_LOG(ret);
    return ret;
}

int orte_ras_base_deallocate(orte_jobid_t job)
{
    /* if we are not a HNP, then use proxy */
    if (!orte_process_info.seed) {
        return orte_ras_base_proxy_deallocate(job);
    }
    return ORTE_SUCCESS;
}


/*
 * Reallocate nodes so another jobid can use them in addition to the
 * specified one
 */
int orte_ras_base_reallocate(orte_jobid_t parent_jobid,
                             orte_jobid_t child_jobid)
{
    opal_list_t current_alloc;
    opal_list_item_t *item;
    int rc;

    
    OBJ_CONSTRUCT(&current_alloc, opal_list_t);
    
    if (ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&current_alloc, parent_jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&current_alloc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ras_base_node_assign(&current_alloc, child_jobid))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* clean up memory */
    while (NULL != (item = opal_list_remove_first(&current_alloc))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&current_alloc);
    
    return rc;
}
