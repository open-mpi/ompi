/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
/** @file:
 *
 */
#include "orte_config.h"

#include <stdio.h>
#include <string.h>

#include "dps/dps.h"
#include "opal/threads/mutex.h"

#include "util/output.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"

/**
 * globals
 */

/*
 * functions
 */

int orte_ns_replica_create_cellid(orte_cellid_t *cellid, char *site, char *resource)
{
    orte_ns_replica_cell_tracker_t *new_cell;
    
    OPAL_THREAD_LOCK(&orte_ns_replica_mutex);

    if (ORTE_CELLID_MAX > orte_ns_replica_next_cellid) {
        *cellid = orte_ns_replica_next_cellid;
	    orte_ns_replica_next_cellid++;
        new_cell = OBJ_NEW(orte_ns_replica_cell_tracker_t);
        if (NULL == new_cell) {
            *cellid = ORTE_CELLID_MAX;
            OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        new_cell->cell = *cellid;
        new_cell->site = strdup(site);
        new_cell->resource = strdup(resource);
        opal_list_append(&orte_ns_replica_cell_tracker, &new_cell->item);
	   OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
	   return ORTE_SUCCESS;
    }
    
    *cellid = ORTE_CELLID_MAX;
	OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
	return ORTE_ERR_OUT_OF_RESOURCE;
}

int orte_ns_replica_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource)
{
    opal_list_item_t *item;
    orte_ns_replica_cell_tracker_t *cell;
    
    for (item = opal_list_get_first(&orte_ns_replica_cell_tracker);
         item != opal_list_get_end(&orte_ns_replica_cell_tracker);
         item = opal_list_get_next(item)) {
        cell = (orte_ns_replica_cell_tracker_t*)item;
        if (cellid == cell->cell) {
            *site = strdup(cell->site);
            *resource = strdup(cell->resource);
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}
                                

int orte_ns_replica_create_jobid(orte_jobid_t *jobid)
{
    orte_ns_replica_name_tracker_t *new_nt;

    OPAL_THREAD_LOCK(&orte_ns_replica_mutex);

    if (ORTE_JOBID_MAX > orte_ns_replica_next_jobid) {
        *jobid = orte_ns_replica_next_jobid;
	    orte_ns_replica_next_jobid++;
	    new_nt = OBJ_NEW(orte_ns_replica_name_tracker_t);
        if (NULL == new_nt) {  /* out of memory */
            *jobid = ORTE_JOBID_MAX;
            OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
	    new_nt->job = *jobid;
	    new_nt->last_used_vpid = 0;
	    opal_list_append(&orte_ns_replica_name_tracker, &new_nt->item);
	    OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
	    return ORTE_SUCCESS;
    }
    
    *jobid = ORTE_JOBID_MAX;
	OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
	return ORTE_ERR_OUT_OF_RESOURCE;
}


int orte_ns_replica_reserve_range(orte_jobid_t job, orte_vpid_t range, orte_vpid_t *start)
{
    orte_ns_replica_name_tracker_t *ptr;

    OPAL_THREAD_LOCK(&orte_ns_replica_mutex);

    for (ptr = (orte_ns_replica_name_tracker_t*)opal_list_get_first(&orte_ns_replica_name_tracker);
	   ptr != (orte_ns_replica_name_tracker_t*)opal_list_get_end(&orte_ns_replica_name_tracker);
	   ptr = (orte_ns_replica_name_tracker_t*)opal_list_get_next(ptr)) {
	   if (job == ptr->job) { /* found the specified job */
	       if ((ORTE_VPID_MAX-range) >= ptr->last_used_vpid) {  /* requested range available */
		      *start = ptr->last_used_vpid;
		      if (0 == job && *start == 0) { /* vpid=0 reserved for job=0 */
		          *start = 1;
		      }
		      ptr->last_used_vpid = *start + range;
		      OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
		      return ORTE_SUCCESS;
	       } else {  /* range not available */
                *start = ORTE_VPID_MAX;
                OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
           }
	   }
    }
    
    /* get here if the job couldn't be found */
    OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    return ORTE_ERR_NOT_FOUND;
}

int orte_ns_replica_assign_rml_tag(orte_rml_tag_t *tag,
                                   char *name)
{
    orte_ns_replica_tagitem_t *tagitem;
    
    OPAL_THREAD_LOCK(&orte_ns_replica_mutex);

    if (NULL != name) {
        /* see if this name is already in list - if so, return tag */
        for (tagitem = (orte_ns_replica_tagitem_t*)opal_list_get_first(&orte_ns_replica_taglist);
             tagitem != (orte_ns_replica_tagitem_t*)opal_list_get_end(&orte_ns_replica_taglist);
             tagitem = (orte_ns_replica_tagitem_t*)opal_list_get_next(tagitem)) {
            if (tagitem->name != NULL && 0 == strcmp(name, tagitem->name)) { /* found name on list */
                *tag = tagitem->tag;
                OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
                return ORTE_SUCCESS;
            }
        }
    }
      
    /* not in list or not provided, so allocate next tag
     * first check to see if one available - else error
     */
    if (ORTE_RML_TAG_MAX > orte_ns_replica_next_rml_tag) {
        /* okay, one available - assign it */
        tagitem = OBJ_NEW(orte_ns_replica_tagitem_t);
        if (NULL == tagitem) { /* out of memory */
            *tag = ORTE_RML_TAG_MAX;
            OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        tagitem->tag = orte_ns_replica_next_rml_tag;
        if (NULL != name) {  /* provided - can look it up later */
            tagitem->name = strdup(name);
        } else {
            tagitem->name = NULL;
        }
        orte_ns_replica_next_rml_tag++;
        opal_list_append(&orte_ns_replica_taglist, &tagitem->item);
    
        *tag = tagitem->tag;
        OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
        return ORTE_SUCCESS;
    }
    
    /* no tag available */
    *tag = ORTE_RML_TAG_MAX;
    OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    return ORTE_ERR_OUT_OF_RESOURCE;

}


int orte_ns_replica_define_data_type(const char *name,
                                     orte_data_type_t *type)
{
    orte_ns_replica_dti_t *dti;
    
    if (NULL == name || 0 < *type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    OPAL_THREAD_LOCK(&orte_ns_replica_mutex);

    /* see if this name is already in list - if so, return id */
    for (dti = (orte_ns_replica_dti_t*)opal_list_get_first(&orte_ns_replica_dtlist);
         dti != (orte_ns_replica_dti_t*)opal_list_get_end(&orte_ns_replica_dtlist);
         dti = (orte_ns_replica_dti_t*)opal_list_get_next(dti)) {
        if (dti->name != NULL && 0 == strcmp(name, dti->name)) { /* found name on list */
            *type = dti->id;
            OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
            return ORTE_SUCCESS;
        }
    }
      
    /* not in list or not provided, so allocate next id
     * first check to see if one available - else error
     */
    if (ORTE_DPS_ID_MAX > orte_ns_replica_next_dti) {
        /* okay, one available - assign it */
        dti = OBJ_NEW(orte_ns_replica_dti_t);
        if (NULL == dti) { /* out of memory */
            *type = ORTE_DPS_ID_MAX;
            OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        dti->id = orte_ns_replica_next_dti;
        dti->name = strdup(name);
        orte_ns_replica_next_dti++;
        opal_list_append(&orte_ns_replica_dtlist, &dti->item);
    
        *type = dti->id;
        OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
        return ORTE_SUCCESS;
    }
    
    /* no id available */
    *type = ORTE_DPS_ID_MAX;
    OPAL_THREAD_UNLOCK(&orte_ns_replica_mutex);
    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    return ORTE_ERR_OUT_OF_RESOURCE;

}


int orte_ns_replica_create_my_name(void)
{
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, 1, &vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                0, jobid, vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}
