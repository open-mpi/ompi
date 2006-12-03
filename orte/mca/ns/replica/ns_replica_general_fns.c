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
/** @file:
 *
 */
#include "orte_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/threads/mutex.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"
#include "ns_replica.h"


/***   GET PEERS   ***/
int orte_ns_replica_get_peers(orte_process_name_t **procs,
                              orte_std_cntr_t *num_procs, opal_list_t *attrs)
{
    orte_std_cntr_t i, isave, npeers;
    orte_jobid_t *jptr;
    orte_cellid_t *cptr;
    orte_attribute_t *attr;
    orte_ns_replica_jobitem_t *job_info, *child;
    opal_list_item_t *item;
    opal_list_t peerlist;
    int rc;
    
    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    /* set default value */
    *procs = NULL;
    *num_procs = 0;
    
    /* check the attributes to see if USE_JOB or USE_CELL has been set. If not, then this is
     * a request for my own job peers - process that one locally
     */

    /* if the cell is given AND it matches my own, then we can process this
     * quickly. Otherwise, we have to do some more work.
     *
     * RHC: when we go multi-cell, we need a way to find all the cells upon
     * which a job is executing so we can make this work!
     */
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_CELL))) {
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&cptr, attr->value, ORTE_CELLID))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return rc;
        }
        if (*cptr != ORTE_PROC_MY_NAME->cellid && *cptr != ORTE_CELLID_WILDCARD) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return ORTE_ERR_NOT_IMPLEMENTED;            
        }
    }

    if (NULL == (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_JOBID))) {
        /* get my own job peers, assuming all are on this cell */
        *procs = (orte_process_name_t*)malloc(orte_process_info.num_procs * sizeof(orte_process_name_t));
        if (NULL == *procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        for (i=0; i < orte_process_info.num_procs; i++) {
            (*procs)[i].cellid = ORTE_PROC_MY_NAME->cellid;
            (*procs)[i].jobid = ORTE_PROC_MY_NAME->jobid;
            (*procs)[i].vpid = orte_process_info.vpid_start + i;
        }
        
        *num_procs = orte_process_info.num_procs;
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_SUCCESS;
    }
    
    /* we get here if the job attribute was passed to us - use that jobid */
    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    
    /* look up this job's record on the tracking database */
    if (NULL == (job_info = orte_ns_replica_find_job(*jptr))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_DESCENDANTS))) {
        /* we want the peers from this job AND ALL of its descendants - start by constructing
         * a flattened list of the descendant jobs
         */
        OBJ_CONSTRUCT(&peerlist, opal_list_t);
        child = OBJ_NEW(orte_ns_replica_jobitem_t);
        child->jobid = job_info->jobid;
        child->next_vpid = job_info->next_vpid;
        opal_list_append(&peerlist, &child->super);  /* add the current job to the list */
        orte_ns_replica_construct_flattened_tree(&peerlist, job_info);
        
        i = opal_list_get_size(&peerlist);
        npeers = 0;
        if (0 < i) {
            for (item = opal_list_get_first(&peerlist);
                 item != opal_list_get_end(&peerlist);
                 item = opal_list_get_next(item)) {
                child = (orte_ns_replica_jobitem_t*)item;
                npeers += child->next_vpid;
            }
            if (0 >= npeers) {
                *num_procs = npeers;
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_SUCCESS;
            }
            
            *procs = (orte_process_name_t*)malloc(npeers * sizeof(orte_process_name_t));
            if (NULL == *procs) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            
            /* populate it from the list */
            isave = 0;
            while (NULL != (item = opal_list_remove_first(&peerlist))) {
                child = (orte_ns_replica_jobitem_t*)item;
                for (i=0; i < child->next_vpid; i++) {
                    (*procs)[i+isave].cellid = ORTE_PROC_MY_NAME->cellid;
                    (*procs)[i+isave].jobid = child->jobid;
                    (*procs)[i+isave].vpid = i;
                }
                isave += child->next_vpid;
            }
        }
        *num_procs = npeers;
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_SUCCESS;
    }
    
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_INCLUDE_CHILDREN))) {
        /* we want the peers from this job AND ONLY its immediate children */
        
        /* determine the number of peers we are going to have */
        npeers = job_info->next_vpid;
        for (item = opal_list_get_first(&job_info->children);
             item != opal_list_get_end(&job_info->children);
             item = opal_list_get_next(item)) {
            child = (orte_ns_replica_jobitem_t*)item;
            npeers += child->next_vpid;
        }
        
        /* create the array */
        if (0 < npeers) {
            *procs = (orte_process_name_t*)malloc(npeers * sizeof(orte_process_name_t));
            if (NULL == *procs) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            
            /* populate it, starting with the specified job followed by its children */
            for (i=0; i < job_info->next_vpid; i++) {
                (*procs)[i].cellid = ORTE_PROC_MY_NAME->cellid;
                (*procs)[i].jobid = *jptr;
                (*procs)[i].vpid = i;
            }
            isave = job_info->next_vpid;
            for (item = opal_list_get_first(&job_info->children);
                 item != opal_list_get_end(&job_info->children);
                 item = opal_list_get_next(item)) {
                child = (orte_ns_replica_jobitem_t*)item;
                for (i=0; i < child->next_vpid; i++) {
                    (*procs)[i+isave].cellid = ORTE_PROC_MY_NAME->cellid;
                    (*procs)[i+isave].jobid = child->jobid;
                    (*procs)[i+isave].vpid = i;
                }
                isave += child->next_vpid;
            }
        }
        *num_procs = npeers;
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_SUCCESS;
    }
    
    /* get here if we want just the peers for the specified job */
    
    /* create the array of peers */
    if (0 < job_info->next_vpid) {
        *procs = (orte_process_name_t*)malloc(job_info->next_vpid * sizeof(orte_process_name_t));
        if (NULL == *procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        for (i=0; i < job_info->next_vpid; i++) {
            (*procs)[i].cellid = ORTE_PROC_MY_NAME->cellid;
            (*procs)[i].jobid = *jptr;
            (*procs)[i].vpid = i;
        }
    }
    
    *num_procs = job_info->next_vpid;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}



/*
 * TAG SERVER functions
 */
int orte_ns_replica_assign_rml_tag(orte_rml_tag_t *tag,
                                   char *name)
{
    orte_ns_replica_tagitem_t *tagitem, **tags;
    orte_std_cntr_t i;
    orte_rml_tag_t j;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    if (NULL != name) {
        /* see if this name is already in list - if so, return tag */
        tags = (orte_ns_replica_tagitem_t**)orte_ns_replica.tags->addr;
        for (i=0, j=0; j < orte_ns_replica.num_tags &&
                       i < (orte_ns_replica.tags)->size; i++) {
            if (NULL != tags[i]) {
                j++;
                if (tags[i]->name != NULL &&
                    0 == strcmp(name, tags[i]->name)) { /* found name on list */
                    *tag = tags[i]->tag;
                    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                    return ORTE_SUCCESS;
                }
            }
        }
    }

    /* not in list or not provided, so allocate next tag */
    *tag = ORTE_RML_TAG_MAX;

    /* check if tag is available - need to do this since the tag type
     * is probably not going to be a orte_std_cntr_t, so we cannot just rely
     * on the pointer_array's size limits to protect us. NOTE: need to
     * reserve ORTE_RML_TAG_MAX as an invalid value, so can't let
     * num_tags get there
      */
    if (ORTE_RML_TAG_MAX-2 < orte_ns_replica.num_tags) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    tagitem = OBJ_NEW(orte_ns_replica_tagitem_t);
    if (NULL == tagitem) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_replica.tags, tagitem))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    tagitem->tag = orte_ns_replica.num_tags + ORTE_RML_TAG_DYNAMIC;
    (orte_ns_replica.num_tags)++;
    if (NULL != name) {  /* provided - can look it up later */
        tagitem->name = strdup(name);
    } else {
        tagitem->name = NULL;
    }

    *tag = tagitem->tag;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


/*
 * DATA TYPE SERVER functions
 */
int orte_ns_replica_define_data_type(const char *name,
                                     orte_data_type_t *type)
{
    orte_ns_replica_dti_t **dti, *dtip;
    orte_std_cntr_t i, j;
    int rc;

    if (NULL == name || 0 < *type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    dti = (orte_ns_replica_dti_t**)orte_ns_replica.dts->addr;
    for (i=0, j=0; j < orte_ns_replica.num_dts &&
                   i < orte_ns_replica.dts->size; i++) {
        if (NULL != dti[i]) {
            j++;
            if (dti[i]->name != NULL &&
                0 == strcmp(name, dti[i]->name)) { /* found name on list */
                *type = dti[i]->id;
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_SUCCESS;
            }
        }
    }

    /* not in list or not provided, so allocate next id */
    *type = ORTE_DSS_ID_MAX;

    /* check if id is available - need to do this since the data type
     * is probably not going to be a orte_std_cntr_t, so we cannot just rely
     * on the pointer_array's size limits to protect us.
      */
    if (ORTE_DSS_ID_MAX-2 < orte_ns_replica.num_dts) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
     }

    dtip = OBJ_NEW(orte_ns_replica_dti_t);
    if (NULL == dtip) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    dtip->name = strdup(name);
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_replica.dts, dtip))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    dtip->id = orte_ns_replica.num_dts;
    (orte_ns_replica.num_dts)++;

    *type = dtip->id;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

/*
 * NAME functions
 */
int orte_ns_replica_create_my_name(void)
{
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    opal_list_t attrs;
    int rc;

    OBJ_CONSTRUCT(&attrs, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobid, &attrs))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&attrs);
        return rc;
    }
    OBJ_DESTRUCT(&attrs);
    
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

