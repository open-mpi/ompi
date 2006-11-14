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
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "ns_replica.h"

/*
 * JOBID functions
 */
int orte_ns_replica_create_jobid(orte_jobid_t *jobid, opal_list_t *attrs)
{
    orte_ns_replica_jobitem_t *child, *parent, *root;
    orte_jobid_t parent_job=ORTE_JOBID_INVALID, *jptr;
    orte_attribute_t *attr;
    int rc;

    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    *jobid = ORTE_JOBID_INVALID;
    
    /* check for attributes */
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_PARENT))) {
        /* declares the specified jobid to be the parent of the new one */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        parent_job = *jptr;
    } else if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_ROOT))) {
        /* use the root of the specified job as the parent of the new one */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (NULL == (root = orte_ns_replica_find_root_job(*jptr))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        parent_job = root->jobid;
    }
    
    /* if the parent jobid is INVALID, then this is the root of a new
     * job family - create it
     */
    if (ORTE_JOBID_INVALID == parent_job) {
        root = OBJ_NEW(orte_ns_replica_jobitem_t);
        if (NULL == root) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        root->jobid = orte_ns_replica.num_jobids;
        opal_list_append(&orte_ns_replica.jobs, &root->super);
        *jobid = root->jobid;
        (orte_ns_replica.num_jobids)++;
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_SUCCESS;
    }
    
    /* if the parent jobid is not INVALID, then the request is for a
     * new child for this parent. Find the job's record
     */
    if (NULL == (parent = orte_ns_replica_find_job(parent_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* add this new job to the parent's list of children */
    child = OBJ_NEW(orte_ns_replica_jobitem_t);
    if (NULL == child) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;            
    }
    opal_list_append(&parent->children, &child->super);
    child->jobid = orte_ns_replica.num_jobids;
    *jobid = child->jobid;
    (orte_ns_replica.num_jobids)++;
    
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_replica_get_job_descendants(orte_jobid_t **descendants, orte_std_cntr_t *num_desc, orte_jobid_t job)
{
    orte_std_cntr_t i, num;
    orte_ns_replica_jobitem_t *ptr, *newptr;
    orte_jobid_t *descs;
    opal_list_t desc_list;
    opal_list_item_t *item;
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    /* default values */
    *descendants = NULL;
    *num_desc = 0;
    
    /* find this job's record on the tree */
    if (NULL == (ptr = orte_ns_replica_find_job(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* construct a flattened list of its descendants - including ourself */
    OBJ_CONSTRUCT(&desc_list, opal_list_t);
    newptr = OBJ_NEW(orte_ns_replica_jobitem_t);
    newptr->jobid = job;
    opal_list_append(&desc_list, &newptr->super);
    
    orte_ns_replica_construct_flattened_tree(&desc_list, ptr);
    
    /* count number of entries */
    num = opal_list_get_size(&desc_list);
    
    /* allocate memory for the array */
    descs = (orte_jobid_t*)malloc(num * sizeof(orte_jobid_t));
    if (NULL == descs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* now fill in the array */
    i = 0;
    while (NULL != (item = opal_list_remove_first(&desc_list))) {
        ptr = (orte_ns_replica_jobitem_t*)item;
        descs[i++] = ptr->jobid;
        OBJ_RELEASE(ptr);
    }
    OBJ_DESTRUCT(&desc_list);
    
    *descendants = descs;
    *num_desc = num;
    
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_replica_get_job_children(orte_jobid_t **children, orte_std_cntr_t *num_childs, orte_jobid_t job)
{
    orte_std_cntr_t i, num;
    orte_ns_replica_jobitem_t *ptr, *newptr;
    orte_jobid_t *descs;
    opal_list_item_t *item;
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    /* default values */
    *children = NULL;
    *num_childs = 0;
    
    /* find this job's record on the tree */
    if (NULL == (ptr = orte_ns_replica_find_job(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* count number of entries in our direct children - include ourselves */
    num = 1 + opal_list_get_size(&ptr->children);
    
    /* allocate memory for the array */
    descs = (orte_jobid_t*)malloc(num * sizeof(orte_jobid_t));
    if (NULL == descs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* now fill in the array - put ourselves first */
    descs[0] = job;
    i = 1;
    for (item = opal_list_get_first(&ptr->children);
         item != opal_list_get_end(&ptr->children);
         item = opal_list_get_next(item)) {
        newptr = (orte_ns_replica_jobitem_t*)item;
        descs[i++] = newptr->jobid;
    }
    
    *children = descs;
    *num_childs = num;
    
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_replica_get_root_job(orte_jobid_t *root_job, orte_jobid_t job)
{
    orte_ns_replica_jobitem_t *root;
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    if (NULL == (root = orte_ns_replica_find_root_job(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;
    }
    
    *root_job = root->jobid;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_replica_get_parent_job(orte_jobid_t *parent_job, orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_ns_replica_jobitem_t *root, *ptr, *parent;
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    /* find this job's parent object */
    for (item = opal_list_get_first(&orte_ns_replica.jobs);
         item != opal_list_get_end(&orte_ns_replica.jobs);
         item = opal_list_get_next(item)) {
        root = (orte_ns_replica_jobitem_t*)item;
        if (NULL != (ptr = down_search(root, &parent, job))) {
            goto REPORT;
        }
    }
    /* don't report an error if not found, just return invalid */
    *parent_job = ORTE_JOBID_INVALID;
    return ORTE_ERR_NOT_FOUND;
    
REPORT:
    /* return the info */
    *parent_job = parent->jobid;
    
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_replica_reserve_range(orte_jobid_t job, orte_vpid_t range,
                                  orte_vpid_t *start)
{
    orte_ns_replica_jobitem_t *ptr;

    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    /* find the job's record */
    if (NULL == (ptr =  orte_ns_replica_find_job(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_NOT_FOUND;        
    }
    
    if ((ORTE_VPID_MAX-range-(ptr->next_vpid)) > 0) {
        *start = ptr->next_vpid;
        ptr->next_vpid += range;
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_SUCCESS;
    }

    /* get here if the range isn't available */
    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_ERR_OUT_OF_RESOURCE;
}
