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

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/errmgr/errmgr.h"

#include "ns_replica.h"

orte_ns_replica_jobitem_t *down_search(orte_ns_replica_jobitem_t *ptr,
                                       orte_ns_replica_jobitem_t **parent_ptr,
                                       orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_ns_replica_jobitem_t *ptr2, *ptr3;
    
    /* check if this is the specified job */
    if (ptr->jobid == job) {
        return ptr;
    }
    
    /* otherwise, look at the children of this ptr. call ourselves
     * to check each one
     */
    for (item = opal_list_get_first(&ptr->children);
         item != opal_list_get_end(&ptr->children);
         item = opal_list_get_next(item)) {
        ptr2 = (orte_ns_replica_jobitem_t*)item;
        *parent_ptr = ptr;
        if (NULL != (ptr3 = down_search(ptr2, parent_ptr, job))) {
            return ptr3;
        }
    }

    return NULL;
}

/* find a job's record, wherever it is on the tree */
orte_ns_replica_jobitem_t* orte_ns_replica_find_job(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_ns_replica_jobitem_t *root, *ptr, *parent;
    
    for (item = opal_list_get_first(&orte_ns_replica.jobs);
         item != opal_list_get_end(&orte_ns_replica.jobs);
         item = opal_list_get_next(item)) {
        root = (orte_ns_replica_jobitem_t*)item;
        if (NULL != (ptr = down_search(root, &parent, job))) {
            return ptr;
        }
    }
    
    /* don't report an error if not found, just return NULL */
    return NULL;    
}

/* given a jobid, find it's root job's object */
orte_ns_replica_jobitem_t* orte_ns_replica_find_root_job(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_ns_replica_jobitem_t *root, *ptr, *parent;
    
    for (item = opal_list_get_first(&orte_ns_replica.jobs);
         item != opal_list_get_end(&orte_ns_replica.jobs);
         item = opal_list_get_next(item)) {
        root = (orte_ns_replica_jobitem_t*)item;
        
        if (NULL != (ptr = down_search(root, &parent, job))) {
            return root;
        }
    }

    /* don't report an error if not found, just return NULL */
    return NULL;
}

/* given a job's record, construct a flattened list of the descendants below it,
 * including the starting point
 */
void orte_ns_replica_construct_flattened_tree(opal_list_t *tree, orte_ns_replica_jobitem_t *ptr)
{
    orte_ns_replica_jobitem_t *job, *newjob;
    opal_list_item_t *item;
    
    for (item = opal_list_get_first(&ptr->children);
         item != opal_list_get_end(&ptr->children);
         item = opal_list_get_next(item)) {
        job = (orte_ns_replica_jobitem_t*)item;
        
        newjob = OBJ_NEW(orte_ns_replica_jobitem_t);
        newjob->jobid = job->jobid;
        newjob->next_vpid = job->next_vpid;
        opal_list_append(tree, &newjob->super);
        
        orte_ns_replica_construct_flattened_tree(tree, job); /* get anyone below this one */
    }
}
