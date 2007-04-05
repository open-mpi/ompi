/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_copy_map(orte_job_map_t **dest, orte_job_map_t *src, orte_data_type_t type)
{
    orte_std_cntr_t i;
    int rc;
    opal_list_item_t *item;
    orte_mapped_node_t *srcnode, *nodeptr;
    
    if (NULL == src) {
        *dest = NULL;
        return ORTE_SUCCESS;
    }
    
    /* create the new object */
    *dest = OBJ_NEW(orte_job_map_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    (*dest)->job = src->job;
    if (NULL != src->mapping_mode) {
        (*dest)->mapping_mode = strdup(src->mapping_mode);
    }
    (*dest)->vpid_start = src->vpid_start;
    (*dest)->vpid_range = src->vpid_range;
    (*dest)->num_apps = src->num_apps;
    
    (*dest)->apps = (orte_app_context_t**)malloc(src->num_apps * sizeof(orte_app_context_t*));
    if (NULL == (*dest)->apps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(*dest);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < src->num_apps; i++) {
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&((*dest)->apps[i]), src->apps[i], ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(*dest);
            return rc;
        }
    }
    
    (*dest)->num_nodes = src->num_nodes;
    for (item = opal_list_get_first(&(src->nodes));
         item != opal_list_get_end(&(src->nodes));
         item = opal_list_get_next(item)) {
        srcnode = (orte_mapped_node_t*)item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_copy_mapped_node(&nodeptr, srcnode, ORTE_MAPPED_NODE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(*dest);
            return rc;
        }
        opal_list_append(&((*dest)->nodes), &nodeptr->super);
    }
    
    return ORTE_SUCCESS;
}

/*
 * MAPPED_PROC
 */
int orte_rmaps_base_copy_mapped_proc(orte_mapped_proc_t **dest, orte_mapped_proc_t *src, orte_data_type_t type)
{
    if (NULL == src) {
        *dest = NULL;
        return ORTE_SUCCESS;
    }
    
    /* create the new object */
    *dest = OBJ_NEW(orte_mapped_proc_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy data into it */
    (*dest)->name = src->name;
    
    (*dest)->rank = src->rank;
    
    (*dest)->app_idx = src->app_idx;
    
    (*dest)->pid = src->pid;
    
    return ORTE_SUCCESS;
}

/*
 * MAPPED_NODE
 */
int orte_rmaps_base_copy_mapped_node(orte_mapped_node_t **dest, orte_mapped_node_t *src, orte_data_type_t type)
{
    int rc;
    opal_list_item_t *item;
    orte_mapped_proc_t *srcproc, *procptr;
    
    if (NULL == src) {
        *dest = NULL;
        return ORTE_SUCCESS;
    }
    
    /* create the new object */
    *dest = OBJ_NEW(orte_mapped_node_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy data into it */
    (*dest)->cell = src->cell;
    
    if (NULL != src->nodename) {
        (*dest)->nodename = strdup(src->nodename);
    }

    (*dest)->launch_id = src->launch_id;
    
    if (NULL != src->username) {
        (*dest)->username = strdup(src->username);
    }
    
    if (NULL != src->daemon) {
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&((*dest)->daemon), src->daemon, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(*dest);
            return rc;
        }
    }
    
    (*dest)->oversubscribed = src->oversubscribed;
    
    (*dest)->num_procs = src->num_procs;
    for (item = opal_list_get_first(&(src->procs));
         item != opal_list_get_end(&(src->procs));
         item = opal_list_get_next(item)) {
        srcproc = (orte_mapped_proc_t*)item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_copy_mapped_proc(&procptr, srcproc, ORTE_MAPPED_PROC))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(*dest);
            return rc;
        }
        opal_list_append(&((*dest)->procs), &procptr->super);
    }
    
    return ORTE_SUCCESS;
    
}

